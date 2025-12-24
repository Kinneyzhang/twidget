;;; twidget-reactive.el --- Vue3-style Reactivity System for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author:  Twidget Team
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords:  reactive, data-binding

;;; Commentary:

;; This package provides a Vue3-style reactivity system for Emacs Lisp.
;; It supports reactive objects, ref, computed properties, and watch functionality.

;;; Code:

(require 'cl-lib)
(require 'subr-x)


;;; ============================================================================
;;; 核心响应式系统
;;; ============================================================================

;; 当前正在执行的 effect
(defvar twidget--active-effect nil
  "The currently active effect being executed.")

;; effect 栈，支持嵌套 effect
(defvar twidget--effect-stack '()
  "Stack of effects for handling nested effects.")

;; 全局依赖存储桶：WeakMap<target, Map<key, Set<effect>>>
(defvar twidget--target-map (make-hash-table :test 'eq :weakness 'key)
  "WeakMap storing dependencies:  target -> (key -> effects).")

;; 标记是否应该追踪依赖
(defvar twidget--should-track t
  "Flag indicating whether dependency tracking is enabled.")

;; 用于标识响应式对象的符号
(defconst twidget--reactive-flag '__twidget_reactive__
  "Symbol used to mark reactive objects.")

(defconst twidget--ref-flag '__twidget_ref__
  "Symbol used to mark ref objects.")

(defconst twidget--readonly-flag '__twidget_readonly__
  "Symbol used to mark readonly objects.")

(defconst twidget--raw-flag '__twidget_raw__
  "Symbol used to store raw object reference.")

;;; ============================================================================
;;; Effect 副作用系统
;;; ============================================================================

(cl-defstruct (twidget-effect (:constructor twidget-effect--create))
  "Structure representing a reactive effect."
  (fn nil :documentation "The effect function to execute.")
  (deps nil :documentation "Set of dependency sets this effect belongs to.")
  (scheduler nil :documentation "Custom scheduler function.")
  (on-stop nil :documentation "Callback when effect is stopped.")
  (active t :documentation "Whether this effect is active.")
  (lazy nil :documentation "Whether to skip initial execution."))

(cl-defstruct (twidget-runner (:constructor twidget-runner--create))
  "Structure representing a runner function for reactive effects."
  (effect nil :documentation "The associated effect.")
  (run nil :documentation "The function to run the effect."))

(defun twidget--cleanup-effect (effect)
  "Clean up EFFECT by removing it from all its dependency sets.
DEPS is a hash table used as a set (keys are the dep-sets this effect belongs to)."
  (let ((deps (twidget-effect-deps effect)))
    (when deps
      ;; For each dep-set that this effect belongs to, remove this effect
      (maphash (lambda (dep-set _)
                 (remhash effect dep-set))
               deps)
      (clrhash deps))))

(defun twidget--run-effect (effect)
  "Run EFFECT and track its dependencies."
  (when (twidget-effect-active effect)
    ;; 清除旧的依赖
    (twidget--cleanup-effect effect)
    ;; 压入 effect 栈
    (push effect twidget--effect-stack)
    (setq twidget--active-effect effect)
    (unwind-protect
        (funcall (twidget-effect-fn effect))
      ;; 弹出 effect 栈
      (pop twidget--effect-stack)
      (setq twidget--active-effect (car twidget--effect-stack)))))

(defun twidget-effect (fn &optional options)
  "Create a reactive effect that runs FN. 
OPTIONS is a plist with the following keys:
  :scheduler - custom scheduler function
  :lazy - if t, don't run immediately
  :on-stop - callback when effect is stopped

Return the effect runner (a twidget-runner struct)."
  (let* ((scheduler (plist-get options :scheduler))
         (lazy (plist-get options :lazy))
         (on-stop (plist-get options :on-stop))
         (effect (twidget-effect--create
                  :fn fn
                  :deps (make-hash-table :test 'eq)
                  :scheduler scheduler
                  :on-stop on-stop
                  :active t
                  :lazy lazy))
         (runner (twidget-runner--create
                  :effect effect
                  :run (lambda () (twidget--run-effect effect)))))
    ;; 非 lazy 模式立即执行
    (unless lazy
      (funcall (twidget-runner-run runner)))
    runner))

(defun twidget-stop (runner)
  "Stop a reactive effect by its RUNNER."
  (let ((effect (if (twidget-runner-p runner)
                    (twidget-runner-effect runner)
                  ;; Fallback for legacy usage
                  (get runner 'twidget-effect))))
    (when (and effect (twidget-effect-active effect))
      (twidget--cleanup-effect effect)
      (when (twidget-effect-on-stop effect)
        (funcall (twidget-effect-on-stop effect)))
      (setf (twidget-effect-active effect) nil))))

;;; ============================================================================
;;; 依赖收集与触发
;;; ============================================================================

(defun twidget-track (target key)
  "Track dependency for TARGET's KEY.
Uses hash tables internally:
- deps-map: hash table mapping keys to dep-sets
- dep-set: hash table used as a set of effects (effect -> t)"
  (when (and twidget--should-track twidget--active-effect)
    (let ((deps-map (gethash target twidget--target-map)))
      ;; 如果 target 没有 deps-map，创建一个
      (unless deps-map
        (setq deps-map (make-hash-table :test 'equal))
        (puthash target deps-map twidget--target-map))
      ;; 获取或创建 key 对应的 dep-set
      (let ((dep-set (gethash key deps-map)))
        (unless dep-set
          (setq dep-set (make-hash-table :test 'eq))
          (puthash key dep-set deps-map))
        ;; 双向关联：effect 记录自己的 deps，dep-set 记录 effect
        (puthash twidget--active-effect t dep-set)
        (puthash dep-set t (twidget-effect-deps twidget--active-effect))))))

(defun twidget-trigger (target key)
  "Trigger effects for TARGET's KEY."
  (let ((deps-map (gethash target twidget--target-map)))
    (when deps-map
      (let ((dep-set (gethash key deps-map))
            (effects-to-run '()))
        (when dep-set
          ;; 收集需要执行的 effects
          (maphash
           (lambda (effect _)
             ;; 避免在 effect 执行过程中触发自身
             (unless (eq effect twidget--active-effect)
               (push effect effects-to-run)))
           dep-set)
          ;; 执行 effects
          (dolist (effect effects-to-run)
            (if (twidget-effect-scheduler effect)
                (funcall (twidget-effect-scheduler effect) effect)
              (twidget--run-effect effect))))))))

(defun twidget-pause-tracking ()
  "Pause dependency tracking."
  (setq twidget--should-track nil))

(defun twidget-resume-tracking ()
  "Resume dependency tracking."
  (setq twidget--should-track t))

(defmacro twidget-without-tracking (&rest body)
  "Execute BODY without tracking dependencies."
  `(progn
     (twidget-pause-tracking)
     (unwind-protect
         (progn ,@body)
       (twidget-resume-tracking))))

;;; ============================================================================
;;; Ref：基础响应式引用
;;; ============================================================================

(cl-defstruct (twidget-ref (:constructor twidget-ref--create))
  "A reactive reference that holds a single value."
  (value nil :documentation "The current value."))

(defun twidget-ref (value)
  "Create a reactive reference with initial VALUE. 
The value can be accessed and modified via `twidget-ref-value'."
  (twidget-ref--create :value value))

(defun twidget-ref-p (obj)
  "Return t if OBJ is a ref."
  (cl-typep obj 'twidget-ref))

(defun twidget-ref-get (ref)
  "Get the value of REF with dependency tracking."
  (twidget-track ref 'value)
  (twidget-ref-value ref))

(defun twidget-ref-set (ref new-value)
  "Set REF to NEW-VALUE and trigger updates."
  (let ((old-value (twidget-ref-value ref)))
    (unless (equal old-value new-value)
      (setf (twidget-ref-value ref) new-value)
      (twidget-trigger ref 'value))))

(defmacro twidget-ref-inc (ref &optional delta)
  "Increment REF by DELTA (default 1)."
  `(twidget-ref-set ,ref (+ (twidget-ref-get ,ref) ,(or delta 1))))

(defmacro twidget-ref-dec (ref &optional delta)
  "Decrement REF by DELTA (default 1)."
  `(twidget-ref-set ,ref (- (twidget-ref-get ,ref) ,(or delta 1))))

;;; ============================================================================
;;; Reactive：响应式对象（类似 Vue3 的 reactive）
;;; ============================================================================

(cl-defstruct (twidget-reactive (:constructor twidget-reactive--create))
  "A reactive object that tracks property access and modification."
  (data nil :documentation "The underlying plist/alist data.")
  (readonly nil :documentation "Flag for readonly status."))

(defun twidget-reactive (data)
  "Create a reactive object from DATA.
DATA can be a plist, alist, or hash-table."
  (cond
   ;; 如果已经是响应式对象，直接返回
   ((twidget-reactive-p data) data)
   ;; plist
   ((and (listp data) (keywordp (car data)))
    (twidget-reactive--create :data data))
   ;; alist
   ((and (listp data) (consp (car data)))
    (twidget-reactive--create
     :data (twidget--alist-to-plist data)))
   ;; hash-table
   ((hash-table-p data)
    (twidget-reactive--create
     :data (twidget--hash-to-plist data)))
   ;; 其他情况，包装为 plist
   (t
    (twidget-reactive--create :data (list :value data)))))

(defun twidget--alist-to-plist (alist)
  "Convert ALIST to plist."
  (let (result)
    (dolist (pair alist)
      (let ((key (if (keywordp (car pair))
                     (car pair)
                   (intern (format ":%s" (car pair)))))
            (value (cdr pair)))
        (push key result)
        (push value result)))
    (nreverse result)))

(defun twidget--hash-to-plist (hash)
  "Convert HASH table to plist."
  (let (result)
    (maphash (lambda (k v)
               (push v result)
               (push (if (keywordp k) k
                       (intern (format ":%s" k)))
                     result))
             hash)
    (nreverse result)))

(defun twidget-reactive-get (obj key)
  "Get KEY from reactive OBJ with dependency tracking."
  (unless (twidget-reactive-p obj)
    (error "Object is not reactive"))
  (twidget-track obj key)
  (plist-get (twidget-reactive-data obj) key))

(defun twidget-reactive-set (obj key value)
  "Set KEY to VALUE in reactive OBJ and trigger updates."
  (unless (twidget-reactive-p obj)
    (error "Object is not reactive"))
  (if (twidget-reactive-readonly obj)
      (progn
        (warn "Cannot modify readonly reactive object")
        nil)
    (let* ((data (twidget-reactive-data obj))
           (old-value (plist-get data key)))
      (unless (equal old-value value)
        (setf (twidget-reactive-data obj)
              (plist-put data key value))
        (twidget-trigger obj key)))))

(defun twidget-reactive-has (obj key)
  "Return t if reactive OBJ has KEY."
  (unless (twidget-reactive-p obj)
    (error "Object is not reactive"))
  (plist-member (twidget-reactive-data obj) key))

(defun twidget-reactive-delete (obj key)
  "Delete KEY from reactive OBJ."
  (unless (twidget-reactive-p obj)
    (error "Object is not reactive"))
  (if (twidget-reactive-readonly obj)
      (progn
        (warn "Cannot modify readonly reactive object")
        nil)
    (let ((data (twidget-reactive-data obj)))
      (when (plist-member data key)
        (setf (twidget-reactive-data obj)
              (twidget--plist-delete data key))
        (twidget-trigger obj key)))))

(defun twidget--plist-delete (plist key)
  "Delete KEY from PLIST."
  (let (result)
    (while plist
      (unless (eq (car plist) key)
        (push (car plist) result)
        (push (cadr plist) result))
      (setq plist (cddr plist)))
    (nreverse result)))

(defun twidget-reactive-keys (obj)
  "Return all keys in reactive OBJ."
  (unless (twidget-reactive-p obj)
    (error "Object is not reactive"))
  (let ((data (twidget-reactive-data obj))
        keys)
    (while data
      (push (car data) keys)
      (setq data (cddr data)))
    (nreverse keys)))

(defun twidget-reactive-to-plist (obj)
  "Convert reactive OBJ to plain plist."
  (unless (twidget-reactive-p obj)
    (error "Object is not reactive"))
  (copy-sequence (twidget-reactive-data obj)))

;;; ============================================================================
;;; Readonly：只读响应式对象
;;; ============================================================================

(defun twidget-readonly (obj)
  "Create a readonly reactive object from OBJ."
  (let ((reactive (if (twidget-reactive-p obj)
                      (copy-twidget-reactive obj)
                    (twidget-reactive obj))))
    (setf (twidget-reactive-readonly reactive) t)
    reactive))

(defun twidget-readonly-p (obj)
  "Return t if OBJ is a readonly reactive object."
  (and (twidget-reactive-p obj)
       (twidget-reactive-readonly obj)))

;;; ============================================================================
;;; Computed：计算属性
;;; ============================================================================

(cl-defstruct (twidget-computed (:constructor twidget-computed--create))
  "A computed property that caches its value."
  (getter nil :documentation "The getter function.")
  (setter nil :documentation "The setter function (optional).")
  (value nil :documentation "The cached value.")
  (dirty t :documentation "Whether the value needs recomputation.")
  (effect nil :documentation "The internal effect. "))

(defun twidget-computed (getter-or-options &optional setter)
  "Create a computed property. 
GETTER-OR-OPTIONS can be a getter function or a plist with :get and :set. 
SETTER is an optional setter function."
  (let (getter-fn setter-fn)
    (if (functionp getter-or-options)
        (setq getter-fn getter-or-options
              setter-fn setter)
      (setq getter-fn (plist-get getter-or-options :get)
            setter-fn (plist-get getter-or-options :set)))
    (let* ((computed (twidget-computed--create
                      :getter getter-fn
                      :setter setter-fn
                      :dirty t))
           ;; 创建内部 effect，使用 scheduler 延迟更新
           (effect-fn (lambda ()
                        (setf (twidget-computed-value computed)
                              (funcall getter-fn))))
           (runner (twidget-effect
                    effect-fn
                    (list :lazy t
                          :scheduler (lambda (_effect)
                                       (unless (twidget-computed-dirty computed)
                                         (setf (twidget-computed-dirty computed) t)
                                         ;; 触发依赖于此 computed 的 effects
                                         (twidget-trigger computed 'value)))))))
      (setf (twidget-computed-effect computed) runner)
      computed)))

(defun twidget-computed-get (computed)
  "Get the value of COMPUTED with dependency tracking."
  (unless (twidget-computed-p computed)
    (error "Object is not a computed property"))
  ;; 如果是脏的，重新计算
  (when (twidget-computed-dirty computed)
    (let ((runner (twidget-computed-effect computed)))
      (funcall (twidget-runner-run runner)))
    (setf (twidget-computed-dirty computed) nil))
  ;; 追踪依赖
  (twidget-track computed 'value)
  (twidget-computed-value computed))

(defun twidget-computed-set (computed value)
  "Set the value of COMPUTED using its setter."
  (unless (twidget-computed-p computed)
    (error "Object is not a computed property"))
  (if (twidget-computed-setter computed)
      (funcall (twidget-computed-setter computed) value)
    (warn "Computed property has no setter")))

;;; ============================================================================
;;; Watch：侦听器
;;; ============================================================================

(defun twidget-watch (source callback &optional options)
  "Watch SOURCE for changes and call CALLBACK. 
SOURCE can be: 
  - A ref
  - A reactive object
  - A getter function
  - A list of the above

CALLBACK receives (new-value old-value).

OPTIONS is a plist: 
  :immediate - if t, call callback immediately
  :deep - if t, deeply watch object changes
  :once - if t, only trigger once

Return a stop function."
  (let* ((immediate (plist-get options :immediate))
         (deep (plist-get options :deep))
         (once (plist-get options :once))
         (getter (twidget--create-getter source deep))
         (old-value 'twidget--initial)
         (first-run t)
         (job nil)
         (cleanup nil)
         (on-cleanup (lambda (fn) (setq cleanup fn)))
         (runner nil))
    (setq job
          (lambda ()
            (let ((new-value (funcall getter)))
              (when (or (not (equal new-value old-value))
                        deep
                        first-run)
                ;; 执行清理函数
                (when cleanup
                  (funcall cleanup)
                  (setq cleanup nil))
                ;; 调用回调
                (funcall callback new-value
                         (if first-run nil old-value)
                         on-cleanup)
                (setq old-value new-value)
                (setq first-run nil)
                ;; 如果是 once，停止 watch
                (when (and once runner)
                  (twidget-stop runner))))))
    ;; Create effect that always runs once to establish dependencies
    ;; The scheduler handles subsequent triggers
    (setq runner
          (twidget-effect
           (lambda ()
             ;; Track dependencies by calling getter
             (let ((current-value (funcall getter)))
               ;; Store initial value for comparison (only for non-immediate)
               (when (and (not immediate) first-run)
                 (setq old-value current-value)
                 (setq first-run nil))))
           (list :lazy nil  ; Always run initially to track deps
                 :scheduler (lambda (_) (funcall job)))))
    ;; 如果 immediate，立即执行回调
    (when immediate
      (funcall job))
    ;; 返回停止函数
    (lambda ()
      (twidget-stop runner)
      (when cleanup (funcall cleanup)))))

(defun twidget--create-getter (source deep)
  "Create a getter function from SOURCE."
  (cond
   ;; Ref
   ((twidget-ref-p source)
    (lambda () (twidget-ref-get source)))
   ;; Computed
   ((twidget-computed-p source)
    (lambda () (twidget-computed-get source)))
   ;; Reactive object
   ((twidget-reactive-p source)
    (if deep
        (lambda ()
          (twidget--traverse source)
          (twidget-reactive-to-plist source))
      (lambda () (twidget-reactive-to-plist source))))
   ;; Function (getter)
   ((functionp source)
    source)
   ;; List of sources
   ((listp source)
    (let ((getters (mapcar (lambda (s)
                             (twidget--create-getter s deep))
                           source)))
      (lambda ()
        (mapcar #'funcall getters))))
   (t
    (error "Invalid watch source"))))

(defun twidget--traverse (obj &optional seen)
  "Traverse OBJ to track all properties (for deep watching).
SEEN is a hash table used as a set to track already visited objects."
  (unless seen
    (setq seen (make-hash-table :test 'eq)))
  (cond
   ((twidget-reactive-p obj)
    (unless (gethash obj seen)
      (puthash obj t seen)
      (dolist (key (twidget-reactive-keys obj))
        (twidget--traverse (twidget-reactive-get obj key) seen))))
   ((twidget-ref-p obj)
    (twidget--traverse (twidget-ref-get obj) seen))
   ((listp obj)
    (dolist (item obj)
      (twidget--traverse item seen)))
   ((hash-table-p obj)
    (maphash (lambda (_k v) (twidget--traverse v seen)) obj)))
  obj)

(defun twidget-watch-effect (effect-fn &optional options)
  "Watch effect that auto-tracks dependencies.
EFFECT-FN is called immediately and re-called when dependencies change.
OPTIONS can include :on-cleanup for cleanup handling. 

Return a stop function."
  (let* ((cleanup nil)
         (on-cleanup (lambda (fn) (setq cleanup fn)))
         (runner
          (twidget-effect
           (lambda ()
             (when cleanup
               (funcall cleanup)
               (setq cleanup nil))
             (funcall effect-fn on-cleanup))
           options)))
    (lambda ()
      (twidget-stop runner)
      (when cleanup (funcall cleanup)))))

;;; ============================================================================
;;; 便捷宏和函数
;;; ============================================================================

(defmacro twidget-with-reactive (bindings &rest body)
  "Create reactive bindings and execute BODY. 
BINDINGS is a list of (symbol value) pairs.

Example:
  (twidget-with-reactive ((state (twidget-reactive \\='(: count 0)))
                          (doubled (twidget-computed
                                    (lambda () (* 2 (twidget-reactive-get state :count))))))
    ... )"
  (declare (indent 1))
  `(let ,(mapcar (lambda (binding)
                   (list (car binding) (cadr binding)))
                 bindings)
     ,@body))

(defmacro twidget-define-reactive (name data)
  "Define a global reactive variable NAME with DATA."
  `(defvar ,name (twidget-reactive ,data)))

(defmacro twidget-define-ref (name value)
  "Define a global ref variable NAME with VALUE."
  `(defvar ,name (twidget-ref ,value)))

;;; ============================================================================
;;; 批量更新
;;; ============================================================================

(defvar twidget--pending-effects nil
  "List of effects pending execution during batch update.")

(defvar twidget--is-flushing nil
  "Whether we are currently flushing pending effects.")

(defvar twidget--is-batching nil
  "Whether we are currently in a batch update.")

(defun twidget--queue-effect (effect)
  "Queue EFFECT for later execution."
  (unless (memq effect twidget--pending-effects)
    (push effect twidget--pending-effects)))

(defun twidget--flush-effects ()
  "Flush all pending effects."
  (unless twidget--is-flushing
    (setq twidget--is-flushing t)
    (unwind-protect
        (while twidget--pending-effects
          (let ((effect (pop twidget--pending-effects)))
            (twidget--run-effect effect)))
      (setq twidget--is-flushing nil))))

(defmacro twidget-batch (&rest body)
  "Execute BODY and batch all reactive updates. 
Effects are only run once at the end, even if triggered multiple times."
  `(let ((twidget--is-batching t)
         (twidget--pending-effects nil))
     (unwind-protect
         (progn ,@body)
       (setq twidget--is-batching nil)
       (twidget--flush-effects))))

;;; ============================================================================
;;; 工具函数
;;; ============================================================================

(defun twidget-is-ref (obj)
  "Return t if OBJ is a ref."
  (twidget-ref-p obj))

(defun twidget-is-reactive (obj)
  "Return t if OBJ is reactive."
  (twidget-reactive-p obj))

(defun twidget-is-computed (obj)
  "Return t if OBJ is a computed property."
  (twidget-computed-p obj))

(defun twidget-is-readonly (obj)
  "Return t if OBJ is readonly."
  (and (twidget-reactive-p obj)
       (twidget-reactive-readonly obj)))

(defun twidget-to-raw (obj)
  "Get the raw underlying data from OBJ."
  (cond
   ((twidget-reactive-p obj) (twidget-reactive-data obj))
   ((twidget-ref-p obj) (twidget-ref-value obj))
   ((twidget-computed-p obj) (twidget-computed-value obj))
   (t obj)))

(defun twidget-to-ref (obj-or-key &optional key)
  "Convert a property of reactive object to a ref.
If OBJ-OR-KEY is a ref, return it. 
If OBJ-OR-KEY is reactive and KEY is provided, return a ref to that property."
  (cond
   ((twidget-ref-p obj-or-key)
    obj-or-key)
   ((and (twidget-reactive-p obj-or-key) key)
    (let ((obj obj-or-key))
      (twidget-computed
       (list :get (lambda () (twidget-reactive-get obj key))
             :set (lambda (v) (twidget-reactive-set obj key v))))))
   (t
    (twidget-ref obj-or-key))))

(defun twidget-unref (ref)
  "Unwrap a ref to get its value.  If not a ref, return as-is."
  (if (twidget-ref-p ref)
      (twidget-ref-get ref)
    ref))

;;; ============================================================================
;;; 便捷的访问器别名
;;; ============================================================================

;; Ref shortcuts

(defun twidget-get (object)
  (cond
   ((twidget-ref-p object) (twidget-ref-get object))
   ((twidget-reactive-p object) (twidget-reactive-get object))
   (t (error "Invalid format of reactive object: %S" object))))

(defun twidget-set (object key-or-new-value &optional value)
  (cond
   ((twidget-ref-p object)
    (twidget-ref-set object key-or-new-value))
   ((twidget-reactive-p object)
    (twidget-reactive-set object key-or-new-value value))
   (t (error "Invalid format of reactive object: %S" object))))

;;; ============================================================================
;;; 调试工具
;;; ============================================================================

(defun twidget-debug-deps (target)
  "Print dependency information for TARGET."
  (let ((deps-map (gethash target twidget--target-map)))
    (if deps-map
        (progn
          (message "Dependencies for target:")
          (maphash
           (lambda (key dep-set)
             (message "  Key: %s -> %d effects"
                      key (hash-table-count dep-set)))
           deps-map))
      (message "No dependencies tracked for this target."))))

(defun twidget-debug-effect-count ()
  "Return the total number of tracked effects."
  (let ((count 0))
    (maphash
     (lambda (_target deps-map)
       (maphash
        (lambda (_key dep-set)
          (setq count (+ count (hash-table-count dep-set))))
        deps-map))
     twidget--target-map)
    count))

;;; ============================================================================
;;; 提供模块
;;; ============================================================================

(provide 'twidget-reactive)

;;; twidget-reactive.el ends here
