;;; twidget-component.el --- Vue3-style Composition API Component System -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Twidget Team
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1") (dash "2.19.1"))
;; Keywords: reactive, components, ui

;;; Commentary:

;; This package provides a Vue3-style Composition API for creating reactive
;; UI components in Emacs.  It integrates with twidget-reactive.el for
;; reactive data and tp.el for text property reactivity.
;;
;; Key features:
;; - `twidget-define-component': Define reusable components
;; - Props system with `$' prefix for global reactive binding
;; - Internal state with local variables
;; - Lifecycle hooks (setup, mounted, unmounted)
;; - Built-in components: button, text, input
;;
;; Example:
;;   (twidget-define-component my-counter
;;     :props (count)
;;     :setup (lambda (props)
;;              (let ((local-count (twidget-ref 0)))
;;                (list :count local-count
;;                      :increment (lambda ()
;;                                   (twidget-ref-inc local-count))))))
;;
;; Inspired by Vue 3 Composition API.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'twidget-reactive)

;; Optional tp.el dependency for text property reactivity
(declare-function tp-set "tp" (start-or-string &optional end-or-prop props-or-val &rest rest))
(declare-function tp-add "tp" (start-or-string &optional end-or-prop props-or-val &rest rest))
(declare-function tp-search-map "tp" (function property &optional value object start end))
(declare-function tp-define-layer "tp" (name &rest args))
(declare-function tp--resolve-reactive-symbols "tp" (form &optional override-alist))

;;; ============================================================================
;;; Global Variables
;;; ============================================================================

(defvar twidget-component-registry (make-hash-table :test 'eq)
  "Registry of defined components.
Maps component name symbols to component definition plists.")

(defvar twidget-component-instances (make-hash-table :test 'eq)
  "Registry of component instances.
Maps instance IDs to instance state plists.")

(defvar twidget--instance-counter 0
  "Counter for generating unique instance IDs.")

(defvar twidget-current-instance nil
  "The currently active component instance during setup/render.")

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(defun twidget--generate-instance-id ()
  "Generate a unique instance ID."
  (cl-incf twidget--instance-counter)
  (intern (format "twidget-instance-%d" twidget--instance-counter)))

(defun twidget--reactive-symbol-p (sym)
  "Return non-nil if SYM is a reactive variable symbol (starts with $)."
  (and (symbolp sym)
       (string-prefix-p "$" (symbol-name sym))))

(defun twidget--reactive-var-symbol (sym)
  "Convert a reactive symbol SYM (e.g., $foo) to its variable symbol (e.g., foo).
Returns nil if SYM is not a reactive symbol."
  (when (twidget--reactive-symbol-p sym)
    (intern (substring (symbol-name sym) 1))))

(defun twidget--collect-reactive-symbols (form)
  "Recursively collect all reactive symbols ($-prefixed) from FORM.
Returns a list of reactive symbols found."
  (cond
   ((twidget--reactive-symbol-p form)
    (list form))
   ((consp form)
    (append (twidget--collect-reactive-symbols (car form))
            (twidget--collect-reactive-symbols (cdr form))))
   (t nil)))

(defun twidget--resolve-reactive-value (form)
  "Resolve reactive symbols in FORM to their current values.
Symbols starting with $ are treated as global reactive variables."
  (cond
   ((twidget--reactive-symbol-p form)
    (let ((var-sym (twidget--reactive-var-symbol form)))
      (if (boundp var-sym)
          (symbol-value var-sym)
        nil)))
   ((consp form)
    (cons (twidget--resolve-reactive-value (car form))
          (twidget--resolve-reactive-value (cdr form))))
   (t form)))

(defun twidget--ensure-reactive-var (sym &optional initial-value)
  "Ensure SYM is defined as a global variable with INITIAL-VALUE.
If SYM starts with $, the $ is removed."
  (let ((var-sym (if (twidget--reactive-symbol-p sym)
                     (twidget--reactive-var-symbol sym)
                   sym)))
    (unless (boundp var-sym)
      (set var-sym initial-value))
    var-sym))

;;; ============================================================================
;;; Component Definition
;;; ============================================================================

(cl-defstruct (twidget-component (:constructor twidget-component--create))
  "Structure representing a component definition."
  (name nil :documentation "Component name symbol.")
  (props nil :documentation "List of prop names.")
  (setup nil :documentation "Setup function.")
  (render nil :documentation "Render function.")
  (mounted nil :documentation "Mounted lifecycle hook.")
  (unmounted nil :documentation "Unmounted lifecycle hook.")
  (emits nil :documentation "List of events the component can emit."))

(cl-defstruct (twidget-instance (:constructor twidget-instance--create))
  "Structure representing a component instance."
  (id nil :documentation "Unique instance ID.")
  (component nil :documentation "Reference to component definition.")
  (props nil :documentation "Resolved props values (plist).")
  (state nil :documentation "Component internal state (plist from setup).")
  (buffer nil :documentation "Buffer where the component is rendered.")
  (start nil :documentation "Start position in buffer.")
  (end nil :documentation "End position in buffer.")
  (cleanup-fns nil :documentation "List of cleanup functions.")
  (watchers nil :documentation "List of active watcher stop functions.")
  (mounted-p nil :documentation "Whether the component is mounted."))

;;;###autoload
(cl-defmacro twidget-define-component (name &key props setup render mounted unmounted emits)
  "Define a component named NAME with Vue3-style Composition API.

PROPS is a list of prop names that the component accepts.
Props can be bound to global reactive variables using the $ prefix.

SETUP is a function that receives (props) and returns a plist of
internal state and methods.  This is called once when the component
is created.

RENDER is a function that receives (props state) and returns a
string or propertized string to display.

MOUNTED is a function called after the component is inserted into a buffer.
It receives (instance) as argument.

UNMOUNTED is a function called before the component is removed.
It receives (instance) as argument.

EMITS is a list of event names that the component can emit.

Example:
  (twidget-define-component my-button
    :props (label disabled)
    :setup (lambda (props)
             (let ((click-count (twidget-ref 0)))
               (list :click-count click-count
                     :on-click (lambda ()
                                 (unless (plist-get props :disabled)
                                   (twidget-ref-inc click-count))))))
    :render (lambda (props state)
              (let ((label (or (plist-get props :label) \"Button\"))
                    (disabled (plist-get props :disabled)))
                (propertize (format \"[%s]\" label)
                            \\='face (if disabled \\='shadow \\='button)))))"
  (declare (indent defun))
  (let ((props-list (if (and props (listp props)) props (list props))))
    `(progn
       (puthash ',name
                (twidget-component--create
                 :name ',name
                 :props ',props-list
                 :setup ,setup
                 :render ,render
                 :mounted ,mounted
                 :unmounted ,unmounted
                 :emits ',emits)
                twidget-component-registry)
       ',name)))

(defun twidget-get-component (name)
  "Get the component definition for NAME."
  (gethash name twidget-component-registry))

;;; ============================================================================
;;; Component Instance Creation
;;; ============================================================================

(defun twidget--resolve-props (component props-plist)
  "Resolve PROPS-PLIST for COMPONENT.
Props with $ prefix are resolved to their global variable values."
  (let ((prop-names (twidget-component-props component))
        (resolved nil))
    (dolist (prop-name prop-names)
      (let* ((prop-key (intern (format ":%s" prop-name)))
             (prop-value (plist-get props-plist prop-key)))
        (when prop-value
          (setq resolved (plist-put resolved prop-key
                                    (twidget--resolve-reactive-value prop-value))))))
    resolved))

(defun twidget--setup-prop-watchers (instance)
  "Set up watchers for reactive props in INSTANCE."
  (let* ((component (twidget-instance-component instance))
         (props-plist (twidget-instance-props instance))
         (prop-names (twidget-component-props component))
         (watchers nil))
    (dolist (prop-name prop-names)
      (let* ((prop-key (intern (format ":%s" prop-name)))
             (prop-value (plist-get props-plist prop-key))
             (reactive-syms (twidget--collect-reactive-symbols prop-value)))
        (dolist (rsym reactive-syms)
          (let ((var-sym (twidget--reactive-var-symbol rsym)))
            (when (and var-sym (boundp var-sym))
              ;; Create a ref wrapper for the global variable
              (let ((ref (twidget-ref (symbol-value var-sym))))
                ;; Watch for changes and re-render
                (push (twidget-watch
                       (lambda () (symbol-value var-sym))
                       (lambda (new-val _old _cleanup)
                         (twidget--update-instance instance))
                       '(:immediate nil))
                      watchers)))))))
    (setf (twidget-instance-watchers instance) watchers)))

(defun twidget-create-instance (component-name &rest props)
  "Create a new instance of COMPONENT-NAME with PROPS.
Returns the instance structure."
  (let ((component (twidget-get-component component-name)))
    (unless component
      (error "Component %s not found" component-name))
    (let* ((instance-id (twidget--generate-instance-id))
           (resolved-props (twidget--resolve-props component props))
           (instance (twidget-instance--create
                      :id instance-id
                      :component component
                      :props resolved-props
                      :buffer nil
                      :start nil
                      :end nil
                      :cleanup-fns nil
                      :watchers nil
                      :mounted-p nil)))
      ;; Run setup function
      (let ((setup-fn (twidget-component-setup component)))
        (when setup-fn
          (let ((twidget-current-instance instance))
            (setf (twidget-instance-state instance)
                  (funcall setup-fn resolved-props)))))
      ;; Store instance
      (puthash instance-id instance twidget-component-instances)
      instance)))

;;; ============================================================================
;;; Component Rendering
;;; ============================================================================

(defun twidget--render-instance (instance)
  "Render INSTANCE and return the resulting string."
  (let* ((component (twidget-instance-component instance))
         (render-fn (twidget-component-render component))
         (props (twidget-instance-props instance))
         (state (twidget-instance-state instance)))
    (if render-fn
        (let ((twidget-current-instance instance))
          (funcall render-fn props state))
      "")))

(defun twidget--update-instance (instance)
  "Update INSTANCE by re-rendering its content."
  (when (and (twidget-instance-mounted-p instance)
             (twidget-instance-buffer instance)
             (buffer-live-p (twidget-instance-buffer instance)))
    (with-current-buffer (twidget-instance-buffer instance)
      (let* ((start (twidget-instance-start instance))
             (end (twidget-instance-end instance))
             (inhibit-read-only t)
             (new-content (twidget--render-instance instance)))
        (when (and start end (>= end start))
          (save-excursion
            (goto-char start)
            (delete-region start end)
            (insert new-content)
            (setf (twidget-instance-end instance) (point))))))))

(defun twidget-mount (instance &optional buffer point)
  "Mount INSTANCE into BUFFER at POINT.
If BUFFER is nil, use current buffer.
If POINT is nil, use current point.
Returns the instance."
  (let ((buf (or buffer (current-buffer)))
        (pos (or point (point))))
    (with-current-buffer buf
      (save-excursion
        (goto-char pos)
        (let* ((start (point))
               (content (twidget--render-instance instance))
               (inhibit-read-only t))
          (insert content)
          (setf (twidget-instance-buffer instance) buf)
          (setf (twidget-instance-start instance) start)
          (setf (twidget-instance-end instance) (point))
          (setf (twidget-instance-mounted-p instance) t)
          ;; Setup prop watchers
          (twidget--setup-prop-watchers instance)
          ;; Call mounted hook
          (let ((mounted-fn (twidget-component-mounted
                             (twidget-instance-component instance))))
            (when mounted-fn
              (let ((twidget-current-instance instance))
                (funcall mounted-fn instance))))
          instance)))))

(defun twidget-unmount (instance)
  "Unmount INSTANCE from its buffer."
  (when (twidget-instance-mounted-p instance)
    ;; Call unmounted hook
    (let ((unmounted-fn (twidget-component-unmounted
                         (twidget-instance-component instance))))
      (when unmounted-fn
        (let ((twidget-current-instance instance))
          (funcall unmounted-fn instance))))
    ;; Stop all watchers
    (dolist (stop-fn (twidget-instance-watchers instance))
      (when (functionp stop-fn)
        (funcall stop-fn)))
    ;; Run cleanup functions
    (dolist (cleanup-fn (twidget-instance-cleanup-fns instance))
      (when (functionp cleanup-fn)
        (funcall cleanup-fn)))
    ;; Remove from buffer if still exists
    (when (and (twidget-instance-buffer instance)
               (buffer-live-p (twidget-instance-buffer instance)))
      (with-current-buffer (twidget-instance-buffer instance)
        (let ((inhibit-read-only t))
          (when (and (twidget-instance-start instance)
                     (twidget-instance-end instance))
            (delete-region (twidget-instance-start instance)
                           (twidget-instance-end instance))))))
    ;; Clear instance state
    (setf (twidget-instance-mounted-p instance) nil)
    (setf (twidget-instance-watchers instance) nil)
    (setf (twidget-instance-cleanup-fns instance) nil)
    ;; Remove from registry
    (remhash (twidget-instance-id instance) twidget-component-instances)))

;;; ============================================================================
;;; Composition API Helpers
;;; ============================================================================

(defun twidget-on-cleanup (fn)
  "Register FN to be called when the current component is unmounted.
Must be called within a setup function."
  (when twidget-current-instance
    (push fn (twidget-instance-cleanup-fns twidget-current-instance))))

(defun twidget-emit (event &rest args)
  "Emit EVENT from the current component with ARGS.
Must be called within a component context."
  (when twidget-current-instance
    (let* ((instance twidget-current-instance)
           (state (twidget-instance-state instance))
           (handler-key (intern (format ":on-%s" event)))
           (handler (plist-get state handler-key)))
      (when (and handler (functionp handler))
        (apply handler args)))))

(defmacro twidget-use-state (initial-value)
  "Create a reactive state with INITIAL-VALUE.
Returns a ref that can be used with twidget-ref-get and twidget-ref-set."
  `(twidget-ref ,initial-value))

(defmacro twidget-use-computed (getter &optional setter)
  "Create a computed value from GETTER.
SETTER is an optional function to set the computed value."
  `(twidget-computed ,getter ,setter))

(defmacro twidget-use-watch (source callback &optional options)
  "Watch SOURCE for changes and call CALLBACK.
OPTIONS is a plist with :immediate and :deep keys."
  `(twidget-watch ,source ,callback ,options))

(defmacro twidget-use-effect (effect-fn &optional options)
  "Create a reactive effect that runs EFFECT-FN when dependencies change."
  `(twidget-watch-effect ,effect-fn ,options))

;;; ============================================================================
;;; Convenient Component Creation
;;; ============================================================================

(defun twidget (component-name &rest props)
  "Create and return a string representation of COMPONENT-NAME with PROPS.
This is a convenience function for creating component content without mounting."
  (let* ((instance (apply #'twidget-create-instance component-name props))
         (content (twidget--render-instance instance)))
    ;; Clean up the temporary instance
    (remhash (twidget-instance-id instance) twidget-component-instances)
    content))

(defun twidget-insert (component-name &rest props)
  "Insert COMPONENT-NAME with PROPS at point and return the instance.
The component is mounted and will be reactive."
  (let ((instance (apply #'twidget-create-instance component-name props)))
    (twidget-mount instance)
    instance))

;;; ============================================================================
;;; Built-in Components
;;; ============================================================================

;; Text Component
(twidget-define-component twidget-text
  :props (content face help-echo)
  :setup (lambda (props)
           (list :content (plist-get props :content)))
  :render (lambda (props state)
            (let* ((content (or (plist-get props :content) ""))
                   (face-val (plist-get props :face))
                   (help-echo-val (plist-get props :help-echo))
                   (text (if (stringp content) content (format "%s" content))))
              (when face-val
                (setq text (propertize text 'face face-val)))
              (when help-echo-val
                (setq text (propertize text 'help-echo help-echo-val)))
              text)))

;; Button Component
(twidget-define-component twidget-button
  :props (label disabled on-click face)
  :emits (click)
  :setup (lambda (props)
           (let ((click-count (twidget-ref 0)))
             (list :click-count click-count
                   :handle-click
                   (lambda ()
                     (unless (plist-get props :disabled)
                       (twidget-ref-inc click-count)
                       (let ((on-click (plist-get props :on-click)))
                         (when (functionp on-click)
                           (funcall on-click))))))))
  :render (lambda (props state)
            (let* ((label (or (plist-get props :label) "Button"))
                   (disabled (plist-get props :disabled))
                   (custom-face (plist-get props :face))
                   (handle-click (plist-get state :handle-click))
                   (face-val (cond
                              (disabled 'shadow)
                              (custom-face custom-face)
                              (t 'button)))
                   (text (format "[%s]" label)))
              (propertize text
                          'face face-val
                          'mouse-face (unless disabled 'highlight)
                          'help-echo (if disabled "Disabled" "Click to activate")
                          'keymap (let ((map (make-sparse-keymap)))
                                    (define-key map [mouse-1]
                                      (lambda ()
                                        (interactive)
                                        (when handle-click
                                          (funcall handle-click))))
                                    (define-key map (kbd "RET")
                                      (lambda ()
                                        (interactive)
                                        (when handle-click
                                          (funcall handle-click))))
                                    map)
                          'twidget-button t))))

;; Input Component
(twidget-define-component twidget-input
  :props (value placeholder on-change disabled width)
  :emits (change input)
  :setup (lambda (props)
           (let* ((initial-value (or (plist-get props :value) ""))
                  (local-value (twidget-ref initial-value)))
             (list :local-value local-value
                   :handle-input
                   (lambda (new-value)
                     (unless (plist-get props :disabled)
                       (twidget-ref-set local-value new-value)
                       (let ((on-change (plist-get props :on-change)))
                         (when (functionp on-change)
                           (funcall on-change new-value))))))))
  :render (lambda (props state)
            (let* ((value (twidget-ref-get (plist-get state :local-value)))
                   (placeholder (or (plist-get props :placeholder) ""))
                   (disabled (plist-get props :disabled))
                   (width (or (plist-get props :width) 20))
                   (display-value (if (string-empty-p value)
                                      placeholder
                                    value))
                   (padded-value (format (format "%%-%ds" width)
                                         (substring display-value
                                                    0 (min (length display-value) width))))
                   (face-val (cond
                              (disabled 'shadow)
                              ((string-empty-p value) 'font-lock-comment-face)
                              (t 'default))))
              (propertize (format "[%s]" padded-value)
                          'face face-val
                          'help-echo (if disabled "Disabled" "Click to edit")
                          'twidget-input t
                          'twidget-input-value value
                          'twidget-input-handler (plist-get state :handle-input)))))

;;; ============================================================================
;;; Component Utilities
;;; ============================================================================

(defun twidget-list-components ()
  "Return a list of all registered component names."
  (let (names)
    (maphash (lambda (k _v) (push k names)) twidget-component-registry)
    (nreverse names)))

(defun twidget-list-instances ()
  "Return a list of all active instance IDs."
  (let (ids)
    (maphash (lambda (k _v) (push k ids)) twidget-component-instances)
    (nreverse ids)))

(defun twidget-get-instance (instance-id)
  "Get the instance for INSTANCE-ID."
  (gethash instance-id twidget-component-instances))

(defun twidget-clear-components ()
  "Clear all component registrations."
  (clrhash twidget-component-registry))

(defun twidget-unmount-all ()
  "Unmount all active instances."
  (maphash (lambda (_k instance)
             (twidget-unmount instance))
           twidget-component-instances))

;;; ============================================================================
;;; Provide
;;; ============================================================================

(provide 'twidget-component)

;;; twidget-component.el ends here
