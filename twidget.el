;;; twidget.el --- Text Widget Component Library for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Version: 0.1.0
;; Keywords: convenience text-properties ui components
;; Author: Geekinney (kinneyzhang666@gmail.com)
;; Package-Requires: ((emacs "28.1") (tp "0.1.0"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;;; Commentary:

;; twidget.el provides a Vue3-inspired component library for building
;; text-based UI components in Emacs.  It leverages tp.el for reactive
;; text properties and implements reactive text that updates when
;; variables change.
;;
;; Key concepts:
;; - Ref: A reactive variable that triggers updates when changed.
;; - Reactive text: Text bound to a ref that auto-updates in buffers.
;; - Component: A reusable UI unit with setup, state, and render functions.
;;
;; Public API:
;; - `twidget-ref': Create a reactive reference.
;; - `twidget-ref-value', `twidget-ref-set': Get/set ref value.
;; - `twidget-watch', `twidget-computed': React to ref changes.
;; - `twidget-text': Create reactive text string.
;; - `define-twidget': Define a component.
;; - `twidget-render', `twidget-unmount': Render/remove components.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tp)


;;;; Customization

(defgroup twidget nil
  "Text Widget Component Library for reactive text-based UI."
  :prefix "twidget-"
  :group 'applications)


;;;; Internal Variables

(defvar twidget--components (make-hash-table :test 'eq)
  "Registry of component definitions.  Key: name, Value: plist.")

(defvar twidget--instances (make-hash-table :test 'eq)
  "Registry of component instances.  Key: instance-id, Value: plist.")

(defvar twidget--refs (make-hash-table :test 'eq)
  "Registry of reactive refs.  Key: ref-symbol, Value: metadata plist.")

(defvar twidget--id-counter 0
  "Counter for generating unique IDs.")

(defvar-local twidget--buffer-instances nil
  "List of component instance IDs in current buffer.")

;;;; ID Generation

(defun twidget--next-id (prefix)
  "Generate a unique ID with PREFIX."
  (intern (format "%s-%d" prefix (cl-incf twidget--id-counter))))


;;;; Reactive Refs

(defun twidget-ref (initial-value &optional name)
  "Create a reactive ref with INITIAL-VALUE.

A ref is a symbol whose value can be observed for changes.
When the value changes, all watchers and bound text regions update.

NAME is an optional symbol name; if nil, one is auto-generated.
Returns the ref symbol.

Example:
  (setq counter (twidget-ref 0))
  (twidget-ref-set counter 10)  ; triggers reactive updates"
  (let ((ref (or name (twidget--next-id "twidget-ref"))))
    (puthash ref (list :watchers nil) twidget--refs)
    (set ref initial-value)
    (add-variable-watcher ref #'twidget--on-ref-change)
    ref))

(defun twidget-ref-p (symbol)
  "Return non-nil if SYMBOL is a twidget ref."
  (and (symbolp symbol) (gethash symbol twidget--refs)))

(defun twidget-ref-value (ref)
  "Return the current value of REF.
Signal an error if REF is not a valid twidget ref."
  (unless (twidget-ref-p ref)
    (error "Not a twidget ref: %s" ref))
  (symbol-value ref))

(defun twidget-ref-set (ref value)
  "Set REF to VALUE, triggering reactive updates.
Signal an error if REF is not a valid twidget ref."
  (unless (twidget-ref-p ref)
    (error "Not a twidget ref: %s" ref))
  (set ref value))

(defun twidget-unref (ref)
  "Destroy REF, removing all watchers and the variable binding."
  (when (twidget-ref-p ref)
    (remove-variable-watcher ref #'twidget--on-ref-change)
    (remhash ref twidget--refs)
    (makunbound ref)))

(defun twidget--on-ref-change (symbol newval operation _where)
  "Variable watcher callback for ref changes.
SYMBOL is the ref, NEWVAL is the new value, OPERATION is the change type."
  (when (and (eq operation 'set) (twidget-ref-p symbol))
    (let ((oldval (symbol-value symbol)))
      (unless (equal oldval newval)
        (twidget--update-reactive-text symbol newval)
        (twidget--notify-watchers symbol newval oldval)))))


;;;; Ref Watchers

(defun twidget-watch (ref callback)
  "Register CALLBACK to be called when REF changes.

CALLBACK is called with arguments (NEW-VALUE OLD-VALUE REF).
Multiple callbacks can be registered for the same ref.

Example:
  (twidget-watch counter
    (lambda (new old _ref)
      (message \"Changed from %s to %s\" old new)))"
  (unless (twidget-ref-p ref)
    (error "Not a twidget ref: %s" ref))
  (let ((data (gethash ref twidget--refs)))
    (plist-put data :watchers (cons callback (plist-get data :watchers)))))

(defun twidget--notify-watchers (ref newval oldval)
  "Call all watchers of REF with NEWVAL and OLDVAL."
  (dolist (callback (plist-get (gethash ref twidget--refs) :watchers))
    (condition-case err
        (funcall callback newval oldval ref)
      (error (message "twidget: watcher error for %s: %s" ref err)))))

(defun twidget-computed (compute-fn &rest deps)
  "Create a computed ref that derives its value from DEPS.

COMPUTE-FN is called to produce the value.  It's re-evaluated
whenever any ref in DEPS changes.

Returns a new ref.

Example:
  (setq doubled (twidget-computed
                  (lambda () (* 2 (twidget-ref-value counter)))
                  counter))"
  (let ((computed (twidget-ref (funcall compute-fn))))
    (dolist (dep deps)
      (twidget-watch dep
        (lambda (new _old dep-ref)
          (cl-progv (list dep-ref) (list new)
            (twidget-ref-set computed (funcall compute-fn))))))
    computed))

;;;; Reactive Text

(defun twidget-text (ref &optional format-fn)
  "Create a reactive text string bound to REF.

The returned string will auto-update in buffers when REF changes.
FORMAT-FN, if provided, transforms the ref value for display.

Returns a propertized string with reactive metadata.

Example:
  (insert (twidget-text counter (lambda (v) (format \"Count: %d\" v))))"
  (let* ((value (twidget-ref-value ref))
         (text (if format-fn
                   (funcall format-fn value)
                 (format "%s" value))))
    (tp-set text
            'twidget-ref ref
            'twidget-format-fn format-fn
            'twidget-text-id (twidget--next-id "twidget-text"))))

(defun twidget--update-reactive-text (ref newval)
  "Update all text regions bound to REF with NEWVAL."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (twidget--update-text-in-buffer ref newval)))))

(defun twidget--update-text-in-buffer (ref newval)
  "Update text bound to REF in current buffer with NEWVAL."
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (while-let ((match (tp-search-forward 'twidget-ref ref t)))
        (let* ((start (prop-match-beginning match))
               (end (prop-match-end match))
               (props (tp-at start))
               (format-fn (plist-get props 'twidget-format-fn))
               (new-text (if format-fn
                             (funcall format-fn newval)
                           (format "%s" newval))))
          (goto-char start)
          (delete-region start end)
          (insert new-text)
          (tp-set start (+ start (length new-text)) props))))))

;;;; Component Definition

(cl-defmacro define-twidget (name &key props setup render)
  "Define a component named NAME.

PROPS is a list of property names the component accepts.
SETUP is a function (props) -> state, called once to initialize.
RENDER is a function (props state) -> vnode, called to produce output.

Example:
  (define-twidget counter
    :props (initial-value)
    :setup (lambda (props)
             (list :count (twidget-ref (plist-get props :initial-value))))
    :render (lambda (_props state)
              (twidget-text (plist-get state :count))))"
  (declare (indent defun))
  `(puthash ',name
            (list :name ',name :props ',props :setup ,setup :render ,render)
            twidget--components))

(defun twidget-get-component (name)
  "Return the component definition for NAME, or nil if not defined."
  (gethash name twidget--components))

;;;; Component Instances

(defun twidget-create-instance (component-name &optional props)
  "Create an instance of COMPONENT-NAME with PROPS.
Returns the instance ID.  Signals an error if component is undefined."
  (let ((component (twidget-get-component component-name)))
    (unless component
      (error "Component not defined: %s" component-name))
    (let* ((instance-id (twidget--next-id "twidget-instance"))
           (setup-fn (plist-get component :setup))
           (state (when setup-fn (funcall setup-fn props))))
      (puthash instance-id
               (list :id instance-id
                     :component component-name
                     :props props
                     :state state
                     :buffer nil
                     :start-marker nil
                     :end-marker nil)
               twidget--instances)
      instance-id)))

(defun twidget-get-instance (instance-id)
  "Return the instance data for INSTANCE-ID, or nil."
  (gethash instance-id twidget--instances))

(defun twidget-instance-state (instance-id)
  "Return the state plist of INSTANCE-ID."
  (plist-get (twidget-get-instance instance-id) :state))

;;;; Virtual DOM Elements

(defun twidget-h (&rest children)
  "Create a fragment element containing CHILDREN.
Each child can be a string, reactive text, or another element."
  (list :type 'fragment :children children))

(defun twidget-span (props &rest children)
  "Create a span element with PROPS applied to CHILDREN.
PROPS is a plist of text properties."
  (list :type 'span :props props :children children))

;;;; Rendering

(defun twidget-render (component-name &optional props buffer point)
  "Render COMPONENT-NAME with PROPS at POINT in BUFFER.
BUFFER defaults to current buffer; POINT defaults to current point.
Returns the instance ID."
  (let* ((buf (or buffer (current-buffer)))
         (instance-id (twidget-create-instance component-name props))
         (instance (twidget-get-instance instance-id))
         (component (twidget-get-component component-name))
         (render-fn (plist-get component :render))
         (vnode (funcall render-fn props (plist-get instance :state))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (or point (point)))
          (let ((start-marker (point-marker)))
            (twidget--render-vnode vnode)
            (set-marker-insertion-type start-marker nil)
            (plist-put instance :buffer buf)
            (plist-put instance :start-marker start-marker)
            (plist-put instance :end-marker (point-marker))
            (push instance-id twidget--buffer-instances)))))
    instance-id))

(defun twidget--render-vnode (vnode)
  "Render VNODE into current buffer at point.  Returns text length."
  (cond
   ;; Reactive text string (has twidget-ref property)
   ((and (stringp vnode) (> (length vnode) 0) (tp-at 0 'twidget-ref vnode))
    (insert vnode)
    (length vnode))
   ;; Plain string
   ((stringp vnode)
    (insert vnode)
    (length vnode))
   ;; Fragment: render children sequentially
   ((eq (plist-get vnode :type) 'fragment)
    (cl-reduce #'+ (plist-get vnode :children)
               :key #'twidget--render-vnode :initial-value 0))
   ;; Span: render children with text properties
   ((eq (plist-get vnode :type) 'span)
    (let ((start (point))
          (len (cl-reduce #'+ (plist-get vnode :children)
                          :key #'twidget--render-vnode :initial-value 0)))
      (when-let ((props (plist-get vnode :props)))
        (tp-set start (point) props))
      len))
   ;; List of elements
   ((listp vnode)
    (cl-reduce #'+ vnode :key #'twidget--render-vnode :initial-value 0))
   ;; Fallback: convert to string
   (t
    (let ((text (format "%s" vnode)))
      (insert text)
      (length text)))))

(defun twidget-unmount (instance-id)
  "Remove INSTANCE-ID from its buffer and clean up resources."
  (when-let ((instance (twidget-get-instance instance-id)))
    (twidget--delete-instance-region instance)
    (twidget--cleanup-instance-markers instance)
    (twidget--cleanup-instance-refs instance)
    (remhash instance-id twidget--instances)
    (twidget--untrack-buffer-instance instance instance-id)))

(defun twidget--delete-instance-region (instance)
  "Delete the text region of INSTANCE from its buffer."
  (let ((buffer (plist-get instance :buffer))
        (start (plist-get instance :start-marker))
        (end (plist-get instance :end-marker)))
    (when (and buffer (buffer-live-p buffer) start end
               (marker-buffer start) (marker-buffer end))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (delete-region (marker-position start) (marker-position end)))))))

(defun twidget--cleanup-instance-markers (instance)
  "Release markers held by INSTANCE."
  (when-let ((start (plist-get instance :start-marker)))
    (set-marker start nil))
  (when-let ((end (plist-get instance :end-marker)))
    (set-marker end nil)))

(defun twidget--cleanup-instance-refs (instance)
  "Destroy refs created in INSTANCE state."
  (when-let ((state (plist-get instance :state)))
    (cl-loop for (_key val) on state by #'cddr
             when (twidget-ref-p val) do (twidget-unref val))))

(defun twidget--untrack-buffer-instance (instance instance-id)
  "Remove INSTANCE-ID from buffer-local tracking."
  (when-let ((buffer (plist-get instance :buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq twidget--buffer-instances
              (delete instance-id twidget--buffer-instances))))))

;;;; Convenience Functions

(defun twidget-insert (text &optional props)
  "Insert TEXT at point with optional text PROPS.
Returns (start-marker . end-marker)."
  (let ((start (point-marker)))
    (insert text)
    (when props
      (tp-set (marker-position start) (point) props))
    (let ((end (point-marker)))
      (set-marker-insertion-type start nil)
      (set-marker-insertion-type end t)
      (cons start end))))

(defun twidget-insert-reactive (ref &optional format-fn props)
  "Insert reactive text bound to REF at point.
FORMAT-FN formats the value; PROPS are additional text properties.
Returns (start . end) positions."
  (let* ((value (twidget-ref-value ref))
         (text (if format-fn (funcall format-fn value) (format "%s" value)))
         (start (point)))
    (insert text)
    (tp-set start (point)
            (append (list 'twidget-ref ref
                          'twidget-format-fn format-fn
                          'twidget-text-id (twidget--next-id "twidget-text"))
                    props))
    (cons start (point))))

;;;; Cleanup

(defun twidget-clear-buffer ()
  "Unmount all component instances in current buffer."
  (interactive)
  (dolist (id twidget--buffer-instances)
    (twidget-unmount id))
  (setq twidget--buffer-instances nil))

(defun twidget-reset ()
  "Reset all twidget state globally."
  (interactive)
  ;; Unmount all instances
  (maphash (lambda (id _) (twidget-unmount id)) twidget--instances)
  (clrhash twidget--instances)
  ;; Destroy all refs
  (maphash (lambda (ref _)
             (remove-variable-watcher ref #'twidget--on-ref-change)
             (when (boundp ref) (makunbound ref)))
           twidget--refs)
  (clrhash twidget--refs)
  ;; Reset counter
  (setq twidget--id-counter 0))

;;;; Built-in Components

(define-twidget twidget-text-display
  :props (value format-fn)
  :setup (lambda (props)
           (list :text-ref (twidget-ref (plist-get props :value))))
  :render (lambda (props state)
            (twidget-text (plist-get state :text-ref)
                          (plist-get props :format-fn))))

(define-twidget twidget-button
  :props (label on-click face)
  :setup (lambda (_props) nil)
  :render (lambda (props _state)
            (let ((label (plist-get props :label))
                  (on-click (plist-get props :on-click))
                  (face (or (plist-get props :face) 'button)))
              (twidget-span
               (list 'face face
                     'mouse-face 'highlight
                     'keymap (let ((map (make-sparse-keymap)))
                               (define-key map [mouse-1] on-click)
                               (define-key map (kbd "RET") on-click)
                               map)
                     'help-echo "Click to activate")
               (format "[%s]" label)))))

(define-twidget twidget-progress
  :props (current total width)
  :setup (lambda (props)
           (list :current (twidget-ref (or (plist-get props :current) 0))
                 :total (twidget-ref (or (plist-get props :total) 100))
                 :width (or (plist-get props :width) 20)))
  :render (lambda (_props state)
            (let* ((current (twidget-ref-value (plist-get state :current)))
                   (total (twidget-ref-value (plist-get state :total)))
                   (width (plist-get state :width))
                   (pct (if (> total 0) (/ (* current 100.0) total) 0))
                   (filled (round (* width (/ pct 100.0))))
                   (empty (- width filled)))
              (twidget-h
               "["
               (twidget-span '(face (:foreground "green"))
                             (make-string filled ?#))
               (make-string empty ?-)
               (format "] %d%%" (round pct))))))

(provide 'twidget)
;;; twidget.el ends here
