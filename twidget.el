;;; twidget.el --- Text Widget Component Library for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2024

;; Version: 0.1.0
;; Keywords: convenience text-properties ui components
;; Author: Geekinney (kinneyzhang666@gmail.com)
;; Package-Requires: ((emacs "28.1") (dash "2.19.1") (tp "0.1.0"))

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
;; Features:
;; - Reactive text: Text that updates when variables change
;; - Component definition: Define reusable UI components
;; - Incremental buffer updates: Only update changed parts
;; - Vue3-style Composition API: setup function, refs, computed values
;;
;; Requires Emacs 28.1+ and tp.el

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; Optional dependency on tp.el for reactive text properties
(declare-function tp-set "tp" (start-or-string &optional end-or-prop props-or-val &rest rest))
(declare-function tp-add "tp" (start-or-string &optional end-or-prop props-or-val &rest rest))
(declare-function tp-at "tp" (pos &optional property-or-object object))
(declare-function tp-clear "tp" (&optional start end object))
(declare-function tp-define-layer "tp" (name &rest args))

;;; Variables

(defgroup twidget nil
  "Text Widget Component Library."
  :prefix "twidget-"
  :group 'development)

(defvar twidget-components (make-hash-table :test 'eq)
  "Hash table storing all defined components.
Key is component name (symbol), value is component definition plist.")

(defvar twidget-instances (make-hash-table :test 'eq)
  "Hash table storing component instances.
Key is instance ID, value is instance data plist.")

(defvar twidget-reactive-refs (make-hash-table :test 'eq)
  "Hash table mapping reactive refs to their metadata.
Key is the ref symbol, value is plist with :value, :watchers, :instances.")

(defvar twidget-reactive-texts (make-hash-table :test 'eq)
  "Hash table storing reactive text regions.
Key is ref symbol, value is list of (buffer start-marker end-marker format-fn).")

(defvar twidget--instance-counter 0
  "Counter for generating unique instance IDs.")

(defvar-local twidget-buffer-instances nil
  "List of component instance IDs in current buffer.")

;;; Reactive System - Core

(defun twidget--generate-instance-id ()
  "Generate a unique instance ID."
  (cl-incf twidget--instance-counter)
  (intern (format "twidget-instance-%d" twidget--instance-counter)))

(defun twidget-ref (initial-value &optional name)
  "Create a reactive ref with INITIAL-VALUE.
NAME is optional symbol name for the ref.
Returns the ref symbol which can be used to get/set the value."
  (let* ((ref-name (or name (intern (format "twidget-ref-%d"
                                            (cl-incf twidget--instance-counter)))))
         (ref-data (list :value initial-value
                         :watchers nil
                         :instances nil)))
    ;; Store ref metadata
    (puthash ref-name ref-data twidget-reactive-refs)
    ;; Define the variable
    (set ref-name initial-value)
    ;; Add variable watcher for reactivity
    (add-variable-watcher ref-name #'twidget--ref-watcher)
    ref-name))

(defun twidget--ref-watcher (symbol newval operation where)
  "Watcher function for reactive refs.
SYMBOL is the ref variable, NEWVAL is new value.
OPERATION is 'set, WHERE indicates context."
  (when (and (eq operation 'set)
             (gethash symbol twidget-reactive-refs))
    (let ((oldval (symbol-value symbol)))
      (unless (equal oldval newval)
        ;; Update reactive text regions
        (twidget--update-reactive-texts symbol newval)
        ;; Call registered watchers
        (twidget--invoke-ref-watchers symbol newval oldval)))))

(defun twidget-ref-value (ref)
  "Get the current value of REF."
  (if (gethash ref twidget-reactive-refs)
      (symbol-value ref)
    (error "Not a twidget ref: %s" ref)))

(defun twidget-ref-set (ref value)
  "Set the value of REF to VALUE."
  (if (gethash ref twidget-reactive-refs)
      (set ref value)
    (error "Not a twidget ref: %s" ref)))

(defun twidget-watch (ref callback)
  "Register CALLBACK to be called when REF changes.
CALLBACK receives (new-value old-value ref-symbol)."
  (let ((ref-data (gethash ref twidget-reactive-refs)))
    (if ref-data
        (plist-put ref-data :watchers
                   (cons callback (plist-get ref-data :watchers)))
      (error "Not a twidget ref: %s" ref))))

(defun twidget--invoke-ref-watchers (ref newval oldval)
  "Invoke all watchers for REF with NEWVAL and OLDVAL."
  (when-let ((ref-data (gethash ref twidget-reactive-refs)))
    (dolist (callback (plist-get ref-data :watchers))
      (condition-case err
          (funcall callback newval oldval ref)
        (error (message "twidget: watcher error for %s: %s" ref err))))))

(defun twidget-computed (compute-fn &rest deps)
  "Create a computed ref from COMPUTE-FN.
DEPS are the reactive refs this computed value depends on.
Returns a new ref that auto-updates when dependencies change."
  (let* ((computed-ref (twidget-ref (funcall compute-fn)))
         (update-fn (lambda (new _old ref)
                      ;; Temporarily bind the changed variable to its new value
                      ;; before recomputing, using cl-progv for dynamic binding
                      (cl-progv (list ref) (list new)
                        (twidget-ref-set computed-ref (funcall compute-fn))))))
    ;; Watch all dependencies
    (dolist (dep deps)
      (twidget-watch dep update-fn))
    computed-ref))

(defun twidget-unref (ref)
  "Remove a reactive ref and its watchers."
  (when (gethash ref twidget-reactive-refs)
    (remove-variable-watcher ref #'twidget--ref-watcher)
    (remhash ref twidget-reactive-refs)
    ;; Clean up reactive texts for this ref
    (remhash ref twidget-reactive-texts)
    (makunbound ref)))

;;; Reactive Text System

(defun twidget--update-reactive-texts (ref newval)
  "Update all reactive text regions for REF with NEWVAL."
  (when-let ((regions (gethash ref twidget-reactive-texts)))
    (dolist (region regions)
      (let ((buffer (nth 0 region))
            (start-marker (nth 1 region))
            (end-marker (nth 2 region))
            (format-fn (nth 3 region)))
        (when (and (buffer-live-p buffer)
                   (marker-buffer start-marker)
                   (marker-buffer end-marker))
          (with-current-buffer buffer
            (let ((inhibit-read-only t)
                  (new-text (if format-fn
                                (funcall format-fn newval)
                              (format "%s" newval)))
                  (start (marker-position start-marker))
                  (end (marker-position end-marker)))
              ;; Preserve text properties from the region
              (let ((props (when (< start end)
                             (text-properties-at start))))
                (save-excursion
                  (goto-char start)
                  (delete-region start end)
                  (insert new-text)
                  ;; Restore text properties
                  (when props
                    (add-text-properties start (+ start (length new-text)) props))
                  ;; Update end marker
                  (set-marker end-marker (point)))))))))))

(defun twidget-text (ref &optional format-fn)
  "Create reactive text bound to REF.
FORMAT-FN is optional function to format the value before display.
Returns a propertized string that will be updated when REF changes."
  (let* ((value (twidget-ref-value ref))
         (text (if format-fn
                   (funcall format-fn value)
                 (format "%s" value))))
    ;; Store format-fn for later use when updating
    (propertize text
                'twidget-ref ref
                'twidget-format-fn format-fn)))

(defun twidget--register-reactive-text (ref buffer start end format-fn)
  "Register a reactive text region for REF.
BUFFER is the buffer, START and END are markers, FORMAT-FN formats value."
  (let* ((start-marker (if (markerp start)
                           start
                         (let ((m (make-marker)))
                           (set-marker m start buffer)
                           (set-marker-insertion-type m nil)
                           m)))
         (end-marker (if (markerp end)
                         end
                       (let ((m (make-marker)))
                         (set-marker m end buffer)
                         (set-marker-insertion-type m t)
                         m)))
         (region (list buffer start-marker end-marker format-fn))
         (existing (gethash ref twidget-reactive-texts)))
    (puthash ref (cons region existing) twidget-reactive-texts)))

;;; Component Definition

(cl-defmacro define-twidget (name &key props setup render)
  "Define a twidget component named NAME.

PROPS is a list of prop names the component accepts.
SETUP is a function that receives props and returns reactive state.
RENDER is a function that receives props and state, returns text/elements.

Example:
  (define-twidget counter
    :props (initial-value)
    :setup (lambda (props)
             (let ((count (twidget-ref (or (plist-get props :initial-value) 0))))
               (list :count count)))
    :render (lambda (props state)
              (let ((count (plist-get state :count)))
                (twidget-h
                  (twidget-text count (lambda (v) (format \"Count: %d\" v)))))))"
  (declare (indent defun))
  `(progn
     (puthash ',name
              (list :name ',name
                    :props ',props
                    :setup ,setup
                    :render ,render)
              twidget-components)
     ',name))

(defun twidget-get-component (name)
  "Get component definition by NAME."
  (gethash name twidget-components))

;;; Component Instance

(defun twidget-create-instance (component-name &optional props)
  "Create an instance of component COMPONENT-NAME with PROPS.
Returns instance ID."
  (let* ((component (twidget-get-component component-name))
         (instance-id (twidget--generate-instance-id))
         (setup-fn (plist-get component :setup))
         (state (when setup-fn (funcall setup-fn props))))
    (unless component
      (error "Component not defined: %s" component-name))
    (puthash instance-id
             (list :id instance-id
                   :component component-name
                   :props props
                   :state state
                   :mounted nil
                   :buffer nil
                   :start-marker nil
                   :end-marker nil)
             twidget-instances)
    instance-id))

(defun twidget-get-instance (instance-id)
  "Get instance data by INSTANCE-ID."
  (gethash instance-id twidget-instances))

(defun twidget-instance-state (instance-id)
  "Get the state of instance INSTANCE-ID."
  (plist-get (twidget-get-instance instance-id) :state))

;;; Rendering

(defun twidget-h (&rest children)
  "Create a virtual element with CHILDREN.
Similar to Vue's h() function for creating vnodes.
Each child can be a string, reactive text, or nested element."
  (list :type 'fragment
        :children children))

(defun twidget-span (props &rest children)
  "Create a span element with PROPS and CHILDREN.
PROPS is a plist of text properties to apply."
  (list :type 'span
        :props props
        :children children))

(defun twidget--render-element (element buffer)
  "Render ELEMENT into BUFFER. Returns the rendered text length."
  (cond
   ;; Reactive text (propertized string with twidget-ref) - check this first
   ((and (stringp element)
         (> (length element) 0)
         (get-text-property 0 'twidget-ref element))
    (let ((start (point))
          (ref (get-text-property 0 'twidget-ref element))
          (format-fn (get-text-property 0 'twidget-format-fn element)))
      (insert element)
      (twidget--register-reactive-text ref buffer start (point) format-fn)
      (length element)))
   
   ;; Plain string - insert directly
   ((stringp element)
    (insert element)
    (length element))
   
   ;; Fragment - render children
   ((and (listp element) (eq (plist-get element :type) 'fragment))
    (let ((total 0))
      (dolist (child (plist-get element :children))
        (cl-incf total (twidget--render-element child buffer)))
      total))
   
   ;; Span - render with text properties
   ((and (listp element) (eq (plist-get element :type) 'span))
    (let ((start (point))
          (props (plist-get element :props))
          (total 0))
      (dolist (child (plist-get element :children))
        (cl-incf total (twidget--render-element child buffer)))
      ;; Apply text properties
      (when props
        (add-text-properties start (point) props))
      total))
   
   ;; List of elements
   ((listp element)
    (let ((total 0))
      (dolist (child element)
        (cl-incf total (twidget--render-element child buffer)))
      total))
   
   ;; Other - convert to string
   (t
    (let ((text (format "%s" element)))
      (insert text)
      (length text)))))

(defun twidget-render (component-name &optional props buffer point)
  "Render component COMPONENT-NAME with PROPS at POINT in BUFFER.
Returns the instance ID."
  (let* ((buffer (or buffer (current-buffer)))
         (instance-id (twidget-create-instance component-name props))
         (instance (twidget-get-instance instance-id))
         (component (twidget-get-component component-name))
         (render-fn (plist-get component :render))
         (state (plist-get instance :state))
         (vnode (funcall render-fn props state)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (start (or point (point))))
        (save-excursion
          (goto-char start)
          (let ((start-marker (point-marker))
                (len (twidget--render-element vnode buffer)))
            (set-marker-insertion-type start-marker nil)
            ;; Update instance data
            (plist-put instance :mounted t)
            (plist-put instance :buffer buffer)
            (plist-put instance :start-marker start-marker)
            (plist-put instance :end-marker (point-marker))
            ;; Track instance in buffer
            (push instance-id twidget-buffer-instances)))))
    instance-id))

(defun twidget-unmount (instance-id)
  "Unmount component instance INSTANCE-ID."
  (when-let ((instance (twidget-get-instance instance-id)))
    (let ((buffer (plist-get instance :buffer))
          (start-marker (plist-get instance :start-marker))
          (end-marker (plist-get instance :end-marker)))
      ;; Remove from buffer
      (when (and buffer (buffer-live-p buffer)
                 start-marker end-marker
                 (marker-buffer start-marker)
                 (marker-buffer end-marker))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (delete-region (marker-position start-marker)
                           (marker-position end-marker)))))
      ;; Clean up markers
      (when start-marker (set-marker start-marker nil))
      (when end-marker (set-marker end-marker nil))
      ;; Clean up state refs
      (let ((state (plist-get instance :state)))
        (when state
          (cl-loop for (key val) on state by #'cddr
                   when (gethash val twidget-reactive-refs)
                   do (twidget-unref val))))
      ;; Remove instance
      (remhash instance-id twidget-instances)
      ;; Remove from buffer tracking
      (when (and buffer (buffer-live-p buffer))
        (with-current-buffer buffer
          (setq twidget-buffer-instances
                (delete instance-id twidget-buffer-instances)))))))

;;; Utility Functions

(defun twidget-insert (text &optional props)
  "Insert TEXT with optional PROPS at point.
Returns markers for start and end of inserted text."
  (let ((start (point-marker)))
    (insert text)
    (when props
      (add-text-properties (marker-position start) (point) props))
    (let ((end (point-marker)))
      (set-marker-insertion-type start nil)
      (set-marker-insertion-type end t)
      (cons start end))))

(defun twidget-insert-reactive (ref &optional format-fn props)
  "Insert reactive text for REF at point.
FORMAT-FN formats the value, PROPS are text properties.
Returns markers for the inserted text."
  (let* ((value (twidget-ref-value ref))
         (text (if format-fn
                   (funcall format-fn value)
                 (format "%s" value)))
         (start (point-marker)))
    (insert text)
    (when props
      (add-text-properties (marker-position start) (point) props))
    (let ((end (point-marker)))
      (set-marker-insertion-type start nil)
      (set-marker-insertion-type end t)
      (twidget--register-reactive-text ref (current-buffer) start end format-fn)
      (cons start end))))

(defun twidget-clear-buffer-instances ()
  "Clear all component instances in current buffer."
  (interactive)
  (dolist (instance-id twidget-buffer-instances)
    (twidget-unmount instance-id))
  (setq twidget-buffer-instances nil))

(defun twidget-reset ()
  "Reset all twidget state."
  (interactive)
  ;; Clear all instances
  (maphash (lambda (id _instance)
             (twidget-unmount id))
           twidget-instances)
  (clrhash twidget-instances)
  ;; Clear all refs
  (maphash (lambda (ref _data)
             (remove-variable-watcher ref #'twidget--ref-watcher)
             (when (boundp ref)
               (makunbound ref)))
           twidget-reactive-refs)
  (clrhash twidget-reactive-refs)
  (clrhash twidget-reactive-texts)
  ;; Reset counter
  (setq twidget--instance-counter 0))

;;; tp.el Integration

(defun twidget-tp-available-p ()
  "Check if tp.el is available."
  (featurep 'tp))

(defun twidget-with-tp-props (text props)
  "Apply text properties PROPS to TEXT using tp.el if available.
Falls back to standard text properties if tp.el is not loaded."
  (if (twidget-tp-available-p)
      (tp-set text props)
    (apply #'propertize text props)))

;;; Built-in Components

(define-twidget twidget-text-display
  :props (value format-fn)
  :setup (lambda (props)
           (let* ((initial (plist-get props :value))
                  (text-ref (twidget-ref initial)))
             (list :text-ref text-ref)))
  :render (lambda (props state)
            (let ((text-ref (plist-get state :text-ref))
                  (format-fn (plist-get props :format-fn)))
              (twidget-text text-ref format-fn))))

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
           (let* ((current (twidget-ref (or (plist-get props :current) 0)))
                  (total (twidget-ref (or (plist-get props :total) 100)))
                  (width (or (plist-get props :width) 20)))
             (list :current current
                   :total total
                   :width width)))
  :render (lambda (props state)
            (let* ((current-ref (plist-get state :current))
                   (total-ref (plist-get state :total))
                   (width (plist-get state :width))
                   (current (twidget-ref-value current-ref))
                   (total (twidget-ref-value total-ref))
                   (percentage (if (> total 0)
                                   (/ (* current 100.0) total)
                                 0))
                   (filled (round (* width (/ percentage 100.0))))
                   (empty (- width filled)))
              (twidget-h
               "["
               (twidget-span '(face (:foreground "green"))
                             (make-string filled ?#))
               (make-string empty ?-)
               (format "] %d%%" (round percentage))))))

(provide 'twidget)
;;; twidget.el ends here
