;;; twidget.el --- Declarative text widget library for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Kinney Zhang

;; Author: Kinney Zhang <kinneyzhang666@gmail.com>
;; URL: https://github.com/Kinneyzhang/twidget
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (tp "0.1"))
;; Keywords: convenience, text, widgets

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Twidget (Text Widget) is a declarative text widget library for Emacs,
;; inspired by modern UI component frameworks like Vue.js and React.
;;
;; Features:
;; - Property system with required/optional properties and defaults
;; - Slot system for flexible content composition
;; - Widget inheritance for creating specialized variants
;; - Seamless integration with Emacs text properties
;;
;; Quick Example:
;;
;;   (define-twidget my-button
;;     :props '((label . "Click"))
;;     :slot t
;;     :render (lambda (props slot)
;;               (tp-set slot 'face 'button)))
;;
;;   (twidget-parse '(my-button "Hello"))
;;
;; See README.md for more examples and documentation.

;;; Code:

(require 'cl-lib)
(require 'tp)

;; Compatibility for Emacs < 29
(unless (fboundp 'plistp)
  (defun plistp (object)
    "Return non-nil if OBJECT is a plist (property list).
A plist is a list with an even number of elements where
every other element (starting from the first) is a keyword."
    (and (listp object)
         (cl-evenp (length object))
         (cl-loop for (key _val) on object by #'cddr
                  always (keywordp key)))))

;;; Variables
;; ============================================================================

(defvar twidget-alist nil
  "Alist of widget definitions: (WIDGET-NAME . DEFINITION).
Each DEFINITION is a plist with :props, :slot, :render, :setup, and :template keys.")

(defvar-local twidget-reactive-data (make-hash-table :test 'equal)
  "Buffer-local hash table for reactive data.
Keys are variable names (strings), values are plists with:
  :value - the current value
  :watchers - list of functions to call when value changes")

(defvar-local twidget-instances (make-hash-table :test 'equal)
  "Buffer-local hash table for widget instances.
Keys are instance IDs (strings), values are plists with:
  :bindings - the reactive bindings plist from setup
  :template - the template sexp
  :overlays - list of overlays for reactive updates")

(defvar-local twidget-ref-registry (make-hash-table :test 'equal)
  "Buffer-local hash table for reactive ref tracking.
Keys are (instance-id . var-name), values are the ref objects.")

(defvar twidget--instance-counter 0
  "Counter for generating unique instance IDs.")

;;; Reactive Reference (twidget-ref)
;; ============================================================================

(cl-defstruct (twidget-ref (:constructor twidget-ref--create))
  "A reactive reference that triggers updates when its value changes."
  value      ; The current value
  watchers)  ; List of watcher functions

(defun twidget-ref (initial-value)
  "Create a reactive reference with INITIAL-VALUE.
Returns a twidget-ref object that can be used in templates.
When the value changes, the UI will be updated automatically."
  (twidget-ref--create :value initial-value :watchers nil))

(defun twidget-ref-p (obj)
  "Return non-nil if OBJ is a twidget-ref."
  (cl-typep obj 'twidget-ref))

(defun twidget--generate-instance-id ()
  "Generate a unique instance ID for a widget instance."
  (format "twidget-instance-%d-%s"
          (cl-incf twidget--instance-counter)
          (format-time-string "%s%N")))

(defun twidget--register-ref (instance-id var-name ref)
  "Register a reactive REF for INSTANCE-ID with VAR-NAME."
  (puthash (cons instance-id var-name) ref twidget-ref-registry))

(defun twidget--apply-reactive-text (text instance-id reactive-vars)
  "Apply reactive tracking to TEXT for INSTANCE-ID.
REACTIVE-VARS is a list of variable names that are reactive in this text.
Returns text with tp.el properties for reactive updates."
  ;; Create a reactive symbol and set its value
  (let ((sym (intern (format "twidget--text-%s" instance-id))))
    (set sym text)
    ;; Use tp-set with the tp-text property type and the reactive symbol reference
    (tp-set text 'tp-text (intern (format "$%s" sym)))))

;;; Widget Definition
;; ============================================================================

(defmacro define-twidget (name &rest args)
  "Define a text widget (widget) named NAME.

There are two ways to define a widget:

1. Simple Widget (using :render):
   For widgets that directly render to a string.

   ARGS should include:
     :props - A quoted list of property definitions. Each can be:
              - A symbol: required property accessed via keyword
              - A cons cell (SYMBOL . DEFAULT): property with default value
     :slot  - Boolean value or list of slot names.
              nil (default) means widget does not support slot.
              t means widget supports a single default slot.
              A list of symbols defines named slots.
     :slots - Alias for :slot with named slots (for clarity)
     :extends - Symbol of a parent widget to inherit from.
     :render - A lambda that returns the rendered string.
               For single slot: (lambda (props slot) ...)
               For named slots: (lambda (props slots) ...) where slots is a plist
               When :extends is used: (lambda (props slot parent-render) ...)

   Example:
     (define-twidget button
       :slot t
       :props \\='(action (bgcolor . \"orange\"))
       :render (lambda (props slot)
                 (let ((action (plist-get props :action))
                       (bgcolor (plist-get props :bgcolor)))
                   (tp-add (format \"%s%s%s\"
                                   (tp-set \" \" \\='tp-space 6)
                                   slot (tp-set \" \" \\='tp-space 6))
                           \\='tp-button `(:bgcolor ,bgcolor :action ,action)))))

2. Composite Widget (using :setup and :template):
   For widgets that compose other widgets with reactive data.

   ARGS should include:
     :props - Same as above
     :slot  - Same as above
     :setup - A lambda that receives props and returns a plist of reactive bindings.
              Use `twidget-ref' to create reactive values.
              (lambda (props) (list :count (twidget-ref \"0\")))
     :template - A quoted template sexp that defines the widget structure.
                 Use {varname} syntax in strings to bind to reactive data.
                 Template elements can be nested widget forms.

   Example:
     (define-twidget my-counter
       :setup (lambda (_props)
                (list :count (twidget-ref \"0\")))
       :template \\='(p (span \"{count}\")
                      \" \"
                      (button :action (lambda ()
                                        (interactive)
                                        (twidget-inc \\='count 1))
                              \"+\")
                      \" \"
                      (button :action (lambda ()
                                        (interactive)
                                        (twidget-dec \\='count 1))
                              \"-\")))"
  (declare (indent defun))
  (let ((props nil)
        (slot :twidget--unspecified)  ; Sentinel value to detect if :slot was provided
        (extends nil)
        (render nil)
        (setup nil)
        (template nil)
        (rest args))
    ;; Parse keyword arguments
    (while rest
      (pcase (car rest)
        (:props (setq props (cadr rest) rest (cddr rest)))
        (:slot (setq slot (cadr rest) rest (cddr rest)))
        (:slots (setq slot (cadr rest) rest (cddr rest)))  ; Alias for named slots
        (:extends (setq extends (cadr rest) rest (cddr rest)))
        (:render (setq render (cadr rest) rest (cddr rest)))
        (:setup (setq setup (cadr rest) rest (cddr rest)))
        (:template (setq template (cadr rest) rest (cddr rest)))
        (_ (error "Unknown keyword %S in define-twidget" (car rest)))))
    ;; Validate: either :render or (:setup and :template) must be provided
    (when (and render (or setup template))
      (error "Cannot use both :render and :setup/:template in define-twidget"))
    (when (and (not render) (or setup template) (not (and setup template)))
      (error "Both :setup and :template must be provided together"))
    ;; Prepare slot value - the sentinel :twidget--unspecified needs to be passed as-is
    ;; Other values (t, nil, or list) should evaluate properly
    (let ((slot-form (if (eq slot :twidget--unspecified)
                         :twidget--unspecified
                       ;; If slot is a quoted list (from ':slots '(x y z)),
                       ;; the value is actually (quote (x y z)), so we just pass it
                       slot)))
      `(twidget-internal ',name ,props ,slot-form ,extends ,render ,setup ,template))))

(defun twidget-internal (name props slot extends render setup template)
  "Internal function to define a widget NAME with PROPS, SLOT, EXTENDS, RENDER, SETUP, and TEMPLATE.
PROPS is a list of property definitions.
SLOT is a boolean, list of slot names, or :twidget--unspecified (not provided).
EXTENDS is a symbol of a parent widget to inherit from.
RENDER is the render function (for simple widgets).
SETUP is a function that returns reactive bindings (for composite widgets).
TEMPLATE is a template sexp (for composite widgets)."
  ;; Handle inheritance if :extends is specified
  (let* ((slot-was-specified (not (eq slot :twidget--unspecified)))
         (final-slot (if slot-was-specified slot nil))
         (final-props props)
         (parent-render nil))
    (when extends
      (let ((parent-def (cdr (assoc extends twidget-alist))))
        (unless parent-def
          (error "Parent widget not found: %S" extends))
        ;; Inherit slot from parent only if child didn't specify :slot
        (unless slot-was-specified
          (setq final-slot (plist-get parent-def :slot)))
        ;; Merge props: child props override parent defaults
        (let ((parent-props (plist-get parent-def :props)))
          (setq final-props (twidget-props parent-props final-props)))
        ;; Store parent render for child to call - resolve the full chain
        (let ((parent-extends (plist-get parent-def :extends)))
          (if parent-extends
              ;; Parent also extends something - wrap parent render to pass its parent
              (let ((grandparent-render (plist-get parent-def :parent-render)))
                (setq parent-render
                      (lambda (props slot)
                        (funcall (plist-get parent-def :render)
                                 props slot grandparent-render))))
            ;; Parent doesn't extend - use parent render directly
            (setq parent-render (plist-get parent-def :render))))))
    (let ((definition (list :props final-props
                            :slot final-slot
                            :extends extends
                            :parent-render parent-render
                            :render render
                            :setup setup
                            :template template))
          (existing (assoc name twidget-alist)))
      (if existing
          (setcdr existing definition)
        (push (cons name definition) twidget-alist)))
    (assoc name twidget-alist)))

;;; Property Helpers
;; ============================================================================

(defun twidget-props (parent-props child-props)
  "Merge PARENT-PROPS with CHILD-PROPS.
Child props override parent props with the same name.
Props without defaults in child inherit defaults from parent."
  (let ((result nil)
        (parent-map (make-hash-table :test 'equal))
        (child-map (make-hash-table :test 'equal)))
    ;; Build maps of prop-name -> prop-def
    (dolist (prop parent-props)
      (puthash (twidget-prop-name prop) prop parent-map))
    (dolist (prop child-props)
      (puthash (twidget-prop-name prop) prop child-map))
    ;; Merge: child overrides parent
    (maphash (lambda (name prop)
               (let ((child-prop (gethash name child-map)))
                 (if child-prop
                     (push child-prop result)
                   (push prop result))))
             parent-map)
    ;; Add any child-only props
    (maphash (lambda (name prop)
               (unless (gethash name parent-map)
                 (push prop result)))
             child-map)
    (nreverse result)))

(defun twidget-prop-name (prop-def)
  "Extract the property name from PROP-DEF.
PROP-DEF can be a symbol or a cons cell (SYMBOL . DEFAULT)."
  (if (consp prop-def)
      (car prop-def)
    prop-def))

(defun twidget-prop-default (prop-def)
  "Extract the default value from PROP-DEF.
Returns nil if no default is specified."
  (if (consp prop-def)
      (cdr prop-def)
    nil))

(defun twidget-prop-has-default-p (prop-def)
  "Return non-nil if PROP-DEF has a default value."
  (consp prop-def))

;;; Slot Helpers
;; ============================================================================

(defun twidget-slot-is-named-p (slot-def)
  "Return non-nil if SLOT-DEF defines named slots (a list of symbols)."
  (and (listp slot-def)
       (not (null slot-def))
       (symbolp (car slot-def))))

(defun twidget-is-slot-sexp-p (form slot-def)
  "Return non-nil if FORM is a named slot sexp like (slot-header content...).
SLOT-DEF is the list of defined slot names."
  (and (listp form)
       (symbolp (car form))
       (let ((name (symbol-name (car form))))
         (and (string-prefix-p "slot-" name)
              (memq (intern (substring name 5)) slot-def)))))

(defun twidget-extract-slot-name (slot-sexp)
  "Extract the slot name from SLOT-SEXP like (slot-header content...).
Returns the slot name as a symbol (e.g., \\='header)."
  (let ((name (symbol-name (car slot-sexp))))
    (intern (substring name 5))))

;;; Widget Parsing
;; ============================================================================

(cl-defun twidget-parse (widget-form &optional bindings)
  "Parse and render a widget invocation.

WIDGET-FORM is a list starting with the widget name, followed by
keyword-value pairs for props, and then slot values (if the widget
supports slots).

The format is: (WIDGET-NAME :prop1 val1 :prop2 val2 ... SLOT-VALUES...)

For named slots, use (slot-<name> content...) sexp format:
  (WIDGET-NAME :prop1 val1
               (slot-header \"Header\")
               (slot-content \"Content\"))

Special directives:
  :for  - Loop over a collection, e.g., :for \"item in items\"

Keyword arguments must come before slot values. Slot values are all
remaining elements after the keyword-value pairs. Each slot value can be:
  - A string: used directly
  - A list starting with a widget name: recursively parsed as a widget

BINDINGS is an optional alist of (VAR-NAME . VALUE) pairs for placeholder
substitution in slot content.

Example:
  (twidget-parse
   \\='(p \"happy hacking \"
       (text \"emacs\")
       (button :action (lambda () (message \"clicked!\"))
               \"click\")))

Returns the rendered string with text properties applied."
  (unless (and (listp widget-form) (symbolp (car widget-form)))
    (error "Invalid widget form: must be a list starting with widget name"))
  (let* ((widget-name (car widget-form))
         (rest (cdr widget-form))
         (definition (cdr (assoc widget-name twidget-alist))))
    (unless definition
      (error "Undefined widget: %S" widget-name))
    (let* ((prop-defs (plist-get definition :props))
           (slot-def (plist-get definition :slot))
           (extends (plist-get definition :extends))
           (parent-render-fn (plist-get definition :parent-render))
           (render-fn (plist-get definition :render))
           (parsed-props nil)
           (slot-value nil)
           (named-slots-p (twidget-slot-is-named-p slot-def)))
      ;; Parse the widget invocation arguments
      ;; Extract keyword arguments and collect slot values
      (let ((args rest)
            (collected-props nil)
            (collected-named-slots nil)
            (slot-parts nil)
            (for-expr nil)
            (local-bindings (copy-alist bindings)))
        ;; Parse keyword arguments first
        (while (and args (keywordp (car args)))
          (let ((key (car args))
                (val (cadr args)))
            (cond
             ;; Handle :for directive
             ((eq key :for)
              (setq for-expr val))
             ;; Regular prop
             (t (push (cons key val) collected-props)))
            (setq args (cddr args))))
        ;; Handle :for directive - iterate and return concatenated result
        (when for-expr
          (let ((parsed (twidget-parse-for-expression for-expr)))
            (if parsed
                (let* ((loop-var (car parsed))
                       (collection-name (cdr parsed))
                       ;; Get collection from bindings
                       (collection (cdr (assoc collection-name local-bindings)))
                       (results nil))
                  (if (listp collection)
                      (progn
                        (dolist (item collection)
                          ;; Create new bindings with loop variable
                          (let ((loop-bindings (cons (cons loop-var item)
                                                     local-bindings)))
                            ;; Reconstruct widget form without :for
                            (let ((new-form (cons widget-name
                                                  (append
                                                   ;; Add remaining props (excluding :for)
                                                   (apply #'append
                                                          (mapcar (lambda (p)
                                                                    (list (car p) (cdr p)))
                                                                  collected-props))
                                                   ;; Add remaining args (slot values)
                                                   args))))
                              (push (twidget-parse new-form loop-bindings) results))))
                        (cl-return-from twidget-parse (apply #'concat (nreverse results))))
                    (warn "twidget-parse: :for collection `%s' is not a list" collection-name)))
              (warn "twidget-parse: Invalid :for expression: %s" for-expr))))
        ;; Process remaining arguments (slot values or named slot sexps)
        (when args
          (if slot-def
              (if named-slots-p
                  ;; Named slots mode - look for (slot-<name> ...) sexps
                  (dolist (arg args)
                    (if (twidget-is-slot-sexp-p arg slot-def)
                        ;; This is a named slot sexp
                        (let* ((slot-name (twidget-extract-slot-name arg))
                               (slot-keyword (intern (format ":%s" slot-name)))
                               (slot-content (cdr arg)))
                          (push (cons slot-keyword
                                      (twidget-process-slot-args slot-content local-bindings))
                                collected-named-slots))
                      ;; Not a slot sexp - could be default slot content
                      (when (memq 'default slot-def)
                        (push (twidget-process-slot-value arg local-bindings) slot-parts))))
                ;; Single slot mode
                (setq slot-value (twidget-process-slot-args args local-bindings)))
            ;; Slot not supported - warn about ignored arguments
            (warn "twidget-parse: Widget `%s' does not support slot content. \
Ignoring arguments: %S" widget-name args)))
        ;; Combine default slot parts if any
        (when (and named-slots-p slot-parts)
          (push (cons :default (apply #'concat (nreverse slot-parts)))
                collected-named-slots))
        ;; Build named slots plist if using named slots
        (when named-slots-p
          (let ((slots-plist nil))
            (dolist (slot-name slot-def)
              (let* ((slot-keyword (intern (format ":%s" slot-name)))
                     (provided (assoc slot-keyword collected-named-slots)))
                (when provided
                  (setq slots-plist
                        (plist-put slots-plist slot-keyword (cdr provided))))))
            (setq slot-value slots-plist)))
        ;; Build the props plist with defaults
        (dolist (prop-def prop-defs)
          (let* ((prop-name (twidget-prop-name prop-def))
                 (prop-keyword (intern (format ":%s" prop-name)))
                 (provided (assoc prop-keyword collected-props)))
            (if provided
                (setq parsed-props
                      (plist-put parsed-props prop-keyword (cdr provided)))
              ;; Use default value if available
              (when (twidget-prop-has-default-p prop-def)
                (setq parsed-props
                      (plist-put parsed-props prop-keyword
                                 (twidget-prop-default prop-def)))))))
        ;; Check if this is a composite widget (has :setup and :template)
        (let ((setup-fn (plist-get definition :setup))
              (template (plist-get definition :template)))
          (if (and setup-fn template)
              ;; Composite widget: call setup, expand template, and render
              (twidget--render-composite setup-fn template parsed-props slot-value)
            ;; Simple widget: call the render function
            (if extends
                ;; With inheritance, pass parent-render as third argument
                (funcall render-fn parsed-props slot-value parent-render-fn)
              ;; Normal render call
              (funcall render-fn parsed-props slot-value))))))))

(defun twidget--render-composite (setup-fn template props slot)
  "Render a composite widget using SETUP-FN and TEMPLATE.
SETUP-FN is a function that receives props and returns reactive bindings.
TEMPLATE is a template sexp to expand and render.
PROPS is the parsed props plist.
SLOT is the slot value (if any)."
  ;; Call setup function to get reactive bindings
  (let* ((reactive-bindings (funcall setup-fn props))
         (instance-id (twidget--generate-instance-id))
         (bindings nil))
    ;; Process reactive bindings from setup
    ;; Convert reactive refs to bindings for template substitution
    (let ((plist reactive-bindings))
      (while plist
        (let ((key (car plist))
              (val (cadr plist)))
          (when (keywordp key)
            (let* ((var-name (substring (symbol-name key) 1))
                   (ref-value (if (twidget-ref-p val)
                                  (twidget-ref-value val)
                                val)))
              ;; Register reactive ref if it is one
              (when (twidget-ref-p val)
                (twidget--register-ref instance-id var-name val))
              (push (cons var-name ref-value) bindings))))
        (setq plist (cddr plist))))
    ;; Store instance info for reactivity
    (puthash instance-id
             (list :bindings reactive-bindings :template template)
             twidget-instances)
    ;; Expand and render the template
    (twidget--expand-template template bindings instance-id)))

(defun twidget--expand-template (template bindings instance-id)
  "Expand TEMPLATE sexp into rendered string.
TEMPLATE is a widget form or list of forms.
BINDINGS is an alist of (VAR-NAME . VALUE) for placeholder substitution.
INSTANCE-ID is the widget instance identifier for reactivity."
  (cond
   ;; String - substitute placeholders and wrap with reactive overlay if needed
   ((stringp template)
    (twidget--expand-template-string template bindings instance-id))
   ;; Widget form - parse it with bindings
   ((and (listp template)
         (symbolp (car template))
         (assoc (car template) twidget-alist))
    (twidget--expand-template-widget template bindings instance-id))
   ;; List of forms - process each
   ((listp template)
    (mapconcat (lambda (form)
                 (twidget--expand-template form bindings instance-id))
               template ""))
   ;; Other - convert to string
   (t (format "%s" template))))

(defun twidget--expand-template-string (str bindings instance-id)
  "Expand STR template string with BINDINGS.
INSTANCE-ID is used for reactive tracking."
  (let ((reactive-refs nil))
    ;; First pass: identify reactive refs
    (let ((temp str))
      (while (string-match "{\\([^}]+\\)}" temp)
        (let* ((var-name (match-string 1 temp))
               (ref-info (gethash (cons instance-id var-name) twidget-ref-registry)))
          (when (and ref-info (assoc var-name bindings))
            (push var-name reactive-refs)))
        (setq temp (substring temp (match-end 0)))))
    ;; Replace all placeholders using replace-regexp-in-string
    (let ((result (replace-regexp-in-string
                   "{\\([^}]+\\)}"
                   (lambda (match)
                     (let* ((var-name (match-string 1 match))
                            (binding (assoc var-name bindings)))
                       (if binding
                           (format "%s" (cdr binding))
                         match)))
                   str t t)))
      ;; If there are reactive refs, wrap with tracking overlay marker
      (if reactive-refs
          (twidget--apply-reactive-text result instance-id reactive-refs)
        result))))

(defun twidget--expand-template-widget (template bindings instance-id)
  "Expand widget TEMPLATE with BINDINGS.
INSTANCE-ID is used for reactive tracking."
  (let ((widget-name (car template))
        (rest (cdr template)))
    ;; Process the widget arguments to substitute bindings
    (let ((processed-args (twidget--process-template-args rest bindings instance-id)))
      (twidget-parse (cons widget-name processed-args) bindings))))

(defun twidget--process-template-args (args bindings instance-id)
  "Process template ARGS, substituting BINDINGS.
INSTANCE-ID is used for reactive tracking.
Returns the processed argument list."
  (let ((result nil))
    (while args
      (let ((arg (car args)))
        (cond
         ;; Keyword - keep as is, process next arg
         ((keywordp arg)
          (push arg result)
          (setq args (cdr args))
          (when args
            (push (twidget--process-template-arg (car args) bindings instance-id) result)
            (setq args (cdr args))))
         ;; Other - process and add
         (t
          (push (twidget--process-template-arg arg bindings instance-id) result)
          (setq args (cdr args))))))
    (nreverse result)))

(defun twidget--process-template-arg (arg bindings instance-id)
  "Process a single template ARG with BINDINGS.
INSTANCE-ID is used for reactive tracking."
  (cond
   ;; String - substitute placeholders
   ((stringp arg)
    (twidget--expand-template-string arg bindings instance-id))
   ;; Widget form - expand it
   ((and (listp arg)
         (symbolp (car arg))
         (assoc (car arg) twidget-alist))
    (twidget--expand-template arg bindings instance-id))
   ;; Lambda/function - return as is
   ((functionp arg) arg)
   ;; List that might be a lambda form
   ((and (listp arg) (eq (car arg) 'lambda)) arg)
   ;; Other list - might need recursive processing
   ((listp arg)
    (mapcar (lambda (x)
              (twidget--process-template-arg x bindings instance-id))
            arg))
   ;; Other - return as is
   (t arg)))

(defun twidget-process-slot-value (val &optional bindings)
  "Process a single slot VAL, recursively parsing widget forms.
BINDINGS is an optional alist of (VAR-NAME . VALUE) pairs for placeholder
substitution."
  (cond
   ((stringp val)
    (twidget-substitute-placeholders val bindings))
   ((and (listp val)
         (symbolp (car val))
         (assoc (car val) twidget-alist))
    (twidget-parse val bindings))
   (t (format "%s" val))))

(defun twidget-process-slot-args (args &optional bindings)
  "Process multiple slot ARGS into a single concatenated string.
BINDINGS is an optional alist of (VAR-NAME . VALUE) pairs for placeholder
substitution."
  (let ((slot-parts nil))
    (dolist (slot-item args)
      (push (twidget-process-slot-value slot-item bindings) slot-parts))
    (apply #'concat (nreverse slot-parts))))

(defun twidget-reset ()
  "Reset all widget definitions."
  (interactive)
  (setq twidget-alist nil))

(defun twidget-extract-variables (form)
  "Extract variable names referenced in :for directives from FORM.
Returns a list of unique variable name symbols."
  (let ((vars nil))
    (cond
     ((not (listp form)) nil)
     ((and (listp form) (symbolp (car form)))
      ;; This is a widget form or subform
      (let ((rest (cdr form)))
        ;; Look for :for in keyword arguments
        (while (and rest (keywordp (car rest)))
          (let ((key (car rest))
                (val (cadr rest)))
            (when (eq key :for)
              (when (stringp val)
                (let ((parsed (twidget-parse-for-expression val)))
                  (when parsed
                    (push (intern (cdr parsed)) vars)))))
            (setq rest (cddr rest))))
        ;; Recursively check remaining elements
        (dolist (elem rest)
          (setq vars (append (twidget-extract-variables elem) vars)))))
     (t
      ;; It's a list but not a widget form, check each element
      (dolist (elem form)
        (setq vars (append (twidget-extract-variables elem) vars)))))
    (cl-remove-duplicates vars)))

(defmacro twidget-insert (form)
  "Insert the rendered widget FORM at point, auto-capturing referenced variables.
FORM should be a quoted widget form. This macro automatically captures
lexical variables referenced in :for directives.

Note: For variable capture to work, FORM must be a quoted literal.
Dynamic forms at runtime cannot capture lexical variables.

Example:
  (let ((editors \\='(\"emacs\" \"vim\" \"vscode\")))
    (twidget-insert
     \\='(div (p :for \"e in editors\" \"Editor: {e}\"))))"
  (declare (indent 0))
  ;; Extract variables at compile time if form is a quoted list
  (if (and (listp form) (eq (car form) 'quote))
      (let* ((widget-form (cadr form))
             (vars (twidget-extract-variables widget-form)))
        `(let ((bindings (list ,@(mapcar (lambda (var)
                                           `(cons ,(symbol-name var) ,var))
                                         vars))))
           (insert (twidget-parse ',widget-form bindings))))
    ;; Runtime extraction (fallback) - cannot capture lexical variables
    `(insert (twidget-parse ,form nil))))

;;; Reactive Data System
;; ============================================================================

(defun twidget-get (var-name &optional key-or-index)
  "Get the value of reactive data variable VAR-NAME.
VAR-NAME should be a symbol. The value is looked up in the current widget context.

If KEY-OR-INDEX is provided:
  - For plist values: KEY-OR-INDEX should be a keyword (e.g., :name) to get that property.
  - For list values: KEY-OR-INDEX should be an integer index (0-based) to get that element.

Examples:
  (twidget-get \\='user)           ; Get the whole value
  (twidget-get \\='user :name)     ; Get :name from plist
  (twidget-get \\='items 0)        ; Get first element from list"
  (let ((key (if (symbolp var-name) (symbol-name var-name) var-name)))
    ;; Search through all instances to find the ref
    (catch 'found
      (maphash (lambda (_inst-key data)
                 (let ((bindings (plist-get data :bindings)))
                   (when bindings
                     (let ((ref (plist-get bindings (intern (format ":%s" key)))))
                       (when (twidget-ref-p ref)
                         (let ((value (twidget-ref-value ref)))
                           (throw 'found
                                  (cond
                                   ;; No key/index - return whole value
                                   ((null key-or-index) value)
                                   ;; Keyword access for plist
                                   ((keywordp key-or-index)
                                    (if (plistp value)
                                        (plist-get value key-or-index)
                                      (error "Cannot use keyword access on non-plist value")))
                                   ;; Integer access for list
                                   ((integerp key-or-index)
                                    (if (listp value)
                                        (nth key-or-index value)
                                      (error "Cannot use index access on non-list value")))
                                   (t (error "KEY-OR-INDEX must be a keyword or integer"))))))))))
               twidget-instances)
      nil)))

(defun twidget-set (var-name value &optional key-or-index)
  "Set the value of reactive data variable VAR-NAME to VALUE.
VAR-NAME should be a symbol. This triggers reactive updates in the buffer.

If KEY-OR-INDEX is provided:
  - For plist values: KEY-OR-INDEX should be a keyword (e.g., :name) to set that property.
  - For list values: KEY-OR-INDEX should be an integer index (0-based) to set that element.

Examples:
  (twidget-set \\='user new-user)           ; Set the whole value
  (twidget-set \\='user \"John\" :name)      ; Set :name in plist
  (twidget-set \\='items \"new-item\" 0)     ; Set first element in list"
  (let ((key (if (symbolp var-name) (symbol-name var-name) var-name)))
    ;; Search through all instances to find and update the ref
    (maphash (lambda (inst-key data)
               (let ((bindings (plist-get data :bindings)))
                 (when bindings
                   (let ((ref (plist-get bindings (intern (format ":%s" key)))))
                     (when (twidget-ref-p ref)
                       (let ((new-value
                              (cond
                               ;; No key/index - set whole value
                               ((null key-or-index) value)
                               ;; Keyword access for plist
                               ((keywordp key-or-index)
                                (let ((current (twidget-ref-value ref)))
                                  (if (or (null current) (plistp current))
                                      (plist-put (copy-sequence (or current nil)) key-or-index value)
                                    (error "Cannot use keyword access on non-plist value"))))
                               ;; Integer access for list
                               ((integerp key-or-index)
                                (let ((current (twidget-ref-value ref)))
                                  (if (listp current)
                                      (let ((new-list (copy-sequence current)))
                                        (setf (nth key-or-index new-list) value)
                                        new-list)
                                    (error "Cannot use index access on non-list value"))))
                               (t (error "KEY-OR-INDEX must be a keyword or integer")))))
                         ;; Update the ref value
                         (setf (twidget-ref-value ref) new-value)
                         ;; Notify watchers
                         (dolist (watcher (twidget-ref-watchers ref))
                           (funcall watcher new-value))
                         ;; Trigger buffer update
                         (twidget--trigger-update inst-key key new-value)))))))
             twidget-instances)))

(defun twidget--trigger-update (instance-id var-name value)
  "Trigger a reactive update for INSTANCE-ID when VAR-NAME changes to VALUE.
This uses tp.el to update the text in the buffer."
  (let* ((data (gethash instance-id twidget-instances))
         (template (plist-get data :template))
         (bindings (plist-get data :bindings))
         (sym (intern (format "twidget--text-%s" instance-id))))
    (when (and data template)
      ;; Rebuild bindings alist with current values
      (let ((new-bindings nil))
        (let ((plist bindings))
          (while plist
            (let ((key (car plist))
                  (val (cadr plist)))
              (when (keywordp key)
                (let* ((name (substring (symbol-name key) 1))
                       (ref-value (if (twidget-ref-p val)
                                      (twidget-ref-value val)
                                    val)))
                  (push (cons name ref-value) new-bindings))))
            (setq plist (cddr plist))))
        ;; Re-render the template with updated bindings
        (let ((new-text (twidget--expand-template template new-bindings instance-id)))
          ;; Update the reactive symbol - tp.el will handle buffer update
          (when (boundp sym)
            (set sym new-text)))))))

(defun twidget-substitute-placeholders (str bindings)
  "Substitute {variable} placeholders in STR with values from BINDINGS.
BINDINGS is an alist of (VAR-NAME . VALUE) pairs."
  (if (stringp str)
      (replace-regexp-in-string
       "{\\([^}]+\\)}"
       (lambda (match)
         (let* ((var-name (match-string 1 match))
                (binding (assoc var-name bindings)))
           (if binding
               (format "%s" (cdr binding))
             match)))
       str t t)
    str))

(defconst twidget--for-expression-rx
  (rx line-start
      (* whitespace)
      (group (+ (any alpha "_")) (* (any alnum "_")))  ; loop variable
      (+ whitespace)
      "in"
      (+ whitespace)
      (group (+ (any alpha "_")) (* (any alnum "_")))  ; collection name
      (* whitespace)
      line-end)
  "Regular expression to parse :for expressions like \"item in items\".")

(defun twidget-parse-for-expression (for-expr)
  "Parse a :for expression like \"item in items\".
Returns a cons cell (LOOP-VAR . COLLECTION-NAME) or nil if invalid."
  (when (string-match twidget--for-expression-rx for-expr)
    (cons (match-string 1 for-expr)
          (match-string 2 for-expr))))

;;; Utilities
;; ============================================================================

(defun twidget-inc (sym num)
  "Increment the numeric value stored in reactive variable SYM by NUM.
SYM is a symbol. The value can be a string or number."
  (let* ((current (twidget-get sym))
         (current-num (if (stringp current)
                          (string-to-number current)
                        (or current 0)))
         (new-val (+ current-num num)))
    (twidget-set sym (if (stringp current)
                         (number-to-string new-val)
                       new-val))))

(defun twidget-dec (sym num)
  "Decrement the numeric value stored in reactive variable SYM by NUM.
SYM is a symbol. The value can be a string or number."
  (let* ((current (twidget-get sym))
         (current-num (if (stringp current)
                          (string-to-number current)
                        (or current 0)))
         (new-val (- current-num num)))
    (twidget-set sym (if (stringp current)
                         (number-to-string new-val)
                       new-val))))

(defun twidget-reactive (var-name &optional initial-value)
  "Create or get a reactive variable named VAR-NAME.
If INITIAL-VALUE is provided, create a new reactive variable.
Returns the twidget-ref object."
  (let ((key (if (symbolp var-name) (symbol-name var-name) var-name)))
    ;; Look for existing ref or create new one
    (catch 'found
      (maphash (lambda (inst-key data)
                 (let ((bindings (plist-get data :bindings)))
                   (when bindings
                     (let ((ref (plist-get bindings (intern (format ":%s" key)))))
                       (when (twidget-ref-p ref)
                         (throw 'found ref))))))
               twidget-instances)
      ;; Not found, create a new one if initial value provided
      (when initial-value
        (twidget-ref initial-value)))))

;;; Built-in widgets

(define-tp tp-button (plist)
  (let ((action (plist-get plist :action))
        (bgcolor (plist-get plist :bgcolor)))
    `( face ( :box (:color "gray" :line-width (2 . 2)
                           :style released-button)
              :background ,bgcolor)
       keymap ,(let ((keymap (make-sparse-keymap)))
                 (define-key keymap (kbd "<RET>") action)
                 (define-key keymap [mouse-1] action)
                 keymap))))

(define-tp tp-space (pixel)
  `(display (space :width (,pixel))))

(define-tp tp-headline (props)
  (let (height boldp)
    (cond ((floatp props)
           (setq height props boldp t))
          ((plistp props)
           (setq height (plist-get props :height)
                 boldp (plist-get props :bold))))
    `(face (:height ,height
                    ,@(when boldp '(:weight bold))))))

(define-twidget button
  :slot t
  :props '(action (bgcolor . "orange"))
  :render (lambda (props slot)
            (let ((action (plist-get props :action))
                  (bgcolor (plist-get props :bgcolor)))
              (tp-add (format "%s%s%s"
                              (tp-set " " 'tp-space 6)
                              slot (tp-set " " 'tp-space 6))
                      'tp-button `(:bgcolor ,bgcolor :action ,action)))))

(define-twidget p
  :slot t :render (lambda (_props slot)
                    (concat slot "\n")))

(define-twidget div
  :slot t :render (lambda (_props slot)
                    (concat slot "\n")))

(define-twidget span
  :slot t :render (lambda (_props slot) slot))

(define-twidget headline
  :slot t
  :props '(height)
  :render (lambda (props slot)
            (concat (tp-set slot 'tp-headline
                            (plist-get props :height))
                    "\n")))

(define-twidget h1
  :extends 'headline
  :props '((height . 2.0))
  :render (lambda (props slot parent-render)
            (funcall parent-render props slot)))

(define-twidget h2
  :extends 'headline
  :props '((height . 1.7))
  :render (lambda (props slot parent-render)
            (funcall parent-render props slot)))

(define-twidget h3
  :extends 'headline
  :props '((height . 1.5))
  :render (lambda (props slot parent-render)
            (funcall parent-render props slot)))

(define-twidget h4
  :extends 'headline
  :props '((height . 1.3))
  :render (lambda (props slot parent-render)
            (funcall parent-render props slot)))

(define-twidget h5
  :extends 'headline
  :props '((height . 1.1))
  :render (lambda (props slot parent-render)
            (funcall parent-render props slot)))

(provide 'twidget)
;;; twidget.el ends here
