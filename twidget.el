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

;;; Variables
;; ============================================================================

(defvar twidget-alist nil
  "Alist of widget definitions: (WIDGET-NAME . DEFINITION).
Each DEFINITION is a plist with :props, :slot, and :render keys.")

(defvar-local twidget-reactive-data (make-hash-table :test 'equal)
  "Buffer-local hash table for storing reactive data bindings.
Keys are string variable names, values are the actual data values.")

;;; Widget Definition
;; ============================================================================

(defmacro define-twidget (name &rest args)
  "Define a text widget (widget) named NAME.

ARGS should include:
  :props - A quoted list of property definitions. Each can be:
           - A symbol: required property accessed via keyword
           - A cons cell (SYMBOL . DEFAULT): property with default value
  :slot  - Boolean value or list of slot names.
           nil (default) means widget does not support slot.
           t means widget supports a single default slot.
           A list of symbols defines named slots, e.g., \\='(header content footer)
  :slots - Alias for :slot with named slots (for clarity)
  :extends - Symbol of a parent widget to inherit from.
             The child widget inherits :props and :slot from the parent.
             Child :props override parent defaults; child :render can call parent-render.
  :render - A lambda that returns the rendered string.
            For single slot: (lambda (props slot) ...)
            For named slots: (lambda (props slots) ...) where slots is a plist
            When :extends is used: (lambda (props slot parent-render) ...)

The render function receives:
  - PROPS: a plist of resolved property values (with :keyword keys)
  - SLOT/SLOTS: For single slot (t), a string containing all slot values.
                For named slots, a plist with slot names as keywords.
  - PARENT-RENDER: When :extends is used, a function to call parent's render.

Named Slots Example:
  (define-twidget card
    :slots \\='(header content footer)
    :render (lambda (props slots)
              (concat (plist-get slots :header) \"\\n\"
                      (plist-get slots :content) \"\\n\"
                      (plist-get slots :footer))))

  ;; Usage - named slots use (slot<name> content...) sexp format:
  (twidget-parse
   \\='(card (slot-header \"Title\")
          (slot-content \"Body text\")
          (slot-footer \"Footer\")))

Component Inheritance Example:
  (define-twidget base-button
    :props \\='((type . \"default\"))
    :slot t
    :render (lambda (props slot)
              (tp-set slot \\='face \\='button)))

  (define-twidget primary-button
    :extends \\='base-button
    :props \\='((type . \"primary\"))
    :render (lambda (props slot parent-render)
              (let ((result (funcall parent-render props slot)))
                (tp-add result \\='face \\='(:foreground \"blue\")))))"
  (declare (indent defun))
  (let ((props nil)
        (slot :twidget--unspecified)  ; Sentinel value to detect if :slot was provided
        (extends nil)
        (render nil)
        (rest args))
    ;; Parse keyword arguments
    (while rest
      (pcase (car rest)
        (:props (setq props (cadr rest) rest (cddr rest)))
        (:slot (setq slot (cadr rest) rest (cddr rest)))
        (:slots (setq slot (cadr rest) rest (cddr rest)))  ; Alias for named slots
        (:extends (setq extends (cadr rest) rest (cddr rest)))
        (:render (setq render (cadr rest) rest (cddr rest)))
        (_ (error "Unknown keyword %S in define-twidget" (car rest)))))
    ;; Prepare slot value - the sentinel :twidget--unspecified needs to be passed as-is
    ;; Other values (t, nil, or list) should evaluate properly
    (let ((slot-form (if (eq slot :twidget--unspecified)
                         :twidget--unspecified
                       ;; If slot is a quoted list (from ':slots '(x y z)),
                       ;; the value is actually (quote (x y z)), so we just pass it
                       slot)))
      `(twidget-internal ',name ,props ,slot-form ,extends ,render))))

(defun twidget-internal (name props slot extends render)
  "Internal function to define a widget NAME with PROPS, SLOT, EXTENDS, and RENDER.
PROPS is a list of property definitions.
SLOT is a boolean, list of slot names, or :twidget--unspecified (not provided).
EXTENDS is a symbol of a parent widget to inherit from.
RENDER is the render function."
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
                            :render render))
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
  :bind - Bind a reactive data variable, e.g., :bind \"varname\"
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
            (bind-var nil)
            (for-expr nil)
            (local-bindings (copy-alist bindings)))
        ;; Parse keyword arguments first
        (while (and args (keywordp (car args)))
          (let ((key (car args))
                (val (cadr args)))
            (cond
             ;; Handle :bind directive
             ((eq key :bind)
              (setq bind-var val))
             ;; Handle :for directive
             ((eq key :for)
              (setq for-expr val))
             ;; Regular prop
             (t (push (cons key val) collected-props)))
            (setq args (cddr args))))
        ;; Handle :bind directive - store the binding value in reactive data
        (when bind-var
          (let* ((bind-key (if (stringp bind-var) bind-var (symbol-name bind-var)))
                 (bind-value (cdr (assoc bind-key local-bindings))))
            (when bind-value
              (twidget-set bind-key bind-value)
              ;; Add to local bindings for placeholder substitution
              (push (cons bind-key bind-value) local-bindings))))
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
                                 (twidget-prop-default prop-def))))))))
      ;; Call the render function
      (if extends
          ;; With inheritance, pass parent-render as third argument
          (funcall render-fn parsed-props slot-value parent-render-fn)
        ;; Normal render call
        (funcall render-fn parsed-props slot-value)))))

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
  "Extract variable names referenced in :bind and :for directives from FORM.
Returns a list of unique variable name symbols."
  (let ((vars nil))
    (cond
     ((not (listp form)) nil)
     ((and (listp form) (symbolp (car form)))
      ;; This is a widget form or subform
      (let ((rest (cdr form)))
        ;; Look for :bind and :for in keyword arguments
        (while (and rest (keywordp (car rest)))
          (let ((key (car rest))
                (val (cadr rest)))
            (cond
             ((eq key :bind)
              (when (stringp val)
                (push (intern val) vars)))
             ((eq key :for)
              (when (stringp val)
                (let ((parsed (twidget-parse-for-expression val)))
                  (when parsed
                    (push (intern (cdr parsed)) vars))))))
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
lexical variables referenced in :bind and :for directives.

Note: For variable capture to work, FORM must be a quoted literal.
Dynamic forms at runtime cannot capture lexical variables.

Example:
  (let ((editor \"emacs\")
        (editors \\='(\"emacs\" \"vim\" \"vscode\")))
    (twidget-insert
     \\='(div (p :bind \"editor\" \"Using {editor}\")
           (p :for \"e in editors\" \"Editor: {e}\"))))"
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

;;; Utilities
;; ============================================================================

(defun twidget-inc (sym num)
  "Increment the numeric string value stored in SYM by NUM.
SYM is a symbol whose value is a string representation of a number."
  (let ((str (symbol-value sym)))
    (set (make-local-variable sym)
         (number-to-string (+ (string-to-number str) num)))))

(defun twidget-dec (sym num)
  "Decrement the numeric string value stored in SYM by NUM.
SYM is a symbol whose value is a string representation of a number."
  (let ((str (symbol-value sym)))
    (set (make-local-variable sym)
         (number-to-string (- (string-to-number str) num)))))

;;; Reactive Data System
;; ============================================================================

(defun twidget-get (var-name)
  "Get the value of reactive data variable VAR-NAME.
VAR-NAME should be a symbol representing the variable name."
  (let ((key (if (symbolp var-name)
                 (symbol-name var-name)
               var-name)))
    (gethash key twidget-reactive-data)))

(defun twidget-set (var-name value)
  "Set the value of reactive data variable VAR-NAME to VALUE.
VAR-NAME should be a symbol representing the variable name."
  (let ((key (if (symbolp var-name)
                 (symbol-name var-name)
               var-name)))
    (puthash key value twidget-reactive-data)))

(defun twidget-substitute-placeholders (str bindings)
  "Substitute {variable} placeholders in STR with values from BINDINGS.
BINDINGS is an alist of (VAR-NAME . VALUE) pairs.
If a placeholder is not found in BINDINGS, it tries `twidget-reactive-data'."
  (if (stringp str)
      (replace-regexp-in-string
       "{\\([^}]+\\)}"
       (lambda (match)
         (let* ((var-name (match-string 1 match))
                (binding (assoc var-name bindings)))
           (if binding
               (format "%s" (cdr binding))
             ;; Try reactive data store
             (let ((reactive-val (gethash var-name twidget-reactive-data)))
               (if reactive-val
                   (format "%s" reactive-val)
                 match)))))
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
