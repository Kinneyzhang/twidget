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
         (let ((valid t)
               (lst object))
           (while (and valid lst)
             (if (and (keywordp (car lst)) (cdr lst))
                 (setq lst (cddr lst))
               (setq valid nil)))
           (and valid (null lst))))))

;;; Variables
;; ============================================================================

(defvar twidget-alist nil
  "Alist of widget definitions: (WIDGET-NAME . DEFINITION).
Each DEFINITION is a plist with :props, :slot, :render, :setup, :template,
:compiled-render, and :compiled-render-code keys.

The :compiled-render key holds a pre-compiled render function generated at
component definition time.  This function is the result of compiling the
:template into executable Elisp code, similar to how Vue 3 compiles templates
into render functions.

The :compiled-render-code key holds the source code (as a sexp) of the
compiled render function, useful for debugging and inspection.")

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

(defvar-local twidget-reactive-symbols (make-hash-table :test 'equal)
  "Buffer-local hash table for reactive text symbol tracking.
Keys are (instance-id . var-name), values are lists of symbols to update.")

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
  (format "twidget-instance-%d" (cl-incf twidget--instance-counter)))

(defun twidget--register-ref (instance-id var-name ref)
  "Register a reactive REF for INSTANCE-ID with VAR-NAME."
  (puthash (cons instance-id var-name) ref twidget-ref-registry))

(defvar-local twidget-reactive-text-counter 0
  "Counter for generating unique reactive text IDs.")

(defvar-local twidget-reactive-prop-counter 0
  "Counter for generating unique reactive property IDs.")

(defvar-local twidget-reactive-prop-symbols (make-hash-table :test 'equal)
  "Buffer-local hash table for reactive text property symbol tracking.
Keys are (instance-id . \"__props__\"), values are lists of (sym value-fn prop-name) triples.")

(defun twidget--parse-dot-notation (var-expr)
  "Parse VAR-EXPR with dot notation like \"info.name\", \"items.0\", or \"data.0.name\".
Returns a cons cell (BASE-VAR . ACCESSORS) where:
  - BASE-VAR is the base variable name (string)
  - ACCESSORS is a list of accessors (can be empty for simple vars).
    Each accessor is a keyword for plist access or an integer for list access.
Examples:
  \"count\" -> (\"count\")
  \"info.name\" -> (\"info\" :name)
  \"items.0\" -> (\"items\" 0)
  \"data.0.name\" -> (\"data\" 0 :name)"
  (save-match-data
    (let ((parts (split-string var-expr "\\." t)))
      (if (= (length parts) 1)
          ;; No dot notation - simple variable, return list with just the base var
          (list var-expr)
        ;; Has dot notation - first part is base var, rest are accessors
        (let ((base-var (car parts))
              (accessor-strs (cdr parts))
              (accessors nil))
          (dolist (acc-str accessor-strs)
            (push (if (string-match "^[0-9]+$" acc-str)
                      (string-to-number acc-str)
                    (intern (format ":%s" acc-str)))
                  accessors))
          (cons base-var (nreverse accessors)))))))

(defun twidget--get-nested-value (value accessors)
  "Get the nested VALUE using ACCESSORS.
ACCESSORS is a list of accessors, each being a keyword (plist access) or an integer (list access).
Throws an error if the accessor is out of bounds for lists.
Examples:
  (twidget--get-nested-value data \\='()) -> data
  (twidget--get-nested-value info \\='(:name)) -> (plist-get info :name)
  (twidget--get-nested-value items \\='(0)) -> (nth 0 items)
  (twidget--get-nested-value data \\='(0 :name)) -> (plist-get (nth 0 data) :name)"
  (if (null accessors)
      value
    (let ((current value))
      (dolist (accessor accessors)
        (cond
         ((keywordp accessor)
          (if (plistp current)
              (setq current (plist-get current accessor))
            (error "Cannot use keyword access on non-plist value")))
         ((integerp accessor)
          (cond
           ((not (listp current))
            (error "Cannot use index access on non-list value"))
           ((null current)
            (error "Cannot access index %d of empty list" accessor))
           ((or (< accessor 0) (>= accessor (length current)))
            (error "Index %d out of bounds for list of length %d" accessor (length current)))
           (t (setq current (nth accessor current)))))
         (t (warn "twidget--get-nested-value: Unknown accessor type: %S" accessor))))
      current)))

(defun twidget--apply-reactive-text (text instance-id var-name)
  "Apply reactive tracking to TEXT for INSTANCE-ID with VAR-NAME.
Returns text with tp.el properties for reactive updates."
  ;; Create a unique reactive symbol for this specific text occurrence
  (let* ((text-id (cl-incf twidget-reactive-text-counter))
         (sym (intern (format "twidget--rtext-%d" text-id))))
    ;; Set the symbol value to just the reactive text (e.g., "0")
    (set sym text)
    ;; Register this symbol for updates when the var changes
    (let ((key (cons instance-id var-name)))
      (puthash key
               (cons sym (gethash key twidget-reactive-symbols))
               twidget-reactive-symbols))
    ;; Use tp-set with the tp-text property type and the reactive symbol reference
    (tp-set text 'tp-text (intern (format "$%s" sym)))))

(defun twidget--apply-static-tp-props (text props-plist)
  "Apply static tp.el text properties from PROPS-PLIST to TEXT.
PROPS-PLIST is a plist like (face (:background \"red\") tp-palette info).
Uses tp-add to apply all properties at once with proper merging."
  (if props-plist
      (apply #'tp-add text props-plist)
    text))

(defun twidget--apply-reactive-tp-props (text value-fn instance-id)
  "Apply reactive tp.el text properties to TEXT using VALUE-FN.
VALUE-FN is a function that returns a props plist when called.
INSTANCE-ID is used for reactive tracking.
Returns text with tp.el properties for reactive updates.
Uses tp-add to apply all properties at once with proper merging."
  ;; For reactive tp-props, we create symbols for each property value
  ;; and build a plist with symbol references, then apply with tp-add
  (let* ((prop-id (cl-incf twidget-reactive-prop-counter))
         (initial-props (funcall value-fn))
         (reactive-props nil))
    ;; Build reactive props plist with symbol references
    (let ((props initial-props))
      (while props
        (let* ((prop-name (car props))
               (prop-value (cadr props))
               (sym (intern (format "twidget--rprop-%d-%s" prop-id prop-name))))
          ;; Set the symbol value
          (set sym prop-value)
          ;; Register for updates
          (let ((key (cons instance-id "__props__")))
            (puthash key
                     (cons (list sym value-fn prop-name) (gethash key twidget-reactive-prop-symbols))
                     twidget-reactive-prop-symbols))
          ;; Add to reactive props plist with symbol reference
          (setq reactive-props (append reactive-props
                                       (list prop-name (intern (format "$%s" sym)))))
          (setq props (cddr props)))))
    ;; Apply all properties at once with tp-add for proper merging
    (if reactive-props
        (apply #'tp-add text reactive-props)
      text)))

(defun twidget--update-reactive-prop-symbols (instance-id)
  "Update all reactive property symbols for INSTANCE-ID.
Called when any reactive variable changes to recompute property values."
  (let* ((key (cons instance-id "__props__"))
         (entries (gethash key twidget-reactive-prop-symbols)))
    (dolist (entry entries)
      (let* ((sym (car entry))
             (value-fn (cadr entry))
             (prop-name (caddr entry)))
        (when (and sym value-fn prop-name)
          ;; Recompute the props plist and extract this property's value
          (let* ((props (funcall value-fn))
                 (new-value (plist-get props prop-name)))
            (set sym new-value)))))))

(defun twidget--parse-tp-props-value (tp-props-expr reactive-bindings)
  "Parse TP-PROPS-EXPR and return a plist with tp-props info.
TP-PROPS-EXPR is a plist of tp.el text properties, e.g.:
  (tp-button (:palette info))
  (face (:background \"red\"))
Or a string referencing a method that returns such a plist:
  \"getProps()\"

REACTIVE-BINDINGS is the plist from :setup.

Returns a plist with:
  :value-fn - a function to get the props (for reactive props)
  :static-value - the static props plist (for non-reactive props)"
  (cond
   ;; String expression - could be method call or variable reference
   ((stringp tp-props-expr)
    (let ((trimmed (string-trim tp-props-expr)))
      (cond
       ;; Method call: "getProps()"
       ((string-match "^\\([a-zA-Z_][a-zA-Z0-9_-]*\\)\\s-*(\\(.*\\))$" trimmed)
        (let* ((method-name (match-string 1 trimmed))
               (method-key (intern (format ":%s" method-name)))
               (method-fn (plist-get reactive-bindings method-key)))
          (if (functionp method-fn)
              (list :value-fn method-fn :static-value nil)
            ;; Not a method, treat as literal string
            (list :value-fn nil :static-value tp-props-expr))))
       ;; Variable reference: "propsVar" (only if it exists in bindings)
       ((string-match "^\\([a-zA-Z_][a-zA-Z0-9_-]*\\)$" trimmed)
        (let* ((var-name trimmed)
               (var-key (intern (format ":%s" var-name)))
               (ref-or-val (plist-get reactive-bindings var-key)))
          (cond
           ;; It's a reactive ref - create a value-fn that reads it
           ((twidget-ref-p ref-or-val)
            (list :value-fn (lambda () (twidget-get (intern var-name)))
                  :static-value nil))
           ;; It's a function - use it as value-fn
           ((functionp ref-or-val)
            (list :value-fn ref-or-val :static-value nil))
           ;; It's a static value from bindings
           (ref-or-val
            (list :value-fn nil :static-value ref-or-val))
           ;; Not found in bindings - treat as literal string
           (t (list :value-fn nil :static-value tp-props-expr)))))
       ;; Other string - treat as literal
       (t (list :value-fn nil :static-value tp-props-expr)))))
   ;; Direct plist value - static
   ((listp tp-props-expr)
    (list :value-fn nil :static-value tp-props-expr))
   ;; Other types (numbers, symbols, etc.) - treat as static values
   (t (list :value-fn nil :static-value tp-props-expr))))

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
              t (default) means widget supports a single default slot.
              nil means widget does not support slot.
              A list of symbols defines named slots, e.g., \\='(header content footer).
     :slots - Alias for :slot with named slots (for clarity)
     :type  - Widget display type (symbol).
              \\='inline (default) - Inline element, no automatic line breaks.
              \\='block - Block element, adds newline after content when followed
                        by other content.
     :extends - Symbol of a parent widget to inherit from.
     :render - A lambda that returns the rendered string.
               Parameters follow consistent types:
               - props: plist type (e.g., (:color \"red\" :size 10))
               - slot: list type (always a list, even for single value)
               For single slot: (lambda (props slot) ...) where slot is a list
               For named slots: (lambda (props slots) ...) where slots is a plist
                                with each value being a list
               When :extends is used: (lambda (props slot parent-render) ...)
               Use `twidget-slot-to-string' to convert a list slot to a string.

   Example:
     (define-twidget button
       :slot t
       :props \\='(action (bgcolor . \"orange\"))
       :render (lambda (props slot)
                 (let ((action (plist-get props :action))
                       (bgcolor (plist-get props :bgcolor))
                       (slot-str (twidget-slot-to-string slot)))
                   (tp-add (format \"%s%s%s\"
                                   (tp-set \" \" \\='tp-space 6)
                                   slot-str (tp-set \" \" \\='tp-space 6))
                           \\='tp-button `(:bgcolor ,bgcolor :action ,action)))))

2. Composite Widget (using :setup and :template):
   For widgets that compose other widgets with reactive data.

   ARGS should include:
     :props - Same as above
     :slot  - Same as above
     :setup - A lambda that receives props and slot, returns a plist of reactive bindings.
              Use `twidget-ref' to create reactive values.
              (lambda (props slot) (list :count (twidget-ref slot)))
     :template - A quoted template sexp that defines the widget structure.
                 Use {varname} syntax in strings to bind to reactive data.
                 Template elements can be nested widget forms.

   Example:
     (define-twidget my-counter
       :slot t
       :setup (lambda (_props slot)
                (list :count (twidget-ref slot)))
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
        (type :twidget--unspecified)  ; Sentinel value to detect if :type was provided
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
        (:type (setq type (cadr rest) rest (cddr rest)))
        (:extends (setq extends (cadr rest) rest (cddr rest)))
        (:render (setq render (cadr rest) rest (cddr rest)))
        (:setup (setq setup (cadr rest) rest (cddr rest)))
        (:template (setq template (cadr rest) rest (cddr rest)))
        (_ (error "Unknown keyword %S in define-twidget" (car rest)))))
    ;; Validate: either :render or (:setup and :template) must be provided
    ;; (unless :extends is used, which inherits from parent)
    (when (and render (or setup template))
      (error "Cannot use both :render and :setup/:template in define-twidget"))
    ;; XOR check: if one of setup/template is provided, both must be provided
    (when (not (eq (null setup) (null template)))
      (error "Both :setup and :template must be provided together"))
    ;; Ensure at least one rendering method is provided (unless extending)
    (when (and (not extends) (not render) (not (and setup template)))
      (error "Widget must have :render or both :setup and :template (or use :extends)"))
    ;; Prepare slot value - the sentinel :twidget--unspecified needs to be passed as-is
    ;; Other values (t, nil, or list) should evaluate properly
    (let ((slot-form (if (eq slot :twidget--unspecified)
                         :twidget--unspecified
                       slot))
          (type-form (if (eq type :twidget--unspecified)
                         :twidget--unspecified
                       type)))
      `(twidget-internal ',name ,props ,slot-form ,type-form ,extends ,render ,setup ,template))))

;;; Template Compilation (Render Function Generation)
;; ============================================================================
;;
;; This module implements Vue 3-style template compilation. At component
;; definition time, the :template sexp is compiled into an executable Elisp
;; render function that contains actual loops, conditionals, and string
;; operations - not just calls to interpreter functions.
;;
;; ## Architecture
;;
;; The compilation system is designed to be extensible for future directives.
;; Each directive (like :for, :if, :show) has a corresponding code generator
;; registered in `twidget--directive-compilers`.
;;
;; ## Directive System
;;
;; Directives are special keywords in widget forms that control rendering:
;; - :for "item in items" - Loop iteration (generates dolist)
;; - :if "condition" - Conditional rendering (generates when) [future]
;; - :show "condition" - Visibility control [future]
;;
;; Each directive compiler is a function that takes:
;;   (directive-value widget-name other-args inner-code-generator)
;; And returns Elisp code that implements the directive's behavior.
;;
;; ## Example Compilation
;;
;;   Template: '(div (span :for "item in items" :if "visible" "{item}"))
;;   Compiles to:
;;     (lambda (bindings instance-id reactive-bindings)
;;       (let ((--result-- ""))
;;         (when (twidget--eval-condition "visible" bindings)  ; :if
;;           (dolist (--item-- (cdr (assoc "items" bindings))) ; :for
;;             (let ((bindings (cons (cons "item" --item--) bindings)))
;;               (setq --result-- (concat --result-- ...)))))
;;         --result--))

(defvar twidget--directive-compilers
  (make-hash-table :test 'eq)
  "Hash table of directive compilers.
Keys are directive keywords (e.g., :for, :if).
Values are compiler functions with signature:
  (directive-value widget-name remaining-args body-code)
Where body-code is the already-generated Elisp code for the inner content.
The function should return Elisp code that wraps the body-code.")

(defun twidget--register-directive-compiler (directive compiler-fn)
  "Register COMPILER-FN as the compiler for DIRECTIVE.
DIRECTIVE is a keyword like :for or :if.
COMPILER-FN is a function that generates code for the directive."
  (puthash directive compiler-fn twidget--directive-compilers))

(defun twidget--get-directive-compiler (directive)
  "Get the compiler function for DIRECTIVE, or nil if not registered."
  (gethash directive twidget--directive-compilers))

(defun twidget--directive-keyword-p (keyword)
  "Return non-nil if KEYWORD is a registered directive."
  (and (keywordp keyword)
       (gethash keyword twidget--directive-compilers)))

;; Register the :for directive compiler
(twidget--register-directive-compiler
 :for
 (lambda (for-expr _widget-name _remaining-args body-code)
   "Compile :for directive into a dolist loop.
BODY-CODE is the generated Elisp code for rendering the inner content.
Returns a list of rendered items, which can then be joined by parent
components (e.g., col joins with newlines, span concatenates)."
   (let ((parsed (twidget-parse-for-expression for-expr)))
     (if parsed
         (let* ((loop-var (car parsed))
                (collection-name (cdr parsed))
                (loop-var-sym (make-symbol (concat "--" loop-var "--")))
                (collection-sym (make-symbol "--collection--"))
                (result-sym (make-symbol "--for-result--")))
           `(let ((,result-sym nil)
                  (,collection-sym (cdr (assoc ,collection-name bindings))))
              (if (listp ,collection-sym)
                  (dolist (,loop-var-sym ,collection-sym)
                    (let ((bindings (cons (cons ,loop-var ,loop-var-sym) bindings)))
                      (push ,body-code ,result-sym)))
                (warn "twidget: :for collection `%s' is not a list" ,collection-name))
              (nreverse ,result-sym)))
       `(progn
          (warn "twidget: Invalid :for expression: %s" ,for-expr)
          nil)))))

;; Register the :if directive compiler
(twidget--register-directive-compiler
 :if
 (lambda (condition-expr _widget-name _remaining-args body-code)
   "Compile :if directive into a conditional.
BODY-CODE is the generated Elisp code for rendering the inner content."
   `(if (twidget--eval-condition-expr ,condition-expr bindings)
        ,body-code
      "")))

;; Register the :show directive compiler (similar to :if but could add CSS-like hiding)
(twidget--register-directive-compiler
 :show
 (lambda (condition-expr _widget-name _remaining-args body-code)
   "Compile :show directive - renders but may mark as hidden.
BODY-CODE is the generated Elisp code for rendering the inner content."
   `(if (twidget--eval-condition-expr ,condition-expr bindings)
        ,body-code
      "")))

(defun twidget--eval-condition-expr (condition-expr bindings)
  "Evaluate CONDITION-EXPR at runtime using BINDINGS.
CONDITION-EXPR is a string like \"count > 0\" or \"visible\".
BINDINGS is an alist of (VAR-NAME . VALUE) pairs.
Returns non-nil if the condition is truthy."
  (let ((trimmed (string-trim condition-expr)))
    (cond
     ;; Simple variable reference - check if truthy
     ((string-match "^\\([a-zA-Z_][a-zA-Z0-9_-]*\\)$" trimmed)
      (let* ((var-name trimmed)
             (binding (assoc var-name bindings)))
        (when binding
          (let ((val (cdr binding)))
            (and val (not (equal val 0)) (not (equal val "")))))))
     ;; Negation: !var
     ((string-match "^!\\s*\\([a-zA-Z_][a-zA-Z0-9_-]*\\)$" trimmed)
      (let* ((var-name (match-string 1 trimmed))
             (binding (assoc var-name bindings)))
        (if binding
            (let ((val (cdr binding)))
              (not (and val (not (equal val 0)) (not (equal val "")))))
          t)))
     ;; Comparison: var === value, var > value, etc.
     ((string-match "^\\([a-zA-Z_][a-zA-Z0-9_-]*\\)\\s*\\(===?\\|!==?\\|[<>]=?\\)\\s*\\(.+\\)$" trimmed)
      (let* ((var-name (match-string 1 trimmed))
             (operator (match-string 2 trimmed))
             (value-str (string-trim (match-string 3 trimmed)))
             (binding (assoc var-name bindings))
             (var-value (when binding (cdr binding)))
             (compare-value (twidget--parse-literal-value value-str)))
        (pcase operator
          ((or "===" "==") (equal var-value compare-value))
          ((or "!==" "!=") (not (equal var-value compare-value)))
          (">" (and (numberp var-value) (numberp compare-value) (> var-value compare-value)))
          ("<" (and (numberp var-value) (numberp compare-value) (< var-value compare-value)))
          (">=" (and (numberp var-value) (numberp compare-value) (>= var-value compare-value)))
          ("<=" (and (numberp var-value) (numberp compare-value) (<= var-value compare-value)))
          (_ nil))))
     ;; Default - try to evaluate as truthy
     (t nil))))

(defun twidget--parse-literal-value (value-str)
  "Parse VALUE-STR as a literal value (string, number, boolean)."
  (let ((trimmed (string-trim value-str)))
    (cond
     ((or (string-match "^\"\\(.*\\)\"$" trimmed)
          (string-match "^'\\(.*\\)'$" trimmed))
      (match-string 1 trimmed))
     ((string-match "^-?[0-9]+\\(?:\\.[0-9]+\\)?$" trimmed)
      (string-to-number trimmed))
     ((string= trimmed "true") t)
     ((string= trimmed "false") nil)
     ((string= trimmed "nil") nil)
     (t trimmed))))

;;; Template Compilation Entry Points
;; ============================================================================

(defun twidget--compile-template-to-render-fn (template)
  "Compile TEMPLATE into an executable render function.

This is the main entry point for template compilation.  It generates
Elisp code from the template and evaluates it to create a function.

The returned function has signature:
  (lambda (bindings instance-id reactive-bindings) ...)

Where:
- BINDINGS is an alist of (VAR-NAME . VALUE) pairs
- INSTANCE-ID is the widget instance identifier
- REACTIVE-BINDINGS is the plist from :setup

The generated render function contains actual Elisp code (loops, conditionals,
string operations) rather than calls to template interpretation functions."
  (let ((render-code (twidget--generate-render-code template)))
    ;; Evaluate the generated code to create the render function
    (eval render-code t)))

(defun twidget--generate-render-code (template)
  "Generate Elisp render function code from TEMPLATE.

Returns a lambda expression that, when evaluated, produces a render function.
The generated code contains actual loops and operations."
  `(lambda (bindings instance-id reactive-bindings)
     ,(twidget--generate-template-expr template)))

(defun twidget--generate-template-expr (template)
  "Generate an expression that renders TEMPLATE.

Returns Elisp code that produces the rendered output for TEMPLATE.
This function dispatches based on template type:
- String: generates placeholder substitution code or literal
- Widget form: generates widget invocation with directive expansion
- List of forms: generates code to render and concatenate each form"
  (cond
   ;; String template - generate placeholder substitution or literal
   ((stringp template)
    (twidget--generate-string-expr template))
   ;; Widget form (symbol at car position)
   ((and (listp template)
         (symbolp (car template)))
    (twidget--generate-widget-expr template))
   ;; List of forms - generate concatenation code
   ((listp template)
    (twidget--generate-list-expr template))
   ;; Other - just convert to string
   (t `(format "%s" ',template))))

(defun twidget--generate-string-expr (str)
  "Generate code for rendering string STR with placeholders.

For strings without placeholders, returns the string literal.
For strings with {var} placeholders, generates substitution code."
  (if (string-match-p "{[^}]+}" str)
      ;; Has placeholders - generate substitution code
      `(twidget--expand-template-string ,str bindings instance-id)
    ;; No placeholders - return literal string
    str))

(defun twidget--generate-widget-expr (widget-form)
  "Generate code for rendering WIDGET-FORM.

WIDGET-FORM is a list like (widget-name :prop val ... slot-content...).
Analyzes the form for directives and generates appropriate code with
nested directive wrappers."
  (let* ((widget-name (car widget-form))
         (args (cdr widget-form))
         ;; Extract all directives and other args
         (parse-result (twidget--extract-directives args))
         (directives (plist-get parse-result :directives))
         (other-args (plist-get parse-result :other-args)))
    ;; Generate code with nested directive wrappers
    (twidget--generate-widget-with-directives widget-name directives other-args)))

(defun twidget--extract-directives (args)
  "Extract directive arguments from ARGS.

Returns a plist with:
  :directives - list of (DIRECTIVE-KEYWORD . VALUE) pairs in order
  :other-args - remaining arguments (props, slots, events)"
  (let ((directives nil)
        (other-args nil)
        (current args))
    (while current
      (let ((arg (car current)))
        (cond
         ;; Check if it's a directive keyword
         ((and (keywordp arg)
               (twidget--directive-keyword-p arg))
          (push (cons arg (cadr current)) directives)
          (setq current (cddr current)))
         ;; Regular keyword argument
         ((keywordp arg)
          (push arg other-args)
          (push (cadr current) other-args)
          (setq current (cddr current)))
         ;; Non-keyword (slot content)
         (t
          (push arg other-args)
          (setq current (cdr current))))))
    (list :directives (nreverse directives)
          :other-args (nreverse other-args))))

(defun twidget--generate-widget-with-directives (widget-name directives other-args)
  "Generate code for WIDGET-NAME with DIRECTIVES applied.

DIRECTIVES is a list of (KEYWORD . VALUE) pairs.
OTHER-ARGS are the remaining props and slots.

Directives are applied in order, wrapping the inner widget rendering.
For example, :if wraps :for which wraps the actual widget call."
  (let* ((inner-form (cons widget-name other-args))
         ;; The innermost code - the actual widget rendering
         (inner-code `(twidget--expand-template-widget ',inner-form bindings instance-id reactive-bindings)))
    ;; Apply directives from innermost to outermost (reverse order)
    (dolist (directive (reverse directives))
      (let* ((directive-key (car directive))
             (directive-value (cdr directive))
             (compiler (twidget--get-directive-compiler directive-key)))
        (when compiler
          (setq inner-code
                (funcall compiler directive-value widget-name other-args inner-code)))))
    inner-code))

(defun twidget--generate-list-expr (forms)
  "Generate code for rendering a list of FORMS.

Generates inline code that renders each form and concatenates the results."
  (if (null forms)
      ""
    (let ((result-sym (make-symbol "--list-result--")))
      ;; Generate code that builds result incrementally
      `(let ((,result-sym ""))
         ,@(cl-loop for form in forms
                    for i from 0
                    for expr = (twidget--generate-template-expr form)
                    for is-first = (= i 0)
                    for is-last = (= i (1- (length forms)))
                    collect
                    `(let ((--rendered-- ,expr)
                           (--is-block-- (twidget-is-block-widget-p ',form)))
                       ;; Add newline BEFORE block element if there's previous content
                       (when (and --is-block-- (not ,is-first) (not (string-empty-p ,result-sym)))
                         (unless (string-suffix-p "\n" ,result-sym)
                           (setq ,result-sym (concat ,result-sym "\n"))))
                       ;; Append rendered content
                       (setq ,result-sym (concat ,result-sym
                                                 (if (stringp --rendered--)
                                                     --rendered--
                                                   (format "%s" --rendered--))))
                       ;; Add newline AFTER block element if not last
                       (when (and --is-block-- (not ,is-last))
                         (unless (string-suffix-p "\n" ,result-sym)
                           (setq ,result-sym (concat ,result-sym "\n"))))))
         ,result-sym))))

(defun twidget-internal (name props slot type extends render setup template)
  "Internal function to define a widget NAME with PROPS, SLOT, TYPE, EXTENDS, RENDER, SETUP, and TEMPLATE.
PROPS is a list of property definitions.
SLOT is a boolean, list of slot names, or :twidget--unspecified (not provided).
TYPE is the widget display type (inline or block), or :twidget--unspecified.
EXTENDS is a symbol of a parent widget to inherit from.
RENDER is the render function (for simple widgets).
SETUP is a function that returns reactive bindings (for composite widgets).
TEMPLATE is a template sexp (for composite widgets)."
  ;; Handle inheritance if :extends is specified
  (let* ((slot-was-specified (not (eq slot :twidget--unspecified)))
         (type-was-specified (not (eq type :twidget--unspecified)))
         (final-slot (if slot-was-specified slot t))  ; Default to t (slot supported)
         (final-type (if type-was-specified type 'inline))  ; Default to 'inline
         (final-props props)
         (parent-render nil)
         (compiled-render nil))
    (when extends
      (let ((parent-def (cdr (assoc extends twidget-alist))))
        (unless parent-def
          (error "Parent widget not found: %S" extends))
        ;; Inherit slot from parent only if child didn't specify :slot
        (unless slot-was-specified
          (setq final-slot (plist-get parent-def :slot)))
        ;; Inherit type from parent only if child didn't specify :type
        (unless type-was-specified
          (setq final-type (or (plist-get parent-def :type) 'inline)))
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
    ;; Compile template into render function if this is a composite widget
    (let ((compiled-render-code nil))
      (when (and setup template)
        (setq compiled-render-code (twidget--generate-render-code template))
        (setq compiled-render (eval compiled-render-code t)))
      (let ((definition (list :props final-props
                              :slot final-slot
                              :type final-type
                              :extends extends
                              :parent-render parent-render
                              :render render
                              :setup setup
                              :template template
                              :compiled-render compiled-render
                              :compiled-render-code compiled-render-code))
            (existing (assoc name twidget-alist)))
        (if existing
            (setcdr existing definition)
          (push (cons name definition) twidget-alist)))
      (assoc name twidget-alist))))

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

;;; Type Helpers
;; ============================================================================

(defun twidget-get-widget-type (widget-name)
  "Get the display type of widget WIDGET-NAME.
Returns \\='inline (default) or \\='block."
  (let ((definition (cdr (assoc widget-name twidget-alist))))
    (if definition
        (or (plist-get definition :type) 'inline)
      'inline)))

(defun twidget-get-render-function (widget-name)
  "Get the compiled render function for widget WIDGET-NAME.

Returns the pre-compiled render function if the widget is a composite
widget (has both :setup and :template), or nil if:
- The widget is not defined
- The widget is a simple widget (uses :render instead of :template)
- The widget has no compiled render function

The returned function has signature:
  (lambda (bindings instance-id reactive-bindings) ...)

Example usage:
  (twidget-get-render-function \\='my-counter)
  ;; => #<compiled-function ...>

  ;; To see the generated code, use `twidget-get-render-function-code' instead."
  (let ((definition (cdr (assoc widget-name twidget-alist))))
    (when definition
      (plist-get definition :compiled-render))))

(defun twidget-get-render-function-code (widget-name)
  "Get the source code of the compiled render function for widget WIDGET-NAME.

Returns the Elisp code (as a sexp) that was generated for the widget's
render function, or nil if the widget has no compiled render function.

This is useful for debugging and understanding what code was generated
from the template.

Example:
  (twidget-get-render-function-code \\='my-counter)
  ;; => (lambda (bindings instance-id reactive-bindings)
  ;;      (let ((--result-- \"\"))
  ;;        ...
  ;;        --result--))"
  (let ((definition (cdr (assoc widget-name twidget-alist))))
    (when definition
      (plist-get definition :compiled-render-code))))

(defun twidget-is-block-widget-p (form)
  "Return non-nil if FORM represents a block-type widget.
FORM should be a widget form like (widget-name ...)."
  (and (listp form)
       (symbolp (car form))
       (assoc (car form) twidget-alist)
       (eq (twidget-get-widget-type (car form)) 'block)))

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

When there is a single slot value, it is passed directly to the render function.
When there are multiple slot values, they are passed as a list, allowing the
component to flexibly organize and connect the slots internally.
Use `twidget-slot-to-string' to convert a list slot to a string when needed.

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
            (tp-props-value nil)  ; Store :tp-props value
            (local-bindings (copy-alist bindings)))
        ;; Parse keyword arguments first
        (while (and args (keywordp (car args)))
          (let ((key (car args))
                (val (cadr args)))
            (cond
             ;; Handle :for directive
             ((eq key :for)
              (setq for-expr val))
             ;; Handle :tp-props specially
             ((eq key :tp-props)
              (setq tp-props-value val))
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
                        ;; Return a list of results, not concatenated string
                        ;; This allows parent components (like col) to join them as needed
                        (cl-return-from twidget-parse (nreverse results)))
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
        ;; Combine default slot parts if any (as a list, not concatenated)
        (when (and named-slots-p slot-parts)
          (push (cons :default (nreverse slot-parts))
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
              (template (plist-get definition :template))
              (compiled-render (plist-get definition :compiled-render))
              (rendered-result nil))
          (setq rendered-result
                (if (and setup-fn template)
                    ;; Composite widget: use compiled render if available
                    (twidget--render-composite setup-fn template parsed-props slot-value compiled-render)
                  ;; Simple widget: call the render function
                  (if extends
                      ;; With inheritance, pass parent-render as third argument
                      (funcall render-fn parsed-props slot-value parent-render-fn)
                    ;; Normal render call
                    (funcall render-fn parsed-props slot-value))))
          ;; Apply :tp-props if specified
          (when tp-props-value
            (setq rendered-result (twidget--apply-static-tp-props rendered-result tp-props-value)))
          rendered-result)))))

(defun twidget--render-composite (setup-fn template props slot &optional compiled-render)
  "Render a composite widget using SETUP-FN and TEMPLATE.
SETUP-FN is a function that receives props and slot, returns reactive bindings.
TEMPLATE is a template sexp to expand and render.
PROPS is the parsed props plist.
SLOT is the slot value (if any).
COMPILED-RENDER is the pre-compiled render function (if available)."
  ;; Call setup function to get reactive bindings (pass both props and slot)
  (let* ((reactive-bindings (funcall setup-fn props slot))
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
    ;; Use compiled render function if available, otherwise expand template
    (if compiled-render
        (funcall compiled-render bindings instance-id reactive-bindings)
      (twidget--expand-template template bindings instance-id reactive-bindings))))

(defun twidget--expand-template (template bindings instance-id &optional reactive-bindings)
  "Expand TEMPLATE sexp into rendered string.
TEMPLATE is a widget form or list of forms.
BINDINGS is an alist of (VAR-NAME . VALUE) for placeholder substitution.
INSTANCE-ID is the widget instance identifier for reactivity.
REACTIVE-BINDINGS is the original plist from :setup, used for event handlers."
  (cond
   ;; String - substitute placeholders and wrap with reactive overlay if needed
   ((stringp template)
    (twidget--expand-template-string template bindings instance-id))
   ;; Widget form - parse it with bindings
   ((and (listp template)
         (symbolp (car template))
         (assoc (car template) twidget-alist))
    (twidget--expand-template-widget template bindings instance-id reactive-bindings))
   ;; List of forms - process each with block element handling
   ((listp template)
    (twidget--expand-template-list template bindings instance-id reactive-bindings))
   ;; Other - convert to string
   (t (format "%s" template))))

(defun twidget--expand-template-list (forms bindings instance-id &optional reactive-bindings)
  "Expand a list of FORMS into a rendered string with block element handling.
BINDINGS is an alist of (VAR-NAME . VALUE) for placeholder substitution.
INSTANCE-ID is the widget instance identifier for reactivity.
REACTIVE-BINDINGS is the original plist from :setup, used for event handlers.

Block element handling follows HTML-like rules:
- Block elements start on a new line (newline added before if preceded by content)
- Block elements are followed by a newline (when followed by other content)
- Inline elements flow together without automatic newlines"
  (let ((result "")
        (forms-list forms)
        (len (length forms)))
    (dotimes (i len)
      (let* ((form (nth i forms-list))
             (is-first (= i 0))
             (is-last (= i (1- len)))
             (is-block (twidget-is-block-widget-p form))
             (rendered-val (twidget--expand-template form bindings instance-id reactive-bindings))
             ;; Convert to string for concatenation if needed
             (rendered (if (stringp rendered-val) rendered-val (format "%s" rendered-val))))
        ;; Add newline BEFORE block element if there's previous content
        ;; (Block elements start on a new line)
        (when (and is-block (not is-first) (not (string-empty-p result)))
          (unless (string-suffix-p "\n" result)
            (setq result (concat result "\n"))))
        ;; Append the rendered content
        (setq result (concat result rendered))
        ;; Add newline AFTER block element if not the last element
        ;; (Block elements are followed by a newline)
        (when (and is-block (not is-last))
          (unless (string-suffix-p "\n" result)
            (setq result (concat result "\n"))))))
    result))

(defun twidget--expand-template-string (str bindings instance-id)
  "Expand STR template string with BINDINGS.
INSTANCE-ID is used for reactive tracking.
Only applies reactive tracking to individual placeholder values, not the entire string.
Supports dot notation for nested access:
  {info.name} - plist property access
  {items.0} - list index access"
  (let ((result "")
        (start 0))
    ;; Process string segment by segment
    (while (string-match "{\\([^}]+\\)}" str start)
      (let* ((match-start (match-beginning 0))
             (match-end (match-end 0))
             (var-expr (match-string 1 str))
             ;; Parse dot notation: "data.0.name" -> ("data" 0 :name)
             (parsed (twidget--parse-dot-notation var-expr))
             (base-var (car parsed))
             (accessors (cdr parsed))
             (binding (assoc base-var bindings))
             ;; Check if base variable is a reactive ref
             (ref-info (gethash (cons instance-id base-var) twidget-ref-registry)))
        ;; Add the non-placeholder part before this match
        (setq result (concat result (substring str start match-start)))
        ;; Add the placeholder value (with reactive tracking if it's a ref)
        (if binding
            (let* ((base-value (cdr binding))
                   ;; Get nested value using accessors
                   (actual-value (twidget--get-nested-value base-value accessors))
                   (value-str (format "%s" actual-value)))
              (if ref-info
                  ;; This is a reactive ref - apply reactive text tracking
                  ;; Use full var-expr (e.g., "info.name") as the key for granular updates
                  (setq result (concat result
                                       (twidget--apply-reactive-text value-str instance-id var-expr)))
                ;; Not a reactive ref - just add the value
                (setq result (concat result value-str))))
          ;; No binding found - keep the placeholder as is
          (setq result (concat result (substring str match-start match-end))))
        (setq start match-end)))
    ;; Add the remaining part after the last match
    (concat result (substring str start))))

(defun twidget--expand-template-widget (template bindings instance-id &optional reactive-bindings)
  "Expand widget TEMPLATE with BINDINGS.
INSTANCE-ID is used for reactive tracking.
REACTIVE-BINDINGS is the original plist from :setup, used for event handlers."
  (let ((widget-name (car template))
        (rest (cdr template)))
    ;; Process the widget arguments to substitute bindings
    ;; Also extract and process event handlers and tp-props
    (let* ((processed-result (twidget--process-template-args-with-events 
                              rest bindings instance-id reactive-bindings))
           (processed-args (plist-get processed-result :args))
           (event-props (plist-get processed-result :event-props))
           (tp-props-info (plist-get processed-result :tp-props-info))
           (rendered-text (twidget-parse (cons widget-name processed-args) bindings))
           (result-text rendered-text))
      ;; Apply event properties to the rendered text
      (when event-props
        (setq result-text (twidget--apply-event-properties result-text event-props)))
      ;; Apply tp.el text properties via :tp-props
      (when tp-props-info
        (let ((value-fn (plist-get tp-props-info :value-fn))
              (static-value (plist-get tp-props-info :static-value)))
          (cond
           ;; Reactive tp-props - use tp.el's reactive mechanism
           (value-fn
            (setq result-text (twidget--apply-reactive-tp-props result-text value-fn instance-id)))
           ;; Static tp-props - apply all properties from the plist
           (static-value
            (setq result-text (twidget--apply-static-tp-props result-text static-value))))))
      result-text)))

(defun twidget--process-template-args (args bindings instance-id)
  "Process template ARGS, substituting BINDINGS.
INSTANCE-ID is used for reactive tracking.
Returns the processed argument list.
Note: When :for directive is present, slot arguments (non-keyword args) are NOT processed
here because they contain placeholders that will be bound during :for iteration."
  (let ((result nil)
        (has-for nil))
    ;; First pass: check if :for directive is present
    (let ((check-args args))
      (while (and check-args (keywordp (car check-args)))
        (when (eq (car check-args) :for)
          (setq has-for t))
        (setq check-args (cddr check-args))))
    ;; Second pass: process arguments
    (while args
      (let ((arg (car args)))
        (cond
         ;; Keyword - keep as is, process next arg (unless it's the :for value)
         ((keywordp arg)
          (push arg result)
          (setq args (cdr args))
          (when args
            (if (eq arg :for)
                ;; :for value should be kept as-is (it's not a template string)
                (push (car args) result)
              (push (twidget--process-template-arg (car args) bindings instance-id) result))
            (setq args (cdr args))))
         ;; Non-keyword (slot) arguments
         (t
          (if has-for
              ;; When :for is present, keep slot args as-is for later processing
              (push arg result)
            ;; No :for, process normally
            (push (twidget--process-template-arg arg bindings instance-id) result))
          (setq args (cdr args))))))
    (nreverse result)))

(defun twidget--process-template-args-with-events (args bindings instance-id reactive-bindings)
  "Process template ARGS with event handling support.
BINDINGS is an alist of (VAR-NAME . VALUE) for placeholder substitution.
INSTANCE-ID is used for reactive tracking.
REACTIVE-BINDINGS is the original plist from :setup, used for event handlers.

Returns a plist with:
  :args - the processed arguments (without event props and tp-props)
  :event-props - text properties for events (or nil)
  :tp-props-info - tp-props info plist with :value-fn and :static-value (or nil)"
  (let ((result nil)
        (event-props nil)
        (tp-props-info nil)
        (has-for nil))
    ;; First pass: check if :for directive is present
    (let ((check-args args))
      (while (and check-args (keywordp (car check-args)))
        (when (eq (car check-args) :for)
          (setq has-for t))
        (setq check-args (cddr check-args))))
    ;; Second pass: process arguments and extract events
    (while args
      (let ((arg (car args)))
        (cond
         ;; Keyword argument
         ((keywordp arg)
          (cond
           ;; Event property (:on-click, etc.)
           ((twidget--is-event-prop-p arg)
            (let ((event-type (twidget--is-event-prop-p arg)))
              (setq args (cdr args))
              (when args
                (let* ((handler-expr (car args))
                       (compiled-props (twidget--process-event-prop 
                                        event-type handler-expr reactive-bindings)))
                  (when compiled-props
                    (setq event-props (append event-props compiled-props))))
                (setq args (cdr args)))))
           ;; tp-props - plist of tp.el text properties
           ((eq arg :tp-props)
            (setq args (cdr args))
            (when args
              (setq tp-props-info (twidget--parse-tp-props-value (car args) reactive-bindings))
              (setq args (cdr args))))
           ;; Regular keyword - keep as is
           (t
            (push arg result)
            (setq args (cdr args))
            (when args
              (if (eq arg :for)
                  (push (car args) result)
                (push (twidget--process-template-arg (car args) bindings instance-id reactive-bindings) result))
              (setq args (cdr args))))))
         ;; Non-keyword (slot) arguments
         (t
          (if has-for
              (push arg result)
            (push (twidget--process-template-arg arg bindings instance-id reactive-bindings) result))
          (setq args (cdr args))))))
    (list :args (nreverse result) :event-props event-props :tp-props-info tp-props-info)))

(defun twidget--process-template-arg (arg bindings instance-id &optional reactive-bindings)
  "Process a single template ARG with BINDINGS.
INSTANCE-ID is used for reactive tracking.
REACTIVE-BINDINGS is the original plist from :setup, used for event handlers."
  (cond
   ;; String - substitute placeholders
   ((stringp arg)
    (twidget--expand-template-string arg bindings instance-id))
   ;; Widget form - expand it
   ((and (listp arg)
         (symbolp (car arg))
         (assoc (car arg) twidget-alist))
    (twidget--expand-template arg bindings instance-id reactive-bindings))
   ;; Lambda/function - return as is
   ((functionp arg) arg)
   ;; List that might be a lambda form
   ((and (listp arg) (eq (car arg) 'lambda)) arg)
   ;; Other list - might need recursive processing
   ((listp arg)
    (mapcar (lambda (x)
              (twidget--process-template-arg x bindings instance-id reactive-bindings))
            arg))
   ;; Other - return as is
   (t arg)))

(defun twidget-process-slot-value (val &optional bindings)
  "Process a single slot VAL, recursively parsing widget forms.
BINDINGS is an optional alist of (VAR-NAME . VALUE) pairs for placeholder
substitution.
Preserves original type for non-string, non-widget values.
For widget forms, marks block widgets with `twidget-block-element' property.
When the widget uses :for directive, returns a list of rendered items."
  (cond
   ((stringp val)
    (twidget-substitute-placeholders val bindings))
   ((and (listp val)
         (symbolp (car val))
         (assoc (car val) twidget-alist))
    (let ((rendered (twidget-parse val bindings))
          (is-block (twidget-is-block-widget-p val)))
      ;; twidget-parse may return a list when :for is used
      (cond
       ;; List result from :for - return as-is
       ((listp rendered) rendered)
       ;; Block element string - mark with property
       ((and is-block (stringp rendered))
        (propertize rendered 'twidget-block-element t))
       ;; Other string - return as-is
       (t rendered))))
   ;; Preserve original type for primitives (numbers, symbols, etc.)
   (t val)))

(defun twidget-process-slot-args (args &optional bindings)
  "Process multiple slot ARGS into slot content.
BINDINGS is an optional alist of (VAR-NAME . VALUE) pairs for placeholder
substitution.

Always returns a list of processed slot values, even for single arguments.
This ensures consistent slot type handling across all components.
- props: plist type
- slot: list type (always)

Use `twidget-slot-to-string' to convert a list slot to a string when needed."
  (cond
   ;; No arguments - return empty list
   ((null args) nil)
   ;; Always return a list of processed values
   (t
    (let ((result nil))
      (dolist (arg args)
        (let ((processed (twidget-process-slot-value arg bindings)))
          ;; If processed value is a list (from :for), flatten it
          (if (and (listp processed) (not (null processed)))
              (setq result (append result processed))
            (when processed
              (push processed result)))))
      (nreverse result)))))

(defun twidget-slot-to-string (slot)
  "Convert SLOT to a string for rendering.
SLOT is always a list type (never a string directly).
SLOT can be:
  - nil: returns empty string
  - a list: elements are converted to strings and concatenated

Block element handling is applied when concatenating list elements:
- Block elements are preceded by a newline (if not first)
- Block elements are followed by a newline (if not last)
Block elements are detected by the `twidget-block-element' text property."
  (cond
   ((null slot) "")
   ((listp slot)
    (let ((result "")
          (len (length slot)))
      (dotimes (i len)
        (let* ((item (nth i slot))
               (item-str (if (stringp item) item (format "%s" item)))
               (is-first (= i 0))
               (is-last (= i (1- len)))
               ;; Check if this item came from a block widget
               ;; Use length check to safely get text property
               (is-block
                (and (stringp item)
                     (> (length item) 0)
                     (get-text-property 0 'twidget-block-element item))))
          ;; Add newline BEFORE block element if there's previous content
          (when (and is-block (not is-first) (not (string-empty-p result)))
            (unless (string-suffix-p "\n" result)
              (setq result (concat result "\n"))))
          ;; Append the item
          (setq result (concat result item-str))
          ;; Add newline AFTER block element if not last
          (when (and is-block (not is-last))
            (unless (string-suffix-p "\n" result)
              (setq result (concat result "\n"))))))
      result))
   ;; Other types - convert to string (fallback for edge cases)
   (t (format "%s" slot))))

(defun twidget-reset ()
  "Reset all widget definitions."
  (interactive)
  (setq twidget-alist nil))

(defun twidget-clear-buffer-state ()
  "Clear all buffer-local widget state.
This should be called before re-inserting widgets to ensure fresh state.
Clears: widget instances, ref registry, reactive symbols, and counters."
  (interactive)
  (clrhash twidget-instances)
  (clrhash twidget-ref-registry)
  (clrhash twidget-reactive-symbols)
  (clrhash twidget-reactive-prop-symbols)
  (setq twidget-reactive-text-counter 0)
  (setq twidget-reactive-prop-counter 0))

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

This macro automatically clears the buffer-local widget state to ensure fresh
reactive values.

Example:
  (let ((editors \\='(\"emacs\" \"vim\" \"vscode\")))
    (twidget-insert
     \\='(div (p :for \"e in editors\" \"Editor: {e}\"))))"
  (declare (indent 0))
  ;; Extract variables at compile time if form is a quoted list
  (if (and (listp form) (eq (car form) 'quote))
      (let* ((widget-form (cadr form))
             (vars (twidget-extract-variables widget-form)))
        `(progn
           (twidget-clear-buffer-state)
           (let ((bindings (list ,@(mapcar (lambda (var)
                                             `(cons ,(symbol-name var) ,var))
                                           vars))))
             (insert (twidget-parse ',widget-form bindings)))))
    ;; Runtime extraction (fallback) - cannot capture lexical variables
    `(progn
       (twidget-clear-buffer-state)
       (insert (twidget-parse ,form nil)))))

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
                                      (plist-put (copy-sequence current) key-or-index value)
                                    (error "Cannot use keyword access on non-plist value"))))
                               ;; Integer access for list
                               ((integerp key-or-index)
                                (let ((current (twidget-ref-value ref)))
                                  (cond
                                   ((not (listp current))
                                    (error "Cannot use index access on non-list value"))
                                   (t
                                    (let ((len (length current)))
                                      (if (or (< key-or-index 0) (>= key-or-index len))
                                          (error "Index %d out of bounds for list of length %d"
                                                 key-or-index len)
                                        (let ((new-list (copy-sequence current)))
                                          (setf (nth key-or-index new-list) value)
                                          new-list)))))))
                               (t (error "KEY-OR-INDEX must be a keyword or integer")))))
                         ;; Update the ref value
                         (setf (twidget-ref-value ref) new-value)
                         ;; Notify watchers
                         (dolist (watcher (twidget-ref-watchers ref))
                           (funcall watcher new-value))
                         ;; Trigger buffer update with accessor info for incremental updates
                         (twidget--trigger-update inst-key key new-value key-or-index value)))))))
             twidget-instances)))

(defun twidget--trigger-update (instance-id var-name value &optional accessor sub-value)
  "Trigger a reactive update for INSTANCE-ID when VAR-NAME changes to VALUE.
This uses tp.el to update the text in the buffer by updating only the
reactive symbols associated with the changed variable.

When ACCESSOR is provided (keyword for plist, integer for list), also triggers
incremental updates for the specific property path (e.g., \"info.name\" or \"items.0\").
SUB-VALUE is the new value for the specific property when ACCESSOR is provided."
  ;; First, update symbols registered under the base variable name
  (let* ((key (cons instance-id var-name))
         (symbols (gethash key twidget-reactive-symbols)))
    ;; Update all reactive text symbols for this variable (whole value)
    ;; Preserve text properties if value is already a string
    ;; (avoids property loss from format conversion)
    (dolist (sym symbols)
      (when (boundp sym)
        (set sym (if (stringp value) value (format "%s" value))))))
  ;; Then, if accessor is provided, update symbols for the specific property path
  (when accessor
    (let* ((accessor-str (cond
                          ((keywordp accessor)
                           ;; Convert :name to "name"
                           (substring (symbol-name accessor) 1))
                          ((integerp accessor)
                           ;; Convert 0 to "0"
                           (number-to-string accessor))
                          (t nil)))
           (property-path (when accessor-str
                            (format "%s.%s" var-name accessor-str)))
           (property-key (when property-path
                           (cons instance-id property-path)))
           (property-symbols (when property-key
                               (gethash property-key twidget-reactive-symbols))))
      ;; Update symbols for the specific property path
      ;; Preserve text properties if sub-value is already a string
      ;; (avoids property loss from format conversion)
      (dolist (sym property-symbols)
        (when (boundp sym)
          (set sym (if (stringp sub-value) sub-value (format "%s" sub-value)))))))
  ;; Update all reactive property symbols for this instance
  ;; Property functions may depend on any reactive variable, so update them all
  (twidget--update-reactive-prop-symbols instance-id))

(defun twidget-substitute-placeholders (str bindings)
  "Substitute {variable} placeholders in STR with values from BINDINGS.
BINDINGS is an alist of (VAR-NAME . VALUE) pairs.
Supports dot notation for nested access:
  {info.name} - plist property access
  {items.0} - list index access
  {data.0.name} - multi-level access"
  (if (stringp str)
      (replace-regexp-in-string
       "{\\([^}]+\\)}"
       (lambda (match)
         ;; Extract the variable expression from the first capture group
         (let* ((var-expr (match-string 1 match))
                ;; Parse dot notation: "data.0.name" -> ("data" 0 :name)
                (parsed (twidget--parse-dot-notation var-expr))
                (base-var (car parsed))
                (accessors (cdr parsed))
                (binding (assoc base-var bindings)))
           (if binding
               (let* ((base-value (cdr binding))
                      (actual-value (twidget--get-nested-value base-value accessors)))
                 (format "%s" actual-value))
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

(defun twidget-ref-set (ref value)
  "Set the value of reactive REF to VALUE and trigger UI updates.
REF must be a twidget-ref object.  This function finds the instance
and variable name for REF and triggers proper reactive updates.
Use this in :setup closures that have direct access to the ref object."
  (unless (twidget-ref-p ref)
    (error "twidget-ref-set: REF must be a twidget-ref object"))
  ;; Update the ref value
  (setf (twidget-ref-value ref) value)
  ;; Notify watchers
  (dolist (watcher (twidget-ref-watchers ref))
    (funcall watcher value))
  ;; Find the instance-id and var-name for this ref by searching the registry
  (catch 'done
    (maphash (lambda (key stored-ref)
               (when (eq stored-ref ref)
                 (let ((instance-id (car key))
                       (var-name (cdr key)))
                   (twidget--trigger-update instance-id var-name value)
                   (throw 'done t))))
             twidget-ref-registry)))

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

;;; Event System
;; ============================================================================

(defvar twidget-event-types
  '((click . (:map-property keymap
              :event-trigger mouse-1
              :doc "Triggered when element is clicked"))
    (mouse-enter . (:map-property keymap
                    :event-trigger mouse-1
                    :doc "Triggered when mouse enters element"))
    (mouse-leave . (:map-property keymap
                    :event-trigger mouse-1
                    :doc "Triggered when mouse leaves element")))
  "Registry of supported event types.
Each entry is (EVENT-TYPE . PLIST) where PLIST contains:
  :map-property - The text property to use for the event map
  :event-trigger - The default event trigger
  :doc - Documentation string")

(defvar-local twidget-event-context nil
  "Buffer-local variable holding the current event context.
This is a plist with :bindings (reactive bindings), :instance-id, etc.
Used to resolve variable references in event handlers.")

(defun twidget--is-event-prop-p (keyword)
  "Return non-nil if KEYWORD is an event property (starts with :on-).
Returns the event type as a symbol if it is an event property."
  (when (keywordp keyword)
    (let ((name (symbol-name keyword)))
      (when (string-prefix-p ":on-" name)
        (intern (substring name 4))))))

(defun twidget--parse-event-expression (expr-string)
  "Parse an event handler expression string EXPR-STRING.
Returns a plist describing the expression.

For single statements, returns a plist with :type being one of:
  \\='method - Simple method reference
  \\='method-call - Method with arguments
  \\='increment - Variable increment (var++)
  \\='decrement - Variable decrement (var--)
  \\='assignment - Variable assignment (var=value)
  \\='ternary - Ternary expression (cond ? a : b)
  \\='logical-and - Logical AND (cond && action)
  \\='logical-or - Logical OR (cond || action)
  \\='raw-expression - Fallback for complex expressions

For multi-statement expressions (separated by ;), returns:
  (:type multi-statement :statements LIST-OF-PARSED-STATEMENTS)

Examples:
  \"doSomething\" -> (:type method :name \"doSomething\")
  \"doSomething(foo)\" -> (:type method-call :name \"doSomething\" :args (...))
  \"count++\" -> (:type increment :var \"count\")
  \"count--\" -> (:type decrement :var \"count\")
  \"count=10\" -> (:type assignment :var \"count\" :value (...))
  \"a++ ; b++\" -> (:type multi-statement :statements (...))
  \"flag ? doA() : doB()\" -> (:type ternary :condition \"flag\" ...)"
  (let* ((trimmed (string-trim expr-string))
         (result nil))
    (cond
     ;; Check for multiple statements separated by ;
     ((string-match-p ";" trimmed)
      (let ((statements (mapcar #'string-trim (split-string trimmed ";" t))))
        (setq result (list :type 'multi-statement
                           :statements (mapcar #'twidget--parse-single-statement statements)))))
     ;; Single statement
     (t
      (setq result (twidget--parse-single-statement trimmed))))
    result))

(defun twidget--parse-ternary-expression (str)
  "Parse STR as a ternary expression (condition ? trueExpr : falseExpr).
Returns a parsed ternary plist or nil if not a valid ternary expression.
Handles nested parentheses correctly."
  (let ((question-pos nil)
        (colon-pos nil)
        (paren-depth 0)
        (i 0)
        (len (length str)))
    ;; Find the first top-level ? (not inside parentheses)
    (while (and (< i len) (not question-pos))
      (let ((char (aref str i)))
        (cond
         ((= char ?\() (cl-incf paren-depth))
         ((= char ?\)) (cl-decf paren-depth))
         ((and (= char ??) (= paren-depth 0))
          (setq question-pos i))))
      (cl-incf i))
    ;; If we found a ?, look for the matching : after it
    (when question-pos
      (setq i (1+ question-pos))
      (setq paren-depth 0)
      (while (and (< i len) (not colon-pos))
        (let ((char (aref str i)))
          (cond
           ((= char ?\() (cl-incf paren-depth))
           ((= char ?\)) (cl-decf paren-depth))
           ((and (= char ?:) (= paren-depth 0))
            (setq colon-pos i))))
        (cl-incf i)))
    ;; If we found both ? and :, extract the parts
    (when (and question-pos colon-pos (> colon-pos question-pos))
      (let ((condition (string-trim (substring str 0 question-pos)))
            (true-expr (string-trim (substring str (1+ question-pos) colon-pos)))
            (false-expr (string-trim (substring str (1+ colon-pos)))))
        (when (and (not (string-empty-p condition))
                   (not (string-empty-p true-expr))
                   (not (string-empty-p false-expr)))
          (list :type 'ternary
                :condition condition
                :true-expr (twidget--parse-single-statement true-expr)
                :false-expr (twidget--parse-single-statement false-expr)))))))

(defun twidget--parse-single-statement (stmt)
  "Parse a single statement STMT.
Returns a plist describing the statement."
  (let ((trimmed (string-trim stmt)))
    (cond
     ;; Increment: count++
     ((string-match "^\\([a-zA-Z_][a-zA-Z0-9_-]*\\)\\+\\+$" trimmed)
      (list :type 'increment :var (match-string 1 trimmed)))

     ;; Decrement: count--
     ((string-match "^\\([a-zA-Z_][a-zA-Z0-9_-]*\\)--$" trimmed)
      (list :type 'decrement :var (match-string 1 trimmed)))

     ;; Assignment: var=value or var = value (but not == or ===)
     ((and (string-match "^\\([a-zA-Z_][a-zA-Z0-9_-]*\\)\\s-*=\\([^=].*\\)$" trimmed)
           ;; Additional check: value must not start with = (catches ===)
           (not (string-match "^\\s-*=" (match-string 2 trimmed))))
      (let ((var (match-string 1 trimmed))
            (value-str (string-trim (match-string 2 trimmed))))
        (list :type 'assignment :var var :value (twidget--parse-value-expr value-str))))

     ;; Ternary: condition ? trueExpr : falseExpr
     ;; Use a custom parser to handle nested parentheses correctly
     ((twidget--parse-ternary-expression trimmed))

     ;; Logical AND: expr && action
     ((string-match "^\\(.+?\\)\\s-*&&\\s-*\\(.+\\)$" trimmed)
      (list :type 'logical-and
            :condition (string-trim (match-string 1 trimmed))
            :action (twidget--parse-single-statement (string-trim (match-string 2 trimmed)))))

     ;; Logical OR: expr || action
     ((string-match "^\\(.+?\\)\\s-*||\\s-*\\(.+\\)$" trimmed)
      (list :type 'logical-or
            :condition (string-trim (match-string 1 trimmed))
            :action (twidget--parse-single-statement (string-trim (match-string 2 trimmed)))))

     ;; Method call with arguments: doSomething(arg1, arg2, ...)
     ((string-match "^\\([a-zA-Z_][a-zA-Z0-9_-]*\\)\\s-*(\\(.*\\))$" trimmed)
      (let* ((method (match-string 1 trimmed))
             (args-str (match-string 2 trimmed))
             (args (if (string-empty-p (string-trim args-str))
                       nil
                     (twidget--parse-arguments args-str))))
        (list :type 'method-call :name method :args args)))

     ;; Simple method reference: doSomething
     ((string-match "^\\([a-zA-Z_][a-zA-Z0-9_-]*\\)$" trimmed)
      (list :type 'method :name trimmed))

     ;; Fallback: treat as raw expression
     (t
      (list :type 'raw-expression :expr trimmed)))))

(defun twidget--parse-arguments (args-str)
  "Parse comma-separated arguments string ARGS-STR.
Returns a list of parsed argument plists."
  (let ((args nil)
        (current "")
        (paren-depth 0)
        (in-string nil)
        (string-char nil))
    (dotimes (i (length args-str))
      (let ((char (aref args-str i)))
        (cond
         ;; Handle string delimiters
         ((and (not in-string) (or (= char ?\") (= char ?\')))
          (setq in-string t
                string-char char)
          (setq current (concat current (string char))))
         ((and in-string (= char string-char))
          (setq in-string nil
                string-char nil)
          (setq current (concat current (string char))))
         ;; Track parentheses depth
         ((and (not in-string) (= char ?\())
          (cl-incf paren-depth)
          (setq current (concat current (string char))))
         ((and (not in-string) (= char ?\)))
          (cl-decf paren-depth)
          (setq current (concat current (string char))))
         ;; Comma at top level = argument separator
         ((and (not in-string) (= paren-depth 0) (= char ?,))
          (push (twidget--parse-argument (string-trim current)) args)
          (setq current ""))
         ;; Regular character
         (t (setq current (concat current (string char)))))))
    ;; Don't forget the last argument
    (when (not (string-empty-p (string-trim current)))
      (push (twidget--parse-argument (string-trim current)) args))
    (nreverse args)))

(defun twidget--parse-argument (arg-str)
  "Parse a single argument string ARG-STR.
Returns a plist describing the argument."
  (let ((trimmed (string-trim arg-str)))
    (cond
     ;; $event - special event object reference
     ((string= trimmed "$event")
      (list :type 'event-ref))
     ;; String literal (single or double quoted)
     ((or (string-match "^\"\\(.*\\)\"$" trimmed)
          (string-match "^'\\(.*\\)'$" trimmed))
      (list :type 'string :value (match-string 1 trimmed)))
     ;; Number literal
     ((string-match "^-?[0-9]+\\(?:\\.[0-9]+\\)?$" trimmed)
      (list :type 'number :value (string-to-number trimmed)))
     ;; Boolean literals
     ((string= trimmed "true")
      (list :type 'boolean :value t))
     ((string= trimmed "false")
      (list :type 'boolean :value nil))
     ((string= trimmed "nil")
      (list :type 'boolean :value nil))
     ;; Variable reference
     ((string-match "^\\([a-zA-Z_][a-zA-Z0-9_-]*\\)$" trimmed)
      (list :type 'variable :name trimmed))
     ;; Expression (fallback)
     (t
      (list :type 'expression :expr trimmed)))))

(defun twidget--parse-value-expr (value-str)
  "Parse a value expression VALUE-STR for assignment.
Returns a plist describing the value."
  (let ((trimmed (string-trim value-str)))
    (cond
     ;; Negation: !variable or !expression
     ((string-match "^!\\s*\\(.+\\)$" trimmed)
      (list :type 'negation :operand (twidget--parse-value-expr (match-string 1 trimmed))))
     ;; Ternary in assignment: count === 0 ? 1 : 0
     ;; Use the proper ternary parser
     ((twidget--parse-ternary-value-expression trimmed))
     ;; String literal
     ((or (string-match "^\"\\(.*\\)\"$" trimmed)
          (string-match "^'\\(.*\\)'$" trimmed))
      (list :type 'string :value (match-string 1 trimmed)))
     ;; Number literal
     ((string-match "^-?[0-9]+\\(?:\\.[0-9]+\\)?$" trimmed)
      (list :type 'number :value (string-to-number trimmed)))
     ;; Boolean
     ((string= trimmed "true")
      (list :type 'boolean :value t))
     ((string= trimmed "false")
      (list :type 'boolean :value nil))
     ;; Variable reference
     ((string-match "^\\([a-zA-Z_][a-zA-Z0-9_-]*\\)$" trimmed)
      (list :type 'variable :name trimmed))
     ;; Expression
     (t
      (list :type 'expression :expr trimmed)))))

(defun twidget--parse-ternary-value-expression (str)
  "Parse STR as a ternary value expression (condition ? trueVal : falseVal).
Returns a parsed ternary plist or nil if not a valid ternary expression.
Similar to `twidget--parse-ternary-expression` but for value expressions."
  (let ((question-pos nil)
        (colon-pos nil)
        (paren-depth 0)
        (i 0)
        (len (length str)))
    ;; Find the first top-level ? (not inside parentheses)
    (while (and (< i len) (not question-pos))
      (let ((char (aref str i)))
        (cond
         ((= char ?\() (cl-incf paren-depth))
         ((= char ?\)) (cl-decf paren-depth))
         ((and (= char ??) (= paren-depth 0))
          (setq question-pos i))))
      (cl-incf i))
    ;; If we found a ?, look for the matching : after it
    (when question-pos
      (setq i (1+ question-pos))
      (setq paren-depth 0)
      (while (and (< i len) (not colon-pos))
        (let ((char (aref str i)))
          (cond
           ((= char ?\() (cl-incf paren-depth))
           ((= char ?\)) (cl-decf paren-depth))
           ((and (= char ?:) (= paren-depth 0))
            (setq colon-pos i))))
        (cl-incf i)))
    ;; If we found both ? and :, extract the parts
    (when (and question-pos colon-pos (> colon-pos question-pos))
      (let ((condition (string-trim (substring str 0 question-pos)))
            (true-val (string-trim (substring str (1+ question-pos) colon-pos)))
            (false-val (string-trim (substring str (1+ colon-pos)))))
        (when (and (not (string-empty-p condition))
                   (not (string-empty-p true-val))
                   (not (string-empty-p false-val)))
          (list :type 'ternary
                :condition condition
                :true-value (twidget--parse-value-expr true-val)
                :false-value (twidget--parse-value-expr false-val)))))))

(defun twidget--compile-event-handler (parsed-expr bindings-plist)
  "Compile PARSED-EXPR into an executable lambda.
BINDINGS-PLIST is the reactive bindings from :setup, containing methods and refs.
Returns a lambda function that can be used as an event handler."
  (let ((stmt-type (plist-get parsed-expr :type)))
    (pcase stmt-type
      ;; Method reference - look up in bindings and return as callable
      ('method
       (let* ((method-name (plist-get parsed-expr :name))
              (method-key (intern (format ":%s" method-name)))
              (method-fn (plist-get bindings-plist method-key)))
         (if method-fn
             (lambda ()
               (interactive)
               (funcall method-fn))
           ;; Method not found in bindings - could be a global function
           (let ((global-fn (intern method-name)))
             (lambda ()
               (interactive)
               (when (fboundp global-fn)
                 (funcall global-fn)))))))

      ;; Method call with arguments
      ('method-call
       (let* ((method-name (plist-get parsed-expr :name))
              (args-parsed (plist-get parsed-expr :args))
              (method-key (intern (format ":%s" method-name)))
              (method-fn (plist-get bindings-plist method-key)))
         (if method-fn
             (lambda ()
               (interactive)
               (let ((args (twidget--resolve-arguments args-parsed bindings-plist)))
                 (apply method-fn args)))
           ;; Try global function
           (let ((global-fn (intern method-name)))
             (lambda ()
               (interactive)
               (when (fboundp global-fn)
                 (let ((args (twidget--resolve-arguments args-parsed bindings-plist)))
                   (apply global-fn args))))))))

      ;; Increment operation
      ('increment
       (let* ((var-name (plist-get parsed-expr :var))
              ;; Try to get the ref directly from bindings-plist at compile time
              (var-key (intern (format ":%s" var-name)))
              (ref-or-val (plist-get bindings-plist var-key)))
         (if (twidget-ref-p ref-or-val)
             ;; Direct ref access - fast path
             (lambda ()
               (interactive)
               (let* ((current (twidget-ref-value ref-or-val))
                      (current-num (if (stringp current)
                                       (string-to-number current)
                                     (or current 0)))
                      (new-val (+ current-num 1)))
                 (twidget-ref-set ref-or-val (if (stringp current)
                                                  (number-to-string new-val)
                                                new-val))))
           ;; Fallback to global lookup - slow path
           (lambda ()
             (interactive)
             (twidget-inc (intern var-name) 1)))))

      ;; Decrement operation
      ('decrement
       (let* ((var-name (plist-get parsed-expr :var))
              ;; Try to get the ref directly from bindings-plist at compile time
              (var-key (intern (format ":%s" var-name)))
              (ref-or-val (plist-get bindings-plist var-key)))
         (if (twidget-ref-p ref-or-val)
             ;; Direct ref access - fast path
             (lambda ()
               (interactive)
               (let* ((current (twidget-ref-value ref-or-val))
                      (current-num (if (stringp current)
                                       (string-to-number current)
                                     (or current 0)))
                      (new-val (- current-num 1)))
                 (twidget-ref-set ref-or-val (if (stringp current)
                                                  (number-to-string new-val)
                                                new-val))))
           ;; Fallback to global lookup - slow path
           (lambda ()
             (interactive)
             (twidget-dec (intern var-name) 1)))))

      ;; Assignment operation
      ('assignment
       (let* ((var-name (plist-get parsed-expr :var))
              (value-parsed (plist-get parsed-expr :value))
              ;; Try to get the ref directly from bindings-plist at compile time
              ;; This avoids O(n) search through all instances at runtime
              (var-key (intern (format ":%s" var-name)))
              (ref-or-val (plist-get bindings-plist var-key)))
         (if (twidget-ref-p ref-or-val)
             ;; Direct ref access - fast path
             (lambda ()
               (interactive)
               (let ((resolved-value (twidget--resolve-value value-parsed bindings-plist)))
                 (twidget-ref-set ref-or-val resolved-value)))
           ;; Fallback to global lookup - slow path (for variables not in bindings)
           (lambda ()
             (interactive)
             (let ((resolved-value (twidget--resolve-value value-parsed bindings-plist)))
               (twidget-set (intern var-name) resolved-value))))))

      ;; Multi-statement - pre-compile all handlers at definition time
      ('multi-statement
       (let* ((statements (plist-get parsed-expr :statements))
              ;; Pre-compile all statement handlers
              (compiled-handlers (mapcar (lambda (stmt)
                                           (twidget--compile-event-handler stmt bindings-plist))
                                         statements)))
         (lambda ()
           (interactive)
           (dolist (handler compiled-handlers)
             (funcall handler)))))

      ;; Ternary expression - pre-compile both branches
      ('ternary
       (let* ((condition-str (plist-get parsed-expr :condition))
              (true-expr (plist-get parsed-expr :true-expr))
              (false-expr (plist-get parsed-expr :false-expr))
              ;; Pre-compile both branch handlers
              (true-handler (twidget--compile-event-handler true-expr bindings-plist))
              (false-handler (twidget--compile-event-handler false-expr bindings-plist)))
         (lambda ()
           (interactive)
           (if (twidget--eval-condition condition-str bindings-plist)
               (funcall true-handler)
             (funcall false-handler)))))

      ;; Logical AND - pre-compile action handler
      ('logical-and
       (let* ((condition-str (plist-get parsed-expr :condition))
              (action-expr (plist-get parsed-expr :action))
              ;; Pre-compile action handler
              (action-handler (twidget--compile-event-handler action-expr bindings-plist)))
         (lambda ()
           (interactive)
           (when (twidget--eval-condition condition-str bindings-plist)
             (funcall action-handler)))))

      ;; Logical OR - pre-compile action handler
      ('logical-or
       (let* ((condition-str (plist-get parsed-expr :condition))
              (action-expr (plist-get parsed-expr :action))
              ;; Pre-compile action handler
              (action-handler (twidget--compile-event-handler action-expr bindings-plist)))
         (lambda ()
           (interactive)
           (unless (twidget--eval-condition condition-str bindings-plist)
             (funcall action-handler)))))

      ;; Raw expression - eval as elisp
      ('raw-expression
       (let ((expr-str (plist-get parsed-expr :expr)))
         (lambda ()
           (interactive)
           (twidget--eval-raw-expression expr-str bindings-plist))))

      ;; Default - no-op
      (_ (lambda () (interactive))))))

(defun twidget--resolve-arguments (args-parsed bindings-plist)
  "Resolve ARGS-PARSED list to actual values using BINDINGS-PLIST."
  (mapcar (lambda (arg)
            (twidget--resolve-value arg bindings-plist))
          args-parsed))

(defun twidget--resolve-value (value-parsed bindings-plist)
  "Resolve VALUE-PARSED to an actual value using BINDINGS-PLIST."
  (let ((value-type (plist-get value-parsed :type)))
    (pcase value-type
      ('string (plist-get value-parsed :value))
      ('number (plist-get value-parsed :value))
      ('boolean (plist-get value-parsed :value))
      ('event-ref nil) ; $event would need special handling
      ('negation
       (let ((operand (plist-get value-parsed :operand)))
         (not (twidget--resolve-value operand bindings-plist))))
      ('variable
       (let* ((var-name (plist-get value-parsed :name))
              (var-key (intern (format ":%s" var-name)))
              (ref-or-val (plist-get bindings-plist var-key)))
         (if (twidget-ref-p ref-or-val)
             (twidget-ref-value ref-or-val)
           (or ref-or-val
               ;; Try getting from reactive system
               (twidget-get (intern var-name))))))
      ('ternary
       (let ((condition-str (plist-get value-parsed :condition))
             (true-val (plist-get value-parsed :true-value))
             (false-val (plist-get value-parsed :false-value)))
         (if (twidget--eval-condition condition-str bindings-plist)
             (twidget--resolve-value true-val bindings-plist)
           (twidget--resolve-value false-val bindings-plist))))
      ('expression
       (twidget--eval-raw-expression (plist-get value-parsed :expr) bindings-plist))
      (_ nil))))

(defun twidget--eval-condition (condition-str bindings-plist)
  "Evaluate CONDITION-STR as a boolean condition using BINDINGS-PLIST.
Supports simple comparisons and variable references."
  (let ((trimmed (string-trim condition-str)))
    (cond
     ;; Triple equals: var === value (strict equality)
     ((string-match "^\\([a-zA-Z_][a-zA-Z0-9_-]*\\)\\s-*===\\s-*\\(.+\\)$" trimmed)
      (let* ((var-name (match-string 1 trimmed))
             (value-str (string-trim (match-string 2 trimmed)))
             (var-value (twidget-get (intern var-name)))
             (compare-value (twidget--parse-literal value-str)))
        (equal var-value compare-value)))

     ;; Double equals: var == value
     ((string-match "^\\([a-zA-Z_][a-zA-Z0-9_-]*\\)\\s-*==\\s-*\\(.+\\)$" trimmed)
      (let* ((var-name (match-string 1 trimmed))
             (value-str (string-trim (match-string 2 trimmed)))
             (var-value (twidget-get (intern var-name)))
             (compare-value (twidget--parse-literal value-str)))
        (equal var-value compare-value)))

     ;; Not equals: var != value
     ((string-match "^\\([a-zA-Z_][a-zA-Z0-9_-]*\\)\\s-*!=\\s-*\\(.+\\)$" trimmed)
      (let* ((var-name (match-string 1 trimmed))
             (value-str (string-trim (match-string 2 trimmed)))
             (var-value (twidget-get (intern var-name)))
             (compare-value (twidget--parse-literal value-str)))
        (not (equal var-value compare-value))))

     ;; Greater than: var > value
     ((string-match "^\\([a-zA-Z_][a-zA-Z0-9_-]*\\)\\s-*>\\s-*\\(.+\\)$" trimmed)
      (let* ((var-name (match-string 1 trimmed))
             (value-str (string-trim (match-string 2 trimmed)))
             (var-value (twidget-get (intern var-name)))
             (compare-value (twidget--parse-literal value-str)))
        (and (numberp var-value) (numberp compare-value)
             (> var-value compare-value))))

     ;; Less than: var < value
     ((string-match "^\\([a-zA-Z_][a-zA-Z0-9_-]*\\)\\s-*<\\s-*\\(.+\\)$" trimmed)
      (let* ((var-name (match-string 1 trimmed))
             (value-str (string-trim (match-string 2 trimmed)))
             (var-value (twidget-get (intern var-name)))
             (compare-value (twidget--parse-literal value-str)))
        (and (numberp var-value) (numberp compare-value)
             (< var-value compare-value))))

     ;; Simple variable reference (truthy check)
     ((string-match "^\\([a-zA-Z_][a-zA-Z0-9_-]*\\)$" trimmed)
      (let ((var-value (twidget-get (intern trimmed))))
        (and var-value (not (equal var-value 0)) (not (string= var-value "")))))

     ;; Negation: !var
     ((string-match "^!\\s*\\([a-zA-Z_][a-zA-Z0-9_-]*\\)$" trimmed)
      (let ((var-value (twidget-get (intern (match-string 1 trimmed)))))
        (not (and var-value (not (equal var-value 0)) (not (string= var-value ""))))))

     ;; Default - try eval as elisp
     (t
      (condition-case nil
          (twidget--eval-raw-expression trimmed bindings-plist)
        (error nil))))))

(defun twidget--parse-literal (value-str)
  "Parse VALUE-STR as a literal value (string, number, boolean)."
  (let ((trimmed (string-trim value-str)))
    (cond
     ;; String literal
     ((or (string-match "^\"\\(.*\\)\"$" trimmed)
          (string-match "^'\\(.*\\)'$" trimmed))
      (match-string 1 trimmed))
     ;; Number
     ((string-match "^-?[0-9]+\\(?:\\.[0-9]+\\)?$" trimmed)
      (string-to-number trimmed))
     ;; Boolean
     ((string= trimmed "true") t)
     ((string= trimmed "false") nil)
     ((string= trimmed "nil") nil)
     ;; Variable reference
     ((string-match "^\\([a-zA-Z_][a-zA-Z0-9_-]*\\)$" trimmed)
      (twidget-get (intern trimmed)))
     ;; Default - return as string
     (t trimmed))))

(defun twidget--eval-raw-expression (expr-str bindings-plist)
  "Evaluate EXPR-STR as a raw expression.
Uses BINDINGS-PLIST to resolve variables if needed.
This is a fallback for complex expressions.

SECURITY NOTE: This function evaluates user expressions in a restricted
environment. Only safe functions from the twidget API are exposed.
Dangerous functions like `shell-command', `eval', `funcall' on arbitrary
symbols, file operations, etc. are NOT available in the evaluation context."
  ;; Validate expression doesn't contain dangerous patterns
  (when (string-match-p (rx (or "shell" "process" "file" "write" "delete"
                                "kill" "exec" "load" "require" "eval"
                                "call-process" "start-process"))
                        expr-str)
    (error "twidget: Expression contains potentially unsafe function calls"))
  ;; For safety, we create a controlled evaluation environment
  ;; First, try to substitute known variable names
  (let ((result-expr expr-str)
        (safe-env nil))
    ;; Build safe evaluation environment with only allowed functions
    (setq safe-env
          `((twidget-get . ,#'twidget-get)
            (twidget-set . ,#'twidget-set)
            (twidget-inc . ,#'twidget-inc)
            (twidget-dec . ,#'twidget-dec)
            (+ . ,#'+)
            (- . ,#'-)
            (* . ,#'*)
            (/ . ,#'/)
            (= . ,#'=)
            (< . ,#'<)
            (> . ,#'>)
            (<= . ,#'<=)
            (>= . ,#'>=)
            (not . ,#'not)
            (and . ,(symbol-function 'and))
            (or . ,(symbol-function 'or))
            (if . ,(symbol-function 'if))
            (when . ,(symbol-function 'when))
            (unless . ,(symbol-function 'unless))
            (equal . ,#'equal)
            (string= . ,#'string=)
            (numberp . ,#'numberp)
            (stringp . ,#'stringp)
            (message . ,#'message)
            (format . ,#'format)))
    ;; Replace variable references with their values
    (let ((plist bindings-plist))
      (while plist
        (let ((key (car plist))
              (val (cadr plist)))
          (when (keywordp key)
            (let* ((var-name (substring (symbol-name key) 1))
                   (actual-value (if (twidget-ref-p val)
                                     (twidget-ref-value val)
                                   val)))
              ;; Replace variable name with value in expression
              (when (string-match-p (regexp-quote var-name) result-expr)
                (setq result-expr
                      (replace-regexp-in-string
                       (format "\\b%s\\b" (regexp-quote var-name))
                       (if (stringp actual-value)
                           (format "\"%s\"" (replace-regexp-in-string "\"" "\\\\\"" actual-value))
                         (format "%S" actual-value))
                       result-expr))))))
        (setq plist (cddr plist))))
    ;; Convert JavaScript-style negation !value to Elisp (not value)
    ;; Handle patterns like !nil, !t, !'string', !123, !(expr)
    (while (string-match "!\\([^!]\\)" result-expr)
      (let* ((start (match-beginning 0))
             (rest-start (match-beginning 1))
             ;; Find the extent of the operand
             (operand-end (twidget--find-operand-end result-expr rest-start)))
        (when operand-end
          (let ((operand (substring result-expr rest-start operand-end)))
            (setq result-expr
                  (concat (substring result-expr 0 start)
                          (format "(not %s)" operand)
                          (substring result-expr operand-end)))))))
    ;; Evaluate the transformed expression in restricted environment
    (condition-case err
        (eval (car (read-from-string (format "(progn %s)" result-expr)))
              safe-env)
      (error
       (message "twidget: Error evaluating expression '%s': %s" expr-str err)
       nil))))

(defun twidget--find-operand-end (str start)
  "Find the end position of an operand in STR starting at START.
Handles atoms (symbols, numbers, strings) and parenthesized expressions."
  (let ((len (length str))
        (i start))
    (when (< i len)
      (let ((char (aref str i)))
        (cond
         ;; Parenthesized expression
         ((= char ?\()
          (let ((depth 1))
            (cl-incf i)
            (while (and (< i len) (> depth 0))
              (let ((c (aref str i)))
                (cond
                 ((= c ?\() (cl-incf depth))
                 ((= c ?\)) (cl-decf depth))))
              (cl-incf i))
            i))
         ;; String literal
         ((or (= char ?\") (= char ?\'))
          (let ((quote-char char))
            (cl-incf i)
            (while (and (< i len) (not (= (aref str i) quote-char)))
              (cl-incf i))
            (when (< i len) (cl-incf i))
            i))
         ;; Symbol or number (word characters)
         (t
          (while (and (< i len)
                      (let ((c (aref str i)))
                        (or (and (>= c ?a) (<= c ?z))
                            (and (>= c ?A) (<= c ?Z))
                            (and (>= c ?0) (<= c ?9))
                            (= c ?_)
                            (= c ?-))))
            (cl-incf i))
          i))))))

(defun twidget--create-click-handler (handler-fn)
  "Create a keymap with HANDLER-FN bound to mouse click.
Returns a keymap suitable for use as a text property."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] handler-fn)
    (define-key map (kbd "RET") handler-fn)
    (define-key map (kbd "<return>") handler-fn)
    map))

(defun twidget--process-event-prop (event-type handler-string bindings-plist)
  "Process an event property of type EVENT-TYPE with HANDLER-STRING.
Uses BINDINGS-PLIST for variable and method resolution.
Returns a plist of text properties to apply."
  (let* ((parsed (twidget--parse-event-expression handler-string))
         (handler-fn (twidget--compile-event-handler parsed bindings-plist)))
    (pcase event-type
      ('click
       (list 'keymap (twidget--create-click-handler handler-fn)
             ;; 'mouse-face 'highlight
             'pointer 'hand))
      (_ nil))))

(defun twidget--apply-event-properties (text event-props)
  "Apply EVENT-PROPS (a plist of text properties) to TEXT.
Returns the TEXT with properties applied."
  (if event-props
      (apply #'propertize text event-props)
    text))

(require 'twidget-builtin)

(provide 'twidget)
;;; twidget.el ends here
