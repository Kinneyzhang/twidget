;;; twidget-component-test.el --- Tests for twidget-component.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the twidget component system.
;; Run with: emacs -batch -l twidget-reactive.el -l twidget-component.el -l twidget-component-test.el -f twidget-component-run-tests

;;; Code:

(require 'cl-lib)
(require 'twidget-reactive)
(require 'twidget-component)

(defvar twidget-component-test-results '()
  "List of test results.")

(defvar twidget-component-test-passed 0
  "Number of passed tests.")

(defvar twidget-component-test-failed 0
  "Number of failed tests.")

(defmacro twidget-component-test (name &rest body)
  "Define a test named NAME with BODY."
  (declare (indent 1))
  `(condition-case err
       (progn
         ,@body
         (setq twidget-component-test-passed (1+ twidget-component-test-passed))
         (push (cons ,name 'passed) twidget-component-test-results)
         (message "✓ PASSED: %s" ,name))
     (error
      (setq twidget-component-test-failed (1+ twidget-component-test-failed))
      (push (cons ,name (format "FAILED: %s" (error-message-string err)))
            twidget-component-test-results)
      (message "✗ FAILED: %s - %s" ,name (error-message-string err)))))

(defmacro twidget-component-assert (condition &optional message)
  "Assert that CONDITION is true."
  `(unless ,condition
     (error (or ,message (format "Assertion failed: %S" ',condition)))))

(defmacro twidget-component-assert-equal (expected actual &optional message)
  "Assert that EXPECTED equals ACTUAL."
  `(let ((exp ,expected)
         (act ,actual))
     (unless (equal exp act)
       (error (or ,message (format "Expected %S but got %S" exp act))))))

;;; ============================================================================
;;; Test: Component Definition
;;; ============================================================================

(defun twidget-test-component-definition ()
  "Test component definition functionality."
  
  (twidget-component-test "Component: define simple component"
    (twidget-clear-components)
    (twidget-define-component test-simple
      :props (label)
      :render (lambda (props _state)
                (or (plist-get props :label) "default")))
    (twidget-component-assert (twidget-get-component 'test-simple)
                              "Component should be registered"))
  
  (twidget-component-test "Component: define with setup"
    (twidget-clear-components)
    (twidget-define-component test-with-setup
      :props (initial-count)
      :setup (lambda (props)
               (let ((count (twidget-ref (or (plist-get props :initial-count) 0))))
                 (list :count count)))
      :render (lambda (_props state)
                (format "%d" (twidget-ref-get (plist-get state :count)))))
    (let ((comp (twidget-get-component 'test-with-setup)))
      (twidget-component-assert (twidget-component-setup comp)
                                "Component should have setup function")))
  
  (twidget-component-test "Component: list components"
    (twidget-clear-components)
    (twidget-define-component comp-a :props (x) :render (lambda (_p _s) "a"))
    (twidget-define-component comp-b :props (y) :render (lambda (_p _s) "b"))
    (let ((names (twidget-list-components)))
      (twidget-component-assert (member 'comp-a names))
      (twidget-component-assert (member 'comp-b names)))))

;;; ============================================================================
;;; Test: Component Instance
;;; ============================================================================

(defun twidget-test-component-instance ()
  "Test component instance creation."
  
  (twidget-component-test "Instance: create instance"
    (twidget-clear-components)
    (twidget-unmount-all)
    (twidget-define-component test-instance
      :props (label)
      :render (lambda (props _state)
                (or (plist-get props :label) "default")))
    (let ((instance (twidget-create-instance 'test-instance :label "Hello")))
      (twidget-component-assert (twidget-instance-p instance))
      (twidget-component-assert-equal "Hello"
                                      (plist-get (twidget-instance-props instance) :label))))
  
  (twidget-component-test "Instance: setup is called"
    (twidget-clear-components)
    (twidget-unmount-all)
    (let ((setup-called nil))
      (twidget-define-component test-setup-call
        :props ()
        :setup (lambda (_props)
                 (setq setup-called t)
                 (list :value 42))
        :render (lambda (_p _s) "test"))
      (let ((instance (twidget-create-instance 'test-setup-call)))
        (twidget-component-assert setup-called "Setup should be called")
        (twidget-component-assert-equal 42
                                        (plist-get (twidget-instance-state instance) :value))))))

;;; ============================================================================
;;; Test: Component Rendering
;;; ============================================================================

(defun twidget-test-component-rendering ()
  "Test component rendering functionality."
  
  (twidget-component-test "Render: basic render"
    (twidget-clear-components)
    (twidget-unmount-all)
    (twidget-define-component test-render
      :props (text)
      :render (lambda (props _state)
                (format "<%s>" (or (plist-get props :text) "empty"))))
    (let ((result (twidget 'test-render :text "hello")))
      (twidget-component-assert-equal "<hello>" result)))
  
  (twidget-component-test "Render: with state"
    (twidget-clear-components)
    (twidget-unmount-all)
    (twidget-define-component test-render-state
      :props ()
      :setup (lambda (_props)
               (list :counter (twidget-ref 42)))
      :render (lambda (_props state)
                (format "Count: %d"
                        (twidget-ref-get (plist-get state :counter)))))
    (let ((result (twidget 'test-render-state)))
      (twidget-component-assert-equal "Count: 42" result))))

;;; ============================================================================
;;; Test: Component Mounting
;;; ============================================================================

(defun twidget-test-component-mounting ()
  "Test component mounting functionality."
  
  (twidget-component-test "Mount: insert into buffer"
    (twidget-clear-components)
    (twidget-unmount-all)
    (twidget-define-component test-mount
      :props (text)
      :render (lambda (props _state)
                (or (plist-get props :text) "mounted")))
    (with-temp-buffer
      (let ((instance (twidget-insert 'test-mount :text "test-content")))
        (twidget-component-assert (twidget-instance-mounted-p instance))
        (twidget-component-assert-equal "test-content" (buffer-string))
        (twidget-unmount instance))))
  
  (twidget-component-test "Mount: lifecycle hooks"
    (twidget-clear-components)
    (twidget-unmount-all)
    (let ((mounted-called nil)
          (unmounted-called nil))
      (twidget-define-component test-lifecycle
        :props ()
        :render (lambda (_p _s) "lifecycle")
        :mounted (lambda (_instance)
                   (setq mounted-called t))
        :unmounted (lambda (_instance)
                     (setq unmounted-called t)))
      (with-temp-buffer
        (let ((instance (twidget-insert 'test-lifecycle)))
          (twidget-component-assert mounted-called "Mounted should be called")
          (twidget-unmount instance)
          (twidget-component-assert unmounted-called "Unmounted should be called"))))))

;;; ============================================================================
;;; Test: Built-in Components
;;; ============================================================================

(defun twidget--register-builtin-components ()
  "Register built-in components for testing."
  ;; Text Component
  (twidget-define-component twidget-text
    :props (content face help-echo)
    :setup (lambda (props)
             (list :content (plist-get props :content)))
    :render (lambda (props _state)
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
                            'twidget-input-handler (plist-get state :handle-input))))))

(defun twidget-test-builtin-components ()
  "Test built-in components."
  ;; Register built-in components
  (twidget--register-builtin-components)
  
  (twidget-component-test "Built-in: twidget-text"
    (let ((result (twidget 'twidget-text :content "Hello World")))
      (twidget-component-assert (stringp result))
      (twidget-component-assert (string-match-p "Hello World" result))))
  
  (twidget-component-test "Built-in: twidget-text with face"
    (let ((result (twidget 'twidget-text :content "Styled" :face 'bold)))
      (twidget-component-assert (stringp result))
      (twidget-component-assert-equal 'bold (get-text-property 0 'face result))))
  
  (twidget-component-test "Built-in: twidget-button"
    (let ((result (twidget 'twidget-button :label "Click")))
      (twidget-component-assert (stringp result))
      (twidget-component-assert (string-match-p "Click" result))
      (twidget-component-assert (get-text-property 0 'keymap result))))
  
  (twidget-component-test "Built-in: twidget-button disabled"
    (let ((result (twidget 'twidget-button :label "Disabled" :disabled t)))
      (twidget-component-assert-equal 'shadow (get-text-property 0 'face result))))
  
  (twidget-component-test "Built-in: twidget-input"
    (let ((result (twidget 'twidget-input :value "test")))
      (twidget-component-assert (stringp result))
      (twidget-component-assert (string-match-p "test" result)))))

;;; ============================================================================
;;; Test: Reactive Props
;;; ============================================================================

(defun twidget-test-reactive-props ()
  "Test reactive props functionality."
  
  (twidget-component-test "Reactive: $ prefix detection"
    (twidget-component-assert (twidget--reactive-symbol-p '$test))
    (twidget-component-assert (not (twidget--reactive-symbol-p 'test)))
    (twidget-component-assert-equal 'test (twidget--reactive-var-symbol '$test)))
  
  (twidget-component-test "Reactive: collect reactive symbols"
    (let ((syms (twidget--collect-reactive-symbols '(face (:foreground $color)))))
      (twidget-component-assert (member '$color syms))))
  
  (twidget-component-test "Reactive: resolve reactive value"
    (defvar test-reactive-var "red")
    (let ((result (twidget--resolve-reactive-value '$test-reactive-var)))
      (twidget-component-assert-equal "red" result))))

;;; ============================================================================
;;; Test: Composition API Helpers
;;; ============================================================================

(defun twidget-test-composition-api ()
  "Test Composition API helpers."
  
  (twidget-component-test "Composition: use-state"
    (let ((state (twidget-use-state 0)))
      (twidget-component-assert (twidget-ref-p state))
      (twidget-component-assert-equal 0 (twidget-ref-get state))))
  
  (twidget-component-test "Composition: use-computed"
    (let* ((a (twidget-ref 2))
           (b (twidget-ref 3))
           (sum (twidget-use-computed
                 (lambda ()
                   (+ (twidget-ref-get a) (twidget-ref-get b))))))
      (twidget-component-assert-equal 5 (twidget-computed-get sum)))))

;;; ============================================================================
;;; Test Runner
;;; ============================================================================

(defun twidget-component-run-tests ()
  "Run all component tests and print summary."
  (setq twidget-component-test-results '())
  (setq twidget-component-test-passed 0)
  (setq twidget-component-test-failed 0)

  (message "\n")
  (message "========================================")
  (message "  Twidget Component System Tests")
  (message "========================================\n")

  ;; Run all test suites
  (twidget-test-component-definition)
  (twidget-test-component-instance)
  (twidget-test-component-rendering)
  (twidget-test-component-mounting)
  (twidget-test-builtin-components)
  (twidget-test-reactive-props)
  (twidget-test-composition-api)

  ;; Print summary
  (message "\n")
  (message "========================================")
  (message "  Test Results Summary")
  (message "========================================")
  (message "  Passed: %d" twidget-component-test-passed)
  (message "  Failed: %d" twidget-component-test-failed)
  (message "  Total:  %d" (+ twidget-component-test-passed twidget-component-test-failed))
  (message "========================================\n")

  ;; Print failed tests if any
  (when (> twidget-component-test-failed 0)
    (message "Failed tests:")
    (dolist (result (reverse twidget-component-test-results))
      (when (stringp (cdr result))
        (message "  - %s: %s" (car result) (cdr result)))))

  ;; Return success/failure
  (= twidget-component-test-failed 0))

(provide 'twidget-component-test)

;;; twidget-component-test.el ends here
