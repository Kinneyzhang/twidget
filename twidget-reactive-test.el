;;; twidget-reactive-test.el --- Tests for twidget-reactive.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Comprehensive tests for the twidget-reactive system.
;; Run with: emacs -batch -l twidget-reactive.el -l twidget-reactive-test.el -f twidget-run-all-tests

;;; Code:

(require 'cl-lib)
(require 'twidget-reactive)

(defvar twidget-test-results '()
  "List of test results.")

(defvar twidget-test-passed 0
  "Number of passed tests.")

(defvar twidget-test-failed 0
  "Number of failed tests.")

(defmacro twidget-test (name &rest body)
  "Define a test named NAME with BODY."
  (declare (indent 1))
  `(condition-case err
       (progn
         ,@body
         (setq twidget-test-passed (1+ twidget-test-passed))
         (push (cons ,name 'passed) twidget-test-results)
         (message "✓ PASSED: %s" ,name))
     (error
      (setq twidget-test-failed (1+ twidget-test-failed))
      (push (cons ,name (format "FAILED: %s" (error-message-string err))) twidget-test-results)
      (message "✗ FAILED: %s - %s" ,name (error-message-string err)))))

(defmacro twidget-assert (condition &optional message)
  "Assert that CONDITION is true, otherwise signal error with MESSAGE."
  `(unless ,condition
     (error (or ,message (format "Assertion failed: %S" ',condition)))))

(defmacro twidget-assert-equal (expected actual &optional message)
  "Assert that EXPECTED equals ACTUAL."
  `(let ((exp ,expected)
         (act ,actual))
     (unless (equal exp act)
       (error (or ,message (format "Expected %S but got %S" exp act))))))

;;; ============================================================================
;;; Test: Ref (Reactive Reference)
;;; ============================================================================

(defun twidget-test-ref ()
  "Test Ref functionality."
  (twidget-test "Ref: create and access"
    (let ((r (twidget-ref 42)))
      (twidget-assert (twidget-ref-p r) "Should be a ref")
      (twidget-assert (twidget-is-ref r) "twidget-is-ref should work")
      (twidget-assert-equal 42 (twidget-ref-get r))
      (twidget-assert-equal 42 (twidget-get r))))

  (twidget-test "Ref: set value"
    (let ((r (twidget-ref 0)))
      (twidget-ref-set r 100)
      (twidget-assert-equal 100 (twidget-ref-get r))
      (twidget-set r 200)
      (twidget-assert-equal 200 (twidget-ref-get r))))

  (twidget-test "Ref: readonly"
    (let ((r (twidget-ref 42 t)))
      (twidget-assert (twidget-is-readonly r) "Should be readonly")
      (twidget-assert-equal 42 (twidget-ref-get r))
      ;; Setting readonly should emit warning and return nil
      (twidget-assert-equal nil (twidget-ref-set r 100))
      ;; Value should remain unchanged
      (twidget-assert-equal 42 (twidget-ref-get r))))

  (twidget-test "Ref: increment and decrement"
    (let ((r (twidget-ref 10)))
      (twidget-ref-inc r)
      (twidget-assert-equal 11 (twidget-ref-get r))
      (twidget-ref-inc r 5)
      (twidget-assert-equal 16 (twidget-ref-get r))
      (twidget-ref-dec r)
      (twidget-assert-equal 15 (twidget-ref-get r))
      (twidget-ref-dec r 3)
      (twidget-assert-equal 12 (twidget-ref-get r))))

  (twidget-test "Ref: unref"
    (let ((r (twidget-ref "hello")))
      (twidget-assert-equal "hello" (twidget-unref r))
      (twidget-assert-equal "world" (twidget-unref "world")))))

;;; ============================================================================
;;; Test: Reactive Object
;;; ============================================================================

(defun twidget-test-reactive ()
  "Test Reactive object functionality."
  (twidget-test "Reactive: create from plist"
    (let ((obj (twidget-reactive '(:name "Alice" :age 25))))
      (twidget-assert (twidget-reactive-p obj))
      (twidget-assert (twidget-is-reactive obj))
      (twidget-assert-equal "Alice" (twidget-reactive-get obj :name))
      (twidget-assert-equal 25 (twidget-reactive-get obj :age))))

  (twidget-test "Reactive: create from alist"
    (let ((obj (twidget-reactive '((name . "Bob") (age . 30)))))
      (twidget-assert-equal "Bob" (twidget-reactive-get obj :name))
      (twidget-assert-equal 30 (twidget-reactive-get obj :age))))

  (twidget-test "Reactive: set value"
    (let ((obj (twidget-reactive '(:count 0))))
      (twidget-reactive-set obj :count 10)
      (twidget-assert-equal 10 (twidget-reactive-get obj :count))
      (twidget-set obj :count 20)
      (twidget-assert-equal 20 (twidget-reactive-get obj :count))))

  (twidget-test "Reactive: has and delete"
    (let ((obj (twidget-reactive '(:a 1 :b 2))))
      (twidget-assert (twidget-reactive-has obj :a))
      (twidget-assert (not (twidget-reactive-has obj :c)))
      (twidget-reactive-delete obj :a)
      (twidget-assert (not (twidget-reactive-has obj :a)))))

  (twidget-test "Reactive: keys and to-plist"
    (let ((obj (twidget-reactive '(:x 1 :y 2 :z 3))))
      (twidget-assert-equal 3 (length (twidget-reactive-keys obj)))
      (twidget-assert-equal '(:x 1 :y 2 :z 3) (twidget-reactive-to-plist obj))))

  (twidget-test "Reactive: readonly"
    (let* ((obj (twidget-reactive '(:value 100)))
           (readonly-obj (twidget-readonly obj)))
      (twidget-assert (twidget-readonly-p readonly-obj))
      (twidget-assert (twidget-is-readonly readonly-obj))
      (twidget-assert-equal 100 (twidget-reactive-get readonly-obj :value))
      ;; Setting readonly should emit warning and return nil
      (twidget-assert-equal nil (twidget-reactive-set readonly-obj :value 999))
      ;; Value should remain unchanged
      (twidget-assert-equal 100 (twidget-reactive-get readonly-obj :value)))))

;;; ============================================================================
;;; Test: Effect (Side Effects)
;;; ============================================================================

(defun twidget-test-effect ()
  "Test Effect functionality."
  (twidget-test "Effect: basic execution"
    (let ((counter (twidget-ref 0))
          (effect-count 0))
      (twidget-effect
       (lambda ()
         (twidget-ref-get counter)
         (setq effect-count (1+ effect-count))))
      ;; Effect runs immediately
      (twidget-assert-equal 1 effect-count)
      ;; Updating ref triggers effect
      (twidget-ref-set counter 1)
      (twidget-assert-equal 2 effect-count)
      (twidget-ref-set counter 2)
      (twidget-assert-equal 3 effect-count)))

  (twidget-test "Effect: lazy option"
    (let ((counter (twidget-ref 0))
          (effect-count 0))
      (twidget-effect
       (lambda ()
         (twidget-ref-get counter)
         (setq effect-count (1+ effect-count)))
       '(:lazy t))
      ;; Effect should not run immediately with :lazy t
      (twidget-assert-equal 0 effect-count)))

  (twidget-test "Effect: stop"
    (let ((counter (twidget-ref 0))
          (effect-count 0))
      (let ((runner (twidget-effect
                     (lambda ()
                       (twidget-ref-get counter)
                       (setq effect-count (1+ effect-count))))))
        (twidget-assert-equal 1 effect-count)
        (twidget-stop runner)
        ;; After stopping, updates should not trigger effect
        (twidget-ref-set counter 100)
        (twidget-assert-equal 1 effect-count))))

  (twidget-test "Effect: without-tracking"
    (let ((counter (twidget-ref 0))
          (effect-count 0))
      (twidget-effect
       (lambda ()
         ;; Access inside without-tracking should not create dependency
         (twidget-without-tracking
          (twidget-ref-get counter))
         (setq effect-count (1+ effect-count))))
      (twidget-assert-equal 1 effect-count)
      ;; Since we didn't track, this shouldn't trigger the effect
      (twidget-ref-set counter 100)
      (twidget-assert-equal 1 effect-count))))

;;; ============================================================================
;;; Test: Computed
;;; ============================================================================

(defun twidget-test-computed ()
  "Test Computed functionality."
  (twidget-test "Computed: basic computation"
    (let* ((a (twidget-ref 2))
           (b (twidget-ref 3))
           (sum (twidget-computed
                 (lambda ()
                   (+ (twidget-ref-get a) (twidget-ref-get b))))))
      (twidget-assert (twidget-computed-p sum))
      (twidget-assert (twidget-is-computed sum))
      (twidget-assert-equal 5 (twidget-computed-get sum))))

  (twidget-test "Computed: reactivity"
    (let* ((price (twidget-ref 100))
           (quantity (twidget-ref 2))
           (total (twidget-computed
                   (lambda ()
                     (* (twidget-ref-get price)
                        (twidget-ref-get quantity))))))
      (twidget-assert-equal 200 (twidget-computed-get total))
      (twidget-ref-set quantity 3)
      (twidget-assert-equal 300 (twidget-computed-get total))
      (twidget-ref-set price 50)
      (twidget-assert-equal 150 (twidget-computed-get total))))

  (twidget-test "Computed: caching"
    (let* ((counter (twidget-ref 1))
           (compute-count 0)
           (doubled (twidget-computed
                     (lambda ()
                       (setq compute-count (1+ compute-count))
                       (* 2 (twidget-ref-get counter))))))
      ;; First access triggers computation
      (twidget-assert-equal 2 (twidget-computed-get doubled))
      (twidget-assert-equal 1 compute-count)
      ;; Second access should use cache
      (twidget-assert-equal 2 (twidget-computed-get doubled))
      (twidget-assert-equal 1 compute-count)
      ;; After dependency change, should recompute
      (twidget-ref-set counter 5)
      (twidget-assert-equal 10 (twidget-computed-get doubled))
      (twidget-assert-equal 2 compute-count)))

  (twidget-test "Computed: with getter and setter"
    (let* ((first-name (twidget-ref "John"))
           (last-name (twidget-ref "Doe"))
           (full-name (twidget-computed
                       (list :get (lambda ()
                                    (format "%s %s"
                                            (twidget-ref-get first-name)
                                            (twidget-ref-get last-name)))
                             :set (lambda (value)
                                    (let ((parts (split-string value " ")))
                                      (twidget-ref-set first-name (car parts))
                                      (twidget-ref-set last-name (or (cadr parts) ""))))))))
      (twidget-assert-equal "John Doe" (twidget-computed-get full-name))
      (twidget-computed-set full-name "Jane Smith")
      (twidget-assert-equal "Jane" (twidget-ref-get first-name))
      (twidget-assert-equal "Smith" (twidget-ref-get last-name)))))

;;; ============================================================================
;;; Test: Watch
;;; ============================================================================

(defun twidget-test-watch ()
  "Test Watch functionality."
  (twidget-test "Watch: basic watching"
    (let ((counter (twidget-ref 0))
          (watch-count 0)
          (last-new nil)
          (last-old nil))
      (let ((stop (twidget-watch
                   counter
                   (lambda (new-val old-val &optional _on-cleanup)
                     (setq watch-count (1+ watch-count))
                     (setq last-new new-val)
                     (setq last-old old-val)))))
        ;; Initial state - watch hasn't fired yet without :immediate
        (twidget-assert-equal 0 watch-count)
        ;; Update triggers watch
        (twidget-ref-set counter 10)
        (twidget-assert-equal 1 watch-count)
        (twidget-assert-equal 10 last-new)
        (twidget-assert-equal 0 last-old)
        ;; Stop watching
        (funcall stop)
        (twidget-ref-set counter 20)
        ;; Should not trigger after stop
        (twidget-assert-equal 1 watch-count))))

  (twidget-test "Watch: immediate option"
    (let ((counter (twidget-ref 5))
          (watch-count 0))
      (twidget-watch
       counter
       (lambda (_new _old &optional _on-cleanup)
         (setq watch-count (1+ watch-count)))
       '(:immediate t))
      ;; Should fire immediately
      (twidget-assert-equal 1 watch-count)))

  (twidget-test "Watch: once option"
    (let ((counter (twidget-ref 0))
          (watch-count 0))
      (twidget-watch
       counter
       (lambda (_new _old &optional _on-cleanup)
         (setq watch-count (1+ watch-count)))
       '(:once t))
      (twidget-ref-set counter 1)
      (twidget-assert-equal 1 watch-count)
      ;; Second update should not trigger due to :once
      (twidget-ref-set counter 2)
      (twidget-assert-equal 1 watch-count)))

  (twidget-test "Watch: watch-effect"
    (let ((a (twidget-ref 1))
          (b (twidget-ref 2))
          (effect-count 0)
          (last-sum nil))
      (let ((stop (twidget-watch-effect
                   (lambda (_on-cleanup)
                     (setq effect-count (1+ effect-count))
                     (setq last-sum (+ (twidget-ref-get a)
                                       (twidget-ref-get b)))))))
        ;; watch-effect runs immediately
        (twidget-assert-equal 1 effect-count)
        (twidget-assert-equal 3 last-sum)
        ;; Update triggers re-run
        (twidget-ref-set a 10)
        (twidget-assert-equal 2 effect-count)
        (twidget-assert-equal 12 last-sum)
        (funcall stop)))))

;;; ============================================================================
;;; Test: Integration
;;; ============================================================================

(defun twidget-test-integration ()
  "Test integration scenarios."
  (twidget-test "Integration: computed in effect"
    (let* ((x (twidget-ref 2))
           (y (twidget-ref 3))
           (product (twidget-computed
                     (lambda ()
                       (* (twidget-ref-get x) (twidget-ref-get y)))))
           (effect-results '()))
      (twidget-effect
       (lambda ()
         (push (twidget-computed-get product) effect-results)))
      ;; Initial effect run
      (twidget-assert-equal '(6) effect-results)
      ;; Update x
      (twidget-ref-set x 5)
      (twidget-assert-equal '(15 6) effect-results)
      ;; Update y
      (twidget-ref-set y 10)
      (twidget-assert-equal '(50 15 6) effect-results)))

  (twidget-test "Integration: reactive with computed and watch"
    (let* ((user (twidget-reactive '(:first-name "John" :last-name "Doe")))
           (full-name (twidget-computed
                       (lambda ()
                         (format "%s %s"
                                 (twidget-reactive-get user :first-name)
                                 (twidget-reactive-get user :last-name)))))
           (name-changes '()))
      (twidget-watch
       full-name
       (lambda (new-val _old &optional _on-cleanup)
         (push new-val name-changes)))
      (twidget-reactive-set user :first-name "Jane")
      (twidget-assert (member "Jane Doe" name-changes))))

  (twidget-test "Integration: to-ref"
    (let* ((user (twidget-reactive '(:name "Alice" :age 25)))
           (name-ref (twidget-to-ref user :name)))
      ;; Reading through ref
      (twidget-assert-equal "Alice" (twidget-computed-get name-ref))
      ;; Writing through ref
      (twidget-computed-set name-ref "Bob")
      (twidget-assert-equal "Bob" (twidget-reactive-get user :name))
      ;; Changes in original reflect in ref
      (twidget-reactive-set user :name "Charlie")
      (twidget-assert-equal "Charlie" (twidget-computed-get name-ref))))

  (twidget-test "Integration: to-raw"
    (let* ((reactive-obj (twidget-reactive '(:a 1 :b 2)))
           (ref-obj (twidget-ref 42))
           (computed-obj (twidget-computed (lambda () 100))))
      (twidget-assert-equal '(:a 1 :b 2) (twidget-to-raw reactive-obj))
      (twidget-assert-equal 42 (twidget-to-raw ref-obj))
      (twidget-computed-get computed-obj) ; Trigger computation
      (twidget-assert-equal 100 (twidget-to-raw computed-obj)))))

;;; ============================================================================
;;; Test Runner
;;; ============================================================================

(defun twidget-run-all-tests ()
  "Run all tests and print summary."
  (setq twidget-test-results '())
  (setq twidget-test-passed 0)
  (setq twidget-test-failed 0)

  (message "\n")
  (message "========================================")
  (message "  Twidget Reactive System Tests")
  (message "========================================\n")

  ;; Run all test suites
  (twidget-test-ref)
  (twidget-test-reactive)
  (twidget-test-effect)
  (twidget-test-computed)
  (twidget-test-watch)
  (twidget-test-integration)

  ;; Print summary
  (message "\n")
  (message "========================================")
  (message "  Test Results Summary")
  (message "========================================")
  (message "  Passed: %d" twidget-test-passed)
  (message "  Failed: %d" twidget-test-failed)
  (message "  Total:  %d" (+ twidget-test-passed twidget-test-failed))
  (message "========================================\n")

  ;; Print failed tests if any
  (when (> twidget-test-failed 0)
    (message "Failed tests:")
    (dolist (result (reverse twidget-test-results))
      (when (stringp (cdr result))
        (message "  - %s: %s" (car result) (cdr result)))))

  ;; Return success/failure
  (= twidget-test-failed 0))

(provide 'twidget-reactive-test)

;;; twidget-reactive-test.el ends here
