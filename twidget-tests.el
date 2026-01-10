;;; twidget-tests.el --- ERT Tests for twidget -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Kinney Zhang

;; Author: Kinney Zhang <kinneyzhang666@gmail.com>
;; URL: https://github.com/Kinneyzhang/twidget

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This file contains ERT tests for the twidget package.
;; Run tests with: M-x ert RET "twidget-" RET
;; Or from command line: emacs -batch -l twidget.el -l twidget-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'twidget)

;;; Test Helpers
;; ============================================================================

(defmacro twidget-test-with-temp-buffer (&rest body)
  "Execute BODY in a temporary buffer with fresh twidget state."
  (declare (indent 0))
  `(with-temp-buffer
     (twidget-clear-buffer-state)
     ,@body))

;;; Basic Widget Definition Tests
;; ============================================================================

(ert-deftest twidget-test-define-simple-widget ()
  "Test defining a simple widget with :render."
  (twidget-reset)
  (define-twidget test-simple
    :render (lambda (_props _slot) "Hello"))
  (should (assoc 'test-simple twidget-alist))
  (should (equal (twidget-parse '(test-simple)) "Hello")))

(ert-deftest twidget-test-define-widget-with-props ()
  "Test defining a widget with properties."
  (twidget-reset)
  (define-twidget test-props
    :props '((name . "World"))
    :render (lambda (props _slot)
              (format "Hello, %s!" (plist-get props :name))))
  (should (equal (twidget-parse '(test-props)) "Hello, World!"))
  (should (equal (twidget-parse '(test-props :name "Emacs")) "Hello, Emacs!")))

(ert-deftest twidget-test-define-widget-with-slot ()
  "Test defining a widget with slot content."
  (twidget-reset)
  (define-twidget test-slot
    :slot t
    :render (lambda (_props slot)
              (format "<<< %s >>>" (twidget-slot-to-string slot))))
  (should (equal (twidget-parse '(test-slot "Content")) "<<< Content >>>")))

(ert-deftest twidget-test-widget-inheritance ()
  "Test widget inheritance with :extends."
  (twidget-reset)
  (define-twidget test-base
    :props '((color . "black"))
    :slot t
    :render (lambda (props slot)
              (format "[%s]%s" (plist-get props :color) (twidget-slot-to-string slot))))
  (define-twidget test-child
    :extends 'test-base
    :props '((color . "red"))
    :render (lambda (props slot parent-render)
              (upcase (funcall parent-render props slot))))
  (should (equal (twidget-parse '(test-child "hello")) "[RED]HELLO")))

;;; Composite Widget Tests
;; ============================================================================

(ert-deftest twidget-test-composite-widget ()
  "Test defining a composite widget with :setup and :template."
  (twidget-reset)
  (twidget-test-with-temp-buffer
    (define-twidget test-composite
      :setup (lambda (_props _slot)
               (list :message "Hello from setup"))
      :template '(span "{message}"))
    (let ((result (twidget-parse '(test-composite))))
      (should (string-match-p "Hello from setup" result)))))

;;; Reactive System Tests
;; ============================================================================

(ert-deftest twidget-test-twidget-ref ()
  "Test creating reactive references."
  (let ((ref (twidget-ref 42)))
    (should (twidget-ref-p ref))
    (should (equal (twidget-ref-value ref) 42))))

(ert-deftest twidget-test-twidget-ref-set ()
  "Test setting reactive reference values."
  (let ((ref (twidget-ref 0)))
    (twidget-ref-set ref 10)
    (should (equal (twidget-ref-value ref) 10))))

(ert-deftest twidget-test-twidget-watch ()
  "Test watching reactive references for changes."
  (let ((ref (twidget-ref 0))
        (changed-values nil))
    (twidget-watch ref (lambda (new old)
                         (push (list new old) changed-values)))
    (twidget-ref-set ref 5)
    (should (equal changed-values '((5 0))))))

(ert-deftest twidget-test-twidget-watch-immediate ()
  "Test immediate callback with twidget-watch."
  (let ((ref (twidget-ref 42))
        (called nil))
    (twidget-watch ref
      (lambda (new old)
        (setq called (list new old)))
      t)  ; immediate
    (should (equal called '(42 42)))))

(ert-deftest twidget-test-twidget-unwatch ()
  "Test unwatching reactive references."
  (let ((ref (twidget-ref 0))
        (call-count 0))
    (let ((watcher (lambda (_new _old)
                     (cl-incf call-count))))
      (twidget-watch ref watcher)
      (twidget-ref-set ref 1)
      (should (equal call-count 1))
      (twidget-unwatch ref watcher)
      (twidget-ref-set ref 2)
      (should (equal call-count 1)))))  ; Should not have increased

;;; Computed Properties Tests
;; ============================================================================

(ert-deftest twidget-test-computed-basic ()
  "Test basic computed property functionality."
  (let* ((a (twidget-ref 1))
         (b (twidget-ref 2))
         (sum (twidget-computed
               (lambda ()
                 (+ (twidget-ref-value a) (twidget-ref-value b)))
               a b)))
    (should (twidget-computed-p sum))
    (should (equal (twidget-computed-get sum) 3))))

(ert-deftest twidget-test-computed-caching ()
  "Test that computed properties cache their values."
  (let* ((call-count 0)
         (a (twidget-ref 10))
         (double (twidget-computed
                  (lambda ()
                    (cl-incf call-count)
                    (* 2 (twidget-ref-value a)))
                  a)))
    ;; First access - should compute
    (should (equal (twidget-computed-get double) 20))
    (should (equal call-count 1))
    ;; Second access - should use cache
    (should (equal (twidget-computed-get double) 20))
    (should (equal call-count 1))))  ; Should not recompute

(ert-deftest twidget-test-computed-invalidation ()
  "Test that computed properties recompute when dependencies change."
  (let* ((call-count 0)
         (a (twidget-ref 10))
         (double (twidget-computed
                  (lambda ()
                    (cl-incf call-count)
                    (* 2 (twidget-ref-value a)))
                  a)))
    (should (equal (twidget-computed-get double) 20))
    (should (equal call-count 1))
    ;; Change dependency
    (twidget-ref-set a 5)
    ;; Computed should be dirty and recompute
    (should (equal (twidget-computed-get double) 10))
    (should (equal call-count 2))))

;;; Memory Management Tests
;; ============================================================================

(ert-deftest twidget-test-clear-buffer-state ()
  "Test that twidget-clear-buffer-state cleans up properly."
  (twidget-test-with-temp-buffer
    (twidget-reset)
    (define-twidget test-widget
      :setup (lambda (_props _slot)
               (list :count (twidget-ref 0)))
      :template '(span "{count}"))
    (twidget-insert '(test-widget))
    ;; State should exist
    (should (> (hash-table-count twidget-ref-registry) 0))
    ;; Clear state
    (twidget-clear-buffer-state)
    ;; State should be empty
    (should (= (hash-table-count twidget-ref-registry) 0))
    (should (= (hash-table-count twidget-ref-by-name) 0))
    (should (= (hash-table-count twidget-reactive-symbols) 0))))

(ert-deftest twidget-test-unmount-instance ()
  "Test that twidget-unmount-instance cleans up a specific instance."
  (twidget-test-with-temp-buffer
    (twidget-reset)
    (define-twidget test-widget
      :setup (lambda (_props _slot)
               (list :count (twidget-ref 0)))
      :template '(span "{count}"))
    (twidget-insert '(test-widget))
    (let ((instance-id (car (hash-table-keys twidget-instances))))
      (should instance-id)
      (twidget-unmount-instance instance-id)
      (should (null (gethash instance-id twidget-instances))))))

;;; Lifecycle Hooks Tests
;; ============================================================================

(ert-deftest twidget-test-on-mounted-hook ()
  "Test that onMounted hook is called."
  (twidget-test-with-temp-buffer
    (twidget-reset)
    (let ((mounted-called nil))
      (define-twidget test-mounted
        :setup (lambda (_props _slot)
                 (list :message "test"
                       :onMounted (lambda () (setq mounted-called t))))
        :template '(span "{message}"))
      (twidget-insert '(test-mounted))
      (should mounted-called))))

(ert-deftest twidget-test-on-unmounted-hook ()
  "Test that onUnmounted hook is called."
  (twidget-test-with-temp-buffer
    (twidget-reset)
    (let ((unmounted-called nil))
      (define-twidget test-unmounted
        :setup (lambda (_props _slot)
                 (list :message "test"
                       :onUnmounted (lambda () (setq unmounted-called t))))
        :template '(span "{message}"))
      (twidget-insert '(test-unmounted))
      (should (not unmounted-called))
      (let ((instance-id (car (hash-table-keys twidget-instances))))
        (twidget-unmount-instance instance-id)
        (should unmounted-called)))))

;;; Event Expression Parsing Tests
;; ============================================================================

(ert-deftest twidget-test-parse-method-reference ()
  "Test parsing method reference expressions."
  (let ((result (twidget--parse-event-expression "doSomething")))
    (should (equal (plist-get result :type) 'method))
    (should (equal (plist-get result :name) "doSomething"))))

(ert-deftest twidget-test-parse-method-call ()
  "Test parsing method call expressions."
  (let ((result (twidget--parse-event-expression "greet('hello')")))
    (should (equal (plist-get result :type) 'method-call))
    (should (equal (plist-get result :name) "greet"))))

(ert-deftest twidget-test-parse-increment ()
  "Test parsing increment expressions."
  (let ((result (twidget--parse-event-expression "count++")))
    (should (equal (plist-get result :type) 'increment))
    (should (equal (plist-get result :var) "count"))))

(ert-deftest twidget-test-parse-decrement ()
  "Test parsing decrement expressions."
  (let ((result (twidget--parse-event-expression "count--")))
    (should (equal (plist-get result :type) 'decrement))
    (should (equal (plist-get result :var) "count"))))

(ert-deftest twidget-test-parse-assignment ()
  "Test parsing assignment expressions."
  (let ((result (twidget--parse-event-expression "count=10")))
    (should (equal (plist-get result :type) 'assignment))
    (should (equal (plist-get result :var) "count"))))

(ert-deftest twidget-test-parse-toggle ()
  "Test parsing toggle expressions."
  (let ((result (twidget--parse-event-expression "flag=!flag")))
    (should (equal (plist-get result :type) 'assignment))
    (should (equal (plist-get result :var) "flag"))
    (should (equal (plist-get (plist-get result :value) :type) 'negation))))

(ert-deftest twidget-test-parse-multi-statement ()
  "Test parsing multi-statement expressions."
  (let ((result (twidget--parse-event-expression "a++;b++")))
    (should (equal (plist-get result :type) 'multi-statement))
    (should (= (length (plist-get result :statements)) 2))))

;;; Hover Event Tests
;; ============================================================================

(ert-deftest twidget-test-hover-event-recognized ()
  "Test that hover event keywords are recognized."
  (should (equal (twidget--is-event-prop-p :on-mouse-enter) 'mouse-enter))
  (should (equal (twidget--is-event-prop-p :on-mouse-leave) 'mouse-leave))
  (should (equal (twidget--is-event-prop-p :on-hover) 'hover)))

(ert-deftest twidget-test-hover-handlers-created ()
  "Test that hover handlers create cursor-sensor-functions."
  (let ((handlers (twidget--create-hover-handlers (lambda () nil) nil)))
    (should (plist-get handlers 'cursor-sensor-functions))
    (should (listp (plist-get handlers 'cursor-sensor-functions)))))

(ert-deftest twidget-test-process-mouse-enter-event ()
  "Test processing mouse-enter event property."
  (let ((result (twidget--process-event-prop 'mouse-enter "doSomething" nil)))
    (should result)
    (should (plist-get result 'cursor-sensor-functions))
    (should (plist-get result 'rear-nonsticky))))

(ert-deftest twidget-test-process-mouse-leave-event ()
  "Test processing mouse-leave event property."
  (let ((result (twidget--process-event-prop 'mouse-leave "doSomething" nil)))
    (should result)
    (should (plist-get result 'cursor-sensor-functions))))

;;; For Expression Parsing Tests
;; ============================================================================

(ert-deftest twidget-test-parse-for-expression ()
  "Test parsing :for expressions."
  (let ((result (twidget-parse-for-expression "item in items")))
    (should result)
    (should (equal (car result) "item"))
    (should (equal (cdr result) "items"))))

(ert-deftest twidget-test-parse-for-expression-with-spaces ()
  "Test parsing :for expressions with extra spaces."
  (let ((result (twidget-parse-for-expression "  fruit   in   fruits  ")))
    (should result)
    (should (equal (car result) "fruit"))
    (should (equal (cdr result) "fruits"))))

;;; Dot Notation Tests
;; ============================================================================

(ert-deftest twidget-test-parse-dot-notation-simple ()
  "Test parsing simple variable names without dots."
  (let ((result (twidget--parse-dot-notation "count")))
    (should (equal result '("count")))))

(ert-deftest twidget-test-parse-dot-notation-plist ()
  "Test parsing plist property access."
  (let ((result (twidget--parse-dot-notation "user.name")))
    (should (equal (car result) "user"))
    (should (equal (cadr result) :name))))

(ert-deftest twidget-test-parse-dot-notation-list ()
  "Test parsing list index access."
  (let ((result (twidget--parse-dot-notation "items.0")))
    (should (equal (car result) "items"))
    (should (equal (cadr result) 0))))

(ert-deftest twidget-test-parse-dot-notation-nested ()
  "Test parsing nested access."
  (let ((result (twidget--parse-dot-notation "data.0.name")))
    (should (equal (car result) "data"))
    (should (equal (cadr result) 0))
    (should (equal (caddr result) :name))))

;;; Placeholder Substitution Tests
;; ============================================================================

(ert-deftest twidget-test-substitute-placeholders-simple ()
  "Test simple placeholder substitution."
  (let ((result (twidget-substitute-placeholders
                 "Hello, {name}!"
                 '(("name" . "World")))))
    (should (equal result "Hello, World!"))))

(ert-deftest twidget-test-substitute-placeholders-multiple ()
  "Test multiple placeholder substitution."
  (let ((result (twidget-substitute-placeholders
                 "{greeting}, {name}!"
                 '(("greeting" . "Hi") ("name" . "Emacs")))))
    (should (equal result "Hi, Emacs!"))))

(ert-deftest twidget-test-substitute-placeholders-missing ()
  "Test that missing placeholders are preserved."
  (let ((result (twidget-substitute-placeholders
                 "Hello, {unknown}!"
                 '(("name" . "World")))))
    (should (equal result "Hello, {unknown}!"))))

;;; Performance Tests (Secondary Index)
;; ============================================================================

(ert-deftest twidget-test-secondary-index-populated ()
  "Test that secondary index is populated when refs are registered."
  (twidget-test-with-temp-buffer
    (twidget-reset)
    (define-twidget test-indexed
      :setup (lambda (_props _slot)
               (list :count (twidget-ref 0)))
      :template '(span "{count}"))
    (twidget-insert '(test-indexed))
    ;; Check secondary index
    (let ((entries (gethash "count" twidget-ref-by-name)))
      (should entries)
      (should (= (length entries) 1)))))

(provide 'twidget-tests)
;;; twidget-tests.el ends here
