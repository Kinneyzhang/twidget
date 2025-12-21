;;; twidget-tests.el --- Tests for twidget.el -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for the twidget component library.

;;; Code:

(require 'ert)
(require 'twidget)

;;; Reactive Ref Tests

(ert-deftest twidget-test-ref-create ()
  "Test creating a reactive ref."
  (twidget-reset)
  (let ((ref (twidget-ref 42)))
    (should (symbolp ref))
    (should (= 42 (twidget-ref-value ref)))
    (twidget-unref ref)))

(ert-deftest twidget-test-ref-set ()
  "Test setting a reactive ref value."
  (twidget-reset)
  (let ((ref (twidget-ref 0)))
    (twidget-ref-set ref 100)
    (should (= 100 (twidget-ref-value ref)))
    (twidget-unref ref)))

(ert-deftest twidget-test-ref-watch ()
  "Test watching a reactive ref."
  (twidget-reset)
  (let* ((ref (twidget-ref 0))
         (called nil)
         (new-val nil)
         (old-val nil))
    (twidget-watch ref
      (lambda (new old _ref)
        (setq called t
              new-val new
              old-val old)))
    (twidget-ref-set ref 42)
    (should called)
    (should (= 42 new-val))
    (should (= 0 old-val))
    (twidget-unref ref)))

(ert-deftest twidget-test-computed ()
  "Test computed refs."
  (twidget-reset)
  (let* ((a (twidget-ref 2))
         (b (twidget-ref 3))
         (sum (twidget-computed
               (lambda ()
                 (+ (twidget-ref-value a)
                    (twidget-ref-value b)))
               a b)))
    (should (= 5 (twidget-ref-value sum)))
    (twidget-ref-set a 10)
    (should (= 13 (twidget-ref-value sum)))
    (twidget-unref a)
    (twidget-unref b)
    (twidget-unref sum)))

;;; Reactive Text Tests

(ert-deftest twidget-test-reactive-text-insert ()
  "Test inserting reactive text."
  (twidget-reset)
  (let ((ref (twidget-ref "Hello")))
    (with-temp-buffer
      (twidget-insert-reactive ref)
      (should (string= "Hello" (buffer-string)))
      ;; Update ref and check buffer
      (twidget-ref-set ref "World")
      (should (string= "World" (buffer-string))))
    (twidget-unref ref)))

(ert-deftest twidget-test-reactive-text-format ()
  "Test reactive text with format function."
  (twidget-reset)
  (let ((ref (twidget-ref 42)))
    (with-temp-buffer
      (twidget-insert-reactive ref (lambda (v) (format "Count: %d" v)))
      (should (string= "Count: 42" (buffer-string)))
      (twidget-ref-set ref 100)
      (should (string= "Count: 100" (buffer-string))))
    (twidget-unref ref)))

;;; Component Definition Tests

(ert-deftest twidget-test-define-component ()
  "Test defining a component."
  (twidget-reset)
  (define-twidget test-component
    :props (value)
    :setup (lambda (props)
             (list :value (twidget-ref (plist-get props :value))))
    :render (lambda (_props state)
              (twidget-text (plist-get state :value))))
  (should (twidget-get-component 'test-component)))

(ert-deftest twidget-test-render-component ()
  "Test rendering a component."
  (twidget-reset)
  (define-twidget simple-text
    :props (text)
    :setup (lambda (props)
             (list :text (twidget-ref (or (plist-get props :text) "default"))))
    :render (lambda (_props state)
              (twidget-text (plist-get state :text))))
  
  (with-temp-buffer
    (let ((instance-id (twidget-render 'simple-text '(:text "Hello"))))
      (should instance-id)
      (should (string= "Hello" (buffer-string)))
      ;; Update state
      (let ((state (twidget-instance-state instance-id)))
        (twidget-ref-set (plist-get state :text) "World"))
      (should (string= "World" (buffer-string)))
      (twidget-unmount instance-id))))

;;; Virtual Element Tests

(ert-deftest twidget-test-h-fragment ()
  "Test twidget-h fragment creation."
  (twidget-reset)
  (let ((element (twidget-h "Hello" " " "World")))
    (should (eq 'fragment (plist-get element :type)))
    (should (= 3 (length (plist-get element :children))))))

(ert-deftest twidget-test-span ()
  "Test twidget-span element creation."
  (twidget-reset)
  (let ((element (twidget-span '(face bold) "Bold text")))
    (should (eq 'span (plist-get element :type)))
    (should (equal '(face bold) (plist-get element :props)))))

;;; Built-in Component Tests

(ert-deftest twidget-test-button-component ()
  "Test the built-in button component."
  (twidget-reset)
  (with-temp-buffer
    (let ((clicked nil))
      (twidget-render 'twidget-button
                      `(:label "Click"
                        :on-click (lambda () (setq clicked t))))
      (should (string-match-p "\\[Click\\]" (buffer-string))))))

(ert-deftest twidget-test-progress-component ()
  "Test the built-in progress component."
  (twidget-reset)
  (with-temp-buffer
    (let ((instance-id (twidget-render 'twidget-progress
                                       '(:current 50 :total 100 :width 10))))
      (should (string-match-p "50%" (buffer-string)))
      (twidget-unmount instance-id))))

;;; Cleanup Tests

(ert-deftest twidget-test-unmount ()
  "Test unmounting a component."
  (twidget-reset)
  (define-twidget unmount-test
    :props nil
    :setup (lambda (_props)
             (list :ref (twidget-ref "test")))
    :render (lambda (_props state)
              (twidget-text (plist-get state :ref))))
  
  (with-temp-buffer
    (let ((instance-id (twidget-render 'unmount-test)))
      (should (string= "test" (buffer-string)))
      (twidget-unmount instance-id)
      (should (string= "" (buffer-string)))
      (should-not (twidget-get-instance instance-id)))))

(ert-deftest twidget-test-reset ()
  "Test resetting all state."
  (twidget-reset)
  (let ((ref (twidget-ref 42)))
    (should (twidget-ref-p ref))
    (twidget-reset)
    (should-not (twidget-ref-p ref))))

(provide 'twidget-tests)
;;; twidget-tests.el ends here
