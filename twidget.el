;;; twidget.el --- Text widget library in emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Kinney Zhang
;;
;; Version: 0.0.1
;; Keywords: widget convenience
;; Author: Kinney Zhang <kinneyzhang666 AT gmail DOT com>
;; URL: https://github.com/Kinneyzhang/twidget
;; Package-Requires: ((emacs "24.4"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Twidget is an implement of widget library based on plain text in emacs.

;;; Code:

;;;; Requires

(require 'ewoc)
(require 'org-id)

;;;; Variables

(defgroup twidget nil
  "Text widget library."
  :group nil)

(defface twidget-choice-selected-face
  '((t :inherit font-lock-function-name-face :bold t))
  "Face for the selected choice.")

(defface twidget-choice-rest-face
  '((t :inherit shadow))
  "Face for the rest of unselected choices.")

(defface twidget-key-hint-face
  '((t :background "red" :foreground "white"))
  "Face for twidget key hint.")

(defvar twidget-ewoc nil
  "EWOC for twidget.")

(defvar twidget-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>") #'twidget-jump-forward)
    (define-key map (kbd "<backtab>") #'twidget-jump-backward)
    map)
  "Keymap for `twidget-mode'.")

(defvar twidget-choice-delimiter "|"
  "Delimiter between each choices.")

(defvar twidget-components
  '(twidget-single-choice)
  "A list of all twidget components.")

(defvar-local twidget-value nil
  "The value of current active twidget.")

(defvar-local twidget-active-id ""
  "The id of current active twidget.")

(defvar-local twidget-data nil
  "The ewoc data of current active twidget.")

;;;; Functions

(defun twidget-active-p (twidget-id)
  "Judge if the twidget with id TWIDGET-ID is actived."
  (string= twidget-active-id twidget-id))

;; twidget component function

;;; single-choice

(defun twidget-single-choice (&rest args)
  "Printer function for 'single choice' twidget."
  (let* ((id (plist-get args :id))
         (value (plist-get args :value))
         (require (plist-get args :require))
         (choices (plist-get args :choices))
         (format (plist-get args :format))
         (twidget-str
          (propertize (string-join choices twidget-choice-delimiter)
                      'twidget-id id))
         line-beg line-end twidget-beg twidget-end)
    (if format
        (progn
          (setq line-beg (point))
          (insert (replace-regexp-in-string "\\[t\\]" twidget-str format t))
          (setq line-end (point))
          (when (string-match "\\[t\\]" format)
            (let* ((beg (match-beginning 0))
                   (end (match-end 0))
                   (rest-len (- (length format) end)))
              (setq twidget-beg (+ line-beg beg))
              (setq twidget-end (- line-end rest-len)))))
      (setq twidget-beg (point))
      (insert twidget-str)
      (setq twidget-end (point))
      (setq line-end twidget-end))
    (when value
      (save-excursion
        (goto-char twidget-beg)
        (when (search-forward value nil t)
          (let ((beg (match-beginning 0))
                (end (match-end 0)))
            (add-face-text-property
             beg end 'twidget-choice-selected-face)
            (pcase nil
              ((guard (= beg twidget-beg))
               (add-face-text-property
                end twidget-end 'twidget-choice-rest-face))
              ((guard (= end twidget-end))
               (add-face-text-property
                twidget-beg beg 'twidget-choice-rest-face))
              (_ (add-face-text-property
                  twidget-beg beg 'twidget-choice-rest-face)
                 (add-face-text-property
                  end twidget-end 'twidget-choice-rest-face)))))))
    (when (twidget-active-p id)
      (goto-char twidget-beg)
      (dotimes (i (length choices))
        (let ((choice (nth i choices)))
          (when (search-forward choice nil t)
            (let* ((beg (match-beginning 0))
                   (letter (buffer-substring-no-properties beg (1+ beg))))
              (add-text-properties
               beg (1+ beg)
               `(display ,(concat (propertize (number-to-string (1+ i))
                                              'face 'twidget-key-hint-face)
                                  letter))))))))
    (goto-char line-end)))

;;;###autoload
(defun twidget-single-choice-switch (choice)
  "Set the value of 'single choice' twidget to CHOICE."
  (interactive)
  (twidget--correct-cursor)
  (let* ((node (ewoc-locate twidget-ewoc))
         (data (ewoc-data node))
         (twidget (car data))
         (plst (plist-put (cdr data) :value choice))
         (new-data (cons twidget plst)))
    (ewoc-set-data node new-data)
    (ewoc-invalidate twidget-ewoc node)
    (setq twidget-value choice)))

;;; multi-choices


;; twidget ewoc

(defun twidget-ewoc-pp (data)
  "Pretty printer for twidget ewoc."
  (pcase data
    ((and (pred listp)
          (guard (member (car data) twidget-components)))
     (let* ((twidget (car data))
            (plst (cdr data)))
       (pcase twidget
         ('twidget-single-choice
          (let ((bind (plist-get plst :bind))
                (value (plist-get plst :value)))
            (apply #'twidget-single-choice plst)
            (set bind value))))))
    (_ (insert data))))

(defun twidget--before-setup ()
  "Some buffer settings for twidget."
  (let ((ewoc (ewoc-create 'twidget-ewoc-pp nil nil)))
    (kill-all-local-variables)
    (remove-overlays)
    (buffer-disable-undo)
    (read-only-mode -1)
    ;; (setq cursor-type nil)
    (set (make-local-variable 'twidget-ewoc) ewoc)))

(defun twidget--after-setup ()
  "Setting keyhint of the first twidget, binding keys and so on."
  (goto-char (point-min))
  (when-let* ((match (text-property-search-forward 'twidget-id))
              (beg (prop-match-beginning match))
              (id (prop-match-value match))
              (data (ewoc-data (ewoc-locate twidget-ewoc))))
    (setq twidget-value (plist-get (cdr data) :value))
    (setq twidget-active-id id)
    (setq twidget-data (cdr data))
    (ewoc-refresh twidget-ewoc)
    (goto-char beg)
    (twidget-bind-key)
    (twidget-mode 1)
    (read-only-mode 1)))

(defun twidget-bind-key ()
  "Bind the key of twidget."
  (let* ((data twidget-data)
         (require (plist-get data :require))
         (bind (plist-get data :bind))
         (choices (plist-get data :choices)))
    (dotimes (i (length choices))
      (define-key twidget-mode-map (kbd (number-to-string (1+ i)))
        (lambda ()
          (interactive)
          (if require
              (progn
                (set bind twidget-value)
                (twidget-single-choice-switch (nth i choices)))
            (if (string= (nth i choices) twidget-value)
                (progn
                  (set bind nil)
                  (twidget-single-choice-switch nil))
              (set bind twidget-value)
              (twidget-single-choice-switch (nth i choices)))))))))

(defun twidget-unbind-key (data)
  "Unbind the key of twidget with data DATA."
  (let* ((choices (plist-get data :choices)))
    (dotimes (i (length choices))
      (unbind-key (kbd (number-to-string (1+ i))) twidget-mode-map))))

(defun twidget--correct-cursor ()
  "Move the cursor point to the beginning of active twidget."
  (unless (twidget-active-p (get-text-property (point) 'twidget-id))
    (goto-char (point-max))
    (text-property-search-backward 'twidget-id twidget-active-id)))

;;;###autoload
(defun twidget-jump-forward ()
  "Jump to the next twidget and active it."
  (interactive)
  ;; unbind the key of last active twidget
  (twidget-unbind-key twidget-data)
  ;; when the cursor is at the active twidget, move cursor to it.
  (twidget--correct-cursor)
  (when-let* ((match (text-property-search-forward 'twidget-id nil nil t))
              (beg (prop-match-beginning match))
              (id (prop-match-value match))
              (data (ewoc-data (ewoc-locate twidget-ewoc))))
    (setq twidget-value (plist-get (cdr data) :value))
    (setq twidget-active-id id)
    (setq twidget-data (cdr data))
    (ewoc-refresh twidget-ewoc)
    ;; bind the key of current active twidget
    (twidget-bind-key)
    (goto-char beg)))

;;;###autoload
(defun twidget-jump-backward ()
  "Jump to the previous twidget and active it."
  (interactive)
  (twidget-unbind-key twidget-data)
  ;; when the cursor is at the active twidget, move cursor to it.
  (twidget--correct-cursor)
  (when-let* ((match (text-property-search-backward 'twidget-id nil nil t))
              (beg (prop-match-beginning match))
              (id (prop-match-value match))
              (data (ewoc-data (ewoc-locate twidget-ewoc))))
    (setq twidget-value (plist-get (cdr data) :value))
    (setq twidget-active-id id)
    (setq twidget-data (cdr data))
    (ewoc-refresh twidget-ewoc)
    ;; bind the key of current active twidget
    (twidget-bind-key)
    (goto-char beg)))

;;;###autoload
(defun twidget-insert (&rest args)
  "Insert a series of twidgets at point."
  (twidget--before-setup)
  (dolist (arg args)
    (pcase arg
      ((and (pred listp)
            (guard (member (car arg) twidget-components)))
       ;; (apply #'twidget-create arg)
       (ewoc-enter-last twidget-ewoc (append arg `(:id ,(org-id-uuid)))))
      (_
       ;; (twidget-create arg)
       (ewoc-enter-last twidget-ewoc arg))))
  (twidget--after-setup))

;;;###autoload
(define-minor-mode twidget-mode
  "Minor mode for text widget."
  nil nil nil)

;;;; test example

(defvar single-choice nil)
(defvar single-choice2 nil)

(progn
  (pop-to-buffer (get-buffer-create "*test*"))
  (let ((inhibit-read-only t))
    (erase-buffer))
  (twidget-insert
   `(twidget-single-choice
     :bind single-choice
     :require t
     :value "daily"
     :format ,(concat (propertize "Repeat" 'face 'bold)
                      " [t] frequency.")
     :choices ,gtd-habit-regular-feq-type)
   ""
   `(twidget-single-choice
     :bind single-choice2
     :require nil
     :format ,(concat (propertize "Choose a week: " 'face 'bold)
                      "[t]")
     :value "Monday"
     :choices ,gtd-habit-frequency-by-day)))

;; (defun twidget-create (&rest twidget)
;;   (pcase twidget
;;     ((and (pred listp)
;;           (guard (member (car twidget) twidget-components)))
;;      (ewoc-enter-last twidget-ewoc (append twidget `(:id ,(org-id-uuid)))))
;;     (_ (ewoc-enter-last twidget-ewoc (car twidget)))))

(provide 'twidget)
;;; twidget.el ends here
