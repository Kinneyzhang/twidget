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
  '(twidget-choice)
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

;;; choice twidget

(defun twidget-choice (&rest args)
  "Printer function for 'single choice' twidget with arguments ARGS."
  (let* ((id (plist-get args :id))
         (value (plist-get args :value))
         (require (plist-get args :require))
         (multiple (plist-get args :multiple))
         (choices (plist-get args :choices))
         (format (plist-get args :format))
         (twidget-str
          (propertize (string-join choices twidget-choice-delimiter)
                      'twidget-id id))
         (twidget-len (length twidget-str))
         line-beg twidget-beg twidget-end)
    ;; insert the whole twidget-str
    (if format
        (if-let ((start (string-match "\\[t\\]" format)))
            (progn
              (setq twidget-beg (+ (point) start))
              (setq twidget-end (+ twidget-beg twidget-len))
              (insert (replace-regexp-in-string "\\[t\\]" twidget-str format t)))
          (error "Invalid value of :format, it should includes '[t]'!"))
      (setq twidget-beg (point))
      (setq twidget-end (+ twidget-beg twidget-len))
      (insert twidget-str))
    ;; add face properties
    (when value
      (add-face-text-property twidget-beg twidget-end
                              'twidget-choice-rest-face)
      (pcase value
        ((pred listp)
         (dolist (val value)
           (let* ((start (string-match val twidget-str))
                  (val-len (length val))
                  (val-beg (+ twidget-beg start))
                  (val-end (+ val-beg val-len)))
             (put-text-property val-beg val-end
                                'face 'twidget-choice-selected-face))))
        ((pred stringp)
         (save-excursion
           (goto-char twidget-beg)
           (when (search-forward value nil twidget-end)
             (put-text-property (match-beginning 0) (match-end 0)
                                'face 'twidget-choice-selected-face))))))
    ;; add keyhint
    (when (twidget-active-p id)
      (save-excursion
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
                                    letter)))))))))))

;; try to put it in :action parameter
;;;###autoload
(defun twidget-choice-select (choice)
  "Select the choice CHOICE."
  (interactive)
  (twidget--correct-cursor)
  (let* ((node (ewoc-locate twidget-ewoc))
         (data (ewoc-data node))
         (twidget (car data))
         (plst (cdr data))
         (bind (plist-get plst :bind))
         (multiple (plist-get plst :multiple))
         (require (plist-get plst :require))
         (value choice)
         plst)
    ;; when 'multiple choices' twidget
    (when multiple
      (if (member value twidget-value)
          (if require
              (if (= (length twidget-value) 1)
                  (setq value (list value))
                (setq value (seq-filter (lambda (el)
                                          (not (string= el value)))
                                        twidget-value)))
            (setq value (seq-filter (lambda (el)
                                      (not (string= el value)))
                                    twidget-value)))
        (setq value (append twidget-value (list value)))))
    (setq plst (plist-put (cdr data) :value value))
    (ewoc-set-data node (cons twidget plst))
    (ewoc-invalidate twidget-ewoc node)
    (setq twidget-value value)
    (set bind twidget-value)))

;; twidget ewoc

(defun twidget-ewoc-pp (data)
  "Pretty printer for twidget ewoc."
  (pcase data
    ((and (pred listp)
          (guard (member (car data) twidget-components)))
     (let* ((twidget (car data))
            (plst (cdr data)))
       (pcase twidget
         ('twidget-choice
          (let ((bind (plist-get plst :bind))
                (value (plist-get plst :value)))
            (apply #'twidget-choice plst)
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
         (multiple (plist-get data :multiple))
         (bind (plist-get data :bind))
         (choices (plist-get data :choices)))
    (dotimes (i (length choices))
      (define-key twidget-mode-map (kbd (number-to-string (1+ i)))
        (lambda ()
          (interactive)
          ;; FIXME: simply following code and `twidget-choice-select'.
          (if multiple
              (twidget-choice-select (nth i choices))
            (if require
                (twidget-choice-select (nth i choices))
              (if (string= (nth i choices) twidget-value)
                  (twidget-choice-select nil)
                (twidget-choice-select (nth i choices))))))))))

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
   `(twidget-choice
     :bind single-choice
     :value "daily"
     :multiple nil
     :require nil
     :format ,(concat (propertize "Repeat" 'face 'bold)
                      " [t] frequency.")
     :choices ,gtd-habit-regular-feq-type)
   ""
   `(twidget-choice
     :bind single-choice2
     :value ("Monday")
     :multiple t
     :require t
     :format ,(concat (propertize "Choose a week: " 'face 'bold)
                      "[t]")
     :choices ,gtd-habit-frequency-by-day)))

(provide 'twidget)
;;; twidget.el ends here
