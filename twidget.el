;;; twidget.el --- Text widget library in emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Kinney Zhang
;;
;; Version: 0.0.1
;; Keywords: widget convenience
;; Author: Kinney Zhang <kinneyzhang666@gmail.com>
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
  '((t :inherit error :italic t))
  "Face for twidget key hint.")

(defvar twidget-ewoc nil
  "EWOC for twidget.")

(defvar twidget-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>") #'twidget-jump-forward)
    (define-key map (kbd "<backtab>") #'twidget-jump-backward)
    map)
  "Keymap for `twidget-mode'.")

(defvar twidget-choice-separator " "
  "Separator between each choices.")

(defvar twidget-components
  '(twidget-choice twidget-text)
  "A list of all twidget components.")

(defvar-local twidget-value nil
  "The value of current active twidget.")

(defvar-local twidget-active-id ""
  "The id of current active twidget.")

(defvar-local twidget-active-data nil
  "The ewoc data of current active twidget.")

(defvar-local twidget-data nil
  "The ewoc data of all twidgets.")

;; (defvar twidget-number-symbols
;;   '("⓪" "①" "②" "③" "④" "⑤" "⑥"
;;     "⑦" "⑧" "⑨" "⑩" "⑪" "⑫" "⑬"
;;     "⑭" "⑮" "⑯" "⑰" "⑱" "⑲" "⑳"))

(defvar twidget-margin 2
  "Left margin of twidget buffer.")

;;;; Functions

;;; helper functions

(defun twidget--plist->clist (plist)
  "Convert plist to clist."
  (if (null plist)'()
    (cons
     (cons (car plist) (cadr plist))
     (twidget--plist->clist (cddr plist)))))

(defun twidget--prop-value (prop1 value1 plst prop2)
  "Return the value of PROP2 in PLST with
PROP2 whose value is VALUE1."
  (plist-get
   (cdr (seq-find (lambda (item)
                    (equal (plist-get (cdr item) prop1) value1))
                  plst))
   prop2))

(defun twidget-active-p (twidget-id)
  "Judge if the twidget with id TWIDGET-ID is actived."
  (string= twidget-active-id twidget-id))

(defun twidget-ewoc-refresh (ewoc)
  "Add left margin to buffer when refresh ewoc."
  (ewoc-refresh ewoc)
  (set-window-margins (selected-window) 2))

(defun twidget-ewoc-invalidate (ewoc &rest nodes)
  "Add left margin to buffer when refresh nodes."
  (apply #'ewoc-invalidate ewoc nodes)
  (set-window-margins (selected-window) 2))

;;; twidget component functions

;; text twidget

(defun twidget-text (&rest args)
  "Printer function for 'text' twidget with arguments ARGS."
  (let* ((id (plist-get args :id))
         (bind (plist-get args :bind))
         (value (or (plist-get args :value) " "))
         (val-len (length value))
         (format (plist-get args :format))
         twidget-beg twidget-end)
    (if format
        (progn
          (setq twidget-beg (+ (point) (string-match "\\[t\\]" format)))
          (setq twidget-end (+ twidget-beg val-len))
          ;; leave one blank after each twidget
          ;; for sake of jumping back correctly
          (insert (replace-regexp-in-string "\\[t\\]" value format t) " "))
      (setq twidget-beg (point))
      (setq twidget-end (+ val-len (point)))
      ;; leave one blank after each twidget
      ;; for sake of jumping back correctly
      (insert value " "))
    ;; add face properties
    (with-silent-modifications
      (add-text-properties twidget-beg twidget-end
                           `(twidget-id ,id face
                                        (twidget-choice-selected-face
                                         :background "#eee"))))
    ;; add keyhint
    (when (twidget-active-p id)
      (let ((letter (buffer-substring-no-properties
                     twidget-beg (1+ twidget-beg))))
        (with-silent-modifications
          (add-text-properties
           twidget-beg twidget-end
           `(display ,(concat (propertize
                               "1"
                               ;; (nth 1 twidget-number-symbols)
                               'face 'twidget-key-hint-face
                               'display '((raise 0.2) (height 0.85)))
                              value))))))
    ;; add :pos property to `twidget-data'
    (twidget--update-data (ewoc-locate twidget-ewoc twidget-beg)
                          :pos twidget-beg)))

(defun twidget-text-update-value (value)
  "Update current active text twidget."
  (twidget--correct-cursor)
  (let* ((node (ewoc-locate twidget-ewoc))
         (twidget (car twidget-active-data))
         (plst (cdr twidget-active-data))
         (bind (plist-get plst :bind))
         (beg (point)))
    (setq plst (plist-put plst :value value))
    (ewoc-set-data node (cons twidget plst))
    ;; (twidget-ewoc-invalidate twidget-ewoc node)
    (twidget-ewoc-refresh twidget-ewoc)
    (setq twidget-value value)
    (set bind twidget-value)
    (goto-char beg)))

;; choice twidget

(defun twidget-choice (&rest args)
  "Printer function for 'choice' twidget with arguments ARGS."
  (let* ((id (plist-get args :id))
         (value (plist-get args :value))
         (separator (plist-get args :separator))
         (require (plist-get args :require))
         (choices (plist-get args :choices))
         (format (plist-get args :format))
         (twidget-str (string-join choices (or separator
                                               twidget-choice-separator)))
         (twidget-str (propertize twidget-str 'twidget-id id))
         (twidget-len (length twidget-str))
         line-beg twidget-beg twidget-end)
    ;; insert the whole twidget-str
    (if format
        (if-let ((start (string-match "\\[t\\]" format)))
            (progn
              (setq twidget-beg (+ (point) start))
              (setq twidget-end (+ twidget-beg twidget-len))
              ;; leave one blank after each twidget
              ;; for sake of jumping back correctly
              (insert (replace-regexp-in-string "\\[t\\]" twidget-str format t) " "))
          (error "Invalid value of :format, it should includes '[t]'!"))
      (setq twidget-beg (point))
      (setq twidget-end (+ twidget-beg twidget-len))
      ;; leave one blank after each twidget
      ;; for sake of jumping back correctly
      (insert twidget-str " "))
    ;; add face properties
    (when value
      (with-silent-modifications
        (add-face-text-property twidget-beg twidget-end
                                'twidget-choice-rest-face))
      (pcase value
        ((pred listp)
         (dolist (val value)
           (let* ((start (string-match val twidget-str))
                  (val-len (length val))
                  (val-beg (+ twidget-beg start))
                  (val-end (+ val-beg val-len)))
             (with-silent-modifications
               (put-text-property val-beg val-end
                                  'face 'twidget-choice-selected-face)))))
        ((pred stringp)
         (save-excursion
           (goto-char twidget-beg)
           (when (search-forward value nil twidget-end)
             (with-silent-modifications
               (put-text-property (match-beginning 0) (match-end 0)
                                  'face 'twidget-choice-selected-face)))))))
    ;; add keyhint
    (when (twidget-active-p id)
      (save-excursion
        (goto-char twidget-beg)
        (dotimes (i (length choices))
          (let ((choice (nth i choices)))
            (when (search-forward choice nil t)
              (let* ((beg (match-beginning 0))
                     (letter (buffer-substring-no-properties beg (1+ beg))))
                (with-silent-modifications
                  (add-text-properties
                   beg (1+ beg)
                   `(display ,(concat (propertize
                                       (number-to-string (1+ i))
                                       ;; (nth (1+ i) twidget-number-symbols)
                                       'face 'twidget-key-hint-face
                                       'display '((raise 0.2) (height 0.85)))
                                      letter))))))))))
    ;; add :pos property to `twidget-data'
    (twidget--update-data (ewoc-locate twidget-ewoc twidget-beg)
                          :pos twidget-beg)))

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
         (action (plist-get plst :action))
         (multiple (plist-get plst :multiple))
         (require (plist-get plst :require))
         (value choice)
         (beg (point)))
    ;; when 'multiple choices' twidget
    (when multiple
      (unless (listp twidget-value)
        (setq twidget-value (list twidget-value)))
      (if (member value twidget-value)
          (if require
              (if (= (length twidget-value) 1)
                  (progn
                    (setq value (list value))
                    (message "At least one choice is required!"))
                (setq value (seq-filter (lambda (el)
                                          (not (string= el value)))
                                        twidget-value)))
            (setq value (seq-filter (lambda (el)
                                      (not (string= el value)))
                                    twidget-value)))
        (setq value (append twidget-value (list value)))))
    (setq plst (plist-put plst :value value))
    (ewoc-set-data node (cons twidget plst))
    ;; (twidget-ewoc-invalidate twidget-ewoc node)
    (twidget-ewoc-refresh twidget-ewoc)
    (setq twidget-value value)
    ;; (when action
    ;;   (funcall action twidget-value node))
    (set bind twidget-value)
    (goto-char beg)))

;;; twidget ewoc

(defun twidget-ewoc-pp (data)
  "Pretty printer for twidget ewoc."
  ;; (message "data:%S" data)
  ;; (cond
  ;;  ((member (car data) twidget-components)
  ;;   (let ((twidget (car data))
  ;;         (plst (cdr data)))
  ;;     (pcase twidget
  ;;       ('twidget-text
  ;;        (let* ((bind (plist-get plst :bind))
  ;;               (value (plist-get plst :value)))
  ;;          (apply #'twidget-text plst)
  ;;          (set bind value)))
  ;;       ('twidget-choice
  ;;        (let ((bind (plist-get plst :bind))
  ;;              (value (plist-get plst :value)))
  ;;          ;; (message "pos:%s" (point))
  ;;          (apply #'twidget-choice plst)
  ;;          (set bind value))))))
  ;;  ((stringp (car data))
  ;;   (apply #'insert data)))
  (pcase data
    ((guard (member (car data) twidget-components))
     (let ((twidget (car data))
           (plst (cdr data)))
       (pcase twidget
         ('twidget-text
          (let* ((bind (plist-get plst :bind))
                 (value (plist-get plst :value)))
            (apply #'twidget-text plst)
            (set bind value)))
         ('twidget-choice
          (let ((bind (plist-get plst :bind))
                (value (plist-get plst :value)))
            (apply #'twidget-choice plst)
            (set bind value)))
         (_ (apply #'insert data)))))
    ((guard (stringp (car data)))
     (apply #'insert data))))

(defun twidget--before-setup ()
  "Some buffer settings for twidget."
  (let* ((info "Press <tab> to jump to next twidget,\
 <shift-tab> to jump backward, press the number key to do selection.")
         (ewoc (ewoc-create
                'twidget-ewoc-pp
                (propertize
                 (concat info "\n\n") 'face '(shadow italic))
                nil t)))
    (kill-all-local-variables)
    (remove-overlays)
    (buffer-disable-undo)
    (read-only-mode -1)
    (setq cursor-type t)
    (set (make-local-variable 'twidget-ewoc) ewoc)))

(defun twidget--update-twidget-data ()
  (ewoc-collect
   twidget-ewoc
   (lambda (data)
     (and (listp data)
          (member (car data) twidget-components)))))

(defun twidget--after-setup ()
  "Setting keyhint of the first twidget, binding keys and so on."
  (goto-char (point-min))
  (when-let* ((match (text-property-search-forward 'twidget-id))
              (beg (prop-match-beginning match))
              (id (prop-match-value match)))
    (goto-char beg)
    (let* ((node (ewoc-locate twidget-ewoc beg))
           (data (ewoc-data node))
           (value (plist-get (cdr data) :value)))
      (setq twidget-active-id id)
      (setq twidget-value value)
      (setq twidget-active-data data)
      ;; save all twidget data in `twidget-data' variable.
      (setq twidget-data (twidget--update-twidget-data))
      (twidget--bind-key)
      ;; (twidget-ewoc-invalidate twidget-ewoc node)
      (twidget-ewoc-refresh twidget-ewoc)
      (twidget-mode 1)
      (read-only-mode 1)
      (goto-char beg))))

(defun twidget--bind-key ()
  "Bind the key of twidget."
  (let ((twidget (car twidget-active-data))
        (plst (cdr twidget-active-data)))
    (pcase twidget
      ('twidget-text
       (define-key twidget-mode-map (kbd "1")
         (lambda ()
           (interactive)
           (setq twidget-value (read-from-minibuffer
                                "Input the value: " twidget-value))
           (twidget-text-update-value twidget-value))))
      ('twidget-choice
       (let ((require (plist-get plst :require))
             (multiple (plist-get plst :multiple))
             (choices (plist-get plst :choices)))
         (dotimes (i (length choices))
           (define-key twidget-mode-map (kbd (number-to-string (1+ i)))
             (lambda ()
               (interactive)
               ;; FIXME: simply following code and `twidget-chosice-select'.
               (if multiple
                   (twidget-choice-select (nth i choices))
                 (if require
                     (if (string= (nth i choices) twidget-value)
                         (message "At least one choice is required!")
                       (twidget-choice-select (nth i choices)))
                   (if (string= (nth i choices) twidget-value)
                       (twidget-choice-select nil)
                     (twidget-choice-select (nth i choices)))))))))))))

(defun twidget--unbind-key (data)
  "Unbind the key of twidget with data DATA."
  (let* ((choices (plist-get (cdr data) :choices)))
    (dotimes (i (length choices))
      (unbind-key (kbd (number-to-string (1+ i))) twidget-mode-map))))

(defun twidget--correct-cursor ()
  "Move the cursor point to the beginning of active twidget."
  (unless (twidget-active-p (get-text-property (point) 'twidget-id))
    (goto-char (point-max))
    ;; FIXME: cannot search twidget-id properly
    ;; when the the widget has one letter only.
    ;; Maybe it's a bug of text-properties code?
    (when-let* ((match (text-property-search-backward
                        'twidget-id twidget-active-id t))
                (beg (prop-match-beginning match)))
      (goto-char beg))))

(defun twidget--next-twidget ()
  "Jump to the beginning of next twidget, 
return a cons cell of position and twidget id."
  (twidget--correct-cursor)
  
  ;; (let* ((pos (next-single-property-change (point) 'twidget-id))
  ;;        (pos (when pos (next-single-property-change pos 'twidget-id))))
  ;;   (when pos (goto-char pos))
  ;;   (cons (get-text-property (point) 'twidget-id) (point)))
  
  (when-let* ((match (text-property-search-forward 'twidget-id nil nil t))
              ;; because the cursor is at the beginning of prop match,
              ;; it should skip current match when search forward.
              (beg (prop-match-beginning match))
              (id (prop-match-value match)))
    (goto-char beg)
    (cons id beg)))

(defun twidget--previous-twidget ()
  "Jump to the beginning of previous twidget,
return a cons cell of position and twidget id."
  (twidget--correct-cursor)
  ;; problem of the following method:
  ;; cannot distinguish different twidget-id when twidget is not continuous.
  
  ;; (when-let* ((match (text-property-search-backward 'twidget-id))
  ;;             ;; because the cursor is at the beginning of prop match,
  ;;             ;; it should not skip next match when search backward.
  ;;             (beg (prop-match-beginning match))
  ;;             (id (prop-match-value match)))
  ;;   (goto-char beg)
  ;;   (cons id beg))
  
  ;; problem of the following method:
  ;; cannot distinguish different twidget-id when twidget is continuous.
  ;; so I choose to add one space between each twidget.
  
  (let* ((pos (previous-single-property-change (point) 'twidget-id))
         (pos (when pos (previous-single-property-change pos 'twidget-id))))
    (when pos (goto-char pos))
    (cons (get-text-property (point) 'twidget-id) (point))))

;;;###autoload
(defun twidget-jump-forward ()
  "Jump to the next twidget and active it."
  (interactive)
  (when-let* ((id-beg (twidget--next-twidget))
              (id (car id-beg))
              (beg (cdr id-beg))
              (data (ewoc-data (ewoc-locate twidget-ewoc))))
    ;; unbind the key of last active twidget
    (twidget--unbind-key twidget-active-data)
    (setq twidget-value (plist-get (cdr data) :value))
    (setq twidget-active-id id)
    (setq twidget-active-data data)
    (setq twidget-data (twidget--update-twidget-data))
    (twidget-ewoc-refresh twidget-ewoc)
    (twidget--bind-key)
    (goto-char beg)))

;;;###autoload
(defun twidget-jump-backward ()
  "Jump to the previous twidget and active it."
  (interactive)
  (when-let* ((id-beg (twidget--previous-twidget))
              (id (car id-beg))
              (beg (cdr id-beg))
              (data (ewoc-data (ewoc-locate twidget-ewoc))))
    (twidget--unbind-key twidget-active-data)
    (setq twidget-value (plist-get (cdr data) :value))
    (setq twidget-active-id id)
    (setq twidget-active-data data)
    (setq twidget-data (twidget--update-twidget-data))
    (twidget-ewoc-refresh twidget-ewoc)
    (twidget--bind-key)
    (goto-char beg)))

;;;###autoload
(defun twidget-create (&rest args)
  "Insert a twidget at point. The car of twidget is the
twidget name and the rests are a series of attributes.

Defaultly, insert a blank after twidget is inserted, for
sake of jumping backward twidgets correctly."
  (when (member (car args) twidget-components)
    (ewoc-enter-last twidget-ewoc (append args `(:id ,(org-id-uuid))))))

;;;###autoload
(defun twidget-insert (&rest args)
  "Insert STRING at point."
  ;; (message "args:%S" args)
  (ewoc-enter-last twidget-ewoc args))

;;;###autoload
(define-minor-mode twidget-mode
  "Minor mode for text widget."
  nil nil nil)

;; (defun twidget-update-node (node prop value)
;;   "Update the PROP of the ewoc NODE to VALUE."
;;   (let* ((data (ewoc-data node))
;;          (twidget (car data))
;;          (plst (plist-put (cdr data) prop value)))
;;     (cons twidget plst)))

(defun twidget--single-node (bind)
  "Return the ewoc node of twidget binded with variable BIND."
  ;; (message "pos:%s" (twidget--prop-value :bind bind twidget-data :pos))
  (ewoc-locate twidget-ewoc
               (twidget--prop-value :bind bind twidget-data :pos)))

;; (defun twidget--multiple-nodes (binds)
;;   "Return a list of ewoc nodes of twidget binded with variable list BINDS."
;;   (mapcar #'twidget--single-node binds))

(defun twidget--update-data (node prop value)
  "Update the value of PROP property in NODE to VALUE."
  (let* ((data (ewoc-data node))
         (twidget (car data))
         (plst (plist-put (cdr data) prop value)))
    (message "twidget:%S" twidget)
    (ewoc-set-data node (cons twidget plst))))

(defun twidget--update-twidget (node properties)
  "Update the ewoc node NODE with twidget.
PROPERTIES is a sequence of PROPERTY VALUE pairs."
  (let* ((data (ewoc-data node))
         (twidget (car data))
         (plst (cdr data))
         (alist (twidget--plist->clist properties)))
    (dolist (item alist)
      (setq plst (plist-put plst (car item) (cdr item))))
    (ewoc-set-data node (cons twidget plst))
    ;; (ewoc-invalidate twidget-ewoc node) ;; invalid
    (ewoc-refresh twidget-ewoc)))

(defun twidget-update (bind &rest properties)
  "Update the properties of twidget binded with variable BIND.
Remaining arguments form a sequence of PROPERTY VALUE pairs for text
properties to update on the node."
  (twidget--update-twidget (twidget--single-node bind) properties))

(defun twidget-update-multiple (&rest bind-properties)
  (let ((clist (twidget--plist->clist bind-properties)))
    (dolist (item clist)
      (apply #'twidget-update (car item) (cdr item)))))

;; (twidget-update-multiple
;;  'gtd-habit-freq-type '(:value "weekly" :separator nil)
;;  'gtd-habit-freq-arg1 '(:format "Every [t] day" :value "520")
;;  'gtd-habit-freq-arg2 '(:format "Next date: [t]")
;;  'gtd-habit-freq-arg3 '(:separator " | " :value "on date"))

;; (defun twidget-update2 (value &optional node)
;;   (if (> (string-to-number value) 1)
;;       (twidget-update-twidget
;;        node :format "Every [t] days")
;;     (twidget-update-twidget
;;      node :format "Every [t] day")))

;;;; test example

(defvar-local gtd-habit-freq-type nil)
(defvar-local gtd-habit-freq-arg1 nil)
(defvar-local gtd-habit-freq-arg2 nil)
(defvar-local gtd-habit-freq-arg3 nil)
(defvar-local gtd-habit-freq-arg4 nil)
(defvar gtd-habit-time-range '("day" "week" "month" "year"))
(defvar gtd-habit-end-type '("never" "after" "on date"))

(defun gtd-habit-repeat-after-completion-twidgets ()
  (twidget-create 'twidget-text
                  :bind 'gtd-habit-freq-arg1 :value "1")
  (twidget-create 'twidget-choice
                  :bind 'gtd-habit-freq-arg2
                  :choices gtd-habit-time-range
                  :format "[t] after previous item is checked off."
                  :value "week"
                  :separator "/"
                  :require t))

(defun gtd-habit-repeat-daily-twidgets ()
  (twidget-create 'twidget-text
                  :bind 'gtd-habit-freq-arg1
                  :format "Every [t] days"
                  :value "11")
  (twidget-insert "\n\n")
  (twidget-create 'twidget-text
                  :bind 'gtd-habit-freq-arg2
                  :format "Next: [t]"
                  :value "2021/ 4/21")
  (twidget-insert "\n")
  (twidget-create 'twidget-choice
                  :bind 'gtd-habit-freq-arg3
                  :choices gtd-habit-end-type
                  :format "Ends: [t]"
                  :value "never"
                  :separator "/"
                  :require t))

(progn
  ;; (display-buffer-in-side-window (get-buffer-create "*twidget test*") nil)
  (pop-to-buffer (get-buffer-create "*twidget test*"))
  ;; (select-window (get-buffer-window "*twidget test*"))
  (let ((inhibit-read-only t)
        node1 node2 node3 node4)
    (erase-buffer))
  ;;; Next Todo:
  ;; write a macro to include `twidget--before-setup' and `twidget--after-setup' codes.
  ;; make the unselected choices invisiable.
  ;; ewoc action
  (twidget--before-setup)
  (twidget-create 'twidget-choice
                  :bind 'gtd-habit-freq-type
                  :choices gtd-habit-regular-feq-type
                  :format "Repeat [t]"
                  :value "after-completion"
                  :separator "/"
                  :require t)
  (twidget-insert "\n\n")
  (gtd-habit-repeat-daily-twidgets)
  (twidget--after-setup))

(provide 'twidget)
;;; twidget.el ends here

;; some error in ewoc-invalidate
