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

(require 'ov)
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

(defface twidget-button-face
  '((t :box (:line-width -2 :color "#444")
       :background "#444" :foreground "#fff"))
  "Face for button twidget.")

(defvar twidget-ewoc nil
  "EWOC for twidget.")

;; (defvar-local twidget-mode-map
;;   "Keymap for `twidget-mode'.")

(defvar twidget-mode-map nil)

(defvar twidget-choice-separator " "
  "Separator between each choices.")

(defvar-local twidget-num 0)

(defvar twidget-group-vars nil
  "The list of variables binded to all groups.")

(defvar twidget-components
  '(twidget-text twidget-choice twidget-button)
  "A list of all twidget components.")

(defvar-local twidget-start-pos nil
  "The start position of all twidget contents in buffer.")

(defvar-local twidget-value nil
  "The value of current active twidget.")

(defvar-local twidget-active-id nil
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

(defvar-local twidget-overlays nil
  "List of overlays in twidget buffer.")

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

(defun twidget--update-twidget-data ()
  "Return the ewoc data of all twidget nodes."
  (ewoc-collect
   twidget-ewoc
   (lambda (data)
     (and (listp data)
          (member (car data) twidget-components)))))

(defun twidget--ids ()
  "Return a list of all twidget ids."
  (mapcar (lambda (data)
            (plist-get (cdr data) :id))
          twidget-data))

;; (defun twidget--ov-in-correction (ovs)
;;   "Return the correct overlay when using `ov-in'."
;;   ;; FIXME: When using ov-in, it will find an 'beg equals to end' overlay
;;   ;; in the beginning. This function is used top ignore the incorrect overlay.
;;   ;; But what's the reason? The reason is: you have to delete overlay manually
;;   ;; when invalidate or delete a ewoc node!
;;   (seq-find
;;    (lambda (ov)
;;      (not (= (ov-beg ov) (ov-end ov))))
;;    ovs))

(defun twidget-active-p (twidget-id)
  "Judge if the twidget with id TWIDGET-ID is actived."
  (when (stringp twidget-id)
    (string= twidget-active-id twidget-id)))

(defun twidget-ewoc-refresh (ewoc)
  "Add left margin to buffer when refresh ewoc."
  ;; (mapcar #'delete-overlay twidget-overlays)
  (ov-reset twidget-overlays)
  (setq twidget-overlays nil)
  (ewoc-refresh ewoc)
  (set-window-margins (selected-window) 2))

(defun twidget-ewoc-invalidate (ewoc &rest nodes)
  "Add left margin to buffer when refresh nodes."
  (let ((ids (mapcar (lambda (node)
                       (plist-get (cdr (ewoc-data node)) :id))
                     nodes)))
    (dolist (id ids)
      (ov-clear 'twidget-id id))
    (apply #'ewoc-invalidate ewoc nodes)
    (setq twidget-overlays (ov-in 'twidget-id))
    (set-window-margins (selected-window) 2)))

(defun twidget-overlay (beg end &rest overlays)
  "Make overlay on BEG and END in buffer with PROPERTIES.
Remaining arguments form a sequence of NAME VALUE pairs."
  ;; When refresh an ewoc node, it will delete the node and insert again.
  ;; So, the front-advance argument in ov-make should be non-nil, for sake
  ;; of not being including in the following overlay!
  (let ((ol (ov-make beg end (current-buffer) t)))
    (while overlays
      (ov-set ol (pop overlays) (pop overlays))
      (push ol twidget-overlays))))

(defun twidget--add-keyhint-face (beg keyhint letter)
  "Add faces to KEYHINT between BEG and BEG+1, display the keyhint 
by adding a 'display' property to the first LETTER of twidget."
  (with-silent-modifications
    (add-text-properties
     beg (1+ beg)
     `(display ,(concat (propertize keyhint
                                    'face 'twidget-key-hint-face
                                    'display '((raise 0.2) (height 0.85)))
                        letter)))))


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
      (setq twidget-end (+ val-len twidget-beg))
      ;; leave one blank after each twidget
      ;; for sake of jumping back correctly
      (insert value " "))
    ;; add twidget-id overlay
    (twidget-overlay twidget-beg twidget-end 'twidget-id id)
    ;; add face properties
    (with-silent-modifications
      (add-text-properties twidget-beg twidget-end
                           `(face (twidget-choice-selected-face
                                   :background "#eee"))))
    ;; add keyhint
    (when (twidget-active-p id)
      (let ((letter (buffer-substring-no-properties
                     twidget-beg (1+ twidget-beg))))
        (twidget--add-keyhint-face twidget-beg "1" letter)))))

(defun twidget-text-update-value (value)
  "Update current active text twidget."
  (twidget--correct-cursor)
  (let* ((node (ewoc-locate twidget-ewoc))
         (twidget (car twidget-active-data))
         (plst (cdr twidget-active-data))
         (bind (plist-get plst :bind))
         (action (plist-get plst :action))
         (beg (point)))
    (setq plst (plist-put plst :value value))
    (ewoc-set-data node (cons twidget plst))
    (twidget-ewoc-invalidate twidget-ewoc node)
    (setq twidget-value value)
    (set bind twidget-value)
    (when action
      (funcall action twidget-value))
    (setq twidget-data (twidget--update-twidget-data))
    (goto-char beg)))

;; choice twidget

(defun twidget-choice (&rest args)
  "Printer function for 'choice' twidget with arguments ARGS."
  (let* ((id (plist-get args :id))
         (value (plist-get args :value))
         (separator (plist-get args :separator))
         (require (plist-get args :require))
         (fold (plist-get args :fold))
         (choices (plist-get args :choices))
         (format (plist-get args :format))
         (twidget-str (string-join choices (or separator
                                               twidget-choice-separator)))
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
    ;; add twidget-id overlay
    (twidget-overlay twidget-beg twidget-end 'twidget-id id)
    ;; add face properties
    (when value
      (pcase value
        ((pred listp)
         (with-silent-modifications
           (add-face-text-property twidget-beg twidget-end
                                   'twidget-choice-rest-face)
           (when fold
             (unless (twidget-active-p id)
               (add-text-properties twidget-beg twidget-end
                                    '(invisible t)))))
         (dolist (val value)
           (let* ((start (string-match val twidget-str))
                  (val-len (length val))
                  (val-beg (+ twidget-beg start))
                  (val-end (+ val-beg val-len)))
             (with-silent-modifications
               (put-text-property val-beg val-end
                                  'face 'twidget-choice-selected-face)
               (when fold
                 (unless (twidget-active-p id)
                   (put-text-property val-beg (+ val-end (length separator))
                                      'invisible nil)))))))
        ((pred stringp)
         (save-excursion
           (goto-char twidget-beg)
           (when (search-forward value nil twidget-end)
             (with-silent-modifications
               (add-face-text-property twidget-beg twidget-end
                                       'twidget-choice-rest-face)
               (put-text-property (match-beginning 0) (match-end 0)
                                  'face 'twidget-choice-selected-face)
               (when fold
                 (unless (twidget-active-p id)
                   (add-text-properties twidget-beg twidget-end
                                        '(invisible t))
                   (put-text-property (match-beginning 0) (match-end 0)
                                      'invisible nil)))))))))
    ;; add keyhint
    (when (twidget-active-p id)
      (save-excursion
        (goto-char twidget-beg)
        (dotimes (i (length choices))
          (let ((choice (nth i choices)))
            (when (search-forward choice nil t)
              (let* ((beg (match-beginning 0))
                     (letter (buffer-substring-no-properties beg (1+ beg))))
                (twidget--add-keyhint-face beg (number-to-string (1+ i)) letter)))))))))

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
    (twidget-ewoc-invalidate twidget-ewoc node)
    (setq twidget-value value)
    (set bind twidget-value)
    (when action
      (funcall action twidget-value))
    (setq twidget-data (twidget--update-twidget-data))
    (goto-char beg)))

(defun twidget-plain (&rest args)
  "Printer function for plain text."
  (let ((id (car (last args)))
        (texts (seq-subseq args 0 -2))
        twidget-beg twidget-end)
    ;; FIXME: find a better function to deal with.
    (setq twidget-beg (point))
    (apply #'insert texts)
    (setq twidget-end (point))
    (twidget-overlay twidget-beg twidget-end 'twidget-id id)))

(defun twidget-button (&rest args)
  "Printer function for 'button' twidget with arguments ARGS."
  (let ((id (plist-get args :id))
        (value (plist-get args :value))
        (action (plist-get args :action))
        (help-echo (plist-get args :help-echo))
        twidget-beg)
    (setq twidget-beg (point))
    (insert-button value
                   'face 'twidget-button-face
                   'follow-link t
                   'action action
                   'help-echo help-echo
                   'twidget-id id)
    (insert " ")
    (when (twidget-active-p id)
      (let ((letter (buffer-substring-no-properties
                     twidget-beg (1+ twidget-beg))))
        (twidget--add-keyhint-face twidget-beg "1" letter)))))

;;;###autoload
(defun twidget-button-push ()
  "Push the twidget button."
  (interactive)
  (let ((beg (ov-beg (car (ov-in 'twidget-id twidget-active-id)))))
    (push-button beg)
    (goto-char beg)))



;;; twidget ewoc

(defun twidget-ewoc-pp (data)
  "Pretty printer for twidget ewoc."
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
         ('twidget-button
          (apply #'twidget-button plst)))))
    ((guard (stringp (car data)))
     (apply #'twidget-plain data))))

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

(defun twidget--after-setup ()
  "Setting keyhint of the first twidget, binding keys and so on."
  (when-let* ((ol (car (last twidget-overlays)))
              (beg (ov-beg ol))
              (id (ov-val ol 'twidget-id))
              (data (progn (goto-char beg)
                           (ewoc-data (ewoc-locate twidget-ewoc))))
              (value (progn (goto-char beg)
                            (plist-get (cdr data) :value))))
    (set-window-margins (selected-window) 2)
    ;; save all twidget data in `twidget-data' variable.
    (setq twidget-value value)
    (setq twidget-active-id id)
    (setq twidget-active-data data)
    (setq twidget-data (twidget--update-twidget-data))
    (twidget-ewoc-refresh twidget-ewoc)
    (twidget-mode 1)
    (twidget--bind-key)
    (read-only-mode 1)
    (goto-char beg)))

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
                     (twidget-choice-select (nth i choices))))))))))
      ('twidget-button
       (define-key twidget-mode-map (kbd "1")
         (lambda () (interactive) (twidget-button-push)))))))

(defun twidget--unbind-key (data)
  "Unbind the key of twidget with data DATA."
  (let* ((choices (plist-get (cdr data) :choices)))
    (dotimes (i (length choices))
      (unbind-key (kbd (number-to-string (1+ i))) twidget-mode-map))))

(defun twidget--correct-cursor ()
  "Move the cursor point to the beginning of active twidget."
  (let* ((ol (ov-at))
         (active-p (when ol
                     (twidget-active-p (ov-val ol 'twidget-id)))))
    (unless active-p
      (when-let ((ov (car (ov-in 'twidget-id twidget-active-id))))
        ;; FIXME: 107~107 error overlay
        (goto-char (ov-beg ov))))))

(defun twidget--next-twidget ()
  "Jump to the beginning of next twidget, 
return a cons cell of position and twidget id."
  (twidget--correct-cursor)
  (let* ((ids (twidget--ids)))
    (when-let* ((len (length ids))
                (ol (ov-at))
                (id (ov-val ol 'twidget-id))
                (nth (seq-position ids id))
                (next-nth (% (1+ nth) len))
                (next-id (nth next-nth ids))
                (next-ol (car (ov-in 'twidget-id next-id)))
                (next-beg (ov-beg next-ol)))
      (cons next-id next-beg))))

(defun twidget--previous-twidget ()
  "Jump to the beginning of previous twidget,
return a cons cell of position and twidget id."
  (twidget--correct-cursor)
  (let ((ids (twidget--ids)))
    (when-let* ((len (length ids))
                (ol (ov-at))
                (id (ov-val ol 'twidget-id))
                (nth (seq-position ids id))
                (previous-nth (% (+ len (1- nth)) len))
                (previous-id (nth previous-nth ids))
                (previous-ol (car (ov-in 'twidget-id previous-id)))
                (previous-beg (ov-beg previous-ol)))
      (cons previous-id previous-beg))))

;;;###autoload
(defun twidget-jump-forward ()
  "Jump to the next twidget and active it."
  (interactive)
  (when-let* ((id-beg (twidget--next-twidget))
              (id (car id-beg))
              (beg (cdr id-beg))
              (data (save-excursion
                      (goto-char beg)
                      (ewoc-data (ewoc-locate twidget-ewoc)))))
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
              (data (save-excursion
                      (goto-char beg)
                      (ewoc-data (ewoc-locate twidget-ewoc)))))
    (twidget--unbind-key twidget-active-data)
    (setq twidget-value (plist-get (cdr data) :value))
    (setq twidget-active-id id)
    (setq twidget-active-data data)
    (setq twidget-data (twidget--update-twidget-data))
    (twidget-ewoc-refresh twidget-ewoc)
    (twidget--bind-key)
    (goto-char beg)))



;; useful commands

(defun twidget--create (&rest args)
  "Insert a twidget at point. The car of twidget is the
twidget name and the rests are a series of attributes.

Defaultly, insert a blank after twidget is inserted, for
sake of jumping backward twidgets correctly."
  (when (member (car args) twidget-components)
    (ewoc-enter-last twidget-ewoc args)))

(defun twidget--insert (&rest args)
  "Insert STRING at point."
  (ewoc-enter-last twidget-ewoc args))

;;;###autoload
(defmacro twidget-create (&rest args)
  (declare (indent defun))
  (let ((id (org-id-uuid)))
    `(twidget--create
      ,@args :id ,id)))

;;;###autoload
(defmacro twidget-insert (&rest args)
  (let ((id (org-id-uuid)))
    `(twidget--insert
      ,@args :id ,id)))

;;;###autoload
(defmacro twidget-group (bind &rest body)
  ;; List all binded variable of twidgets in body to BIND, leave body.
  (declare (indent defun))
  (let* (expanded-body
         (binds (mapcar
                 (lambda (code)
                   (let ((expanded-code (macroexpand code)))
                     ;; should only expand once for sake of the unique id.
                     (push expanded-code expanded-body)
                     (car (last expanded-code))))
                 body))
         (body (reverse expanded-body)))
    `(progn
       (set (make-local-variable ,bind) (list ,@binds))
       ,@body)))

;; update twidget node.

(defun twidget--node (id-or-var)
  "Return the ewoc node of twidget with twidget-id ID."
  (let (id)
    (pcase id-or-var
      ((and (pred stringp)
            (pred org-uuidgen-p))
       (setq id id-or-var))
      ((pred symbolp)
       (setq id (twidget--prop-value :bind id-or-var twidget-data :id))))
    (if-let ((ov (car (ov-in 'twidget-id id))))
        (ewoc-locate twidget-ewoc (ov-beg ov))
      (error "twidget with id %s searched failed!" id))))

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
    (twidget-ewoc-invalidate twidget-ewoc node)))

;;;###autoload
(defun twidget-update (bind &rest properties)
  "Update the properties of twidget binded with variable BIND.
Remaining arguments form a sequence of PROPERTY VALUE pairs for text
properties to update on the node."
  (twidget--update-twidget (twidget--node bind) properties))

;;;###autoload
(defun twidget-update-multiple (&rest bind-properties)
  (let ((clist (twidget--plist->clist bind-properties)))
    (dolist (item clist)
      (apply #'twidget-update (car item) (cdr item)))))

;;;###autoload
(defun twidget-delete (id)
  "Delete the ewoc nodes with id ID."
  (let ((inhibit-read-only t)
        (node (twidget--node id)))
    ;; delete the overlay when deleting a node.
    (ov-clear 'twidget-id id)
    (setq twidget-overlays (ov-in 'twidget-id))
    (ewoc-delete twidget-ewoc node)))

;;;###autoload
(defun twidget-delete-group (var)
  "Delete the twidget-group binded with variable VAR."
  (let* ((inhibit-read-only t)
         (ids (symbol-value var))
         (nodes (mapcar #'twidget--node ids)))
    (dolist (id ids)
      (ov-clear 'twidget-id id))
    (setq twidget-overlays (ov-in 'twidget-id))
    (apply #'ewoc-delete twidget-ewoc nodes)))

;;;###autoload
(define-minor-mode twidget-mode
  "Minor mode for text widget."
  nil nil
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>") #'twidget-jump-forward)
    (define-key map (kbd "<backtab>") #'twidget-jump-backward)
    map))


;;;; test example

;; (twidget-update-multiple
;;  'gtd-habit-freq-type '(:value "weekly" :separator nil)
;;  'gtd-habit-freq-arg1 '(:format "Every [t] day" :value "520")
;;  'gtd-habit-freq-arg2 '(:format "Next date: [t]")
;;  'gtd-habit-end-type '(:separator " | " :value "on date"))


(defvar-local gtd-habit-freq-type nil)
(defvar-local gtd-habit-freq-arg1 nil)
(defvar-local gtd-habit-freq-arg2 nil)
(defvar-local gtd-habit-freq-arg3 nil)
(defvar-local gtd-habit-freq-arg4 nil)
(defvar-local gtd-habit-end-type nil)
(defvar gtd-habit-weekdays
  '("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))
(defvar gtd-habit-time-range '("day" "week" "month" "year"))
(defvar gtd-habit-end-types '("never" "after" "on date"))

;; twidget action functions

(defun gtd-habit-update-choices (value)
  (if (> (string-to-number value) 1)
      (twidget-update 'gtd-habit-freq-arg2
                      :choices
                      (mapcar (lambda (el)
                                (concat el "s"))
                              gtd-habit-time-range)
                      :value
                      (if (string= "s" (substring gtd-habit-freq-arg2 -1))
                          gtd-habit-freq-arg2
                        (concat gtd-habit-freq-arg2 "s")))
    (twidget-update 'gtd-habit-freq-arg2
                    :choices gtd-habit-time-range
                    :value
                    (if (string= "s" (substring gtd-habit-freq-arg2 -1))
                        (string-trim-right gtd-habit-freq-arg2 "s")
                      gtd-habit-freq-arg2))))

;; twidget groups

(defun gtd-habit-repeat-twidgets ()
  (twidget-group 'twidget-group1
    (twidget-create 'twidget-choice
      :bind 'gtd-habit-freq-type
      :choices gtd-habit-regular-feq-type
      :action #'gtd-habit-freq-type-switch
      :format "Repeat [t]"
      :value "after-completion"
      :separator "/"
      :require t)
    (twidget-insert "\n\n")))

(defun gtd-habit-repeat-after-completion-twidgets ()
  (twidget-group 'twidget-group2
    (twidget-create 'twidget-text
      :bind 'gtd-habit-freq-arg1
      :value "1"
      :action #'gtd-habit-update-choices)
    (twidget-create 'twidget-choice
      :bind 'gtd-habit-freq-arg2
      :choices gtd-habit-time-range
      :format "[t] after previous item is checked off."
      :value "week"
      :separator "/"
      :require t)))

(defun gtd-habit-repeat-daily-twidgets ()
  (twidget-group 'twidget-group2
    (twidget-create 'twidget-text
      :bind 'gtd-habit-freq-arg1
      :format "Every [t] day"
      :action (lambda (value)
                (if (> (string-to-number value) 1)
                    (twidget-update 'gtd-habit-freq-arg1
                                    :format "Every [t] days")
                  (twidget-update 'gtd-habit-freq-arg1
                                  :format "Every [t] day")))
      :value "1")
    (twidget-insert "\n\n")
    (twidget-create 'twidget-text
      :bind 'gtd-habit-freq-arg2
      :format "Next: [t]"
      :value "2021/ 4/21")))

(defun gtd-habit-repeat-weekly-twidgets ()
  (twidget-group 'twidget-group2
    (twidget-create 'twidget-text
      :bind 'gtd-habit-freq-arg1
      :format "Every [t] weeks"
      :value "1")
    (twidget-create 'twidget-choice
      :bind 'gtd-habit-freq-arg2
      :format "on [t]"
      :choices gtd-habit-weekdays
      :value "Monday"
      :separator "/"
      :fold t
      :multiple t
      :require t)
    (twidget-insert "\n\n")
    (twidget-create 'twidget-text
      :bind 'gtd-habit-freq-arg3
      :format "Next: [t]"
      :value "2021/ 4/21")))

(defun gtd-habit-repeat-monthly-twidgets ()
  (twidget-group 'twidget-group2
    (twidget-create 'twidget-text
      :bind 'gtd-habit-freq-arg1
      :format "Every [t] months"
      :value "1")
    
    (twidget-create 'twidget-text
      :bind 'gtd-habit-freq-arg2
      :format "\non the [t]st"
      :choices gtd-habit-weekdays
      :value "1")
    (twidget-create 'twidget-choice
      :bind 'gtd-habit-freq-arg3
      :choices (cons "day" gtd-habit-weekdays)
      :format "[t]" :value "day" :separator "/"
      :fold t :multiple t :require t)
    (twidget-create 'twidget-button
      :value "add"
      :help-echo "Add a twidget group."
      :action (lambda (btn)
                ))
    (twidget-create 'twidget-button
      :value "remove"
      :help-echo "Remove a twidget group"
      :action (lambda (btn)
                (message "remove a new twidget group")))
    (twidget-insert "\n")
    
    (twidget-insert "\n")
    (twidget-create 'twidget-text
      :bind 'gtd-habit-freq-arg4
      :format "Next: [t]"
      :value "2021/ 4/21")))

(defun gtd-habit-ends-widgets ()
  (twidget-group 'twidget-group3
    (twidget-insert "\n")
    (twidget-create 'twidget-choice
      :bind 'gtd-habit-end-type
      :choices gtd-habit-end-types
      :format "Ends: [t]"
      :value "never"
      :separator "/"
      :require t)))

;;; TODO: when ewoc-delete, delete the related overlays.
;;; update the value of twidget-data
;;; figure out the main action of of twidget, write in macros
;;; process overlay and twidget-data.

;; twidget-update, twidget-delete, 

(defun gtd-habit-freq-type-switch (value)
  (pcase gtd-habit-freq-type
    ("after-completion"
     (twidget-delete-group 'twidget-group2)
     (twidget-delete-group 'twidget-group3)
     (setq twidget-group3 nil)
     (gtd-habit-repeat-after-completion-twidgets))
    (_
     (twidget-delete-group 'twidget-group2)
     (when (bound-and-true-p twidget-group3)
       (twidget-delete-group 'twidget-group3))
     (pcase gtd-habit-freq-type
       ("daily" (gtd-habit-repeat-daily-twidgets))
       ("weekly" (gtd-habit-repeat-weekly-twidgets))
       ("monthly" (gtd-habit-repeat-monthly-twidgets))
       ("yearly" (gtd-habit-repeat-yearly-twidgets)))
     (gtd-habit-ends-widgets))))

(progn
  ;; (display-buffer-in-side-window (get-buffer-create "*twidget test*") nil)
  (pop-to-buffer (get-buffer-create "*twidget test*"))
  ;; (select-window (get-buffer-window "*twidget test*"))
  (let ((inhibit-read-only t))
    (erase-buffer))
  ;;; Next Todo:
  ;; write a macro to include `twidget--before-setup' and `twidget--after-setup' codes.
  ;; make the unselected choices invisiable.
  ;; ewoc action
  ;; value validation for twidget-text.
  (twidget--before-setup)
  (gtd-habit-repeat-twidgets)
  (gtd-habit-repeat-after-completion-twidgets)
  (twidget--after-setup))

(provide 'twidget)
;;; twidget.el ends here
