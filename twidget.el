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
  '((t :box (:line-width -3 :color "#888")
       :background "#888" :foreground "#fff"))
  "Face for button twidget.")

(defvar-local twidget-ewoc nil
  "EWOC for twidget.")

(defvar-local twidget-curr-groups nil)

(defvar twidget-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>") #'twidget-jump-forward)
    (define-key map (kbd "<backtab>") #'twidget-jump-backward)
    map))

(defvar twidget-choice-separator " "
  "Separator between each choices.")

(defvar twidget-widgets
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
          (member (car data) twidget-widgets)))))

(defun twidget--ids ()
  "Return a list of all twidget ids except the plain text twidget."
  (remove nil (mapcar (lambda (data)
                        (unless (and (eq (car data) 'twidget-text)
                                     (plist-get (cdr data) :plain))
                          (plist-get (cdr data) :id)))
                      twidget-data)))

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
  "Clear the unused overlays when using `ewoc-refresh'."
  (ov-reset twidget-overlays)
  (setq twidget-overlays nil)
  (ewoc-refresh ewoc))

(defun twidget-ewoc-invalidate (ewoc &rest nodes)
  "Clear the unused overlays when using `ewoc-invalidate'."
  (let ((ids (mapcar (lambda (node)
                       (plist-get (cdr (ewoc-data node)) :id))
                     nodes)))
    (dolist (id ids)
      (ov-clear 'twidget-id id))
    (apply #'ewoc-invalidate ewoc nodes)
    (setq twidget-overlays (ov-in 'twidget-id))))

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

(defun twidget--format-number-string (len num)
  "Format the number NUM according to LEN in twidget choices."
  (pcase len
    ((pred (> 10)) (number-to-string num))
    ((and (pred (<= 10)) (pred (> 100)))
     (if (< num 10)
         (format "0%s" num)
       (number-to-string num)))
    (_ (error "the length of choices is too large!"))))


;;; twidget component functions

;; text twidget

;; Ideas:
;; macro or function to customize a twidget.
;; basic twidgets and inherited twidgets.

(defun twidget-text (&rest args)
  "Printer function for 'text' twidget with arguments ARGS."
  (let* ((id (plist-get args :id))
         (bind (plist-get args :bind))
         (value (or (plist-get args :value) " "))
         (val-len (length value))
         (format (plist-get args :format))
         (plain (plist-get args :plain))
         twidget-beg twidget-end)
    (pcase value
      ((pred listp)
       (let ((len (length value))
             beg end alst)
         (if format
             (progn
               (dotimes (i len)
                 (setq beg (+ (point) (string-match (format "\\[t%s\\]" i) format)))
                 (setq end (+ beg (length (nth i value))))
                 (push (cons beg end) alst)
                 (pcase i
                   (0 (setq twidget-beg beg))
                   ((pred (= (1- len)))
                    (setq twidget-end end)))
                 (setq format (replace-regexp-in-string (format "\\[t%s\\]" i)
                                                        (nth i value) format t)))
               (insert format " "))
           (setq twidget-beg (point))
           (dotimes (i len)
             (setq beg (point))
             (insert (nth i value) " ")
             (setq end (point))
             (push (cons beg end) alst)
             (pcase i
               (0 (setq twidget-beg beg))
               ((pred (= (1- len)))
                (setq twidget-end end)))))
         (setq alst (reverse alst))
         (dotimes (i len)
           (setq beg (car (nth i alst)))
           (setq end (cdr (nth i alst)))
           (unless plain
             ;; add face properties
             (with-silent-modifications
               (add-text-properties beg end '(face (twidget-choice-selected-face
                                                    :background "#eee"))))
             ;; add keyhint
             (when (twidget-active-p id)
               (let ((letter (buffer-substring-no-properties beg (1+ beg))))
                 (twidget--add-keyhint-face beg (format "%s" (1+ i)) letter)))))))
      (_
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
       (unless plain
         ;; add face properties
         (with-silent-modifications
           (add-text-properties twidget-beg twidget-end
                                '(face (twidget-choice-selected-face
                                        :background "#eee"))))
         ;; add keyhint
         (when (twidget-active-p id)
           (let ((letter (buffer-substring-no-properties
                          twidget-beg (1+ twidget-beg))))
             (twidget--add-keyhint-face twidget-beg "1" letter))))))
    ;; add twidget-id overlay
    (twidget-overlay twidget-beg twidget-end 'twidget-id id)))

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
    ;; before eval the action function, `twidget-value' is still the old one.
    ;; the old value of `twidget-value' is useful in action function.
    (when action
      (pcase action
        ((guard (eq 'lambda (car-safe action)))
         (funcall action value))
        ((pred symbolp) (funcall action value))
        (_ (dolist (func action)
             (funcall func value)))))
    ;; after eval the action function, `twidget-value' is updated.
    (setq twidget-value value)
    (set bind twidget-value)
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
        ;; multiple t
        ((pred listp)
         (with-silent-modifications
           (add-face-text-property twidget-beg twidget-end
                                   'twidget-choice-rest-face)
           (when fold
             (unless (twidget-active-p id)
               (add-text-properties twidget-beg twidget-end
                                    '(invisible t)))))
         ;; (message "twidget-str:%s" twidget-str)
         (dolist (val value)
           ;; (message "val:%s" val)
           (when-let* ((start (string-match val twidget-str))
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
        ;; multiple nil
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
                (twidget--add-keyhint-face
                 beg (twidget--format-number-string (length choices) (1+ i))
                 letter)))))))))

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
    (when action
      (pcase action
        ((guard (eq 'lambda (car-safe action)))
         (funcall action value))
        ((pred symbolp) (funcall action value))
        (_ (dolist (func action)
             (funcall func value)))))
    (setq twidget-value value)
    (set bind twidget-value)
    (setq twidget-data (twidget--update-twidget-data))
    (goto-char beg)))

(defun twidget-plain (&rest args)
  "Printer function for plain text."
  (let ((id (nth (1+ (seq-position args :id)) args))
        (texts (seq-subseq args 0 (seq-position args :id)))
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
        (follow-link (plist-get args :follow-link))
        (action (plist-get args :action))
        (help-echo (plist-get args :help-echo))
        twidget-beg)
    (setq twidget-beg (point))
    (insert-button value
                   'face 'twidget-button-face
                   'follow-link follow-link
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
    ((guard (member (car data) twidget-widgets))
     (let ((twidget (car data))
           (plst (cdr data)))
       (pcase twidget
         ('twidget-text
          (let* ((bind (plist-get plst :bind))
                 (value (plist-get plst :value)))
            (apply #'twidget-text plst)
            (set bind value)))
         ('twidget-choice
          ;; for twidget-choice, if multiple is non-nil,
          ;; convert the type of value to list.
          (let* ((bind (plist-get plst :bind))
                 (origin-value (plist-get plst :value))
                 (value (if (plist-get plst :multiple)
                            (if (not (listp origin-value))
                                (list origin-value)
                              origin-value)
                          origin-value))
                 (plst (plist-put plst :value value)))
            (apply #'twidget-choice plst)
            (set bind value)))
         ('twidget-button
          (apply #'twidget-button plst)))))
    ((guard (stringp (car data)))
     (apply #'twidget-plain data))))

(defun twidget-buffer-setup ()
  "Some buffer settings for twidget."
  (let* (;; (info "Press <tab> to jump to next twidget,\
         ;; <shift-tab> to jump backward, press the number key to do selection.")
         (ewoc (ewoc-create
                'twidget-ewoc-pp
                nil
                ;; (propertize
                ;;  (concat info "\n\n") 'face '(shadow italic))
                nil t)))
    (kill-all-local-variables)
    (remove-overlays)
    (buffer-disable-undo)
    (read-only-mode -1)
    (setq cursor-type nil)
    (set (make-local-variable 'twidget-ewoc) ewoc)))

(defun twidget-bind-keymap ()
  "Setting keyhint of the first twidget, binding keys and so on."
  (when-let* ((datas (twidget--update-twidget-data))
              (id (plist-get (cdr (car datas)) :id))
              (beg (ov-beg (car (ov-in 'twidget-id id))))
              (data (progn (goto-char beg)
                           (ewoc-data (ewoc-locate twidget-ewoc))))
              (value (progn (goto-char beg)
                            (plist-get (cdr data) :value))))
    ;; save all twidget data in `twidget-data' variable.
    (setq twidget-value value)
    (setq twidget-active-id id)
    (setq twidget-active-data data)
    (setq twidget-data datas)
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
       (pcase twidget-value
         ((pred listp)
          (dotimes (i (length twidget-value))
            (define-key twidget-mode-map (kbd (format "%s" (1+ i)))
              (lambda ()
                (interactive)
                (setf (nth i twidget-value)
                      (read-from-minibuffer
                       "Input the value: " (nth i twidget-value)))
                (twidget-text-update-value twidget-value)))))
         (_
          (define-key twidget-mode-map (kbd "1")
            (lambda ()
              (interactive)
              (setq twidget-value (read-from-minibuffer
                                   "Input the value: " twidget-value))
              (twidget-text-update-value twidget-value))))))
      ('twidget-choice
       (let* ((require (plist-get plst :require))
              (multiple (plist-get plst :multiple))
              (choices (plist-get plst :choices))
              (len (length choices)))
         (dotimes (i len)
           (define-key twidget-mode-map
             (kbd (twidget--format-number-string len (1+ i)))
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

(defun twidget--unbind-key ()
  "Unbind the key of twidget."
  (let ((twidget (car twidget-active-data))
        (plst (cdr twidget-active-data)))
    (pcase twidget
      ((or 'twidget-text 'twidget-button)
       (unbind-key (kbd "1") twidget-mode-map))
      ('twidget-choice
       (let* ((choices (plist-get plst :choices))
              (len (length choices)))
         (dotimes (i len)
           (unbind-key
            (kbd (twidget--format-number-string len (1+ i))) twidget-mode-map)))))))

(defun twidget--correct-cursor ()
  "Move the cursor point to the beginning of active twidget."
  (let* ((ol (ov-at))
         (active-p (when ol
                     (twidget-active-p (ov-val ol 'twidget-id)))))
    (unless active-p
      (when-let ((ov (car (ov-in 'twidget-id twidget-active-id))))
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
    (twidget--unbind-key)
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
    (twidget--unbind-key)
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
  (when (member (car args) twidget-widgets)
    (if-let* ((next-id (plist-get (cdr args) :next-id))
              (node (twidget--node next-id)))
        (ewoc-enter-before twidget-ewoc node args)
      (ewoc-enter-last twidget-ewoc args))))

(defun twidget--insert (&rest args)
  "Insert STRING at point."
  (if-let* ((nth (seq-position args :next-id))
            (next-id (nth (1+ nth) args))
            (node (twidget--node next-id)))
      (ewoc-enter-before twidget-ewoc node args)
    (ewoc-enter-last twidget-ewoc args)))

(defun twidget--node (bind-or-id)
  "Return the ewoc node of twidget with binded
variable BIND or twidget-id ID."
  (let (id)
    (pcase bind-or-id
      ((and (pred stringp)
            (pred org-uuidgen-p))
       (setq id bind-or-id))
      ((pred symbolp)
       (setq id (twidget--prop-value :bind bind-or-id twidget-data :id))))
    (if-let ((ov (car (ov-in 'twidget-id id))))
        (ewoc-locate twidget-ewoc (ov-beg ov))
      (error "twidget with id %s searched failed!" id))))

(defun twidget--update-twidget (node properties)
  "Update the ewoc node NODE with twidget.
PROPERTIES is a sequence of PROPERTY VALUE pairs."
  (let* ((data (ewoc-data node))
         (twidget (car data))
         (plst (cdr data))
         (alist (twidget--plist->clist properties))
         value)
    (dolist (item alist)
      (setq plst (plist-put plst (car item) (cdr item)))
      (when (eq :value (car item))
        (setq value (cdr item))))
    (ewoc-set-data node (cons twidget plst))
    (twidget-ewoc-invalidate twidget-ewoc node)
    ;; if value is updated, do action.
    (when value
      (when-let ((action (plist-get plst :action)))
        (pcase action
          ((guard (eq 'lambda (car-safe action)))
           (funcall action value))
          ((pred symbolp) (funcall action value))
          (_ (dolist (func action)
               (funcall func value))))))))

(defun twidget--refresh (old new)
  "Update ewoc nodos With the minimum cost.
Replace the old groups OLD with the new groups NEW"
  (let ((len1 (length old))
        (len2 (length new))
        (i 0) (j 0))
    (while (< i len1)
      (if (eq (nth i old) (nth j new))
          (let* ((codes (symbol-value (nth i old)))
                 (twidget-codes (seq-filter
                                 (lambda (code)
                                   (eq (car code) 'twidget--create))
                                 codes))
                 (datas (mapcar #'cdr twidget-codes)))
            (dolist (data datas)
              (let* ((plst (cdr data))
                     (id (plist-get plst :id))
                     (value (plist-get plst :value))
                     (local-p (plist-get plst :local))
                     (node (twidget--node id))
                     ;; specially care about value!
                     (new-value (plist-get (cdr (ewoc-data node)) :value)))
                (when local-p
                  ;; The twidget is local, should be recovered to original
                  ;; format every time refresh groups.
                  (if (equal value new-value)
                      (let* ((pos (seq-position plst :value))
                             (plst1 (seq-subseq plst 0 pos))
                             (plst2 (seq-subseq plst pos))
                             (new-plst2 (seq-drop plst2 2))
                             (new-plst (append plst1 new-plst2)))
                        ;; delete :value pair in plst
                        (apply #'twidget-update id (mapcar #'eval new-plst)))
                    (apply #'twidget-update id (mapcar #'eval plst))))))
            (incf i)
            (incf j))
        (if (not (member (nth i old) new))
            (progn
              (twidget-group-delete (nth i old))
              (incf i))
          (twidget-group-create (nth j new) (nth i old))
          (incf j))))
    (while (< j len2)
      (twidget-group-create (nth j new))
      (incf j))))

;;;###autoload
  (defmacro twidget-create (twidget &rest args)
    "Create the twidget TWIDGET with ARGS in current buffer.
ARGS is a series of form of property value pairs."
    (declare (indent defun))
    (let ((id (org-id-uuid)))
      `(twidget--create ,twidget ,@args :id ,id)))

;;;###autoload
  (defmacro twidget-insert (&rest args)
    "Insert a a series of ARGS string."
    (let ((id (org-id-uuid)))
      `(twidget--insert
        ,@args :id ,id)))

;;;###autoload
  (defun twidget-query (bind-or-id property)
    "Return the value of PROPERTY of BIND-OR-ID twidget."
    (plist-get (cdr (ewoc-data (twidget--node bind-or-id))) property))

;;;###autoload
  (defmacro twidget-group (bind &rest body)
    "Return the relative data of twidget group in BODY.
The first element of the data is a twidget-id list.  
The rest of data is a list of codes for creating twidgets."
    (declare (indent defun))
    (let* (expanded-body
           (binds (mapcar
                   (lambda (code)
                     (let ((expanded-code (macroexpand code)))
                       ;; should only expand once for sake of the unique id.
                       (push expanded-code expanded-body)
                       (plist-get (cddr expanded-code) :id)))
                   body))
           (body (reverse expanded-body)))
      `(set ,bind (cons ',binds ',body))))

;;;###autoload
  (defun twidget-update (bind-or-id &rest properties)
    "Update the properties of twidget with binded variable or
twidget-id.  BIND-OR-ID is either a binded variable or a twidget-id.

Remaining arguments PROPERTIES is a sequence of property value
pairs for text properties to update on the node."
    (twidget--update-twidget (twidget--node bind-or-id) properties))

;;;###autoload
  (defun twidget-multi-update (&rest twidget-properties)
    "Update multiple nodes.  Each TWIDGET-PROPERTIES is a form of 
cons cell of twidget properties.  Twidget is either a binded variable
or a twidget id."
    (let ((clist (twidget--plist->clist twidget-properties)))
      (dolist (item clist)
        (apply #'twidget-update (car item) (cdr item)))))

;;;###autoload
  (defun twidget-delete (&rest binds-or-ids)
    "Delete the twidget with binded variable or twidget id.
BIND-OR-ID is either the variable or id."
    (let ((inhibit-read-only t)
          node)
      (dolist (bind-or-id binds-or-ids)
        (setq node (twidget--node bind-or-id))
        ;; delete the overlay when deleting a node.
        (ov-clear 'twidget-id (plist-get (cdr (ewoc-data node)) :id))
        (setq twidget-overlays (ov-in 'twidget-id))
        (ewoc-delete twidget-ewoc node)
        (set (plist-get (cdr (ewoc-data node)) :bind) nil))))

;;;###autoload
  (defun twidget-group-create (group &optional next-group)
    "Create a twidget GROUP.  If NEXT-GROUP is non-nil, 
the created group is before the BEXT-GROUP."
    (let ((sexps (cdr (symbol-value group)))
          next-id)
      (if next-group
          (progn
            (setq next-id (caar (symbol-value next-group)))
            (dolist (sexp sexps)
              (eval (append sexp `(:next-id ,next-id)))))
        (dolist (sexp sexps)
          (eval sexp)))))

;;;###autoload
  (defun twidget-group-delete (group)
    "Delete the twidget-group binded with variable GROUP."
    (let* ((inhibit-read-only t)
           (ids (car (symbol-value group)))
           (nodes (mapcar #'twidget--node ids)))
      (dolist (id ids)
        (ov-clear 'twidget-id id))
      (setq twidget-overlays (ov-in 'twidget-id))
      (apply #'ewoc-delete twidget-ewoc nodes)))

;;;###autoload
  (defun twidget-page-refresh (&rest groups)
    "Refresh the whole twidget buffer according
to the rest arguments twidget GROUPS.  This command
only update the groups with changes, such as deleting
updating or inserting groups."
    (let ((old-groups twidget-curr-groups)
          (new-groups groups))
      (twidget--refresh old-groups new-groups)
      (setq twidget-curr-groups new-groups)))

;;;###autoload
  (defun twidget-page-create (&rest groups)
    "Create a twidget page with GROUPS."
    (mapcar #'twidget-group-create groups)
    (setq twidget-curr-groups groups))

;;;###autoload
  (defmacro with-twidget-setup (&rest body)
    "Prepare the environment for twidget.
Add `twidget-buffer-setup' function before BODY codes and 
`twidget-bind-keymap' function after BODY codes."
    `(progn
       (twidget-buffer-setup)
       ,@body
       (twidget-bind-keymap)))

;;;###autoload
  (defmacro with-twidget-buffer (buffer-or-name &rest body)
    "A modified macro of `with-current-buffer' for twidget."
    (declare (indent defun))
    `(progn
       (pop-to-buffer (get-buffer-create ,buffer-or-name))
       (let ((inhibit-read-only t))
         (erase-buffer))
       (with-twidget-setup
        ,@body)))

;;;###autoload
  (define-minor-mode twidget-mode
    "Minor mode for text widget."
    nil nil nil
    (if twidget-mode
        (add-hook 'kill-buffer-hook #'twidget--unbind-key nil t)
      (remove-hook 'kill-buffer-hook #'twidget--unbind-key t)))

  (provide 'twidget)
;;; twidget.el ends here
