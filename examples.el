;;;; Examples of using twidget.el

;;; example1: habit frequency selection

(defvar habit-freq-type nil)
(defvar habit-freq-arg1 nil)
(defvar habit-freq-arg2 nil)
(defvar habit-freq-arg3 nil)
(defvar habit-freq-arg4 nil)
(defvar habit-next-date nil)
(defvar habit-end-type nil)
(defvar habit-time-range '("day" "week" "month" "year"))
(defvar habit-end-types '("never" "after" "on date"))
(defvar habit-weekdays '("Monday" "Tuesday" "Wednesday"
                         "Thursday" "Friday" "Saturday" "Sunday"))
(defvar habit-months '("Jan" "Feb" "Mar" "Apr" "May"
                       "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(twidget-group 'habit-repeat-type-group
  (twidget-create 'twidget-choice
    :bind 'habit-freq-type
    :choices gtd-habit-regular-feq-type
    :action 'habit-freq-type-switch
    :format "Repeat [t]" :value "after-completion"
    :separator "/" :require t)
  (twidget-insert "\n\n"))

(twidget-group 'habit-after-completion-group
  (twidget-create 'twidget-text
    :bind 'habit-freq-arg1 :value "1"
    :action 'habit-plural-choices)
  (twidget-create 'twidget-choice
    :bind 'habit-freq-arg2
    :choices habit-time-range
    :format "[t] after previous item is checked off."
    :value "week" :separator "/" :require t))

(defun habit-plural-unit (value)
  (let ((unit (pcase habit-freq-type
                ("daily" "day")
                ("weekly" "week")
                ("monthly" "month")
                ("yearly" "year"))))
    (if (string= value "1")
        (twidget-update 'habit-freq-arg1
                        :format (concat "Every [t] " unit))
      (twidget-update 'habit-freq-arg1
                      :format (concat "Every [t] " unit "s")))))

(defun habit--date-change (operation num &optional from)
  "Make NUM times of OPERATION change to FROM date.
If FROM is nil, make changes to the current date."
  (let ((from (string-join (split-string from "/") "-")))
    (eval `(format-time-string
            "%Y/%m/%d"
            (,operation
             ,(time-to-seconds (date-to-time
                                (concat from " 00:00:00")))
             ,(* num 86400))))))

(defun habit--next-date-update (first-date &rest args)
  "Update the series of next dates.
ARGS are the data used to caculate next dates."
  (twidget-update
   'habit-next-date
   :format (apply #'habit-next-dates--format first-date args)))

(defun habit-next-dates-change-by-interval (value)
  (pcase habit-freq-type
    ("daily" (habit--next-date-update habit-next-date value))
    ("weekly" (habit--next-date-update
               habit-next-date value habit-freq-arg2))
    ("monthly" (habit--next-date-update
                habit-next-date value habit-freq-arg2 habit-freq-arg3))
    ("yearly" (habit--next-date-update
               habit-next-date value habit-freq-arg2
               habit-freq-arg3 habit-freq-arg4))))

(twidget-group 'habit-daily-specific-group
  (twidget-create 'twidget-text
    :bind 'habit-freq-arg1
    :format "Every [t] day"
    :action '(habit-plural-unit
              habit-next-dates-change-by-interval)
    :value "1"))

(twidget-group 'habit-weekly-specific-group
  (twidget-create 'twidget-text
    :bind 'habit-freq-arg1
    :format "Every [t] week"
    :action '(habit-plural-unit habit-next-dates-change-by-interval)
    :value "1")
  (twidget-create 'twidget-choice
    :bind 'habit-freq-arg2
    :choices habit-weekdays
    :value '("Monday" "Friday")
    :format "on [t]" :separator "/"
    ;; :action 
    :fold t :multiple t :require t))

;; habit-next-dates-change-by-arg
;; habit-first-date-change-by-arg

;; change the first date and the caculate rule at the same time!
(defun habit-first-date-change-by-arg (value)
  (pcase habit-freq-type
    ("weekly"
     (twidget-update 'habit-next-date
                     :value (habit-first-date "weekly")))))

(defun habit-next-dates-change-by-arg (value)
  (pcase habit-freq-type
    ("weekly"
     (habit--next-date-update
      (habit-first-date "weekly") habit-freq-arg1 value))))

;; ----------------------------------------------------------

(twidget-group 'habit-monthly-specific-group
  (twidget-create 'twidget-text
    :bind 'habit-freq-arg1
    :format "Every [t] month"
    :action '(habit-plural-unit
              habit-next-dates-change-by-interval)
    :value "1")
  (twidget-create 'twidget-text
    :bind 'habit-freq-arg2
    :choices habit-weekdays
    :format "\non the [t]st" :value "1")
  (twidget-create 'twidget-choice
    :bind 'habit-freq-arg3
    :choices (cons "day" habit-weekdays)
    :format "[t]" :value "day"
    :fold t :require t))

(twidget-group 'habit-yearly-specific-group
  (twidget-create 'twidget-text
    :bind 'habit-freq-arg1
    :format "Every [t] year"
    :action '(habit-plural-unit
              habit-next-dates-change-by-interval)
    :value "1")
  (twidget-insert "\n")
  (twidget-create 'twidget-text
    :bind 'habit-freq-arg2
    :choices habit-weekdays
    :format "on the [t]st" :value "1")
  (twidget-create 'twidget-choice
    :bind 'habit-freq-arg3
    :choices (cons "day" habit-weekdays)
    :format "[t]" :value "day"
    :fold t :require t)
  (twidget-create 'twidget-choice
    :bind 'habit-freq-arg4
    :choices habit-months
    :format "in [t]" :value "Jan"
    :fold t :require t))

(twidget-group 'habit-add-remove-button-group
  (twidget-insert " ")
  (twidget-create 'twidget-button
    :value "ADD"
    :help-echo "Add a twidget group."
    :action (lambda (btn)
              (message "remove a new twidget group")))
  (twidget-create 'twidget-button
    :value "REMOVE"
    :help-echo "Remove a twidget group"
    :action (lambda (btn)
              (message "remove a new twidget group"))))

(twidget-group 'habit-next-dates-group
  (twidget-insert "\n\n")
  (twidget-create 'twidget-text
    :bind 'habit-next-date
    :value (habit-first-date habit-freq-type)
    :format (habit-next-dates-format (habit-first-date habit-freq-type))
    :action 'habit-next-dates-change-by-first-date
    :local t))

(defun habit-first-date (type)
  (let ((curr-date (format-time-string "%Y/%m/%d")))
    (pcase type
      ("daily" curr-date)
      ("weekly"
       (let* ((num habit-freq-arg1)
              (selected-weekdays habit-freq-arg2)
              (selected-nths
               (mapcar #'1+ (mapcar
                             (lambda (weekday)
                               (seq-position habit-weekdays weekday))
                             selected-weekdays)))
              (first-nth (car selected-nths))
              (curr-nth (string-to-number (format-time-string "%u"))))
         (if (< first-nth curr-nth)
             (habit--date-change '+ (- 6 (- curr-nth first-nth)) curr-date)
           (habit--date-change '+ (- first-nth curr-nth) curr-date))))
      ("monthly" "monthly")
      ("yearly" "yearly"))))

(defun habit-next-dates--format (first-date &rest args)
  (pcase habit-freq-type
    ("daily"
     (let ((num (string-to-number (nth 0 args)))
           (format-str "")
           next-date)
       (dotimes (i 5)
         (if (= i 0) 
             (setq format-str (concat format-str "Next: [t]"))
           (setq next-date (habit--date-change '+ (* i num) first-date))
           (setq format-str (concat format-str ", " next-date))))
       (concat format-str ", ...")))
    ("weekly"
     (let* ((format-str "")
            (week-num (string-to-number (nth 0 args)))
            (selected-weekdays (nth 1 args))
            (_ (message "weekdays:%S" selected-weekdays))
            (selected-nths
             (mapcar #'1+ (mapcar
                           (lambda (weekday)
                             (seq-position habit-weekdays weekday))
                           selected-weekdays)))
            (first-nth (car selected-nths))
            (nth-len (length selected-nths))
            (group-num (/ 5 nth-len))
            (rest-num (% 5 nth-len))
            (all-nths (list selected-nths)))
       (dotimes (_ (1- group-num))
         (setq all-nths (append all-nths (list selected-nths))))
       (unless (= rest-num 0)
         (setq all-nths (append all-nths
                                (list (seq-subseq selected-nths 0 rest-num)))))
       (dotimes (i (length all-nths))
         (let ((group (nth i all-nths)))
           (dotimes (j nth-len)
             (when-let ((curr-nth (nth j group)))
               (if (and (= i 0) (= j 0))
                   (setq format-str (concat format-str "Next: [t]"))
                 (setq format-str
                       (concat format-str ", "
                               (habit--date-change '+ (+ (* (* i 7) week-num)
                                                         (- curr-nth first-nth))
                                                   first-date))))))))
       (concat format-str ", ...")))
    ("monthly" "[t] monthly")
    ("yearly" "[t] yearly")))

(defun habit-next-dates-format (first-date)
  (let ((date first-date))
    (pcase habit-freq-type
      ("daily" (habit-next-dates--format date habit-freq-arg1))
      ("weekly" (habit-next-dates--format date habit-freq-arg1
                                          habit-freq-arg2))
      ("monthly" (habit-next-dates--format date habit-freq-arg1
                                           habit-freq-arg2 habit-freq-arg3))
      ("yearly" (habit-next-dates--format date habit-freq-arg1
                                          habit-freq-arg2 habit-freq-arg3
                                          habit-freq-arg4)))))

(defun habit-next-dates-change-by-first-date (value)
  (pcase habit-freq-type
    ("daily" (habit--next-date-update value habit-freq-arg1))
    ("weekly" (habit--next-date-update
               value habit-freq-arg1 habit-freq-arg2))
    ("monthly" (habit--next-date-update
                value habit-freq-arg1 habit-freq-arg2 habit-freq-arg3))
    ("yearly" (habit--next-date-update
               value habit-freq-arg1 habit-freq-arg2
               habit-freq-arg3 habit-freq-arg4))))

(twidget-group 'habit-end-type-group
  (twidget-insert "\n")
  (twidget-create 'twidget-choice
    :bind 'habit-end-type
    :choices habit-end-types
    :format "Ends: [t]" :value "never"
    :local t :require t :fold t))

;;==============================
;; test global twidget
(twidget-group 'habit-title-group
  (twidget-create 'twidget-text
    :bind 'habit-title
    :value "Habit Title")
  (twidget-insert "\n\n"))
;;==============================

;; action functions

(defun habit-freq-type-switch (value)
  (pcase habit-freq-type
    ("after-completion"
     (twidget-page-refresh
      'habit-title-group
      'habit-repeat-type-group
      'habit-after-completion-group))
    ("daily"
     (twidget-page-refresh
      'habit-title-group
      'habit-repeat-type-group
      'habit-daily-specific-group
      'habit-next-dates-group
      'habit-end-type-group))
    ("weekly"
     (twidget-page-refresh
      'habit-title-group
      'habit-repeat-type-group
      'habit-weekly-specific-group
      'habit-next-dates-group
      'habit-end-type-group))
    ("monthly"
     (twidget-page-refresh
      'habit-title-group
      'habit-repeat-type-group
      'habit-monthly-specific-group
      'habit-add-remove-button-group
      'habit-next-dates-group
      'habit-end-type-group))
    ("yearly"
     (twidget-page-refresh
      'habit-title-group
      'habit-repeat-type-group
      'habit-yearly-specific-group
      'habit-add-remove-button-group
      'habit-next-dates-group
      'habit-end-type-group))))

(defun habit-plural-choices (value)
  (if (> (string-to-number value) 1)
      (twidget-update 'habit-freq-arg2
                      :choices
                      (mapcar (lambda (el)
                                (concat el "s"))
                              habit-time-range)
                      :value
                      (if (string= "s" (substring habit-freq-arg2 -1))
                          habit-freq-arg2
                        (concat habit-freq-arg2 "s")))
    (twidget-update 'habit-freq-arg2
                    :choices habit-time-range
                    :value
                    (if (string= "s" (substring habit-freq-arg2 -1))
                        (string-trim-right habit-freq-arg2 "s")
                      habit-freq-arg2))))

(with-twidget-buffer "*twidget test*"
  (twidget-page-create
   'habit-title-group
   'habit-repeat-type-group
   'habit-after-completion-group))
