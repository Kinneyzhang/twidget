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

(defvar habit-repeat-type-group
  (twidget-group
    (twidget-create 'twidget-choice
      :bind 'habit-freq-type
      :choices gtd-habit-regular-feq-type
      :action #'habit-freq-type-switch
      :format "Repeat [t]" :value "after-completion"
      :separator "/" :require t)
    (twidget-insert "\n\n")))

(defvar habit-after-completion-group
  (twidget-group
    (twidget-create 'twidget-text
      :bind 'habit-freq-arg1 :value "1"
      :action '(habit-plural-choices))
    (twidget-create 'twidget-choice
      :bind 'habit-freq-arg2
      :choices habit-time-range
      :format "[t] after previous item is checked off."
      :value "week" :separator "/" :require t)))

(defvar habit-interval-group
  (twidget-group
    (twidget-create 'twidget-text
      :bind 'habit-freq-arg1
      :format (habit-interval-unit-display)
      :action '(habit-plural-unit
                habit-next-dates-change-by-interval)
      :value "1" :local t)))

(defun habit-interval-unit-display ()
  (let ((unit (pcase habit-freq-type
                ("daily" "day")
                ("weekly" "week")
                ("monthly" "month")
                ("yearly" "year"))))
    (concat "Every [t] " unit)))

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

(defun habit--next-date-update (date &rest args)
  "Update the series of next dates.
ARGS are the data used to caculate next dates."
  (pcase habit-freq-type
    ("daily"
     (let ((num-str (string-to-number (car-safe args)))
           (format-str "Next: [t]"))
       (dotimes (i 5)
         (let ((next-date (if (= i 4)
                              "..."
                            (habit--date-change '+ (* (1+ i) num-str)
                                                date))))
           (setq format-str
                 (concat format-str ", " next-date))))
       (twidget-update 'habit-next-date :format format-str)))
    ("weekly" (message "weekly dates update"))
    ("monthly" (message "monthly dates update"))
    ("yearly" (message "yearly dates update"))))

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

(defvar habit-weekly-specific-group
  (twidget-group
    (twidget-create 'twidget-choice
      :bind 'habit-freq-arg2
      :format "on [t]"
      :choices habit-weekdays
      :value "Monday" :separator "/"
      :fold t :multiple t :require t)))

(defvar habit-monthly-specific-group
  (twidget-group
    (twidget-create 'twidget-text
      :bind 'habit-freq-arg2
      :choices habit-weekdays
      :format "\non the [t]st" :value "1")
    (twidget-create 'twidget-choice
      :bind 'habit-freq-arg3
      :choices (cons "day" habit-weekdays)
      :format "[t]" :value "day"
      :fold t :require t)))

(defvar habit-yearly-specific-group
  (twidget-group
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
      :fold t :require t)))

(defvar habit-add-remove-button-group
  (twidget-group
    (twidget-insert " ")
    (twidget-create 'twidget-button
      :value "+"
      :help-echo "Add a twidget group."
      :action (lambda (btn)
                (message "remove a new twidget group")))
    (twidget-create 'twidget-button
      :value "-"
      :help-echo "Remove a twidget group"
      :action (lambda (btn)
                (message "remove a new twidget group")))
    (twidget-insert "\n")))

(defvar habit-next-dates-group
  (twidget-group
    (twidget-insert "\n\n")
    (twidget-create 'twidget-text
      :bind 'habit-next-date
      :format (habit-next-dates-display (format-time-string "%Y/%m/%d"))
      :action 'habit-next-dates-change-by-first-date
      :value (format-time-string "%Y/%m/%d")
      :local t)))

(defun habit-next-dates-display (value)
  (pcase habit-freq-type
    ("daily"
     (let ((num-str (string-to-number habit-freq-arg1))
           (format-str "Next: [t]"))
       (dotimes (i 5)
         (let ((next-date
                (if (= i 4) "..."
                  (habit--date-change '+ (* (1+ i) num-str) value))))
           (setq format-str (concat format-str ", " next-date))))
       format-str))
    ("weekly"
     (let ((num habit-freq-arg1)
           (weekdays habit-freq-arg2)
           (today-weekday (format-time-string "%u")))
       "[t] monthly"))
    ("monthly" "[t] monthly")
    ("yearly" "[t] yearly")))

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

(defvar habit-end-type-group
  (twidget-group
    (twidget-insert "\n")
    (twidget-create 'twidget-choice
      :bind 'habit-end-type
      :choices habit-end-types
      :format "Ends: [t]" :value "never"
      :local t :require t :fold t)))

;; action functions

(defun habit-freq-type-switch (value)
  (pcase habit-freq-type
    ("after-completion"
     (twidget-refresh 'habit-repeat-type-group
                      'habit-after-completion-group))
    ("daily"
     (twidget-refresh
      'habit-repeat-type-group
      'habit-interval-group
      'habit-next-dates-group
      'habit-end-type-group))
    ("weekly"
     (twidget-refresh
      'habit-repeat-type-group
      'habit-interval-group
      'habit-weekly-specific-group
      'habit-next-dates-group
      'habit-end-type-group))
    ("monthly"
     (twidget-refresh
      'habit-repeat-type-group
      'habit-interval-group
      'habit-monthly-specific-group
      'habit-next-dates-group
      'habit-end-type-group))
    ("yearly"
     (twidget-refresh
      'habit-repeat-type-group
      'habit-interval-group
      'habit-yearly-specific-group
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

(defun habit-freq-customize ()
  (interactive)
  (pop-to-buffer (get-buffer-create "*twidget test*"))
  (let ((inhibit-read-only t))
    (erase-buffer))
  (twidget--before-setup)
  (twidget-page-create 'habit-repeat-type-group
                       'habit-after-completion-group)
  (twidget--after-setup))

(habit-freq-customize)
