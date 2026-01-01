;;; Built-in widgets

(define-twidget p
  :slot t :render (lambda (_props slot)
                    (concat slot "\n")))

(define-twidget div
  :slot t :render (lambda (_props slot)
                    (concat slot "\n")))

(define-twidget span
  :slot t :render (lambda (_props slot) slot))

(define-twidget headline
  :slot t
  :props '(height)
  :render (lambda (props slot)
            (concat (tp-set slot 'tp-headline
                            (plist-get props :height))
                    "\n")))

(define-twidget h1
  :extends 'headline
  :props '((height . 2.0))
  :render (lambda (props slot parent-render)
            (funcall parent-render props slot)))

(define-twidget h2
  :extends 'headline
  :props '((height . 1.7))
  :render (lambda (props slot parent-render)
            (funcall parent-render props slot)))

(define-twidget h3
  :extends 'headline
  :props '((height . 1.5))
  :render (lambda (props slot parent-render)
            (funcall parent-render props slot)))

(define-twidget h4
  :extends 'headline
  :props '((height . 1.3))
  :render (lambda (props slot parent-render)
            (funcall parent-render props slot)))

(define-twidget h5
  :extends 'headline
  :props '((height . 1.1))
  :render (lambda (props slot parent-render)
            (funcall parent-render props slot)))

(provide 'twidget-builtin)
