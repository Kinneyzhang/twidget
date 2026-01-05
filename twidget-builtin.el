;;; twidget-builtin.el --- Built-in widgets for twidget -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Kinney Zhang

;; Author: Kinney Zhang <kinneyzhang666@gmail.com>
;; URL: https://github.com/Kinneyzhang/twidget
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience, text, widgets

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides built-in widgets for twidget, organized from simple
;; to complex components.  Components are designed to be composable and
;; reusable, with inheritance used to avoid code duplication.
;;
;; Widget :type property:
;; - 'inline (default) - Inline elements, no automatic line breaks
;; - 'block - Block elements, newline added after content when followed
;;            by other elements
;;
;; Component categories:
;; 1. Basic Text/Typography - p, div, span, text, strong, em, code, pre, etc.
;; 2. Headings - headline, h1-h6
;; 3. Lists - ul, ol, li
;; 4. Interactive - link, button
;; 5. Form Display - label, badge, alert
;; 6. Layout - container, section, group, spacer, hr, br
;; 7. Advanced UI - card, progress, kbd, mark, blockquote

;;; Code:

;; ============================================================================
;; Section 1: Basic Text and Container Components
;; ============================================================================
;; These are the foundational components that other widgets build upon.

;; span - Inline text container.  Does not add any line breaks.
(define-twidget span
  :slot t
  :render (lambda (_props slot) slot))

;; p - Paragraph block element.
(define-twidget p
  :type 'block
  :slot t
  :render (lambda (_props slot) slot))

;; div - Block container element.
(define-twidget div
  :type 'block
  :slot t
  :render (lambda (_props slot) slot))

;; strong - Bold/strong text.  Applies bold face for emphasis.
(define-twidget strong
  :slot t
  :render (lambda (_props slot)
            (tp-set slot 'face 'bold)))

;; em - Emphasized (italic) text.
(define-twidget em
  :slot t
  :render (lambda (_props slot)
            (tp-set slot 'face 'italic)))

;; underline - Underlined text.
(define-twidget u
  :slot t
  :render (lambda (_props slot)
            (tp-set slot 'face 'underline)))

;; strike - Strikethrough text.
(define-twidget del
  :slot t
  :render (lambda (_props slot)
            (tp-set slot 'face '(:strike-through t))))

;; code - Inline code.  Displays text in monospace with subtle background.
;; Props: :bgcolor - Background color (default: light gray)
(define-twidget code
  :slot t
  :props '((palette . code))
  :render (lambda (props slot)
            (tp-set slot
                    'tp-palette (plist-get props :palette)
                    'face `(:family "monospace"))))

;; pre - Preformatted code block.  Preserves whitespace and uses monospace font.
;; Props: :bgcolor - Background color (default: light gray)
(define-twidget pre
  :slot t
  :props '((palette . code))
  :render (lambda (props slot)
            (tp-set slot
                    'tp-palette (plist-get props :palette)
                    'face `(:family "monospace" :box nil))))

;; blockquote - Block quotation.  Indents content with a vertical bar prefix.
;; Props: :prefix - Quote prefix (default: "│ "), :fgcolor - Foreground color
(define-twidget blockquote
  :type 'block
  :slot t
  :props '((prefix . "│") (palette . quote))
  :render (lambda (props slot)
            (let* ((palette (plist-get props :palette))
                   (prefix (tp-set (plist-get props :prefix)
                                   'tp-palette
                                   (tp-suffix-symbol palette "-fg"))))
              (tp-set slot
                      'line-prefix prefix
                      'wrap-prefix prefix
                      'tp-palette palette
                      'face '(:box nil)))))

;; mark - Highlighted/marked text with background color.
;; Props: :bgcolor - Highlight background color (default: yellow)
(define-twidget mark
  :slot t
  :props '((palette . mark-fbg))
  :render (lambda (props slot)
            (let ((palette (plist-get props :palette)))
              (tp-set slot 'tp-palette palette))))

;; kbd - Keyboard key representation.  Styled to look like a keyboard key.
;; Props: :bgcolor, :border-color
(define-twidget kbd
  :slot t
  :props '((palette . tag))
  :render (lambda (props slot)
            (tp-set slot 'tp-palette (plist-get props :palette))))

;; small - Small text.  Reduces text height.
;; Props: :height - Text height multiplier (default: 0.85)
(define-twidget small
  :slot t
  :props '((height . 0.85))
  :render (lambda (props slot)
            (let ((height (plist-get props :height)))
              (tp-set slot 'face `(:height ,height)))))

;; headline - Base heading component with configurable height.
(define-twidget headline
  :type 'block
  :slot t
  :props '(height)
  :render (lambda (props slot)
            (tp-set slot 'tp-headline (plist-get props :height))))

;; h1 - Level 1 heading (largest).  Height: 2.0x
(define-twidget h1
  :extends 'headline
  :props '((height . 2.0))
  :render (lambda (props slot parent-render)
            (funcall parent-render props slot)))

;; h2 - Level 2 heading.  Height: 1.7x
(define-twidget h2
  :extends 'headline
  :props '((height . 1.7))
  :render (lambda (props slot parent-render)
            (funcall parent-render props slot)))

;; h3 - Level 3 heading.  Height: 1.5x
(define-twidget h3
  :extends 'headline
  :props '((height . 1.5))
  :render (lambda (props slot parent-render)
            (funcall parent-render props slot)))

;; h4 - Level 4 heading.  Height: 1.3x
(define-twidget h4
  :extends 'headline
  :props '((height . 1.3))
  :render (lambda (props slot parent-render)
            (funcall parent-render props slot)))

;; h5 - Level 5 heading.  Height: 1.1x
(define-twidget h5
  :extends 'headline
  :props '((height . 1.1))
  :render (lambda (props slot parent-render)
            (funcall parent-render props slot)))

;; h6 - Level 6 heading (smallest).  Height: 1.0x with bold face.
(define-twidget h6
  :extends 'headline
  :props '((height . 1.0))
  :render (lambda (props slot parent-render)
            (tp-set (funcall parent-render props slot) 'face 'bold)))

;; br - Line break.  Inserts a newline character.
(define-twidget br
  :slot t
  :render (lambda (_props slot)
            (let ((num (if slot
                           (string-to-number slot)
                         1)))
              (make-string num ?\n))))

;; hr - Horizontal rule/divider.  Creates a visual separator line.
;; Props: :width - Width in characters, :char - Character for the rule
(define-twidget hr
  :type 'block
  :props '((width . nil)
           (color . "gray")
           (char . "─"))
  :render (lambda (props _slot)
            (let ((color (plist-get props :color))
                  (width (or (plist-get props :width)
                             (1- (window-body-width))))
                  (char (plist-get props :char)))
              (tp-set
               (make-string width (string-to-char char))
               'tp-fg color))))

;; ul - Unordered list container.  Wraps list items with bullet points.
(define-twidget ul
  :type 'block
  :slot t
  :render (lambda (_props slot) slot))

;; li - List item.  A single item in a list with a bullet or number.
(define-twidget li
  :type 'block
  :slot t
  :props '((bullet . "•"))
  :render (lambda (props slot)
            (let ((bullet (plist-get props :bullet)))
              (concat bullet " " slot))))

;; link - Clickable text link with optional URL.
(define-twidget link
  :slot t
  :props '((text . nil))
  :render (lambda (props slot)
            (let* ((url slot)
                   (text (or (plist-get props :text) url)))
              (tp-set text
                      'tp-link t
                      'tp-action (lambda ()
                                   (interactive)
                                   (browse-url url))))))

;; button - Interactive button with configurable style.
;; button type supports: primary, secondary, info, success, warning, danger
(define-twidget button
  :slot t
  :props '((type . primary) (padding . (4)))
  :render (lambda (props slot)
            (let* ((btn-type (plist-get props :type))
                   (padding (plist-get props :padding))
                   (space (tp-set " " 'tp-space padding))
                   (btn-text (format "%s%s%s" space slot space)))
              (tp-set btn-text
                      'tp-button btn-type
                      'mouse-face 'highlight
                      'cursor 'hand))))

;; badge - Small status badge/tag with colored background.
(define-twidget badge
  :slot t
  :props '((type . primary) (padding . (4)))
  :render (lambda (props slot)
            (let* ((type (plist-get props :type))
                   (padding (plist-get props :padding))
                   (space (tp-set " " 'tp-space padding))
                   (badge-text (format "%s%s%s" space slot space)))
              (tp-set badge-text
                      'tp-palette
                      (intern (concat "button-" (symbol-name type)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 复合组件

;; (define-twidget checklist
;;   :slot t
;;   :props ((todo-bullet . "○")
;;           (done-bullet . "◉"))
;;   :setup (lambda (_props slot)
;;            ())
;;   :template '()
;;   )

;; (define-twidget todolist
;;   )

;; (define-twidget card
;;   :type 'block
;;   :slots '(header content footer)
;;   :props '((width . 40)
;;            (border . "─"))
;;   :render (lambda (props slots)
;;             (let* ((width (plist-get props :width))
;;                    (border-char (plist-get props :border))
;;                    (header (plist-get slots :header))
;;                    (content (plist-get slots :content))
;;                    (footer (plist-get slots :footer))
;;                    (line (make-string width (string-to-char border-char))))
;;               (concat
;;                "╭" line "╮\n"
;;                (when header
;;                  (concat "│ " header
;;                          (make-string
;;                           (max 0 (- width (length header) 1)) ?\s)
;;                          "│\n"
;;                          "├" line "┤\n"))
;;                (when content
;;                  (concat "│ " content
;;                          (make-string
;;                           (max 0 (- width (length content) 1)) ?\s)
;;                          "│\n"))
;;                (when footer
;;                  (concat "├" line "┤\n"
;;                          "│ " footer
;;                          (make-string
;;                           (max 0 (- width (length footer) 1)) ?\s)
;;                          "│\n"))
;;                "╰" line "╯"))))

;; (define-twidget progress)
;; (define-twidget icon)

(provide 'twidget-builtin)
;;; twidget-builtin.el ends here
