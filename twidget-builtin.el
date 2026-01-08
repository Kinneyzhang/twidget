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

;; div - Block container element.
(define-twidget div
  :type 'block
  :render (lambda (_props slot)
            (twidget-slot-to-string slot)))

(define-twidget col
  :render (lambda (_props slot)
            (string-join slot "\n")))

;; span - Inline text container.  Does not add any line breaks.
(define-twidget span
  :render (lambda (_props slot)
            (twidget-slot-to-string slot)))

;; p - Paragraph block element.
(define-twidget p
  :type 'block
  :render (lambda (_props slot)
            (twidget-slot-to-string slot)))

;; strong - Bold/strong text.  Applies bold face for emphasis.
(define-twidget strong
  :render (lambda (_props slot)
            (tp-set (twidget-slot-to-string slot)
                    'face 'bold)))

;; em - Emphasized (italic) text.
(define-twidget em
  :render (lambda (_props slot)
            (tp-set (twidget-slot-to-string slot)
                    'face 'italic)))

;; underline - Underlined text.
(define-twidget u
  :render (lambda (_props slot)
            (tp-set (twidget-slot-to-string slot)
                    'face 'underline)))

;; strike - Strikethrough text.
(define-twidget del
  :render (lambda (_props slot)
            (tp-set (twidget-slot-to-string slot)
                    'face '(:strike-through t))))

;; code - Inline code.  Displays text in monospace with subtle background.
(define-twidget code
  :props '((palette . code))
  :render (lambda (props slot)
            (tp-set (twidget-slot-to-string slot)
                    'tp-palette (plist-get props :palette)
                    'face `(:family "monospace"))))

;; pre - Preformatted code block.  Preserves whitespace and uses monospace font.
(define-twidget pre
  :props '((palette . code))
  :render (lambda (props slot)
            (tp-set (twidget-slot-to-string slot)
                    'tp-palette (plist-get props :palette)
                    'face `(:family "monospace" :box nil))))

;; blockquote - Block quotation.  Indents content with a vertical bar prefix.
(define-twidget blockquote
  :type 'block
  :props '((prefix . "│") (palette . quote))
  :render (lambda (props slot)
            (let* ((palette (plist-get props :palette))
                   (prefix (tp-set (plist-get props :prefix)
                                   'tp-palette
                                   (tp-suffix-symbol palette "-fg"))))
              (tp-set (twidget-slot-to-string slot)
                      'line-prefix prefix
                      'wrap-prefix prefix
                      'tp-palette palette
                      'face '(:box nil)))))

;; mark - Highlighted/marked text with background color.
(define-twidget mark
  :props '((palette . mark-fbg))
  :render (lambda (props slot)
            (let ((palette (plist-get props :palette)))
              (tp-set (twidget-slot-to-string slot)
                      'tp-palette palette))))

;; kbd - Keyboard key representation.  Styled to look like a keyboard key.
(define-twidget kbd
  :props '((palette . tag))
  :render (lambda (props slot)
            (tp-set (twidget-slot-to-string slot)
                    'tp-palette (plist-get props :palette))))

;; small - Small text.  Reduces text height.
(define-twidget small
  :props '((height . 0.85))
  :render (lambda (props slot)
            (let ((height (plist-get props :height))
                  (slot (twidget-slot-to-string slot)))
              (tp-set slot 'face `(:height ,height)))))

;; headline - Base heading component with configurable height.
(define-twidget headline
  :type 'block
  :props '(height)
  :render (lambda (props slot)
            (tp-set (twidget-slot-to-string slot)
                    'tp-headline (plist-get props :height))))

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
;; Slot: Optional number of line breaks (default: 1).
;;   Accepts: number, string representation of a number, or nil.
;;   This widget expects a single numeric value. If multiple slot values
;;   are provided (as a list), only the first value is used because
;;   it doesn't make semantic sense to "concatenate" line break counts.
;;   Examples: (br), (br 3), (br "2")
(define-twidget br
  :render (lambda (_props slot)
            (let* (;; Extract the first value if slot is a list
                   (slot-val (cond
                              ((listp slot) (car slot))
                              (t slot)))
                   (num (cond
                         ((null slot-val) 1)
                         ((stringp slot-val) (string-to-number slot-val))
                         ((numberp slot-val) slot-val)
                         (t 1))))
              (make-string num ?\n))))

;; hr - Horizontal rule/divider.  Creates a visual separator line.
;; Props: :width - Width in characters, :char - Character for the rule
(define-twidget hr
  :type 'block
  :slot nil
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
  :render (lambda (_props slot)
            (twidget-slot-to-string slot)))

;; li - List item.  A single item in a list with a bullet or number.
(define-twidget li
  :type 'block
  :props '((bullet . "•"))
  :render (lambda (props slot)
            (let ((bullet (plist-get props :bullet)))
              (concat bullet " " (twidget-slot-to-string slot)))))

;; link - Clickable text link with optional URL.
(define-twidget link
  :props '((text . nil))
  :render (lambda (props slot)
            (let* ((url (twidget-slot-to-string slot))
                   (text (or (plist-get props :text) url)))
              (tp-set text
                      'tp-link t
                      'tp-action (lambda ()
                                   (interactive)
                                   (browse-url url))))))

;; button - Interactive button with configurable style.
;; button type supports: primary, secondary, info, success, warning, danger
(define-twidget button
  :props '((type . primary) (padding . (4)))
  :render (lambda (props slot)
            (let* ((btn-type (plist-get props :type))
                   (padding (plist-get props :padding))
                   (space (tp-set " " 'tp-space padding))
                   (slot-str (twidget-slot-to-string slot))
                   (btn-text (format "%s%s%s" space slot-str space)))
              (tp-set btn-text
                      'tp-button btn-type
                      'mouse-face 'highlight
                      'cursor 'hand))))

;; badge - Small status badge/tag with colored background.
(define-twidget badge
  :props '((type . primary) (padding . (4)))
  :render (lambda (props slot)
            (let* ((type (plist-get props :type))
                   (padding (plist-get props :padding))
                   (space (tp-set " " 'tp-space padding))
                   (slot-str (twidget-slot-to-string slot))
                   (badge-text (format "%s%s%s" space slot-str space)))
              (tp-set badge-text
                      'tp-palette
                      (intern (concat "button-" (symbol-name type)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 复合组件

(define-twidget checkbox
  :props '((todo-bullet . "○")
           (done-bullet . "◉"))
  :setup (lambda (props slot)
           (let* ((todo-bullet (plist-get props :todo-bullet))
                  (done-bullet (plist-get props :done-bullet))
                  (bullet (twidget-ref todo-bullet))
                  (slot-str (twidget-slot-to-string slot))
                  (content (twidget-ref slot-str)))
             (list :bullet bullet
                   :content content
                   :change-status
                   ;; Use closure-captured refs directly with twidget-ref-set
                   ;; instead of global twidget-get/twidget-set to ensure
                   ;; each checkbox instance operates on its own state.
                   ;; Made interactive to avoid wrapper lambda overhead in keymap.
                   (lambda ()
                     (interactive)
                     (let ((curr-bullet (twidget-ref-value bullet))
                           (curr-content (twidget-ref-value content)))
                       (if (string= curr-bullet todo-bullet)
                           (progn
                             (twidget-ref-set bullet done-bullet)
                             (twidget-ref-set
                              content
                              (tp-add curr-content 'tp-delete t)))
                         (twidget-ref-set bullet todo-bullet)
                         (twidget-ref-set
                          content
                          (tp-remove curr-content 'tp-delete))))))))
  :template '(span :on-click "change-status"
                   "{bullet}" " " "{content}"))

;; checklist - A list of checkboxes.
(define-twidget checklist
  :props '((todo . "○")
           (done . "◉")
           (data . nil))
  :setup (lambda (props _slot)
           ;; slot can be:
           ;; - a list of items when multiple slot args are passed
           ;; - a single item when one slot arg is passed
           ;; Ensure items is always a list for :for iteration
           (list :todo (plist-get props :todo)
                 :done (plist-get props :done)
                 :items (plist-get props :data)))
  :template '(col (checkbox
                   :todo-bullet "{todo}"
                   :done-bullet "{done}"
                   :for "item in items" "{item}")))

;; tabs - Interactive tabbed interface component with click-to-switch support
;; Props:
;;   :items - List of tab items, each is a plist with:
;;            :key - Unique identifier for the tab
;;            :label - Display text for the tab header
;;            :children - Content to display when tab is active (string)
;;            :disabled - Whether the tab is disabled (optional)
;;   :default-active-key - Key of initially active tab (defaults to first item)
;;   :type - Tab style: line (default) or card
;;   :size - Tab size: small, middle (default), or large
;;   :on-change - Callback function called with (new-key old-key) when tab changes
;;   :on-tab-click - Callback function called with (key) when any tab is clicked
;;
;; Example usage:
;;   (twidget-pop-to-buffer "*tabs-demo*"
;;     (twidget-insert
;;      '(tabs :items ((:key "1" :label "Tab 1" :children "Content for Tab 1")
;;                     (:key "2" :label "Tab 2" :children "Content for Tab 2")
;;                     (:key "3" :label "Tab 3" :children "Content for Tab 3" :disabled t))
;;             :default-active-key "1"
;;             :type line
;;             :size middle
;;             :on-change (lambda (new old) (message "Switched from %s to %s" old new)))))
(define-twidget tabs
  :type 'block
  :props '((items . nil)
           (default-active-key . nil)
           (type . line)
           (size . middle)
           (on-change . nil)
           (on-tab-click . nil))
  :setup (lambda (props _slot)
           (let* ((items (plist-get props :items))
                  (default-key (or (plist-get props :default-active-key)
                                   (when items (plist-get (car items) :key))))
                  (tab-type (or (plist-get props :type) 'line))
                  (tab-size (or (plist-get props :size) 'middle))
                  (on-change (plist-get props :on-change))
                  (on-tab-click (plist-get props :on-tab-click))
                  ;; Reactive state for active key and content
                  (active-key (twidget-ref default-key))
                  (content (twidget-ref
                            (or (plist-get
                                 (cl-find-if
                                  (lambda (item)
                                    (equal (plist-get item :key) default-key))
                                  items)
                                 :children)
                                "")))
                  ;; Tab click handler - switches tabs and updates content
                  (handle-tab-click
                   (lambda (key)
                     (let ((item (cl-find-if
                                  (lambda (i) (equal (plist-get i :key) key))
                                  items)))
                       (when (and item (not (plist-get item :disabled)))
                         ;; Call on-tab-click callback if provided
                         (when on-tab-click
                           (funcall on-tab-click key))
                         ;; Switch tab if different from current
                         (let ((old-key (twidget-ref-value active-key)))
                           (unless (equal key old-key)
                             ;; Call on-change callback if provided
                             (when on-change
                               (funcall on-change key old-key))
                             ;; Update reactive state
                             (twidget-ref-set active-key key)
                             (twidget-ref-set content
                                              (or (plist-get item :children) ""))))))))
                  ;; Build reactive header string with click handlers
                  (build-tab-header
                   (lambda (item)
                     (let* ((key (plist-get item :key))
                            (label (plist-get item :label))
                            (disabled (plist-get item :disabled))
                            (is-active (equal key (twidget-ref-value active-key)))
                            (padding (pcase tab-size
                                       ('small 2)
                                       ('large 6)
                                       (_ 4)))
                            (space (make-string padding ?\s))
                            (tab-text (format "%s%s%s" space label space)))
                       (cond
                        ;; Disabled tab
                        (disabled
                         (tp-set tab-text 'face '(:foreground "gray")))
                        ;; Active tab - styled but no click handler needed
                        (is-active
                         (if (eq tab-type 'card)
                             (tp-set tab-text
                                     'face 'bold
                                     'tp-palette 'button-primary)
                           (tp-set tab-text
                                   'face '(:weight bold :underline t))))
                        ;; Inactive tab - clickable
                        (t
                         (let ((map (make-sparse-keymap)))
                           (define-key map [mouse-1]
                             (lambda ()
                               (interactive)
                               (funcall handle-tab-click key)))
                           (define-key map (kbd "RET")
                             (lambda ()
                               (interactive)
                               (funcall handle-tab-click key)))
                           (tp-set tab-text
                                   'keymap map
                                   'mouse-face 'highlight
                                   'cursor 'hand)))))))
                  ;; Helper to build all headers
                  (build-all-headers
                   (lambda ()
                     (string-join (mapcar build-tab-header items) " ")))
                  ;; Build complete headers string
                  (headers (twidget-ref (funcall build-all-headers))))
             ;; Watch active-key to update headers when tab changes
             (twidget-watch active-key
                            (lambda (_new _old)
                              (twidget-ref-set headers (funcall build-all-headers))))
             ;; Return bindings
             (list :headers headers
                   :content content)))
  :template '(div "{headers}" (br) "{content}"))

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
