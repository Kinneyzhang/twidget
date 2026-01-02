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
  :type 'inline
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

;; text - Styled text component with face property.
;; Props: :face - Face specification (symbol, plist, or list of faces)
(define-twidget text
  :type 'inline
  :slot t
  :props '((face . nil))
  :render (lambda (props slot)
            (let ((face (plist-get props :face)))
              (if face
                  (tp-set slot 'face face)
                slot))))

;; ============================================================================
;; Section 2: Typography Components
;; ============================================================================
;; Semantic text styling components.

;; strong - Bold/strong text.  Applies bold face for emphasis.
(define-twidget strong
  :type 'inline
  :slot t
  :render (lambda (_props slot)
            (tp-set slot 'face 'bold)))

;; em - Emphasized (italic) text.
(define-twidget em
  :type 'inline
  :slot t
  :render (lambda (_props slot)
            (tp-set slot 'face 'italic)))

;; underline - Underlined text.
(define-twidget u
  :type 'inline
  :slot t
  :render (lambda (_props slot)
            (tp-set slot 'face 'underline)))

;; strike - Strikethrough text.
(define-twidget del
  :type 'inline
  :slot t
  :render (lambda (_props slot)
            (tp-set slot 'face '(:strike-through t))))

;; code - Inline code.  Displays text in monospace with subtle background.
;; Props: :bgcolor - Background color (default: light gray)
(define-twidget code
  :type 'inline
  :slot t
  :props '((palette . org-code))
  :render (lambda (props slot)
            (tp-set slot
                    'tp-palette (plist-get props :palette)
                    'face `(:family "monospace"))))

;; pre - Preformatted code block.  Preserves whitespace and uses monospace font.
;; Props: :bgcolor - Background color (default: light gray)
(define-twidget pre
  :type 'inline
  :slot t
  :props '((palette . org-code))
  :render (lambda (props slot)
            (tp-set slot
                    'tp-palette (plist-get props :palette)
                    'face `(:family "monospace" :box nil))))

;; blockquote - Block quotation.  Indents content with a vertical bar prefix.
;; Props: :prefix - Quote prefix (default: "│ "), :fgcolor - Foreground color
(define-twidget blockquote
  :type 'block
  :slot t
  :props '((prefix . "│") (palette . org-quote))
  :render (lambda (props slot)
            (let* ((palette (plist-get props :palette))
                   (fgcolor (tp-palette-fg-color palette))
                   (prefix (tp-set (plist-get props :prefix)
                                   'tp-fg fgcolor)))
              (tp-set slot
                      'line-prefix prefix
                      'wrap-prefix prefix
                      'tp-palette palette
                      'face '(:box nil)))))

;; mark - Highlighted/marked text with background color.
;; Props: :bgcolor - Highlight background color (default: yellow)
(define-twidget mark
  :type 'inline
  :slot t
  :props '((palette . search-match))
  :render (lambda (props slot)
            (let ((palette (plist-get props :palette)))
              (tp-set slot 'tp-bg (tp-palette-bg-color palette)))))

;; kbd - Keyboard key representation.  Styled to look like a keyboard key.
;; Props: :bgcolor, :border-color
(define-twidget kbd
  :type 'inline
  :slot t
  :props '((palette . org-tag))
  :render (lambda (props slot)
            (tp-set slot 'tp-palette (plist-get props :palette))))

;; small - Small text.  Reduces text height.
;; Props: :height - Text height multiplier (default: 0.85)
(define-twidget small
  :type 'inline
  :slot t
  :props '((height . 0.85))
  :render (lambda (props slot)
            (let ((height (plist-get props :height)))
              (tp-set slot 'face `(:height ,height)))))

;; ============================================================================
;; Section 3: Heading Components
;; ============================================================================
;; Heading components with configurable sizes using inheritance.

;; headline - Base heading component with configurable height.
;; Props: :height - Text height multiplier (e.g., 2.0 for double size)
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

;; ============================================================================
;; Section 4: Line and Spacing Components
;; ============================================================================
;; Components for controlling layout and spacing.

;; br - Line break.  Inserts a newline character.
(define-twidget br
  :type 'inline
  :render (lambda (_props _slot) "\n"))

;; hr - Horizontal rule/divider.  Creates a visual separator line.
;; Props: :width - Width in characters, :char - Character for the rule
(define-twidget hr
  :type 'block
  :props '((width . nil)
           (char . "─"))
  :render (lambda (props _slot)
            (let ((width
                   (or (plist-get props :width)
                       (1- (window-body-width))))
                  (char (plist-get props :char)))
              (make-string width (string-to-char char)))))

;; spacer - Vertical spacer.  Adds empty lines for spacing.
;; Props: :lines - Number of empty lines (default: 1)
(define-twidget spacer
  :type 'inline
  :props '((lines . 1))
  :render (lambda (props _slot)
            (let ((lines (plist-get props :lines)))
              (make-string lines ?\n))))

;; indent - Indented content block.  Adds leading spaces to content.
;; Props: :level - Indentation level, each level adds 2 spaces (default: 1)
(define-twidget indent
  :type 'inline
  :slot t
  :props '((level . 1))
  :render (lambda (props slot)
            (let* ((level (plist-get props :level))
                   (spaces (make-string (* level 2) ?\s)))
              (concat spaces slot))))

;; ============================================================================
;; Section 5: List Components
;; ============================================================================
;; Components for creating lists.

;; li - List item.  A single item in a list with a bullet or number.
;; Props: :bullet - The bullet/marker for the item (default: "• ")
(define-twidget li
  :type 'block
  :slot t
  :props '((bullet . "• "))
  :render (lambda (props slot)
            (let ((bullet (plist-get props :bullet)))
              (concat bullet slot))))

;; ul - Unordered list container.  Wraps list items with bullet points.
(define-twidget ul
  :type 'block
  :slot t
  :render (lambda (_props slot) slot))

;; ol - Ordered list container.  Numbers are handled by using li with :bullet.
(define-twidget ol
  :type 'block
  :slot t
  :render (lambda (_props slot) slot))

;; ============================================================================
;; Section 6: Interactive Components
;; ============================================================================
;; Components with click actions and interactive behavior.

;; link - Clickable text link with optional URL or action.
;; Props: :url, :action, :fgcolor, :underline
(define-twidget link
  :type 'inline
  :slot t
  :props '((url . nil)
           (action . nil)
           (fgcolor . "#0066cc")
           (underline . t))
  :render (lambda (props slot)
            (let* ((url (plist-get props :url))
                   (action (plist-get props :action))
                   (fgcolor (plist-get props :fgcolor))
                   (ul (plist-get props :underline))
                   (face `(:foreground ,fgcolor
                                       :underline ,(if ul t nil)))
                   (keymap (make-sparse-keymap))
                   (handler (or action
                                (when url
                                  (lambda ()
                                    (interactive)
                                    (browse-url url))))))
              (when handler
                (define-key keymap [mouse-1] handler)
                (define-key keymap (kbd "RET") handler))
              (propertize (tp-set slot 'face face)
                          'keymap keymap
                          'mouse-face 'highlight
                          'cursor 'hand))))

;; button - Interactive button with configurable style.
;; Props: :action, :bgcolor, :fgcolor, :type (primary/secondary/success/danger/warning), :padding
;; (define-twidget button
;;   :type 'inline
;;   :slot t
;;   :props '((action . nil)
;;            (type . "primary")
;;            (padding . 1))
;;   :render (lambda (props slot)
;;             (let* ((action (plist-get props :action))
;;                    (btn-type (plist-get props :type))
;;                    (padding (plist-get props :padding))
;;                    (pad-str (make-string padding ?\s))
;;                    (bgcolor (or (plist-get props :bgcolor) (car type-colors)))
;;                    (fgcolor (or (plist-get props :fgcolor) (cdr type-colors)))
;;                    (face `( :background ,bgcolor
;;                             :foreground ,fgcolor
;;                             :box (:line-width -1 :style released-button)))
;;                    (btn-text (format "%s%s%s" pad-str slot pad-str))
;;                    (keymap (make-sparse-keymap)))
;;               (when action
;;                 (define-key keymap [mouse-1] action)
;;                 (define-key keymap (kbd "RET") action))
;;               (propertize (tp-set btn-text 'face face)
;;                           'keymap keymap
;;                           'mouse-face 'highlight
;;                           'cursor 'hand))))

;; ============================================================================
;; Section 7: Badge and Alert Components
;; ============================================================================
;; Status indicators and notification components.

;; badge - Small status badge/tag with colored background.
;; Props: :type (info/success/warning/danger/secondary), :bgcolor, :fgcolor
(define-twidget badge
  :type 'inline
  :slot t
  :props '((type . "info")
           (bgcolor . nil)
           (fgcolor . nil))
  :render (lambda (props slot)
            (let* ((badge-type (plist-get props :type))
                   (type-colors (pcase badge-type
                                  ("info" '("#17a2b8" . "#ffffff"))
                                  ("success" '("#28a745" . "#ffffff"))
                                  ("warning" '("#ffc107" . "#000000"))
                                  ("danger" '("#dc3545" . "#ffffff"))
                                  ("secondary" '("#6c757d" . "#ffffff"))
                                  ("primary" '("#007bff" . "#ffffff"))
                                  (_ '("#17a2b8" . "#ffffff"))))
                   (bgcolor (or (plist-get props :bgcolor) (car type-colors)))
                   (fgcolor (or (plist-get props :fgcolor) (cdr type-colors))))
              (tp-set (format " %s " slot)
                      'face `(:background ,bgcolor :foreground ,fgcolor)))))

;; alert - Alert/notification message block with icon and styling.
;; Props: :type (info/success/warning/danger), :icon, :dismissible
(define-twidget alert
  :type 'block
  :slot t
  :props '((type . "info")
           (icon . nil)
           (dismissible . nil))
  :render (lambda (props slot)
            (let* ((alert-type (plist-get props :type))
                   (custom-icon (plist-get props :icon))
                   (type-config (pcase alert-type
                                  ("info" '("ℹ️" . "#d1ecf1"))
                                  ("success" '("✓" . "#d4edda"))
                                  ("warning" '("⚠" . "#fff3cd"))
                                  ("danger" '("✕" . "#f8d7da"))
                                  ("error" '("✕" . "#f8d7da"))
                                  (_ '("ℹ️" . "#d1ecf1"))))
                   (icon (or custom-icon (car type-config)))
                   (bgcolor (cdr type-config)))
              (tp-set (format "%s %s" icon slot)
                      'face `(:background ,bgcolor)))))

;; label - Form label text.  Can be associated with form elements.
;; Props: :for (reserved), :required - Whether to show required indicator
(define-twidget label
  :type 'inline
  :slot t
  :props '((for . nil)
           (required . nil))
  :render (lambda (props slot)
            (let ((required (plist-get props :required)))
              (if required
                  (concat (tp-set slot 'face 'bold)
                          (tp-set " *" 'face '(:foreground "red")))
                (tp-set slot 'face 'bold)))))

;; ============================================================================
;; Section 8: Layout Components
;; ============================================================================
;; Components for structuring and organizing content.

;; container - Width-constrained container for content.
;; Props: :width (reference only), :center (reserved)
(define-twidget container
  :type 'block
  :slot t
  :props '((width . 80)
           (center . nil))
  :render (lambda (_props slot) slot))

;; section - Semantic section container.  Adds spacing before and after content.
;; Props: :title - Optional section title
(define-twidget section
  :type 'block
  :slot t
  :props '((title . nil))
  :render (lambda (props slot)
            (let ((title (plist-get props :title)))
              (if title
                  (concat (tp-set title 'face 'bold) "\n" slot)
                slot))))

;; group - Inline group container.  Groups elements without adding line breaks.
(define-twidget group
  :type 'inline
  :slot t
  :render (lambda (_props slot) slot))

;; row - Horizontal row container (block element).
(define-twidget row
  :type 'block
  :slot t
  :render (lambda (_props slot) slot))

;; ============================================================================
;; Section 9: Card Component
;; ============================================================================
;; Card component with named slots for header, content, and footer.

;; card - Card with header, content, and footer slots.
;; Named slots: slot-header, slot-content, slot-footer
;; Props: :width, :border
(define-twidget card
  :type 'block
  :slots '(header content footer)
  :props '((width . 40)
           (border . "─"))
  :render (lambda (props slots)
            (let* ((width (plist-get props :width))
                   (border-char (plist-get props :border))
                   (header (plist-get slots :header))
                   (content (plist-get slots :content))
                   (footer (plist-get slots :footer))
                   (line (make-string width (string-to-char border-char))))
              (concat
               "╭" line "╮\n"
               (when header
                 (concat "│ " header
                         (make-string (max 0 (- width (length header) 1)) ?\s) "│\n"
                         "├" line "┤\n"))
               (when content
                 (concat "│ " content
                         (make-string (max 0 (- width (length content) 1)) ?\s) "│\n"))
               (when footer
                 (concat "├" line "┤\n"
                         "│ " footer
                         (make-string (max 0 (- width (length footer) 1)) ?\s) "│\n"))
               "╰" line "╯"))))

;; ============================================================================
;; Section 10: Progress Bar Component
;; ============================================================================
;; Visual progress indicator.

;; progress - Progress bar component.
;; Props: :value, :max, :width, :filled-char, :empty-char, :show-percent, :fgcolor
(define-twidget progress
  :type 'inline
  :props '((value . 0)
           (max . 100)
           (width . 20)
           (filled-char . "█")
           (empty-char . "░")
           (show-percent . t)
           (fgcolor . "#28a745"))
  :render (lambda (props _slot)
            (let* ((value (plist-get props :value))
                   (max-val (plist-get props :max))
                   (width (plist-get props :width))
                   (filled-char (plist-get props :filled-char))
                   (empty-char (plist-get props :empty-char))
                   (show-percent (plist-get props :show-percent))
                   (fgcolor (plist-get props :fgcolor))
                   (percent (min 100 (max 0 (* 100 (/ (float value) max-val)))))
                   (filled-width (round (* width (/ percent 100.0))))
                   (empty-width (- width filled-width))
                   (filled-str (make-string filled-width (string-to-char filled-char)))
                   (empty-str (make-string empty-width (string-to-char empty-char)))
                   (bar (concat
                         (tp-set filled-str 'face `(:foreground ,fgcolor))
                         empty-str)))
              (if show-percent
                  (format "[%s] %3.0f%%" bar percent)
                (format "[%s]" bar)))))

;; ============================================================================
;; Section 11: Tooltip Component (Display Only)
;; ============================================================================
;; Tooltip that shows on hover (using help-echo).

;; tooltip - Text with tooltip on hover.
;; Props: :tip - Tooltip text to display on hover
(define-twidget tooltip
  :type 'inline
  :slot t
  :props '((tip . ""))
  :render (lambda (props slot)
            (let ((tip (plist-get props :tip)))
              (propertize slot 'help-echo tip))))

;; ============================================================================
;; Section 12: Icon Component
;; ============================================================================
;; Unicode/emoji icon display.

;; icon - Icon component using Unicode/emoji characters.
;; Props: :name (info/success/warning/error/check/cross/star/heart/etc.), :custom
(define-twidget icon
  :type 'inline
  :props '((name . "info")
           (custom . nil))
  :render (lambda (props _slot)
            (let ((custom (plist-get props :custom))
                  (name (plist-get props :name)))
              (or custom
                  (pcase name
                    ("info" "ℹ️")
                    ("success" "✓")
                    ("warning" "⚠")
                    ("error" "✕")
                    ("danger" "⚠")
                    ("check" "✓")
                    ("cross" "✕")
                    ("star" "★")
                    ("star-empty" "☆")
                    ("heart" "♥")
                    ("heart-empty" "♡")
                    ("arrow-right" "→")
                    ("arrow-left" "←")
                    ("arrow-up" "↑")
                    ("arrow-down" "↓")
                    ("circle" "●")
                    ("circle-empty" "○")
                    ("square" "■")
                    ("square-empty" "□")
                    ("triangle" "▲")
                    ("menu" "☰")
                    ("search" "🔍")
                    ("settings" "⚙")
                    ("user" "👤")
                    ("folder" "📁")
                    ("file" "📄")
                    ("edit" "✎")
                    ("delete" "🗑")
                    ("plus" "+")
                    ("minus" "−")
                    (_ "•"))))))

;; ============================================================================
;; Section 13: Abbr (Abbreviation) Component
;; ============================================================================
;; Abbreviation with expanded text on hover.

;; abbr - Abbreviation with expanded text shown on hover.
;; Props: :title - Full expanded text for the abbreviation
(define-twidget abbr
  :type 'inline
  :slot t
  :props '((title . ""))
  :render (lambda (props slot)
            (let ((title (plist-get props :title)))
              (propertize
               (tp-set slot 'face '(:underline (:style wave)))
               'help-echo title))))

(provide 'twidget-builtin)
;;; twidget-builtin.el ends here
