# twidget

A declarative text widget library for Emacs, inspired by modern UI component frameworks.

English | [中文](README-zh-CN.md)

## Overview

`twidget` (Text Widget) provides a powerful and flexible way to define reusable text widgets in Emacs. It offers a declarative syntax similar to modern frontend frameworks, supporting:

- **Property System** - Define required and optional properties with default values
- **Slot System** - Support for single slot and named slots for flexible content composition
- **Widget Inheritance** - Extend parent widgets to create specialized variants
- **Text Properties** - Seamless integration with Emacs text properties

## Installation

### Dependencies

This package requires the [tp](https://github.com/Kinneyzhang/tp) (text properties) library.

### Manual Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/Kinneyzhang/twidget.git
   ```

2. Add to your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "/path/to/twidget")
   (require 'twidget)
   ```

### Using use-package with straight.el

```elisp
(use-package twidget
  :straight (:host github :repo "Kinneyzhang/twidget")
  :after tp)
```

## Quick Start

### Defining a Simple Widget

```elisp
;; Define a paragraph widget with a single slot
(define-twidget p
  :slot t
  :render (lambda (props slot)
            (concat slot "\n")))

;; Use the widget
(twidget-parse '(p "Hello, World!"))
;; => "Hello, World!\n"
```

### Widget with Properties

```elisp
;; Define a text widget with color property
(define-twidget styled-text
  :props '((color . "black"))
  :slot t
  :render (lambda (props slot)
            (tp-set slot 'face `(:foreground ,(plist-get props :color)))))

;; Use with custom color
(twidget-parse '(styled-text :color "blue" "Colored text"))
```

### Nested Widgets

```elisp
;; Widgets can be nested
(twidget-parse
 '(p "Start "
     (styled-text :color "red" "important")
     " end"))
```

## API Reference

### `define-twidget`

```elisp
(define-twidget NAME &rest ARGS)
```

Define a text widget named NAME.

**Keyword Arguments:**

| Keyword | Description |
|---------|-------------|
| `:props` | Property definitions. Each can be a symbol (required) or cons cell `(symbol . default)` |
| `:slot` | Boolean or list. `nil` (no slot), `t` (single slot), or list of slot names |
| `:slots` | Alias for `:slot` with named slots |
| `:extends` | Symbol of parent widget to inherit from |
| `:render` | Lambda function that returns rendered string |

**Render Function Signatures:**

- Single slot: `(lambda (props slot) ...)`
- Named slots: `(lambda (props slots) ...)` where slots is a plist
- With inheritance: `(lambda (props slot parent-render) ...)`

### `twidget-parse`

```elisp
(twidget-parse WIDGET-FORM)
```

Parse and render a widget invocation. Returns the rendered string with text properties applied.

**WIDGET-FORM Format:**
```elisp
(WIDGET-NAME :prop1 val1 :prop2 val2 ... SLOT-VALUES...)
```

### `twidget-reset`

```elisp
(twidget-reset)
```

Reset all widget definitions. Useful for development and testing.

## Advanced Features

### Named Slots

Named slots allow you to specify where different pieces of content should be placed:

```elisp
(define-twidget card
  :slots '(header content footer)
  :render (lambda (props slots)
            (concat "┌──────────────────┐\n"
                    "│ " (or (plist-get slots :header) "") "\n"
                    "├──────────────────┤\n"
                    "│ " (or (plist-get slots :content) "") "\n"
                    "├──────────────────┤\n"
                    "│ " (or (plist-get slots :footer) "") "\n"
                    "└──────────────────┘\n")))

;; Usage with named slots
(twidget-parse
 '(card
   (slot-header "Title")
   (slot-content "Main content here")
   (slot-footer "Footer text")))
```

### Widget Inheritance

Create specialized widgets by extending base widgets:

```elisp
;; Base button widget
(define-twidget base-button
  :props '((type . "default"))
  :slot t
  :render (lambda (props slot)
            (tp-set slot 'face 'button)))

;; Primary button extending base-button
(define-twidget primary-button
  :extends 'base-button
  :props '((type . "primary"))
  :render (lambda (props slot parent-render)
            (let ((result (funcall parent-render props slot)))
              (tp-add result 'face '(:foreground "blue")))))

;; Usage
(twidget-parse '(primary-button "Click Me"))
```

### Property Inheritance

Child widgets inherit properties from parent widgets, with child properties taking precedence:

```elisp
(define-twidget parent-widget
  :props '((size . "medium")
           (color . "gray"))
  :slot t
  :render ...)

(define-twidget child-widget
  :extends 'parent-widget
  :props '((color . "blue"))  ; Overrides parent's color, inherits size
  :render ...)
```

## Utility Functions

### `twidget-inc`

```elisp
(twidget-inc SYM NUM)
```

Increment a numeric string stored in symbol SYM by NUM.

### `twidget-dec`

```elisp
(twidget-dec SYM NUM)
```

Decrement a numeric string stored in symbol SYM by NUM.

## Examples

### Creating a Badge Widget

```elisp
(define-twidget badge
  :props '((type . "info"))
  :slot t
  :render (lambda (props slot)
            (let ((face (pcase (plist-get props :type)
                          ("info" '(:background "blue" :foreground "white"))
                          ("success" '(:background "green" :foreground "white"))
                          ("warning" '(:background "orange" :foreground "black"))
                          ("error" '(:background "red" :foreground "white"))
                          (_ '(:background "gray" :foreground "white")))))
              (tp-set (concat " " slot " ") 'face face))))

(twidget-parse '(badge :type "success" "OK"))
```

### Creating a Layout Widget

```elisp
(define-twidget flex-row
  :slot t
  :render (lambda (props slot)
            (concat "[ " slot " ]")))

(twidget-parse
 '(flex-row
   (badge :type "info" "A")
   " | "
   (badge :type "success" "B")
   " | "
   (badge :type "error" "C")))
```

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.

## License

This project is licensed under the GNU General Public License v3.0 - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- Inspired by modern UI component frameworks like Vue.js and React
- Built with Emacs Lisp for the Emacs text editor
