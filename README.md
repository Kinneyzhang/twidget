# twidget

A declarative text widget library for Emacs, inspired by modern UI component frameworks.

English | [中文](README-zh-CN.md)

## Overview

`twidget` (Text Widget) provides a powerful and flexible way to define reusable text widgets in Emacs. It offers a declarative syntax similar to modern frontend frameworks, supporting:

- **Property System** - Define required and optional properties with default values
- **Slot System** - Support for single slot and named slots for flexible content composition
- **Widget Inheritance** - Extend parent widgets to create specialized variants
- **Text Properties** - Seamless integration with Emacs text properties
- **Reactive Data** - Create reactive UIs with automatic updates using `twidget-ref`
- **Composite Widgets** - Build complex widgets using `:setup` and `:template`

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
| `:render` | Lambda function that returns rendered string (for simple widgets) |
| `:setup` | Lambda function that returns reactive bindings plist (for composite widgets) |
| `:template` | Template sexp for widget structure (for composite widgets) |

**Two Ways to Define Widgets:**

1. **Simple Widget** - Using `:render` for direct rendering
2. **Composite Widget** - Using `:setup` and `:template` for composition with reactive data

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

### Composite Widgets with Reactive Data

For complex widgets that compose other widgets with reactive data, use `:setup` and `:template`:

```elisp
;; Define a counter widget with reactive state
(define-twidget my-counter
  :setup (lambda (_props)
           ;; Return a plist with reactive bindings
           (list :count (twidget-ref "0")))
  :template '(p (span "{count}")
                " "
                (button :action (lambda ()
                                  (interactive)
                                  (twidget-inc 'count 1))
                        "+")
                " "
                (button :action (lambda ()
                                  (interactive)
                                  (twidget-dec 'count 1))
                        "-")))

;; Use the counter
(twidget-parse '(my-counter))
```

**Key Concepts:**

- **`twidget-ref`** - Creates a reactive reference. When its value changes, the UI updates automatically.
- **`:setup`** - A function that receives props and returns a plist of reactive bindings.
- **`:template`** - A quoted sexp defining the widget structure. Use `{varname}` for reactive placeholders.

### Reactive Data API

```elisp
;; Create a reactive reference
(twidget-ref "initial-value")

;; Get a reactive value
(twidget-get 'varname)

;; Set a reactive value (triggers UI update)
(twidget-set 'varname "new-value")

;; Increment/decrement numeric values
(twidget-inc 'varname 1)
(twidget-dec 'varname 1)
```

## Utility Functions

### `twidget-ref`

```elisp
(twidget-ref INITIAL-VALUE)
```

Create a reactive reference with INITIAL-VALUE. Returns a twidget-ref object that can be used in `:setup` functions.

### `twidget-get`

```elisp
(twidget-get SYM &optional KEY-OR-INDEX)
```

Get the current value of reactive variable SYM.

For plist/list values, you can access nested values:
- Use a keyword (e.g., `:name`) to access a plist property
- Use an integer index (0-based) to access a list element

```elisp
;; Get the whole value
(twidget-get 'user)

;; Get :name from a plist value
(twidget-get 'user :name)

;; Get first element from a list value
(twidget-get 'items 0)
```

### `twidget-set`

```elisp
(twidget-set SYM VALUE &optional KEY-OR-INDEX)
```

Set the value of reactive variable SYM to VALUE. This triggers reactive updates in the buffer.

For plist/list values, you can set nested values:
- Use a keyword (e.g., `:name`) to set a plist property
- Use an integer index (0-based) to set a list element

```elisp
;; Set the whole value
(twidget-set 'user new-user)

;; Set :name in a plist value
(twidget-set 'user "John" :name)

;; Set first element in a list value
(twidget-set 'items "new-item" 0)
```

### `twidget-inc`

```elisp
(twidget-inc SYM NUM)
```

Increment the numeric value stored in reactive variable SYM by NUM.

### `twidget-dec`

```elisp
(twidget-dec SYM NUM)
```

Decrement the numeric value stored in reactive variable SYM by NUM.

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
