<div align="center">

# 🎨 twidget

**A declarative text widget library for Emacs, inspired by modern UI component frameworks.**

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Emacs](https://img.shields.io/badge/Emacs-26.1+-purple.svg)](https://www.gnu.org/software/emacs/)

English | [中文](README-zh-CN.md)

</div>

---

## 📑 Table of Contents

- [Overview](#-overview)
- [Features](#-features)
- [Installation](#-installation)
- [Quick Start](#-quick-start)
- [Core Concepts](#-core-concepts)
  - [Simple Widgets](#simple-widgets-with-render)
  - [Composite Widgets](#composite-widgets-with-setup-and-template)
  - [Property System](#property-system)
  - [Slot System](#slot-system)
  - [Widget Inheritance](#widget-inheritance)
- [Reactive System](#-reactive-system)
- [Event System](#-event-system)
- [Built-in Widgets](#-built-in-widgets)
- [API Reference](#-api-reference)
- [Examples](#-examples)
- [Contributing](#-contributing)
- [License](#-license)

---

## 🌟 Overview

**twidget** (Text Widget) is a declarative text widget library for Emacs that revolutionizes how you build text-based user interfaces. Inspired by modern frontend frameworks like Vue.js and React, twidget brings component-based UI development to Emacs buffers.

### Core Capabilities

twidget enables you to:

- **Define Reusable Components**: Create self-contained text widgets with their own properties, slots, and rendering logic that can be reused across your Emacs applications
- **Build Reactive Interfaces**: Use `twidget-ref` to create reactive data bindings that automatically update the UI when data changes—no manual refresh needed
- **Handle User Interactions**: Bind click events and other interactions using Vue3-like `:on-click` syntax with support for inline expressions (`count++`, `flag=!flag`)
- **Compose Complex UIs**: Nest widgets within widgets, use named slots for flexible content injection, and extend existing widgets through inheritance
- **Iterate Over Data**: Use the `:for` directive to dynamically render lists of items from collections

### How It Works

```elisp
;; 1. Define a widget with reactive state
(define-twidget counter
  :setup (lambda (_props _slot)
           (list :count (twidget-ref 0)))           ; Reactive state
  :template '(div
              (span "Count: {count} ")               ; Data binding
              (span :on-click "count++" "[+]")))     ; Event handling

;; 2. Render the widget in a buffer
(tp-pop-to-buffer "*demo*"
  (twidget-insert '(counter)))

;; 3. Click [+] to increment—UI updates automatically!
```

### Why twidget?

| Benefit | Description |
|---------|-------------|
| 🎯 **Declarative** | Define *what* your widget looks like, not *how* to render it step-by-step |
| 🧩 **Composable** | Build complex UIs by combining simple, focused widgets |
| ⚡ **Reactive** | UI updates automatically when underlying data changes |
| 🔄 **Familiar** | If you know Vue.js or React, the patterns will feel natural |
| 📝 **Text-Native** | Designed specifically for Emacs text buffers with full text property support |

---

## ✨ Features

| Feature | Description |
|---------|-------------|
| 🏷️ **Property System** | Define required and optional properties with default values |
| 📦 **Slot System** | Single slot and named slots for flexible content composition |
| 🧬 **Widget Inheritance** | Extend parent widgets to create specialized variants |
| 🎨 **Text Properties** | Seamless integration with Emacs text properties via [tp](https://github.com/Kinneyzhang/tp) |
| ⚡ **Reactive Data** | Create reactive UIs with automatic updates using `twidget-ref` |
| 🧩 **Composite Widgets** | Build complex widgets using `:setup` and `:template` |
| 🖱️ **Event System** | Vue3-like declarative event binding with `:on-click` and inline expressions |
| 🔄 **Loop Directive** | Iterate over collections with `:for` directive |

---

## 📦 Installation

### Dependencies

This package requires the [tp](https://github.com/Kinneyzhang/tp) (text properties) library.

### Manual Installation

```bash
# Clone both repositories
git clone https://github.com/Kinneyzhang/tp.git
git clone https://github.com/Kinneyzhang/twidget.git
```

```elisp
;; Add to your Emacs configuration
(add-to-list 'load-path "/path/to/tp")
(add-to-list 'load-path "/path/to/twidget")
(require 'twidget)
```

### Using use-package with straight.el

```elisp
(use-package tp
  :straight (:host github :repo "Kinneyzhang/tp"))

(use-package twidget
  :straight (:host github :repo "Kinneyzhang/twidget")
  :after tp)
```

### Using Quelpa

```elisp
(quelpa '(tp :fetcher github :repo "Kinneyzhang/tp"))
(quelpa '(twidget :fetcher github :repo "Kinneyzhang/twidget"))
(require 'twidget)
```

---

## 🚀 Quick Start

### Your First Widget

```elisp
;; Define a simple greeting widget
(define-twidget greeting
  :props '((name . "World"))
  :render (lambda (props _slot)
            (format "Hello, %s!\n" (plist-get props :name))))

;; Use the widget
(twidget-parse '(greeting))
;; => "Hello, World!\n"

(twidget-parse '(greeting :name "Emacs"))
;; => "Hello, Emacs!\n"
```

### Interactive Counter (Try it!)

Copy and evaluate this in Emacs to see a working interactive counter:

```elisp
(define-twidget counter
  :setup (lambda (_props _slot)
           (list :count (twidget-ref 0)))
  :template '(div
              (span "Count: {count} ")
              (span :on-click "count++" "[+]")
              (span " ")
              (span :on-click "count--" "[-]")
              (span " ")
              (span :on-click "count=0" "[Reset]")))

;; Display the counter in a buffer
(tp-pop-to-buffer "*counter-demo*"
  (twidget-insert '(counter)))
```

Click `[+]`, `[-]`, or `[Reset]` to interact with the counter!

---

## 📚 Core Concepts

twidget supports two ways to define widgets:

### Simple Widgets with `:render`

For widgets that directly produce output, use `:render`:

```elisp
(define-twidget my-text
  :props '((color . "black"))    ; Property with default value
  :slot t                         ; Accept slot content
  :render (lambda (props slot)
            ;; props: plist of properties (:color "black")
            ;; slot: the content passed to the widget
            (tp-set slot 'face `(:foreground ,(plist-get props :color)))))

;; Usage
(twidget-parse '(my-text :color "red" "Hello!"))
```

### Composite Widgets with `:setup` and `:template`

For widgets that compose other widgets with reactive state:

```elisp
(define-twidget toggle-button
  :props '((label . "Toggle"))
  :setup (lambda (props _slot)
           ;; Initialize reactive state
           (list :active (twidget-ref nil)
                 :buttonLabel (plist-get props :label)
                 ;; Define a function to compute text properties based on state
                 :getProps (lambda ()
                             (if (twidget-get 'active)
                                 '(face (:background "green" :foreground "white"))
                               '(face (:background "gray" :foreground "black"))))))
  :template '(span :on-click "active = !active"
                   :tp-props "getProps()"
                   "[{buttonLabel}: {active}]"))

;; Usage
(twidget-parse '(toggle-button :label "Dark Mode"))
```

### Property System

Properties define the configurable aspects of your widget:

```elisp
(define-twidget styled-box
  :props '(
    title               ; Required property (no default)
    (width . 20)        ; Optional with default value
    (border . t)        ; Boolean property
  )
  :slot t
  :render (lambda (props slot)
            (let ((title (plist-get props :title))
                  (width (plist-get props :width))
                  (border (plist-get props :border)))
              (if border
                  (format "┌%s┐\n│ %s │\n│ %s │\n└%s┘\n"
                          (make-string width ?─)
                          (format (format "%%-%ds" (- width 2)) title)
                          (format (format "%%-%ds" (- width 2)) slot)
                          (make-string width ?─))
                (format "%s\n%s\n" title slot)))))

;; Usage
(twidget-parse '(styled-box :title "Note" :width 30 "This is content"))
```

### Slot System

Slots allow you to pass content into widgets:

#### Single Slot

```elisp
(define-twidget wrapper
  :slot t    ; Enable single slot
  :render (lambda (_props slot)
            (concat "<<< " slot " >>>")))

(twidget-parse '(wrapper "Hello"))
;; => "<<< Hello >>>"
```

#### Named Slots

```elisp
(define-twidget card
  :slots '(header content footer)
  :render (lambda (_props slots)
            (concat
             "╭────────────────────╮\n"
             "│ " (or (plist-get slots :header) "Untitled") "\n"
             "├────────────────────┤\n"
             "│ " (or (plist-get slots :content) "") "\n"
             "├────────────────────┤\n"
             "│ " (or (plist-get slots :footer) "") "\n"
             "╰────────────────────╯\n")))

;; Usage with named slots
(twidget-parse
 '(card
   (slot-header "My Card Title")
   (slot-content "This is the main content.")
   (slot-footer "Footer information")))
```

### Widget Inheritance

Create specialized widgets by extending base widgets:

```elisp
;; Base widget
(define-twidget alert-base
  :props '((type . "info")
           (dismissible . nil))
  :slot t
  :render (lambda (props slot)
            (let ((icon (pcase (plist-get props :type)
                          ("info" "ℹ️")
                          ("warning" "⚠️")
                          ("error" "❌")
                          ("success" "✅")
                          (_ "📝"))))
              (format "%s %s\n" icon slot))))

;; Derived widget - inherits and overrides
(define-twidget error-alert
  :extends 'alert-base
  :props '((type . "error"))    ; Override default type
  :render (lambda (props slot parent-render)
            ;; Call parent render and add styling
            (let ((result (funcall parent-render props slot)))
              (tp-set result 'face '(:foreground "red")))))

;; Usage
(twidget-parse '(error-alert "Something went wrong!"))
;; => "❌ Something went wrong!\n" (with red foreground)
```

---

## ⚡ Reactive System

The reactive system allows your UI to automatically update when data changes.

### Creating Reactive Data

```elisp
;; In :setup function, use twidget-ref to create reactive values
:setup (lambda (_props _slot)
         (list :count (twidget-ref 0)
               :name (twidget-ref "Emacs")
               :items (twidget-ref '("apple" "banana" "orange"))))
```

### Binding Reactive Data in Templates

Use `{varname}` syntax in template strings:

```elisp
:template '(div
            (span "Hello, {name}!")
            (span "Count: {count}"))
```

### Accessing Nested Values

Use dot notation for nested access:

```elisp
:setup (lambda (_props _slot)
         (list :user (twidget-ref '(:name "John" :age 30))
               :items (twidget-ref '("a" "b" "c"))))

:template '(div
            (span "Name: {user.name}")    ; plist access
            (span "Age: {user.age}")
            (span "First: {items.0}"))    ; list index access
```

### Reactive API Reference

| Function | Description | Example |
|----------|-------------|---------|
| `twidget-ref` | Create reactive reference | `(twidget-ref 0)` |
| `twidget-get` | Get reactive value | `(twidget-get 'count)` |
| `twidget-set` | Set reactive value | `(twidget-set 'count 10)` |
| `twidget-inc` | Increment numeric value | `(twidget-inc 'count 1)` |
| `twidget-dec` | Decrement numeric value | `(twidget-dec 'count 1)` |

#### Accessing Nested Properties

```elisp
;; Get nested property
(twidget-get 'user :name)      ; Get :name from plist
(twidget-get 'items 0)         ; Get first element from list

;; Set nested property
(twidget-set 'user "Jane" :name)  ; Set :name in plist
(twidget-set 'items "x" 0)        ; Set first element in list
```

---

## 🖱️ Event System

The event system provides Vue3-like declarative event binding.

### Basic Click Handler

```elisp
(define-twidget click-demo
  :setup (lambda (_props _slot)
           (list :handleClick (lambda ()
                                (message "Clicked!"))))
  :template '(span :on-click "handleClick" "[Click Me]"))
```

### Expression Types

| Expression | Example | Description |
|------------|---------|-------------|
| Method reference | `:on-click "doSomething"` | Call method from `:setup` |
| Method with args | `:on-click "greet('Hello')"` | Method with arguments |
| Increment | `:on-click "count++"` | Increment reactive variable |
| Decrement | `:on-click "count--"` | Decrement reactive variable |
| Assignment | `:on-click "count=0"` | Assign value |
| Toggle | `:on-click "flag=!flag"` | Toggle boolean |
| Multi-statement | `:on-click "a++;b++"` | Multiple statements (`;` separated) |
| Ternary | `:on-click "flag ? on() : off()"` | Conditional execution |
| Logical AND | `:on-click "enabled && action()"` | Execute if truthy |
| Logical OR | `:on-click "!enabled \|\| warn()"` | Execute if falsy |

### Condition Operators

| Operator | Example | Description |
|----------|---------|-------------|
| `===` | `count === 0` | Strict equality |
| `==` | `count == 0` | Equality |
| `!=` | `count != 0` | Not equal |
| `>` | `count > 10` | Greater than |
| `<` | `count < 10` | Less than |
| `!` | `!flag` | Logical NOT |

### Complete Event Example

```elisp
(define-twidget todo-item
  :props '((text . ""))
  :setup (lambda (props _slot)
           (list :done (twidget-ref nil)
                 :text (plist-get props :text)
                 :toggle (lambda ()
                           (twidget-set 'done (not (twidget-get 'done))))))
  :template '(div
              (span :on-click "toggle"
                    "[{done}] {text}")))

(tp-pop-to-buffer "*todo-demo*"
  (twidget-insert
   '(div
     (todo-item :text "Learn Emacs Lisp")
     (todo-item :text "Build widgets")
     (todo-item :text "Create awesome UIs"))))
```

For more details, see [Event System Documentation](docs/event-system.md).

---

## 🎨 Reactive Text Properties

twidget supports reactive text properties through tp.el's property system using the `:tp-props` attribute. This allows you to set multiple tp.el text properties that can be bound to reactive values.

### Basic Usage

```elisp
(define-twidget toggle-button
  :props '((label . "Toggle"))
  :setup (lambda (props _slot)
           (list :active (twidget-ref nil)
                 :buttonLabel (plist-get props :label)
                 :getProps (lambda ()
                             (if (twidget-get 'active)
                                 '(face (:background "green" :foreground "white"))
                               '(face (:background "gray" :foreground "black"))))))
  :template '(span :on-click "active = !active"
                   :tp-props "getProps()"
                   "[{buttonLabel}: {active}]"))

(tp-pop-to-buffer "*toggle-demo*"
  (twidget-insert '(toggle-button :label "Dark Mode")))
```

Click the button to toggle between green and gray backgrounds!

### Static Properties

For static properties, pass a plist directly:

```elisp
(span :tp-props (face (:background "blue") tp-button (:palette info))
      "Click me")
```

### tp-props Value Types

The `:tp-props` attribute accepts:

| Type | Example | Description |
|------|---------|-------------|
| Plist | `:tp-props (face bold)` | Static property plist |
| Method call | `:tp-props "getProps()"` | Reactive - calls function from `:setup` |
| Variable ref | `:tp-props "propsVar"` | References variable from `:setup` |

---

## 🧱 Built-in Widgets

twidget comes with commonly used widgets out of the box:

| Widget | Description | Example |
|--------|-------------|---------|
| `p` | Paragraph (adds newline) | `(p "Text")` |
| `div` | Block container (adds newline) | `(div "Content")` |
| `span` | Inline container | `(span "Inline text")` |
| `h1` - `h5` | Headings (with tp-headline) | `(h1 "Title")` |
| `headline` | Base heading (configurable height) | `(headline :height 1.5 "Title")` |

### Heading Examples

```elisp
;; Using heading widgets
(twidget-parse '(h1 "Main Title"))
(twidget-parse '(h2 "Section Title"))
(twidget-parse '(h3 "Subsection"))

;; Custom heading height
(twidget-parse '(headline :height 2.5 "Large Title"))
```

### Nesting Widgets

```elisp
(twidget-parse
 '(div
   (h1 "Welcome")
   (p "This is a paragraph with "
      (span "inline content")
      " mixed in.")
   (div
    (p "Nested paragraph inside a div."))))
```

---

## 📖 API Reference

### Widget Definition

#### `define-twidget`

```elisp
(define-twidget NAME &rest ARGS)
```

Define a text widget named NAME.

| Keyword | Description |
|---------|-------------|
| `:props` | Property definitions: symbol (required) or `(symbol . default)` |
| `:slot` | `nil` (no slot), `t` (single slot), or `'(name1 name2 ...)` |
| `:slots` | Alias for `:slot` with named slots |
| `:extends` | Parent widget symbol to inherit from |
| `:render` | Render function for simple widgets |
| `:setup` | Setup function for composite widgets (returns reactive bindings) |
| `:template` | Template sexp for composite widgets |

### Widget Usage

#### `twidget-parse`

```elisp
(twidget-parse WIDGET-FORM) -> string
```

Parse and render a widget. Returns a string with text properties.

```elisp
(twidget-parse '(widget-name :prop1 val1 :prop2 val2 "slot content"))
```

#### `twidget-insert`

```elisp
(twidget-insert FORM)
```

Macro that parses and inserts widget at point. Automatically captures lexical variables for `:for` directives.

```elisp
(let ((items '("a" "b" "c")))
  (twidget-insert
   '(div (p :for "item in items" "- {item}"))))
```

### Utility Functions

| Function | Signature | Description |
|----------|-----------|-------------|
| `twidget-reset` | `()` | Clear all widget definitions |
| `twidget-clear-buffer-state` | `()` | Clear buffer-local reactive state |
| `twidget-ref` | `(value)` | Create reactive reference |
| `twidget-get` | `(sym &optional key)` | Get reactive value |
| `twidget-set` | `(sym value &optional key)` | Set reactive value |
| `twidget-inc` | `(sym num)` | Increment reactive value |
| `twidget-dec` | `(sym num)` | Decrement reactive value |

---

## 💡 Examples

### Badge Component

```elisp
(define-twidget badge
  :props '((type . "info"))
  :slot t
  :render (lambda (props slot)
            (let ((face (pcase (plist-get props :type)
                          ("info" '(:background "#3498db" :foreground "white"))
                          ("success" '(:background "#2ecc71" :foreground "white"))
                          ("warning" '(:background "#f39c12" :foreground "black"))
                          ("error" '(:background "#e74c3c" :foreground "white"))
                          (_ '(:background "#95a5a6" :foreground "white")))))
              (tp-set (format " %s " slot) 'face face))))

;; Usage
(twidget-parse '(badge :type "success" "OK"))
(twidget-parse '(badge :type "error" "Failed"))
(twidget-parse '(badge :type "warning" "Pending"))
```

### List with Loop

```elisp
(let ((fruits '("🍎 Apple" "🍌 Banana" "🍊 Orange" "🍇 Grape")))
  (tp-pop-to-buffer "*fruits*"
    (twidget-insert
     '(div
       (h2 "Fruit List")
       (p :for "fruit in fruits" "• {fruit}")))))
```

### Interactive Tabs

```elisp
(define-twidget tabs
  :setup (lambda (_props _slot)
           (list :active (twidget-ref 0)
                 :tab1 (lambda () (twidget-set 'active 0))
                 :tab2 (lambda () (twidget-set 'active 1))
                 :tab3 (lambda () (twidget-set 'active 2))))
  :template '(div
              (div
               (span :on-click "tab1" "[Tab 1]")
               (span " ")
               (span :on-click "tab2" "[Tab 2]")
               (span " ")
               (span :on-click "tab3" "[Tab 3]"))
              (p "Active tab: {active}")))

(tp-pop-to-buffer "*tabs-demo*"
  (twidget-insert '(tabs)))
```

### Form with Multiple Inputs

```elisp
(define-twidget step-input
  :props '((label . "Value") (step . 1))
  :setup (lambda (props _slot)
           (let ((step (plist-get props :step)))
             (list :value (twidget-ref 0)
                   :label (plist-get props :label)
                   :stepVal step
                   :increase (lambda () (twidget-inc 'value step))
                   :decrease (lambda () (twidget-dec 'value step)))))
  :template '(div
              (span "{label}: {value} ")
              (span :on-click "decrease" "[-]")
              (span " ")
              (span :on-click "increase" "[+]")))

(tp-pop-to-buffer "*form-demo*"
  (twidget-insert
   '(div
     (h2 "Settings")
     (step-input :label "Volume" :step 5)
     (step-input :label "Brightness" :step 10)
     (step-input :label "Speed" :step 1))))
```

---

## 🤝 Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.

### Development Setup

```bash
git clone https://github.com/Kinneyzhang/twidget.git
cd twidget
```

### Running Tests

```elisp
(require 'twidget)
(twidget-reset)  ; Clear state for fresh testing
```

---

## 📄 License

This project is licensed under the GNU General Public License v3.0 - see the [LICENSE](LICENSE) file for details.

---

## 🙏 Acknowledgments

- Inspired by modern UI component frameworks like [Vue.js](https://vuejs.org/) and [React](https://react.dev/)
- Built with Emacs Lisp for the [GNU Emacs](https://www.gnu.org/software/emacs/) text editor
- Uses [tp](https://github.com/Kinneyzhang/tp) for enhanced text property handling

---

<div align="center">

**Happy Widget Building! 🎉**

Made with ❤️ for the Emacs community

</div>
