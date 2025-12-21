# twidget.el - Text Widget Component Library for Emacs

<p align="center">
  <strong>A Vue3-inspired component library for building text-based UI in Emacs</strong>
</p>

<p align="center">
  <a href="#features">Features</a> •
  <a href="#installation">Installation</a> •
  <a href="#quick-start">Quick Start</a> •
  <a href="#api-reference">API Reference</a> •
  <a href="#examples">Examples</a>
</p>

---

## Overview

**twidget.el** is a component library for Emacs that brings Vue3-inspired Composition API patterns to text-based UI development. It enables you to build reactive, reusable UI components using plain text and text properties.

### Core Features

1. **Reactive Text System**: Text that automatically updates when variables change, using text properties for identification (like tp.el)
2. **Component Definition**: Define reusable UI components with setup and render functions
3. **Incremental Buffer Updates**: Only update changed parts of the buffer by searching for text with reactive properties
4. **Vue3-style Composition API**: Familiar patterns like `ref`, `computed`, `watch`
5. **tp.el Integration**: Optional integration with tp.el for reactive text properties

## Requirements

- **Emacs 28.1+**
- **dash.el** (list manipulation)
- **tp.el** (optional, for reactive text properties)

## Installation

```elisp
;; Add to load-path
(add-to-list 'load-path "/path/to/twidget")
(require 'twidget)
```

Or with `use-package`:

```elisp
(use-package twidget
  :load-path "/path/to/twidget")
```

## Quick Start

### Creating a Reactive Value

```elisp
;; Create a reactive ref
(setq my-counter (twidget-ref 0))

;; Get value
(twidget-ref-value my-counter)  ;; => 0

;; Set value - triggers updates
(twidget-ref-set my-counter 5)

;; Watch for changes
(twidget-watch my-counter
  (lambda (new old ref)
    (message "Counter changed from %d to %d" old new)))
```

### Creating Reactive Text

```elisp
;; Insert reactive text that updates automatically
(with-current-buffer (get-buffer-create "*twidget-demo*")
  (erase-buffer)
  (setq my-name (twidget-ref "World"))
  (insert "Hello, ")
  (twidget-insert-reactive my-name)
  (insert "!")
  (display-buffer (current-buffer)))

;; Change the value - text updates automatically!
(twidget-ref-set my-name "Emacs")
;; Buffer now shows: "Hello, Emacs!"
```

### Defining a Component

```elisp
(define-twidget greeting
  :props (name)
  :setup (lambda (props)
           (let* ((name (plist-get props :name))
                  (greeting-ref (twidget-ref (or name "World")))
                  (count-ref (twidget-ref 0)))
             (list :greeting greeting-ref
                   :count count-ref)))
  :render (lambda (props state)
            (let ((greeting (plist-get state :greeting))
                  (count (plist-get state :count)))
              (twidget-h
               "Hello, "
               (twidget-text greeting)
               "!\n"
               "Visit count: "
               (twidget-text count (lambda (v) (format "%d" v)))))))
```

### Rendering a Component

```elisp
(with-current-buffer (get-buffer-create "*greeting*")
  (erase-buffer)
  (setq greeting-instance
        (twidget-render 'greeting '(:name "Emacs")))
  (display-buffer (current-buffer)))

;; Update the component state
(let ((state (twidget-instance-state greeting-instance)))
  (twidget-ref-set (plist-get state :count) 42))
;; Buffer updates automatically!
```

## API Reference

### Reactive System

#### `twidget-ref`

```elisp
(twidget-ref INITIAL-VALUE &optional NAME)
```

Create a reactive ref with initial value. Returns a symbol that can be used to get/set the value.

```elisp
(setq counter (twidget-ref 0))
(setq named-ref (twidget-ref "hello" 'my-ref))
```

#### `twidget-ref-value`

```elisp
(twidget-ref-value REF)
```

Get the current value of a ref.

```elisp
(twidget-ref-value counter)  ;; => 0
```

#### `twidget-ref-set`

```elisp
(twidget-ref-set REF VALUE)
```

Set the value of a ref. Triggers reactive updates.

```elisp
(twidget-ref-set counter 10)
```

#### `twidget-watch`

```elisp
(twidget-watch REF CALLBACK)
```

Register a callback to be called when ref changes. Callback receives `(new-value old-value ref-symbol)`.

```elisp
(twidget-watch counter
  (lambda (new old ref)
    (message "Changed: %s -> %s" old new)))
```

#### `twidget-computed`

```elisp
(twidget-computed COMPUTE-FN &rest DEPS)
```

Create a computed ref that auto-updates when dependencies change.

```elisp
(setq first-name (twidget-ref "John"))
(setq last-name (twidget-ref "Doe"))
(setq full-name (twidget-computed
                  (lambda ()
                    (concat (twidget-ref-value first-name)
                            " "
                            (twidget-ref-value last-name)))
                  first-name last-name))
```

#### `twidget-unref`

```elisp
(twidget-unref REF)
```

Remove a reactive ref and its watchers.

### Reactive Text

#### `twidget-text`

```elisp
(twidget-text REF &optional FORMAT-FN)
```

Create reactive text bound to a ref. For use in render functions.

```elisp
(twidget-text counter (lambda (v) (format "Count: %d" v)))
```

#### `twidget-insert-reactive`

```elisp
(twidget-insert-reactive REF &optional FORMAT-FN PROPS)
```

Insert reactive text at point. The text is identified by unique text properties
(`twidget-ref`, `twidget-format-fn`, `twidget-text-id`) rather than buffer positions,
so it remains reactive even when the buffer is edited.
Returns start/end positions as a cons cell.

```elisp
(twidget-insert-reactive counter
  (lambda (v) (format "%d" v))
  '(face bold))
```

### Component Definition

#### `define-twidget`

```elisp
(define-twidget NAME
  :props (PROP-NAMES...)
  :setup SETUP-FN
  :render RENDER-FN)
```

Define a twidget component.

- **NAME**: Component name (symbol)
- **:props**: List of prop names the component accepts
- **:setup**: Function receiving props, returns reactive state
- **:render**: Function receiving props and state, returns virtual elements

### Rendering

#### `twidget-render`

```elisp
(twidget-render COMPONENT-NAME &optional PROPS BUFFER POINT)
```

Render a component. Returns instance ID.

```elisp
(twidget-render 'greeting '(:name "Emacs"))
```

#### `twidget-unmount`

```elisp
(twidget-unmount INSTANCE-ID)
```

Unmount a component instance.

### Virtual Elements

#### `twidget-h`

```elisp
(twidget-h &rest CHILDREN)
```

Create a virtual element (fragment) with children. Similar to Vue's `h()`.

```elisp
(twidget-h
  "Line 1\n"
  "Line 2\n"
  (twidget-text some-ref))
```

#### `twidget-span`

```elisp
(twidget-span PROPS &rest CHILDREN)
```

Create a span element with text properties.

```elisp
(twidget-span '(face bold)
  "Bold text")
```

### Utility Functions

#### `twidget-insert`

```elisp
(twidget-insert TEXT &optional PROPS)
```

Insert text with optional properties.

#### `twidget-clear-buffer-instances`

Clear all component instances in current buffer.

#### `twidget-reset`

Reset all twidget state (refs, instances, etc.).

## Built-in Components

### twidget-text-display

Simple text display component.

```elisp
(twidget-render 'twidget-text-display
  '(:value "Hello" :format-fn nil))
```

### twidget-button

Button component with click handler.

```elisp
(twidget-render 'twidget-button
  '(:label "Click Me"
    :on-click (lambda () (interactive) (message "Clicked!"))))
```

### twidget-progress

Progress bar component.

```elisp
(setq instance (twidget-render 'twidget-progress
                 '(:current 30 :total 100 :width 20)))
;; Updates automatically when :current or :total refs change
```

## Examples

### Counter Example

```elisp
(define-twidget counter
  :props (initial)
  :setup (lambda (props)
           (let ((count (twidget-ref (or (plist-get props :initial) 0))))
             (list :count count)))
  :render (lambda (props state)
            (let ((count (plist-get state :count)))
              (twidget-h
               "Count: "
               (twidget-text count (lambda (v) (format "%d" v)))
               "\n"))))

(with-current-buffer (get-buffer-create "*counter*")
  (erase-buffer)
  (setq counter-instance (twidget-render 'counter '(:initial 0)))
  (display-buffer (current-buffer)))

;; Increment counter
(let ((state (twidget-instance-state counter-instance)))
  (let ((count-ref (plist-get state :count)))
    (twidget-ref-set count-ref (1+ (twidget-ref-value count-ref)))))
```

### Todo List Example

```elisp
(define-twidget todo-item
  :props (text done)
  :setup (lambda (props)
           (let ((done (twidget-ref (plist-get props :done))))
             (list :done done)))
  :render (lambda (props state)
            (let ((text (plist-get props :text))
                  (done (plist-get state :done)))
              (twidget-h
               (twidget-text done (lambda (v) (if v "[x] " "[ ] ")))
               text
               "\n"))))

(with-current-buffer (get-buffer-create "*todos*")
  (erase-buffer)
  (setq todo1 (twidget-render 'todo-item '(:text "Buy milk" :done nil)))
  (setq todo2 (twidget-render 'todo-item '(:text "Write code" :done t)))
  (display-buffer (current-buffer)))

;; Toggle todo
(let* ((state (twidget-instance-state todo1))
       (done-ref (plist-get state :done)))
  (twidget-ref-set done-ref (not (twidget-ref-value done-ref))))
```

## tp.el Integration

twidget.el can optionally integrate with [tp.el](https://github.com/Kinneyzhang/tp) for reactive text properties:

```elisp
;; Check if tp.el is available
(twidget-tp-available-p)

;; Apply properties using tp.el when available
(twidget-with-tp-props "text" '(face bold))
```

When tp.el is loaded, twidget components can leverage tp.el's reactive text property layers for advanced styling.

## License

GNU General Public License v3 or later.

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

---

<p align="center">
  <em>twidget.el - Vue3-style components for Emacs</em>
</p>
