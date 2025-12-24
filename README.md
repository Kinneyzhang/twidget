# Twidget - Vue3-style Reactive Widget System for Emacs

<p align="center">
  <strong>A Vue3-style Composition API for creating reactive UI components in Emacs</strong>
</p>

<p align="center">
  <a href="#features">Features</a> •
  <a href="#installation">Installation</a> •
  <a href="#quick-start">Quick Start</a> •
  <a href="#api-reference">API Reference</a> •
  <a href="#built-in-components">Built-in Components</a> •
  <a href="#中文文档">中文文档</a>
</p>

---

## Overview

Twidget is a Vue3-style reactive widget system for Emacs. It provides a Composition API for creating reactive UI components that automatically update when their data changes.

The system consists of three main parts:

1. **twidget-reactive.el** - Core reactivity system (ref, reactive, computed, watch)
2. **twidget-component.el** - Vue3-style component system with Composition API
3. **tp.el** - Text property reactivity for buffer integration (optional dependency)

## Features

- **Vue3-style Composition API** - Familiar patterns for Vue.js developers
- **Reactive Data Binding** - Components automatically update when data changes
- **Props System** - Support `$` prefix for global reactive variable binding
- **Internal State** - Local state management with `twidget-ref`
- **Computed Properties** - Derived values with automatic dependency tracking
- **Watch Functions** - React to data changes with callbacks
- **Lifecycle Hooks** - `mounted` and `unmounted` hooks for component lifecycle
- **Built-in Components** - Ready-to-use `button`, `text`, and `input` components

## Installation

```elisp
;; Add to load-path
(add-to-list 'load-path "/path/to/twidget")
(require 'twidget)

;; Optional: for text property reactivity
(add-to-list 'load-path "/path/to/tp")
(require 'tp)
```

Or with `use-package`:

```elisp
(use-package twidget
  :load-path "/path/to/twidget")
```

## Quick Start

### Basic Component

```elisp
;; Define a simple greeting component
(twidget-define-component greeting
  :props (name)
  :render (lambda (props _state)
            (format "Hello, %s!" (or (plist-get props :name) "World"))))

;; Use the component
(twidget 'greeting :name "Emacs")
;; => "Hello, Emacs!"

;; Insert into buffer (mounted and reactive)
(twidget-insert 'greeting :name "User")
```

### Component with State

```elisp
;; Define a counter component with internal state
(twidget-define-component counter
  :props (initial-value)
  :setup (lambda (props)
           (let ((count (twidget-ref (or (plist-get props :initial-value) 0))))
             (list :count count
                   :increment (lambda () (twidget-ref-inc count))
                   :decrement (lambda () (twidget-ref-dec count)))))
  :render (lambda (_props state)
            (format "Count: %d" (twidget-ref-get (plist-get state :count)))))

;; Create and use
(let ((instance (twidget-create-instance 'counter :initial-value 5)))
  (funcall (plist-get (twidget-instance-state instance) :increment))
  (twidget--render-instance instance))
;; => "Count: 6"
```

### Reactive Props with Global Variables

```elisp
;; Define a global reactive variable
(defvar $theme-color "blue")

;; Component using reactive prop
(twidget-define-component themed-text
  :props (color)
  :render (lambda (props _state)
            (let ((color (plist-get props :color)))
              (propertize "Themed Text" 'face `(:foreground ,color)))))

;; Use with reactive binding - component updates when $theme-color changes
(twidget-insert 'themed-text :color $theme-color)

;; Later, change the global variable
(setq $theme-color "red")
;; Component automatically updates!
```

## API Reference

### Component Definition

#### `twidget-define-component`

```elisp
(twidget-define-component NAME &key props setup render mounted unmounted emits)
```

Define a component named NAME with Vue3-style Composition API.

**Parameters:**
- `:props` - List of prop names the component accepts
- `:setup` - Function receiving `(props)`, returns plist of state/methods
- `:render` - Function receiving `(props state)`, returns string to display
- `:mounted` - Function called after component is mounted, receives `(instance)`
- `:unmounted` - Function called before unmount, receives `(instance)`
- `:emits` - List of event names the component can emit

**Example:**

```elisp
(twidget-define-component my-button
  :props (label disabled)
  :setup (lambda (props)
           (let ((click-count (twidget-ref 0)))
             (list :click-count click-count
                   :on-click (lambda ()
                               (unless (plist-get props :disabled)
                                 (twidget-ref-inc click-count))))))
  :render (lambda (props state)
            (let ((label (or (plist-get props :label) "Button"))
                  (disabled (plist-get props :disabled)))
              (propertize (format "[%s]" label)
                          'face (if disabled 'shadow 'button)))))
```

### Component Usage

#### `twidget`

```elisp
(twidget COMPONENT-NAME &rest PROPS)
```

Create and return a string representation of a component without mounting.

```elisp
(twidget 'greeting :name "World")
;; => "Hello, World!"
```

#### `twidget-insert`

```elisp
(twidget-insert COMPONENT-NAME &rest PROPS)
```

Insert a component at point and return the mounted instance.

```elisp
(twidget-insert 'my-button :label "Click Me")
```

#### `twidget-create-instance`

```elisp
(twidget-create-instance COMPONENT-NAME &rest PROPS)
```

Create a component instance without mounting.

#### `twidget-mount`

```elisp
(twidget-mount INSTANCE &optional BUFFER POINT)
```

Mount an instance into a buffer at a position.

#### `twidget-unmount`

```elisp
(twidget-unmount INSTANCE)
```

Unmount an instance and clean up resources.

### Composition API Helpers

#### `twidget-use-state`

```elisp
(twidget-use-state INITIAL-VALUE)
```

Create a reactive state ref. Alias for `twidget-ref`.

```elisp
(let ((count (twidget-use-state 0)))
  (twidget-ref-inc count)
  (twidget-ref-get count))
;; => 1
```

#### `twidget-use-computed`

```elisp
(twidget-use-computed GETTER &optional SETTER)
```

Create a computed value. Alias for `twidget-computed`.

```elisp
(let* ((a (twidget-ref 2))
       (b (twidget-ref 3))
       (sum (twidget-use-computed
             (lambda ()
               (+ (twidget-ref-get a) (twidget-ref-get b))))))
  (twidget-computed-get sum))
;; => 5
```

#### `twidget-use-watch`

```elisp
(twidget-use-watch SOURCE CALLBACK &optional OPTIONS)
```

Watch a source for changes. Alias for `twidget-watch`.

#### `twidget-use-effect`

```elisp
(twidget-use-effect EFFECT-FN &optional OPTIONS)
```

Create a reactive effect. Alias for `twidget-watch-effect`.

#### `twidget-on-cleanup`

```elisp
(twidget-on-cleanup FN)
```

Register a cleanup function to be called when the component unmounts.
Must be called within a setup function.

```elisp
:setup (lambda (props)
         (let ((timer (run-with-timer 1 1 #'do-something)))
           (twidget-on-cleanup (lambda () (cancel-timer timer)))
           (list :timer timer)))
```

### Reactivity System

The reactivity system (`twidget-reactive.el`) provides Vue3-style reactive primitives:

- `twidget-ref` - Reactive reference for single values
- `twidget-reactive` - Reactive object for structured data
- `twidget-computed` - Computed property with caching
- `twidget-watch` - Watcher for reacting to changes
- `twidget-effect` - Side effect that auto-tracks dependencies

For detailed documentation, see `twidget-reactive.md`.

## Built-in Components

### twidget-text

Display text with optional face and help-echo.

**Props:**
- `:content` - Text content to display
- `:face` - Face for styling
- `:help-echo` - Tooltip text

```elisp
(twidget 'twidget-text :content "Hello" :face 'bold)
```

### twidget-button

Interactive button with click handling.

**Props:**
- `:label` - Button text
- `:disabled` - Whether button is disabled
- `:on-click` - Click handler function
- `:face` - Custom face

```elisp
(twidget 'twidget-button
         :label "Submit"
         :on-click (lambda () (message "Clicked!")))
```

### twidget-input

Text input field with value binding.

**Props:**
- `:value` - Initial/current value
- `:placeholder` - Placeholder text
- `:on-change` - Change handler receiving new value
- `:disabled` - Whether input is disabled
- `:width` - Display width

```elisp
(twidget 'twidget-input
         :value ""
         :placeholder "Enter text..."
         :on-change (lambda (val) (message "Value: %s" val)))
```

## Examples

### Todo List Component

```elisp
(twidget-define-component todo-item
  :props (text completed on-toggle on-delete)
  :render (lambda (props _state)
            (let ((text (plist-get props :text))
                  (completed (plist-get props :completed)))
              (concat
               (if completed "[x] " "[ ] ")
               (if completed
                   (propertize text 'face 'shadow)
                 text)))))

(twidget-define-component todo-list
  :props ()
  :setup (lambda (_props)
           (let ((items (twidget-ref '((:text "Learn Emacs" :completed nil)
                                       (:text "Write code" :completed t)))))
             (list :items items
                   :add-item (lambda (text)
                               (twidget-ref-set 
                                items 
                                (append (twidget-ref-get items)
                                        (list (list :text text :completed nil))))))))
  :render (lambda (_props state)
            (let ((items (twidget-ref-get (plist-get state :items))))
              (mapconcat
               (lambda (item)
                 (twidget 'todo-item
                          :text (plist-get item :text)
                          :completed (plist-get item :completed)))
               items
               "\n"))))
```

### Progress Bar Component

```elisp
(twidget-define-component progress-bar
  :props (value max width)
  :render (lambda (props _state)
            (let* ((value (or (plist-get props :value) 0))
                   (max (or (plist-get props :max) 100))
                   (width (or (plist-get props :width) 20))
                   (filled (round (* width (/ (float value) max))))
                   (empty (- width filled)))
              (concat "["
                      (propertize (make-string filled ?█) 'face 'success)
                      (make-string empty ?░)
                      "] "
                      (format "%d%%" (round (* 100 (/ (float value) max))))))))

;; Usage
(twidget 'progress-bar :value 75 :max 100 :width 30)
;; => "[██████████████████████░░░░░░░░] 75%"
```

---

# 中文文档

## 概述

Twidget 是一个为 Emacs 设计的 Vue3 风格响应式组件系统。它提供了一套组合式 API，用于创建当数据变化时自动更新的响应式 UI 组件。

系统由三个主要部分组成：

1. **twidget-reactive.el** - 核心响应式系统（ref、reactive、computed、watch）
2. **twidget-component.el** - Vue3 风格的组件系统和组合式 API
3. **tp.el** - 文本属性响应式，用于缓冲区集成（可选依赖）

## 功能特性

- **Vue3 风格组合式 API** - 对 Vue.js 开发者友好的设计模式
- **响应式数据绑定** - 组件在数据变化时自动更新
- **Props 系统** - 支持使用 `$` 前缀绑定全局响应式变量
- **内部状态** - 使用 `twidget-ref` 进行局部状态管理
- **计算属性** - 具有自动依赖追踪的派生值
- **侦听器** - 通过回调响应数据变化
- **生命周期钩子** - `mounted` 和 `unmounted` 组件生命周期钩子
- **内置组件** - 开箱即用的 `button`、`text` 和 `input` 组件

## 安装

```elisp
;; 添加到 load-path
(add-to-list 'load-path "/path/to/twidget")
(require 'twidget)

;; 可选：用于文本属性响应式
(add-to-list 'load-path "/path/to/tp")
(require 'tp)
```

## 快速开始

### 基本组件

```elisp
;; 定义一个简单的问候组件
(twidget-define-component greeting
  :props (name)
  :render (lambda (props _state)
            (format "你好, %s!" (or (plist-get props :name) "世界"))))

;; 使用组件
(twidget 'greeting :name "Emacs")
;; => "你好, Emacs!"

;; 插入到缓冲区（挂载并响应式）
(twidget-insert 'greeting :name "用户")
```

### 带状态的组件

```elisp
;; 定义一个带内部状态的计数器组件
(twidget-define-component counter
  :props (initial-value)
  :setup (lambda (props)
           (let ((count (twidget-ref (or (plist-get props :initial-value) 0))))
             (list :count count
                   :increment (lambda () (twidget-ref-inc count))
                   :decrement (lambda () (twidget-ref-dec count)))))
  :render (lambda (_props state)
            (format "计数: %d" (twidget-ref-get (plist-get state :count)))))
```

### 使用全局变量的响应式 Props

```elisp
;; 定义一个全局响应式变量
(defvar $theme-color "blue")

;; 使用响应式 prop 的组件
(twidget-define-component themed-text
  :props (color)
  :render (lambda (props _state)
            (let ((color (plist-get props :color)))
              (propertize "主题文本" 'face `(:foreground ,color)))))

;; 使用响应式绑定 - 当 $theme-color 变化时组件会更新
(twidget-insert 'themed-text :color $theme-color)

;; 之后，改变全局变量
(setq $theme-color "red")
;; 组件自动更新！
```

## API 参考

### 组件定义

#### `twidget-define-component`

```elisp
(twidget-define-component NAME &key props setup render mounted unmounted emits)
```

使用 Vue3 风格组合式 API 定义名为 NAME 的组件。

**参数:**
- `:props` - 组件接受的 prop 名称列表
- `:setup` - 接收 `(props)` 的函数，返回状态/方法的 plist
- `:render` - 接收 `(props state)` 的函数，返回要显示的字符串
- `:mounted` - 组件挂载后调用的函数，接收 `(instance)`
- `:unmounted` - 卸载前调用的函数，接收 `(instance)`
- `:emits` - 组件可以发出的事件名称列表

### 组件使用

#### `twidget`

```elisp
(twidget COMPONENT-NAME &rest PROPS)
```

创建并返回组件的字符串表示，不挂载。

#### `twidget-insert`

```elisp
(twidget-insert COMPONENT-NAME &rest PROPS)
```

在光标处插入组件并返回挂载的实例。

#### `twidget-mount`

```elisp
(twidget-mount INSTANCE &optional BUFFER POINT)
```

将实例挂载到缓冲区的指定位置。

#### `twidget-unmount`

```elisp
(twidget-unmount INSTANCE)
```

卸载实例并清理资源。

### 组合式 API 辅助函数

#### `twidget-use-state`

创建响应式状态 ref。

#### `twidget-use-computed`

创建计算值。

#### `twidget-use-watch`

侦听源的变化。

#### `twidget-use-effect`

创建响应式副作用。

#### `twidget-on-cleanup`

注册组件卸载时调用的清理函数。

## 内置组件

### twidget-text

显示带有可选 face 和 help-echo 的文本。

**Props:**
- `:content` - 要显示的文本内容
- `:face` - 样式 face
- `:help-echo` - 提示文本

### twidget-button

带点击处理的交互按钮。

**Props:**
- `:label` - 按钮文本
- `:disabled` - 是否禁用
- `:on-click` - 点击处理函数
- `:face` - 自定义 face

### twidget-input

带值绑定的文本输入字段。

**Props:**
- `:value` - 初始/当前值
- `:placeholder` - 占位符文本
- `:on-change` - 接收新值的变化处理函数
- `:disabled` - 是否禁用
- `:width` - 显示宽度

## 与 Vue3 的对比

| Vue3 | Twidget | 说明 |
|------|---------|------|
| `defineComponent()` | `twidget-define-component` | 定义组件 |
| `props` | `:props` | 组件属性 |
| `setup()` | `:setup` | 组合式 API 入口 |
| `ref()` | `twidget-ref` / `twidget-use-state` | 响应式引用 |
| `computed()` | `twidget-computed` / `twidget-use-computed` | 计算属性 |
| `watch()` | `twidget-watch` / `twidget-use-watch` | 侦听器 |
| `onMounted()` | `:mounted` | 挂载钩子 |
| `onUnmounted()` | `:unmounted` | 卸载钩子 |
| `$emit()` | `twidget-emit` | 发出事件 |

## 许可证

MIT License

---

<p align="center">
  <em>Twidget - 为 Emacs 带来 Vue3 风格的响应式组件系统</em>
</p>
