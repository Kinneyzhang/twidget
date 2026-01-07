# Twidget 组件预编译设计文档

[English](#component-precompilation-design-document) | 中文

## 概述

Twidget 组件预编译系统是一个性能优化机制，在组件定义阶段（`define-twidget`）将模板编译成渲染函数，在组件渲染阶段（`twidget-parse`）直接调用预编译的渲染函数，从而减少运行时开销。

## 设计目标

1. **减少运行时解析开销** - 模板结构分析只在定义时执行一次
2. **优化 `:for` 指令处理** - 循环表达式在定义时预解析
3. **预编译事件处理器** - 事件表达式在定义时编译成 lambda
4. **保持向后兼容** - 现有 API 完全兼容

## 架构设计

### 组件定义流程

```
┌─────────────────────────────────────────────────────────────┐
│                    组件定义阶段                              │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  define-twidget                                             │
│       │                                                     │
│       ▼                                                     │
│  twidget-internal                                           │
│       │                                                     │
│       ├── 处理 :extends 继承                                │
│       │                                                     │
│       ├── 合并 props                                        │
│       │                                                     │
│       └── 编译模板 (如果有 :setup 和 :template)             │
│               │                                             │
│               ▼                                             │
│       twidget--compile-template                             │
│               │                                             │
│               ├── 字符串模板 → twidget--compile-template-string
│               │       └── 预解析占位符模式                   │
│               │                                             │
│               ├── 组件模板 → twidget--compile-template-widget
│               │       └── 预解析参数和事件                   │
│               │                                             │
│               └── 列表模板 → twidget--compile-template-list │
│                       └── 递归编译子元素                     │
│                                                             │
│  存储到 twidget-alist:                                      │
│    (:compiled-render <compiled-fn>                          │
│     :props ... :slot ... :template ... :setup ...)          │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

### 组件渲染流程

```
┌─────────────────────────────────────────────────────────────┐
│                    组件渲染阶段                              │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  twidget-parse                                              │
│       │                                                     │
│       ▼                                                     │
│  检查 :compiled-render                                      │
│       │                                                     │
│       ├── 有编译函数 → twidget--render-composite-compiled   │
│       │       │                                             │
│       │       ├── 调用 setup-fn 获取响应式绑定              │
│       │       │                                             │
│       │       ├── 处理响应式引用注册                        │
│       │       │                                             │
│       │       └── 调用预编译渲染函数                        │
│       │               ↓                                     │
│       │       (funcall compiled-render-fn                   │
│       │                bindings instance-id reactive-bindings)
│       │                                                     │
│       └── 无编译函数 → twidget--render-composite (回退)     │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

## 核心组件

### 模板编译函数

| 函数 | 描述 |
|------|------|
| `twidget--compile-template` | 主编译入口，根据模板类型分发 |
| `twidget--compile-template-string` | 编译字符串模板，预解析占位符 |
| `twidget--compile-template-widget` | 编译组件模板，预解析参数 |
| `twidget--compile-template-list` | 编译列表模板，递归编译子元素 |
| `twidget--precompile-widget-args` | 预解析组件参数 |

### 渲染函数

| 函数 | 描述 |
|------|------|
| `twidget--render-composite-compiled` | 使用预编译函数渲染复合组件 |
| `twidget--render-precompiled-widget` | 渲染预编译的组件模板 |
| `twidget--render-widget-with-props` | 使用预处理参数渲染组件 |

## 预编译内容

### 1. 字符串模板预编译

字符串模板在定义时预解析为部分列表：

```elisp
;; 原始模板
"Hello, {user.name}! Count: {count}"

;; 预解析结果
((:literal "Hello, ")
 (:placeholder "user.name" "user" (:name))
 (:literal "! Count: ")
 (:placeholder "count" "count" nil))
```

在运行时，只需遍历预解析的部分列表，无需重复执行正则匹配。

### 2. `:for` 指令预编译

`:for` 表达式在定义时预解析：

```elisp
;; 原始表达式
:for "item in items"

;; 预解析结果
("item" . "items")  ; (loop-var . collection-name)
```

### 3. 事件处理器预编译

事件处理器表达式在渲染时编译（需要 reactive-bindings 上下文），但参数预解析在定义时完成：

```elisp
;; 原始表达式
:on-click "count++"

;; 预解析存储
(click . "count++")  ; (event-type . handler-string)
```

## 性能优势

### 定义时开销（一次性）

- 模板结构分析
- 占位符正则匹配
- `:for` 表达式解析
- 参数预处理

### 运行时开销（每次渲染）

- 调用 setup 函数
- 响应式绑定处理
- 值替换（使用预解析的结构）
- 字符串拼接

### 优化效果

| 操作 | 优化前 | 优化后 |
|------|--------|--------|
| 模板解析 | 每次渲染 | 定义时一次 |
| 占位符匹配 | 每次渲染 | 定义时一次 |
| `:for` 解析 | 每次渲染 | 定义时一次 |
| 事件编译 | 每次渲染 | 定义时一次 |

## 向后兼容性

- 所有现有 API 保持不变
- 如果没有 `:compiled-render`，自动回退到原有的 `twidget--render-composite`
- 简单组件（使用 `:render`）不受影响

## 使用示例

```elisp
;; 定义组件 - 模板在此时编译
(define-twidget counter
  :setup (lambda (_props _slot)
           (list :count (twidget-ref 0)))
  :template '(div
              (span "Count: {count} ")
              (span :on-click "count++" "[+]")))

;; 渲染组件 - 使用预编译的渲染函数
(twidget-parse '(counter))
```

## 实现细节

### 编译函数结构

每个编译函数返回一个闭包，接受以下参数：
- `bindings` - 运行时绑定列表 `((var-name . value) ...)`
- `instance-id` - 组件实例 ID
- `reactive-bindings` - 来自 `:setup` 的响应式绑定 plist

```elisp
;; 编译后的字符串模板函数示例
(lambda (bindings instance-id _reactive-bindings)
  (let ((result ""))
    (dolist (part compiled-parts)
      (pcase (car part)
        (:literal (setq result (concat result (cadr part))))
        (:placeholder
         (let* ((binding (assoc (nth 2 part) bindings))
                (value (cdr binding)))
           (setq result (concat result (format "%s" value)))))))
    result))
```

---

# Component Precompilation Design Document

English | [中文](#twidget-组件预编译设计文档)

## Overview

The Twidget Component Precompilation System is a performance optimization mechanism that compiles templates into render functions at component definition time (`define-twidget`) and directly invokes the precompiled render functions at component rendering time (`twidget-parse`), thereby reducing runtime overhead.

## Design Goals

1. **Reduce Runtime Parsing Overhead** - Template structure analysis happens only once at definition time
2. **Optimize `:for` Directive Handling** - Loop expressions are pre-parsed at definition time
3. **Precompile Event Handlers** - Event expressions are compiled into lambdas at definition time
4. **Maintain Backward Compatibility** - Existing APIs remain fully compatible

## Architecture Design

### Component Definition Flow

```
┌─────────────────────────────────────────────────────────────┐
│                Component Definition Phase                    │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  define-twidget                                             │
│       │                                                     │
│       ▼                                                     │
│  twidget-internal                                           │
│       │                                                     │
│       ├── Handle :extends inheritance                       │
│       │                                                     │
│       ├── Merge props                                       │
│       │                                                     │
│       └── Compile template (if :setup and :template exist) │
│               │                                             │
│               ▼                                             │
│       twidget--compile-template                             │
│               │                                             │
│               ├── String → twidget--compile-template-string │
│               │       └── Pre-parse placeholder patterns    │
│               │                                             │
│               ├── Widget → twidget--compile-template-widget │
│               │       └── Pre-parse args and events         │
│               │                                             │
│               └── List → twidget--compile-template-list     │
│                       └── Recursively compile children      │
│                                                             │
│  Store in twidget-alist:                                    │
│    (:compiled-render <compiled-fn>                          │
│     :props ... :slot ... :template ... :setup ...)          │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

### Component Rendering Flow

```
┌─────────────────────────────────────────────────────────────┐
│                 Component Rendering Phase                    │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  twidget-parse                                              │
│       │                                                     │
│       ▼                                                     │
│  Check :compiled-render                                     │
│       │                                                     │
│       ├── Has compiled fn → twidget--render-composite-compiled
│       │       │                                             │
│       │       ├── Call setup-fn to get reactive bindings    │
│       │       │                                             │
│       │       ├── Process reactive ref registration         │
│       │       │                                             │
│       │       └── Call precompiled render function          │
│       │               ↓                                     │
│       │       (funcall compiled-render-fn                   │
│       │                bindings instance-id reactive-bindings)
│       │                                                     │
│       └── No compiled fn → twidget--render-composite (fallback)
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

## Core Components

### Template Compilation Functions

| Function | Description |
|----------|-------------|
| `twidget--compile-template` | Main compilation entry, dispatches by template type |
| `twidget--compile-template-string` | Compiles string templates, pre-parses placeholders |
| `twidget--compile-template-widget` | Compiles widget templates, pre-parses arguments |
| `twidget--compile-template-list` | Compiles list templates, recursively compiles children |
| `twidget--precompile-widget-args` | Pre-parses widget arguments |

### Rendering Functions

| Function | Description |
|----------|-------------|
| `twidget--render-composite-compiled` | Renders composite widgets using precompiled function |
| `twidget--render-precompiled-widget` | Renders precompiled widget templates |
| `twidget--render-widget-with-props` | Renders widgets with preprocessed arguments |

## What Gets Precompiled

### 1. String Template Precompilation

String templates are pre-parsed into a parts list at definition time:

```elisp
;; Original template
"Hello, {user.name}! Count: {count}"

;; Pre-parsed result
((:literal "Hello, ")
 (:placeholder "user.name" "user" (:name))
 (:literal "! Count: ")
 (:placeholder "count" "count" nil))
```

At runtime, just iterate through the pre-parsed parts list without repeated regex matching.

### 2. `:for` Directive Precompilation

`:for` expressions are pre-parsed at definition time:

```elisp
;; Original expression
:for "item in items"

;; Pre-parsed result
("item" . "items")  ; (loop-var . collection-name)
```

### 3. Event Handler Precompilation

Event handler expressions are compiled at render time (need reactive-bindings context), but argument pre-parsing happens at definition time:

```elisp
;; Original expression
:on-click "count++"

;; Pre-parsed storage
(click . "count++")  ; (event-type . handler-string)
```

## Performance Benefits

### Definition-Time Overhead (One-time)

- Template structure analysis
- Placeholder regex matching
- `:for` expression parsing
- Argument preprocessing

### Runtime Overhead (Per Render)

- Call setup function
- Reactive binding processing
- Value substitution (using pre-parsed structure)
- String concatenation

### Optimization Effect

| Operation | Before | After |
|-----------|--------|-------|
| Template parsing | Every render | Once at definition |
| Placeholder matching | Every render | Once at definition |
| `:for` parsing | Every render | Once at definition |
| Event compilation | Every render | Once at definition |

## Backward Compatibility

- All existing APIs remain unchanged
- Automatic fallback to original `twidget--render-composite` if no `:compiled-render`
- Simple widgets (using `:render`) are unaffected

## Usage Example

```elisp
;; Define widget - template is compiled at this point
(define-twidget counter
  :setup (lambda (_props _slot)
           (list :count (twidget-ref 0)))
  :template '(div
              (span "Count: {count} ")
              (span :on-click "count++" "[+]")))

;; Render widget - uses precompiled render function
(twidget-parse '(counter))
```

## Implementation Details

### Compiled Function Structure

Each compiled function returns a closure that accepts the following parameters:
- `bindings` - Runtime bindings alist `((var-name . value) ...)`
- `instance-id` - Widget instance ID
- `reactive-bindings` - Reactive bindings plist from `:setup`

```elisp
;; Example compiled string template function
(lambda (bindings instance-id _reactive-bindings)
  (let ((result ""))
    (dolist (part compiled-parts)
      (pcase (car part)
        (:literal (setq result (concat result (cadr part))))
        (:placeholder
         (let* ((binding (assoc (nth 2 part) bindings))
                (value (cdr binding)))
           (setq result (concat result (format "%s" value)))))))
    result))
```
