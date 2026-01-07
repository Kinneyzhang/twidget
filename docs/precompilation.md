# Twidget 模板编译系统设计文档

[English](#template-compilation-system-design-document) | 中文

## 概述

Twidget 模板编译系统采用类似 Vue 3 的编译策略：在组件定义阶段（`define-twidget`）将模板编译成**可执行的 Elisp 渲染函数**。该渲染函数包含实际的循环（`dolist`）、条件判断（`when`/`if`）等 Elisp 代码，而不是运行时解释模板结构。

## 设计目标

1. **生成真正的渲染函数** - 模板编译成包含实际 Elisp 代码的函数
2. **指令系统可扩展** - 通过注册机制支持新增指令（`:for`、`:if`、`:show` 等）
3. **编译时处理指令** - 所有指令在编译阶段转换为对应的 Elisp 代码
4. **减少运行时开销** - 渲染时只执行预编译的代码

## 核心架构

### 指令编译器注册表

```elisp
(defvar twidget--directive-compilers (make-hash-table :test 'eq)
  "指令编译器哈希表。
键是指令关键字（如 :for, :if）。
值是编译器函数。")
```

每个指令编译器的签名：

```elisp
(lambda (directive-value widget-name remaining-args body-code)
  ;; 返回包装了 body-code 的 Elisp 代码
  ...)
```

### 已注册的指令

| 指令 | 描述 | 生成的代码 |
|------|------|-----------|
| `:for` | 循环遍历集合 | `dolist` 循环 |
| `:if` | 条件渲染 | `if` 表达式 |
| `:show` | 显示/隐藏控制 | `if` 表达式 |

### 编译流程

```
┌─────────────────────────────────────────────────────────────┐
│                    模板编译流程                              │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  模板: '(div (span :for "item in items" :if "visible"      │
│                    "{item}"))                               │
│       │                                                     │
│       ▼                                                     │
│  twidget--compile-template-to-render-fn                     │
│       │                                                     │
│       ▼                                                     │
│  twidget--generate-render-code                              │
│       │                                                     │
│       ▼                                                     │
│  twidget--generate-template-expr                            │
│       │                                                     │
│       ├── 字符串 → 直接返回或生成替换代码                   │
│       │                                                     │
│       ├── 组件 → twidget--generate-widget-expr              │
│       │       │                                             │
│       │       ├── 提取指令 (twidget--extract-directives)    │
│       │       │                                             │
│       │       └── 生成嵌套指令代码                          │
│       │           (twidget--generate-widget-with-directives)│
│       │                                                     │
│       └── 列表 → twidget--generate-list-expr                │
│                                                             │
│  生成的渲染函数:                                            │
│  (lambda (bindings instance-id reactive-bindings)           │
│    (let ((--result-- ""))                                   │
│      (when (twidget--compile-eval-condition "visible" ...)  │
│        (dolist (--item-- (cdr (assoc "items" bindings)))    │
│          (let ((bindings (cons (cons "item" --item--) ...)))│
│            (setq --result-- (concat --result-- ...)))))     │
│      --result--))                                           │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

## 指令系统

### 注册新指令

```elisp
(twidget--register-directive-compiler
 :my-directive
 (lambda (value widget-name other-args body-code)
   ;; value: 指令的值（如 ":for" 的 "item in items"）
   ;; widget-name: 组件名称
   ;; other-args: 除指令外的其他参数
   ;; body-code: 内部渲染代码
   `(my-wrapper
     ,body-code)))
```

### :for 指令编译器

输入：
```elisp
(span :for "item in items" "{item}")
```

生成的代码：
```elisp
(let ((--result-- "")
      (--collection-- (cdr (assoc "items" bindings))))
  (if (listp --collection--)
      (dolist (--item-- --collection--)
        (let ((bindings (cons (cons "item" --item--) bindings)))
          (setq --result-- (concat --result-- <inner-render>))))
    (warn "twidget: :for collection `items' is not a list"))
  --result--)
```

### :if 指令编译器

输入：
```elisp
(span :if "visible" "Hello")
```

生成的代码：
```elisp
(if (twidget--compile-eval-condition "visible" bindings reactive-bindings)
    <inner-render>
  "")
```

### 多指令组合

指令按顺序嵌套，外层指令包装内层：

```elisp
(span :if "visible" :for "item in items" "{item}")
```

生成（:if 包装 :for）：
```elisp
(if (twidget--compile-eval-condition "visible" ...)
    (let ((--result-- ""))
      (dolist (--item-- ...)
        ...)
      --result--)
  "")
```

## 条件表达式

`:if` 和 `:show` 指令支持的条件表达式：

| 表达式 | 示例 | 说明 |
|--------|------|------|
| 变量 | `"visible"` | 变量值是否为真 |
| 取反 | `"!visible"` | 变量值是否为假 |
| 相等 | `"count === 0"` | 严格相等比较 |
| 不等 | `"count != 0"` | 不相等比较 |
| 大于/小于 | `"count > 10"` | 数值比较 |

## 性能优势

| 方面 | 传统方式 | 编译方式 |
|------|----------|----------|
| 模板解析 | 每次渲染 | 定义时一次 |
| :for 处理 | 运行时解析表达式 | 直接执行 dolist |
| :if 处理 | 运行时评估条件 | 直接执行 if |
| 代码路径 | 多层函数调用 | 内联代码执行 |

## 扩展指南

### 添加新指令

1. 定义编译器函数：

```elisp
(defun my-directive-compiler (value widget-name other-args body-code)
  "编译 :my-directive 指令。"
  `(my-custom-wrapper
     :value ,value
     :body ,body-code))
```

2. 注册编译器：

```elisp
(twidget--register-directive-compiler :my-directive #'my-directive-compiler)
```

3. 使用：

```elisp
(define-twidget my-widget
  :template '(div (span :my-directive "some-value" "content")))
```

---

# Template Compilation System Design Document

English | [中文](#twidget-模板编译系统设计文档)

## Overview

The Twidget Template Compilation System adopts a Vue 3-like compilation strategy: at component definition time (`define-twidget`), templates are compiled into **executable Elisp render functions**. These render functions contain actual Elisp code like loops (`dolist`) and conditionals (`when`/`if`), rather than runtime template interpretation.

## Design Goals

1. **Generate Real Render Functions** - Templates compile into functions with actual Elisp code
2. **Extensible Directive System** - Support new directives (`:for`, `:if`, `:show`, etc.) via registration
3. **Compile-Time Directive Processing** - All directives are transformed into corresponding Elisp code at compile time
4. **Reduce Runtime Overhead** - Only execute pre-compiled code during rendering

## Core Architecture

### Directive Compiler Registry

```elisp
(defvar twidget--directive-compilers (make-hash-table :test 'eq)
  "Hash table of directive compilers.
Keys are directive keywords (e.g., :for, :if).
Values are compiler functions.")
```

Each directive compiler signature:

```elisp
(lambda (directive-value widget-name remaining-args body-code)
  ;; Returns Elisp code wrapping body-code
  ...)
```

### Registered Directives

| Directive | Description | Generated Code |
|-----------|-------------|----------------|
| `:for` | Loop over collection | `dolist` loop |
| `:if` | Conditional rendering | `if` expression |
| `:show` | Show/hide control | `if` expression |

### Compilation Flow

```
┌─────────────────────────────────────────────────────────────┐
│                 Template Compilation Flow                    │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  Template: '(div (span :for "item in items" :if "visible"  │
│                        "{item}"))                           │
│       │                                                     │
│       ▼                                                     │
│  twidget--compile-template-to-render-fn                     │
│       │                                                     │
│       ▼                                                     │
│  twidget--generate-render-code                              │
│       │                                                     │
│       ▼                                                     │
│  twidget--generate-template-expr                            │
│       │                                                     │
│       ├── String → Return literal or generate substitution  │
│       │                                                     │
│       ├── Widget → twidget--generate-widget-expr            │
│       │       │                                             │
│       │       ├── Extract directives                        │
│       │       │   (twidget--extract-directives)             │
│       │       │                                             │
│       │       └── Generate nested directive code            │
│       │           (twidget--generate-widget-with-directives)│
│       │                                                     │
│       └── List → twidget--generate-list-expr                │
│                                                             │
│  Generated Render Function:                                 │
│  (lambda (bindings instance-id reactive-bindings)           │
│    (let ((--result-- ""))                                   │
│      (when (twidget--compile-eval-condition "visible" ...)  │
│        (dolist (--item-- (cdr (assoc "items" bindings)))    │
│          (let ((bindings (cons (cons "item" --item--) ...)))│
│            (setq --result-- (concat --result-- ...)))))     │
│      --result--))                                           │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

## Directive System

### Registering New Directives

```elisp
(twidget--register-directive-compiler
 :my-directive
 (lambda (value widget-name other-args body-code)
   ;; value: directive value (e.g., "item in items" for :for)
   ;; widget-name: widget name
   ;; other-args: remaining arguments excluding directives
   ;; body-code: inner rendering code
   `(my-wrapper
     ,body-code)))
```

### :for Directive Compiler

Input:
```elisp
(span :for "item in items" "{item}")
```

Generated code:
```elisp
(let ((--result-- "")
      (--collection-- (cdr (assoc "items" bindings))))
  (if (listp --collection--)
      (dolist (--item-- --collection--)
        (let ((bindings (cons (cons "item" --item--) bindings)))
          (setq --result-- (concat --result-- <inner-render>))))
    (warn "twidget: :for collection `items' is not a list"))
  --result--)
```

### :if Directive Compiler

Input:
```elisp
(span :if "visible" "Hello")
```

Generated code:
```elisp
(if (twidget--compile-eval-condition "visible" bindings reactive-bindings)
    <inner-render>
  "")
```

### Multiple Directive Composition

Directives are nested in order, outer directives wrap inner ones:

```elisp
(span :if "visible" :for "item in items" "{item}")
```

Generates (:if wrapping :for):
```elisp
(if (twidget--compile-eval-condition "visible" ...)
    (let ((--result-- ""))
      (dolist (--item-- ...)
        ...)
      --result--)
  "")
```

## Condition Expressions

Condition expressions supported by `:if` and `:show`:

| Expression | Example | Description |
|------------|---------|-------------|
| Variable | `"visible"` | Check if variable is truthy |
| Negation | `"!visible"` | Check if variable is falsy |
| Equality | `"count === 0"` | Strict equality comparison |
| Inequality | `"count != 0"` | Inequality comparison |
| Greater/Less | `"count > 10"` | Numeric comparison |

## Performance Benefits

| Aspect | Traditional | Compiled |
|--------|-------------|----------|
| Template parsing | Every render | Once at definition |
| :for processing | Parse expression at runtime | Direct dolist execution |
| :if processing | Evaluate condition at runtime | Direct if execution |
| Code path | Multiple function calls | Inline code execution |

## Extension Guide

### Adding New Directives

1. Define the compiler function:

```elisp
(defun my-directive-compiler (value widget-name other-args body-code)
  "Compile :my-directive directive."
  `(my-custom-wrapper
     :value ,value
     :body ,body-code))
```

2. Register the compiler:

```elisp
(twidget--register-directive-compiler :my-directive #'my-directive-compiler)
```

3. Use in templates:

```elisp
(define-twidget my-widget
  :template '(div (span :my-directive "some-value" "content")))
```
