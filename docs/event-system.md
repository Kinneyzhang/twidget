# Twidget 事件系统设计文档

[English](#event-system-design-document) | 中文

## 概述

Twidget 事件系统是一个类似 Vue3 的声明式事件处理系统，允许在组件模板中通过 `:on-*` 语法绑定事件处理器。该系统支持多种语法格式，包括方法引用、带参数的方法调用、以及各种内联表达式。

## 设计原则

1. **声明式语法** - 事件处理器直接在模板中以字符串形式声明
2. **与 setup 集成** - 事件处理器可以访问 `:setup` 中定义的方法和响应式变量
3. **通用可扩展** - 事件系统设计为可扩展的，方便添加更多事件类型
4. **安全性** - 表达式在受控环境中执行

## 架构设计

### 核心组件

```
┌─────────────────────────────────────────────────────────────┐
│                     事件系统架构                             │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  ┌─────────────┐    ┌──────────────┐    ┌───────────────┐  │
│  │ 事件类型    │    │ 表达式解析器 │    │ 处理器编译器  │  │
│  │ 注册表      │───▶│              │───▶│               │  │
│  │             │    │              │    │               │  │
│  └─────────────┘    └──────────────┘    └───────────────┘  │
│        │                   │                    │          │
│        ▼                   ▼                    ▼          │
│  twidget-event-   twidget--parse-      twidget--compile-   │
│  types            event-expression      event-handler      │
│                                                             │
│  ┌──────────────────────────────────────────────────────┐  │
│  │                 模板展开流程                          │  │
│  │                                                       │  │
│  │  twidget--expand-template-widget                     │  │
│  │         │                                            │  │
│  │         ▼                                            │  │
│  │  twidget--process-template-args-with-events          │  │
│  │         │                                            │  │
│  │         ├──▶ 提取 :on-* 属性                         │  │
│  │         │                                            │  │
│  │         ├──▶ 编译事件处理器                          │  │
│  │         │                                            │  │
│  │         └──▶ 应用文本属性 (keymap, mouse-face)       │  │
│  │                                                       │  │
│  └──────────────────────────────────────────────────────┘  │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

### 事件类型注册表

```elisp
(defvar twidget-event-types
  '((click . (:map-property keymap
              :event-trigger mouse-1
              :doc "点击事件"))
    (mouse-enter . (:map-property keymap
                    :event-trigger mouse-1
                    :doc "鼠标进入事件"))
    (mouse-leave . (:map-property keymap
                    :event-trigger mouse-1
                    :doc "鼠标离开事件"))))
```

### 表达式解析器

支持的表达式类型：

| 类型 | 示例 | 解析结果 |
|------|------|----------|
| method | `"doSomething"` | `(:type method :name "doSomething")` |
| method-call | `"doSomething(foo)"` | `(:type method-call :name "doSomething" :args ...)` |
| increment | `"count++"` | `(:type increment :var "count")` |
| decrement | `"count--"` | `(:type decrement :var "count")` |
| assignment | `"count=10"` | `(:type assignment :var "count" :value ...)` |
| negation-assign | `"flag=!flag"` | `(:type assignment :var "flag" :value (:type negation ...))` |
| ternary | `"flag ? a() : b()"` | `(:type ternary :condition "flag" ...)` |
| logical-and | `"show && hide()"` | `(:type logical-and :condition "show" ...)` |
| logical-or | `"show \|\| hide()"` | `(:type logical-or :condition "show" ...)` |
| multi-statement | `"a++ ; b++"` | `(:type multi-statement :statements ...)` |

### 值表达式解析器

赋值表达式右侧支持的值类型：

| 类型 | 示例 | 说明 |
|------|------|------|
| string | `"'hello'"` 或 `'"hello"'` | 字符串字面量 |
| number | `"10"`, `"-3.14"` | 数字字面量 |
| boolean | `"true"`, `"false"` | 布尔值 |
| variable | `"count"` | 变量引用 |
| negation | `"!flag"` | 逻辑非运算 |
| ternary | `"a > b ? 1 : 0"` | 三元表达式 |

## 使用指南

### 基本用法

#### 1. 方法引用

在 `:setup` 中定义方法，在模板中引用：

```elisp
(define-twidget my-button
  :slot t
  :setup (lambda (_props slot)
           (list :label (twidget-ref slot)
                 :handleClick (lambda ()
                                (message "Button clicked!"))))
  :template '(span :on-click "handleClick" "{label}"))

;; 使用
(twidget-parse '(my-button "Click Me"))
```

#### 2. 带参数的方法调用

```elisp
(define-twidget greeting-button
  :slot t
  :setup (lambda (_props slot)
           (list :name (twidget-ref slot)
                 :greet (lambda (greeting)
                          (message "%s" greeting))))
  :template '(span :on-click "greet('Hello World')" "Say Hello"))
```

#### 3. 内联表达式 - 递增/递减

```elisp
(define-twidget counter
  :slot t
  :setup (lambda (_props slot)
           (list :count (twidget-ref (string-to-number slot))))
  :template '(div
              (span "{count}")
              " "
              (span :on-click "count++" "[+1]")
              " "
              (span :on-click "count--" "[-1]")))
```

#### 4. 内联表达式 - 赋值

```elisp
(define-twidget resettable-counter
  :slot t
  :setup (lambda (_props slot)
           (list :count (twidget-ref 0)))
  :template '(div
              (span "{count}")
              " "
              (span :on-click "count=0" "[Reset]")
              " "
              (span :on-click "count=100" "[Set 100]")))
```

#### 5. 多语句表达式

```elisp
(define-twidget dual-counter
  :setup (lambda (_props _slot)
           (list :a (twidget-ref 0)
                 :b (twidget-ref 0)))
  :template '(div
              (span "A: {a}, B: {b}")
              " "
              (span :on-click "a++ ; b++" "[Both +1]")))
```

#### 6. 条件表达式（三元运算符）

```elisp
(define-twidget toggle
  :setup (lambda (_props _slot)
           (list :flag (twidget-ref t)
                 :showOn (lambda () (message "ON!"))
                 :showOff (lambda () (message "OFF!"))))
  :template '(div
              (span :on-click "flag ? showOn() : showOff()" "[Toggle]")))
```

#### 7. 逻辑非赋值（Toggle 模式）

使用 `!variable` 语法来切换布尔值：

```elisp
(define-twidget toggle-switch
  :setup (lambda (_props _slot)
           (list :on (twidget-ref nil)
                 :notify (lambda ()
                           (message (if (twidget-get 'on) "ON!" "OFF!")))))
  :template '(div
              (span :on-click "on = !on ; notify()" "[Toggle: {on}]")))
```

更简洁的写法：

```elisp
(define-twidget simple-toggle
  :setup (lambda (_props _slot)
           (list :flag (twidget-ref t)))
  :template '(div
              (span :on-click "flag = !flag" "[{flag}]")))
```

#### 8. 逻辑表达式

```elisp
(define-twidget conditional-action
  :setup (lambda (_props _slot)
           (list :enabled (twidget-ref t)
                 :doAction (lambda () (message "Action executed!"))))
  :template '(div
              (span :on-click "enabled && doAction()" "[Do if enabled]")))
```

#### 9. 复杂赋值表达式

```elisp
(define-twidget flip-counter
  :setup (lambda (_props _slot)
           (list :count (twidget-ref 0)))
  :template '(div
              (span "{count}")
              " "
              (span :on-click "count = count === 0 ? 1 : 0" "[Flip]")))
```

### 参数类型

事件处理器支持以下参数类型：

| 类型 | 示例 | 说明 |
|------|------|------|
| 字符串 | `"greet('hello')"` | 单引号或双引号 |
| 数字 | `"setCount(42)"` | 整数或浮点数 |
| 布尔值 | `"setFlag(true)"` | `true`、`false`、`nil` |
| 变量 | `"greet(name)"` | 引用 setup 中的变量 |
| `$event` | `"handleClick($event)"` | 事件对象引用（保留） |

### 条件表达式

支持的比较运算符：

| 运算符 | 示例 | 说明 |
|--------|------|------|
| `===` | `count === 0` | 严格相等 |
| `==` | `count == 0` | 相等 |
| `!=` | `count != 0` | 不相等 |
| `>` | `count > 10` | 大于 |
| `<` | `count < 10` | 小于 |
| `!` | `!flag` | 取反 |

## 扩展事件类型

要添加新的事件类型，修改 `twidget-event-types` 并更新 `twidget--process-event-prop`：

```elisp
;; 添加新事件类型到注册表
(push '(double-click . (:map-property keymap
                        :event-trigger mouse-1
                        :doc "双击事件"))
      twidget-event-types)

;; 在 twidget--process-event-prop 中添加处理逻辑
(pcase event-type
  ('click ...)
  ('double-click
   (list 'keymap (twidget--create-double-click-handler handler-fn)
         'mouse-face 'highlight))
  ...)
```

## 技术实现细节

### 表达式解析流程

```
输入: "count++"
    │
    ▼
twidget--parse-event-expression
    │
    ▼
twidget--parse-single-statement
    │
    ▼
匹配正则: ^([a-zA-Z_][a-zA-Z0-9_]*)\+\+$
    │
    ▼
输出: (:type increment :var "count")
```

### 处理器编译流程

```
输入: (:type increment :var "count")
    │
    ▼
twidget--compile-event-handler
    │
    ▼
生成 lambda:
    (lambda ()
      (interactive)
      (twidget-inc (intern "count") 1))
```

### 文本属性应用

```
输入: "Click Me", (:keymap <map> :mouse-face highlight)
    │
    ▼
twidget--apply-event-properties
    │
    ▼
输出: #("Click Me" 0 8 (keymap <map> mouse-face highlight))
```

## API 参考

### 核心函数

| 函数 | 说明 |
|------|------|
| `twidget--is-event-prop-p` | 检测关键字是否为事件属性 |
| `twidget--parse-event-expression` | 解析事件表达式字符串 |
| `twidget--compile-event-handler` | 编译解析结果为 lambda |
| `twidget--process-event-prop` | 处理事件属性，返回文本属性 |
| `twidget--apply-event-properties` | 应用事件属性到文本 |

### 辅助函数

| 函数 | 说明 |
|------|------|
| `twidget--parse-single-statement` | 解析单个语句 |
| `twidget--parse-arguments` | 解析函数参数 |
| `twidget--resolve-value` | 解析值表达式 |
| `twidget--eval-condition` | 评估条件表达式 |
| `twidget--create-click-handler` | 创建点击事件 keymap |

---

# Event System Design Document

English | [中文](#twidget-事件系统设计文档)

## Overview

The Twidget event system is a Vue3-like declarative event handling system that allows binding event handlers in component templates using `:on-*` syntax. The system supports multiple syntax formats including method references, method calls with arguments, and various inline expressions.

## Design Principles

1. **Declarative Syntax** - Event handlers are declared directly in templates as strings
2. **Setup Integration** - Event handlers can access methods and reactive variables defined in `:setup`
3. **Extensible Design** - The event system is designed to be extensible for adding more event types
4. **Safety** - Expressions are executed in a controlled environment

## Usage Guide

### Basic Usage

#### 1. Method Reference

Define methods in `:setup`, reference them in templates:

```elisp
(define-twidget my-button
  :slot t
  :setup (lambda (_props slot)
           (list :label (twidget-ref slot)
                 :handleClick (lambda ()
                                (message "Button clicked!"))))
  :template '(span :on-click "handleClick" "{label}"))
```

#### 2. Inline Expressions

```elisp
(define-twidget counter
  :slot t
  :setup (lambda (_props slot)
           (list :count (twidget-ref 0)))
  :template '(div
              (span "{count}")
              " "
              (span :on-click "count++" "[+1]")
              " "
              (span :on-click "count--" "[-1]")))
```

### Supported Expression Types

| Type | Example | Description |
|------|---------|-------------|
| Method | `"doSomething"` | Simple method reference |
| Method Call | `"doSomething(arg)"` | Method with arguments |
| Increment | `"count++"` | Increment variable |
| Decrement | `"count--"` | Decrement variable |
| Assignment | `"count=10"` | Assign value |
| Ternary | `"flag ? a() : b()"` | Conditional |
| Logical AND | `"show && action()"` | Execute if truthy |
| Logical OR | `"show \|\| action()"` | Execute if falsy |
| Multi-statement | `"a++ ; b++"` | Multiple statements |

## Extending Event Types

To add new event types, modify `twidget-event-types` and update `twidget--process-event-prop`:

```elisp
(push '(double-click . (:map-property keymap
                        :event-trigger mouse-1
                        :doc "Double click event"))
      twidget-event-types)
```

## API Reference

### Core Functions

| Function | Description |
|----------|-------------|
| `twidget--is-event-prop-p` | Check if keyword is an event property |
| `twidget--parse-event-expression` | Parse event expression string |
| `twidget--compile-event-handler` | Compile parsed result to lambda |
| `twidget--process-event-prop` | Process event property, return text properties |
| `twidget--apply-event-properties` | Apply event properties to text |
