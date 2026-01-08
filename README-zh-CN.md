<div align="center">

# 🎨 twidget

**一个受现代 UI 组件框架启发的 Emacs 声明式文本组件库**

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Emacs](https://img.shields.io/badge/Emacs-26.1+-purple.svg)](https://www.gnu.org/software/emacs/)

[English](README.md) | 中文

</div>

---

## 📑 目录

- [概述](#-概述)
- [特性](#-特性)
- [安装](#-安装)
- [快速开始](#-快速开始)
- [核心概念](#-核心概念)
  - [简单组件](#使用-render-的简单组件)
  - [复合组件](#使用-setup-和-template-的复合组件)
  - [属性系统](#属性系统)
  - [插槽系统](#插槽系统)
  - [组件继承](#组件继承)
- [响应式系统](#-响应式系统)
- [事件系统](#-事件系统)
- [内置组件](#-内置组件)
- [API 参考](#-api-参考)
- [示例](#-示例)
- [贡献](#-贡献)
- [许可证](#-许可证)

---

## 🌟 概述

**twidget**（Text Widget）是一个革新性的 Emacs 声明式文本组件库，它彻底改变了在 Emacs 中构建文本界面的方式。受 Vue.js 和 React 等现代前端框架启发，twidget 将基于组件的 UI 开发模式引入 Emacs buffer。

### 核心能力

twidget 让你能够：

- **定义可复用组件**：创建具有独立属性、插槽和渲染逻辑的自包含文本组件，可在整个 Emacs 应用中复用
- **构建响应式界面**：使用 `twidget-ref` 创建响应式数据绑定，数据变化时 UI 自动更新——无需手动刷新
- **处理用户交互**：使用类似 Vue3 的 `:on-click` 语法绑定点击事件和其他交互，支持内联表达式（`count++`、`flag=!flag`）
- **组合复杂 UI**：在组件中嵌套组件，使用命名插槽实现灵活的内容注入，通过继承扩展现有组件
- **遍历数据集合**：使用 `:for` 指令从集合中动态渲染列表项

### 工作原理

```elisp
;; 1. 定义一个带响应式状态的组件
(define-twidget counter
  :setup (lambda (_props _slot)
           (list :count (twidget-ref 0)))           ; 响应式状态
  :template '(div
              (span "计数: {count} ")                ; 数据绑定
              (span :on-click "count++" "[+]")))     ; 事件处理

;; 2. 在 buffer 中渲染组件
(tp-pop-to-buffer "*demo*"
  (twidget-insert '(counter)))

;; 3. 点击 [+] 递增——UI 自动更新！
```

### 为什么选择 twidget？

| 优势 | 描述 |
|------|------|
| 🎯 **声明式** | 定义组件的*外观*，而不是逐步描述*如何渲染* |
| 🧩 **可组合** | 通过组合简单、专注的组件构建复杂 UI |
| ⚡ **响应式** | 底层数据变化时 UI 自动更新 |
| 🔄 **熟悉感** | 如果你了解 Vue.js 或 React，这些模式会感觉很自然 |
| 📝 **文本原生** | 专为 Emacs 文本 buffer 设计，完整支持文本属性 |

---

## ✨ 特性

| 特性 | 描述 |
|------|------|
| 🏷️ **属性系统** | 定义必选和可选属性，支持默认值 |
| 📦 **插槽系统** | 单一插槽和命名插槽，实现灵活的内容组合 |
| 🧬 **组件继承** | 扩展父组件以创建特化变体 |
| 🎨 **文本属性** | 通过 [tp](https://github.com/Kinneyzhang/tp) 与 Emacs 文本属性无缝集成 |
| ⚡ **响应式数据** | 使用 `twidget-ref` 创建响应式 UI，自动更新 |
| 🧩 **复合组件** | 使用 `:setup` 和 `:template` 构建复杂组件 |
| 🖱️ **事件系统** | 类似 Vue3 的声明式事件绑定，支持 `:on-click` 和内联表达式 |
| 🔄 **循环指令** | 使用 `:for` 指令遍历集合 |
| 🚀 **模板预编译** | 组件定义时预编译模板，减少运行时开销 |

---

## 📦 安装

### 依赖

本包需要 [tp](https://github.com/Kinneyzhang/tp)（text properties）库。

### 手动安装

```bash
# 克隆两个仓库
git clone https://github.com/Kinneyzhang/tp.git
git clone https://github.com/Kinneyzhang/twidget.git
```

```elisp
;; 添加到你的 Emacs 配置中
(add-to-list 'load-path "/path/to/tp")
(add-to-list 'load-path "/path/to/twidget")
(require 'twidget)
```

### 使用 use-package 和 straight.el

```elisp
(use-package tp
  :straight (:host github :repo "Kinneyzhang/tp"))

(use-package twidget
  :straight (:host github :repo "Kinneyzhang/twidget")
  :after tp)
```

### 使用 Quelpa

```elisp
(quelpa '(tp :fetcher github :repo "Kinneyzhang/tp"))
(quelpa '(twidget :fetcher github :repo "Kinneyzhang/twidget"))
(require 'twidget)
```

---

## 🚀 快速开始

### 你的第一个组件

```elisp
;; 定义一个简单的问候组件
(define-twidget greeting
  :props '((name . "World"))
  :render (lambda (props _slot)
            (format "Hello, %s!\n" (plist-get props :name))))

;; 使用组件
(twidget-parse '(greeting))
;; => "Hello, World!\n"

(twidget-parse '(greeting :name "Emacs"))
;; => "Hello, Emacs!\n"
```

### 交互式计数器（试一试！）

复制并在 Emacs 中执行以下代码，查看可交互的计数器：

```elisp
(define-twidget counter
  :setup (lambda (_props _slot)
           (list :count (twidget-ref 0)))
  :template '(div
              (span "计数: {count} ")
              (span :on-click "count++" "[+]")
              (span " ")
              (span :on-click "count--" "[-]")
              (span " ")
              (span :on-click "count=0" "[重置]")))

;; 在缓冲区中显示计数器
(tp-pop-to-buffer "*counter-demo*"
  (twidget-insert '(counter)))
```

点击 `[+]`、`[-]` 或 `[重置]` 与计数器交互！

---

## 📚 核心概念

twidget 支持两种定义组件的方式：

### 使用 `:render` 的简单组件

对于直接产生输出的组件，使用 `:render`：

```elisp
(define-twidget my-text
  :props '((color . "black"))    ; 带默认值的属性
  ;; :slot t 是默认值，可省略
  :render (lambda (props slot)
            ;; props: 属性的 plist (:color "black")
            ;; slot: 传递给组件的内容
            (tp-set slot 'face `(:foreground ,(plist-get props :color)))))

;; 使用
(twidget-parse '(my-text :color "red" "你好！"))
```

### 使用 `:setup` 和 `:template` 的复合组件

对于组合其他组件并具有响应式状态的组件：

```elisp
(define-twidget toggle-button
  :props '((label . "切换"))
  :setup (lambda (props _slot)
           ;; 初始化响应式状态
           (list :active (twidget-ref nil)
                 :buttonLabel (plist-get props :label)
                 ;; 定义根据状态计算文本属性的函数
                 :getProps (lambda ()
                             (if (twidget-get 'active)
                                 '(face (:background "green" :foreground "white"))
                               '(face (:background "gray" :foreground "black"))))))
  :template '(span :on-click "active = !active"
                   :tp-props "getProps()"
                   "[{buttonLabel}: {active}]"))

;; 使用
(twidget-parse '(toggle-button :label "深色模式"))
```

### 属性系统

属性定义了组件的可配置部分：

```elisp
(define-twidget styled-box
  :props '(
    title               ; 必选属性（无默认值）
    (width . 20)        ; 可选属性带默认值
    (border . t)        ; 布尔属性
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

;; 使用
(twidget-parse '(styled-box :title "注意" :width 30 "这是内容"))
```

### 插槽系统

插槽允许你向组件传递内容。默认情况下，组件支持单一插槽（`:slot t`）。

#### 单一插槽（默认）

```elisp
(define-twidget wrapper
  ;; :slot t 是默认值，组件默认接受插槽内容
  :render (lambda (_props slot)
            (concat "<<< " slot " >>>")))

(twidget-parse '(wrapper "你好"))
;; => "<<< 你好 >>>"
```

#### 无插槽

```elisp
(define-twidget hr
  :slot nil    ; 显式禁用插槽
  :render (lambda (_props _slot)
            (make-string 40 ?─)))
```

#### 命名插槽

```elisp
(define-twidget card
  :slot '(header content footer)
  :render (lambda (_props slots)
            (concat
             "╭────────────────────╮\n"
             "│ " (or (plist-get slots :header) "无标题") "\n"
             "├────────────────────┤\n"
             "│ " (or (plist-get slots :content) "") "\n"
             "├────────────────────┤\n"
             "│ " (or (plist-get slots :footer) "") "\n"
             "╰────────────────────╯\n")))

;; 使用命名插槽
(twidget-parse
 '(card
   (slot-header "我的卡片标题")
   (slot-content "这是主要内容。")
   (slot-footer "页脚信息")))
```

#### 插槽类型保留

当传递单个非字符串值给插槽时，其原始类型会被保留：

```elisp
(define-twidget repeat-char
  :props '((char . "*"))
  :render (lambda (props slot)
            ;; slot 可以是数字，而不仅仅是字符串
            (let ((count (if (numberp slot) slot (string-to-number slot))))
              (make-string count (string-to-char (plist-get props :char))))))

;; 使用数字 - 类型被保留
(twidget-parse '(repeat-char 5))
;; => "*****"
```

### 组件继承

通过扩展基础组件创建特化组件：

```elisp
;; 基础组件
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

;; 派生组件 - 继承并覆盖
(define-twidget error-alert
  :extends 'alert-base
  :props '((type . "error"))    ; 覆盖默认类型
  :render (lambda (props slot parent-render)
            ;; 调用父组件渲染并添加样式
            (let ((result (funcall parent-render props slot)))
              (tp-set result 'face '(:foreground "red")))))

;; 使用
(twidget-parse '(error-alert "出错了！"))
;; => "❌ 出错了！\n" (带红色前景)
```

---

## ⚡ 响应式系统

响应式系统允许 UI 在数据变化时自动更新。

### 创建响应式数据

```elisp
;; 在 :setup 函数中，使用 twidget-ref 创建响应式值
:setup (lambda (_props _slot)
         (list :count (twidget-ref 0)
               :name (twidget-ref "Emacs")
               :items (twidget-ref '("苹果" "香蕉" "橙子"))))
```

### 在模板中绑定响应式数据

在模板字符串中使用 `{varname}` 语法：

```elisp
:template '(div
            (span "你好，{name}！")
            (span "计数：{count}"))
```

### 访问嵌套值

使用点号表示法进行嵌套访问：

```elisp
:setup (lambda (_props _slot)
         (list :user (twidget-ref '(:name "张三" :age 30))
               :items (twidget-ref '("甲" "乙" "丙"))))

:template '(div
            (span "姓名：{user.name}")    ; plist 访问
            (span "年龄：{user.age}")
            (span "第一个：{items.0}"))   ; 列表索引访问
```

### 响应式 API 参考

| 函数 | 描述 | 示例 |
|------|------|------|
| `twidget-ref` | 创建响应式引用 | `(twidget-ref 0)` |
| `twidget-get` | 获取响应式值 | `(twidget-get 'count)` |
| `twidget-set` | 设置响应式值 | `(twidget-set 'count 10)` |
| `twidget-inc` | 增加数值 | `(twidget-inc 'count 1)` |
| `twidget-dec` | 减少数值 | `(twidget-dec 'count 1)` |
| `twidget-watch` | 注册变更处理器 | `(twidget-watch ref callback)` |
| `twidget-unwatch` | 移除变更处理器 | `(twidget-unwatch ref callback)` |

#### 访问嵌套属性

```elisp
;; 获取嵌套属性
(twidget-get 'user :name)      ; 从 plist 获取 :name
(twidget-get 'items 0)         ; 从列表获取第一个元素

;; 设置嵌套属性
(twidget-set 'user "李四" :name)  ; 设置 plist 中的 :name
(twidget-set 'items "丁" 0)       ; 设置列表中的第一个元素
```

#### 监听变更 (on-change)

使用 `twidget-watch` 注册当响应式值变更时触发的回调函数：

```elisp
(define-twidget watched-counter
  :setup (lambda (_props _slot)
           (let ((count (twidget-ref 0)))
             ;; 注册变更处理器
             (twidget-watch count
                            (lambda (new-value old-value)
                              (message "计数从 %s 变更为 %s"
                                       old-value new-value)))
             (list :count count)))
  :template '(div
              (span "计数: {count} ")
              (span :on-click "count++" "[+]")))

(tp-pop-to-buffer "*watched-counter*"
  (twidget-insert '(watched-counter)))
```

回调函数接收两个参数：新值和旧值。使用可选的 `immediate` 参数可以在注册时立即以初始值触发回调：

```elisp
(twidget-watch ref callback t)  ; 立即以初始值调用
```

---

## 🖱️ 事件系统

事件系统提供类似 Vue3 的声明式事件绑定。

### 基本点击处理器

```elisp
(define-twidget click-demo
  :setup (lambda (_props _slot)
           (list :handleClick (lambda ()
                                (message "点击了！"))))
  :template '(span :on-click "handleClick" "[点击我]"))
```

### 表达式类型

| 表达式 | 示例 | 描述 |
|--------|------|------|
| 方法引用 | `:on-click "doSomething"` | 调用 `:setup` 中的方法 |
| 带参数方法 | `:on-click "greet('你好')"` | 带参数的方法 |
| 递增 | `:on-click "count++"` | 递增响应式变量 |
| 递减 | `:on-click "count--"` | 递减响应式变量 |
| 赋值 | `:on-click "count=0"` | 赋值 |
| 切换 | `:on-click "flag=!flag"` | 切换布尔值 |
| 多语句 | `:on-click "a++;b++"` | 多个语句（`;` 分隔） |
| 三元表达式 | `:on-click "flag ? on() : off()"` | 条件执行 |
| 逻辑与 | `:on-click "enabled && action()"` | 条件为真时执行 |
| 逻辑或 | `:on-click "!enabled \|\| warn()"` | 条件为假时执行 |

### 条件运算符

| 运算符 | 示例 | 描述 |
|--------|------|------|
| `===` | `count === 0` | 严格相等 |
| `==` | `count == 0` | 相等 |
| `!=` | `count != 0` | 不相等 |
| `>` | `count > 10` | 大于 |
| `<` | `count < 10` | 小于 |
| `!` | `!flag` | 逻辑非 |

### 完整事件示例

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
     (todo-item :text "学习 Emacs Lisp")
     (todo-item :text "构建组件")
     (todo-item :text "创建出色的 UI"))))
```

更多详情请参阅[事件系统文档](docs/event-system.md)。

关于性能优化详情，请参阅[预编译文档](docs/precompilation.md)。

---

## 🎨 响应式文本属性

twidget 通过 tp.el 的属性系统支持响应式文本属性，使用 `:tp-props` 属性设置。可以设置多个 tp.el 文本属性，并支持绑定到响应式值。

### 基本用法

```elisp
(define-twidget toggle-button
  :props '((label . "切换"))
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
  (twidget-insert '(toggle-button :label "深色模式")))
```

点击按钮可以在绿色和灰色背景之间切换！

### 静态属性

对于静态属性，直接传递一个 plist：

```elisp
(span :tp-props (face (:background "blue") tp-button (:palette info))
      "点击我")
```

### tp-props 值类型

`:tp-props` 属性支持以下值类型：

| 类型 | 示例 | 说明 |
|------|------|------|
| Plist | `:tp-props (face bold)` | 静态属性 plist |
| 方法调用 | `:tp-props "getProps()"` | 响应式 - 调用 `:setup` 中的方法 |
| 变量引用 | `:tp-props "propsVar"` | 引用 `:setup` 中的变量 |

---

## 🧱 内置组件

twidget 自带常用组件：

| 组件 | 描述 | 示例 |
|------|------|------|
| `p` | 段落（添加换行） | `(p "文本")` |
| `div` | 块容器（添加换行） | `(div "内容")` |
| `span` | 行内容器 | `(span "行内文本")` |
| `h1` - `h5` | 标题（使用 tp-headline） | `(h1 "标题")` |
| `headline` | 基础标题（可配置高度） | `(headline :height 1.5 "标题")` |

### 标题示例

```elisp
;; 使用标题组件
(twidget-parse '(h1 "主标题"))
(twidget-parse '(h2 "章节标题"))
(twidget-parse '(h3 "子章节"))

;; 自定义标题高度
(twidget-parse '(headline :height 2.5 "大标题"))
```

### 嵌套组件

```elisp
(twidget-parse
 '(div
   (h1 "欢迎")
   (p "这是一个段落，"
      (span "包含行内内容")
      "。")
   (div
    (p "div 内的嵌套段落。"))))
```

---

## 📖 API 参考

### 组件定义

#### `define-twidget`

```elisp
(define-twidget NAME &rest ARGS)
```

定义名为 NAME 的文本组件。

| 关键字 | 描述 |
|--------|------|
| `:props` | 属性定义：符号（必选）或 `(symbol . default)` |
| `:slot` | `t`（默认，单一插槽）、`nil`（无插槽）或 `'(name1 name2 ...)` 用于命名插槽 |
| `:slots` | `:slot` 的别名，用于命名插槽 |
| `:extends` | 要继承的父组件符号 |
| `:render` | 简单组件的渲染函数 |
| `:setup` | 复合组件的设置函数（返回响应式绑定） |
| `:template` | 复合组件的模板 sexp |

### 组件使用

#### `twidget-parse`

```elisp
(twidget-parse WIDGET-FORM) -> string
```

解析并渲染组件。返回带有文本属性的字符串。

```elisp
(twidget-parse '(widget-name :prop1 val1 :prop2 val2 "插槽内容"))
```

#### `twidget-insert`

```elisp
(twidget-insert FORM)
```

解析并在光标处插入组件的宏。自动捕获 `:for` 指令引用的词法变量。

```elisp
(let ((items '("甲" "乙" "丙")))
  (twidget-insert
   '(div (p :for "item in items" "- {item}"))))
```

### 实用函数

| 函数 | 签名 | 描述 |
|------|------|------|
| `twidget-reset` | `()` | 清除所有组件定义 |
| `twidget-clear-buffer-state` | `()` | 清除缓冲区本地的响应式状态 |
| `twidget-ref` | `(value)` | 创建响应式引用 |
| `twidget-get` | `(sym &optional key)` | 获取响应式值 |
| `twidget-set` | `(sym value &optional key)` | 设置响应式值 |
| `twidget-inc` | `(sym num)` | 增加响应式值 |
| `twidget-dec` | `(sym num)` | 减少响应式值 |
| `twidget-watch` | `(ref callback &optional immediate)` | 注册变更处理器 |
| `twidget-unwatch` | `(ref callback)` | 移除变更处理器 |

---

## 💡 示例

### 徽章组件

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

;; 使用
(twidget-parse '(badge :type "success" "成功"))
(twidget-parse '(badge :type "error" "失败"))
(twidget-parse '(badge :type "warning" "待处理"))
```

### 带循环的列表

```elisp
(let ((fruits '("🍎 苹果" "🍌 香蕉" "🍊 橙子" "🍇 葡萄")))
  (tp-pop-to-buffer "*fruits*"
    (twidget-insert
     '(div
       (h2 "水果列表")
       (p :for "fruit in fruits" "• {fruit}")))))
```

### 交互式标签页

```elisp
(define-twidget tabs
  :setup (lambda (_props _slot)
           (list :active (twidget-ref 0)
                 :tab1 (lambda () (twidget-set 'active 0))
                 :tab2 (lambda () (twidget-set 'active 1))
                 :tab3 (lambda () (twidget-set 'active 2))))
  :template '(div
              (div
               (span :on-click "tab1" "[标签 1]")
               (span " ")
               (span :on-click "tab2" "[标签 2]")
               (span " ")
               (span :on-click "tab3" "[标签 3]"))
              (p "当前标签：{active}")))

(tp-pop-to-buffer "*tabs-demo*"
  (twidget-insert '(tabs)))
```

### 带多个输入的表单

```elisp
(define-twidget step-input
  :props '((label . "值") (step . 1))
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
     (h2 "设置")
     (step-input :label "音量" :step 5)
     (step-input :label "亮度" :step 10)
     (step-input :label "速度" :step 1))))
```

---

## 🤝 贡献

欢迎贡献！请随时提交问题和拉取请求。

### 开发设置

```bash
git clone https://github.com/Kinneyzhang/twidget.git
cd twidget
```

### 运行测试

```elisp
(require 'twidget)
(twidget-reset)  ; 清除状态以便全新测试
```

---

## 📄 许可证

本项目采用 GNU 通用公共许可证 v3.0 授权 - 详见 [LICENSE](LICENSE) 文件。

---

## 🙏 致谢

- 灵感来自 [Vue.js](https://vuejs.org/) 和 [React](https://react.dev/) 等现代 UI 组件框架
- 使用 Emacs Lisp 为 [GNU Emacs](https://www.gnu.org/software/emacs/) 文本编辑器构建
- 使用 [tp](https://github.com/Kinneyzhang/tp) 增强文本属性处理

---

<div align="center">

**祝你组件构建愉快！🎉**

用 ❤️ 为 Emacs 社区制作

</div>
