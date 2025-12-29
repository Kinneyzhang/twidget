# twidget

一个为 Emacs 设计的声明式文本组件库，灵感来自现代 UI 组件框架。

[English](README.md) | 中文

## 概述

`twidget`（Text Widget）提供了一种强大而灵活的方式来定义 Emacs 中的可复用文本组件。它提供了类似于现代前端框架的声明式语法，支持：

- **属性系统** - 定义必选和可选属性，支持默认值
- **插槽系统** - 支持单一插槽和命名插槽，实现灵活的内容组合
- **组件继承** - 扩展父组件以创建特定变体
- **文本属性** - 与 Emacs 文本属性无缝集成

## 安装

### 依赖

本包需要 [tp](https://github.com/Kinneyzhang/tp)（text properties）库。

### 手动安装

1. 克隆此仓库：
   ```bash
   git clone https://github.com/Kinneyzhang/twidget.git
   ```

2. 添加到 Emacs 配置中：
   ```elisp
   (add-to-list 'load-path "/path/to/twidget")
   (require 'twidget)
   ```

### 使用 use-package 和 straight.el

```elisp
(use-package twidget
  :straight (:host github :repo "Kinneyzhang/twidget")
  :after tp)
```

## 快速开始

### 定义简单组件

```elisp
;; 定义一个带有单一插槽的段落组件
(define-twidget p
  :slot t
  :render (lambda (props slot)
            (concat slot "\n")))

;; 使用组件
(twidget-parse '(p "Hello, World!"))
;; => "Hello, World!\n"
```

### 带属性的组件

```elisp
;; 定义一个带颜色属性的文本组件
(define-twidget styled-text
  :props '((color . "black"))
  :slot t
  :render (lambda (props slot)
            (tp-set slot 'face `(:foreground ,(plist-get props :color)))))

;; 使用自定义颜色
(twidget-parse '(styled-text :color "blue" "彩色文本"))
```

### 嵌套组件

```elisp
;; 组件可以嵌套使用
(twidget-parse
 '(p "开始 "
     (styled-text :color "red" "重要内容")
     " 结束"))
```

## API 参考

### `define-twidget`

```elisp
(define-twidget NAME &rest ARGS)
```

定义一个名为 NAME 的文本组件。

**关键字参数：**

| 关键字 | 描述 |
|--------|------|
| `:props` | 属性定义。可以是符号（必选）或 cons 单元 `(symbol . default)` |
| `:slot` | 布尔值或列表。`nil`（无插槽）、`t`（单一插槽）或插槽名称列表 |
| `:slots` | `:slot` 的别名，用于命名插槽 |
| `:extends` | 要继承的父组件符号 |
| `:render` | 返回渲染字符串的 lambda 函数 |

**渲染函数签名：**

- 单一插槽：`(lambda (props slot) ...)`
- 命名插槽：`(lambda (props slots) ...)` 其中 slots 是一个 plist
- 带继承：`(lambda (props slot parent-render) ...)`

### `twidget-parse`

```elisp
(twidget-parse WIDGET-FORM)
```

解析并渲染组件调用。返回应用了文本属性的渲染字符串。

**WIDGET-FORM 格式：**
```elisp
(WIDGET-NAME :prop1 val1 :prop2 val2 ... SLOT-VALUES...)
```

### `twidget-reset`

```elisp
(twidget-reset)
```

重置所有组件定义。在开发和测试时很有用。

## 高级特性

### 命名插槽

命名插槽允许您指定不同内容的放置位置：

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

;; 使用命名插槽
(twidget-parse
 '(card
   (slot-header "标题")
   (slot-content "这里是主要内容")
   (slot-footer "页脚文本")))
```

### 组件继承

通过扩展基础组件创建特化组件：

```elisp
;; 基础按钮组件
(define-twidget base-button
  :props '((type . "default"))
  :slot t
  :render (lambda (props slot)
            (tp-set slot 'face 'button)))

;; 继承 base-button 的主按钮
(define-twidget primary-button
  :extends 'base-button
  :props '((type . "primary"))
  :render (lambda (props slot parent-render)
            (let ((result (funcall parent-render props slot)))
              (tp-add result 'face '(:foreground "blue")))))

;; 使用
(twidget-parse '(primary-button "点击我"))
```

### 属性继承

子组件从父组件继承属性，子组件的属性优先级更高：

```elisp
(define-twidget parent-widget
  :props '((size . "medium")
           (color . "gray"))
  :slot t
  :render ...)

(define-twidget child-widget
  :extends 'parent-widget
  :props '((color . "blue"))  ; 覆盖父组件的 color，继承 size
  :render ...)
```

## 实用函数

### `twidget-inc`

```elisp
(twidget-inc SYM NUM)
```

将符号 SYM 中存储的数字字符串增加 NUM。

### `twidget-dec`

```elisp
(twidget-dec SYM NUM)
```

将符号 SYM 中存储的数字字符串减少 NUM。

## 示例

### 创建徽章组件

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

(twidget-parse '(badge :type "success" "成功"))
```

### 创建布局组件

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

## 贡献

欢迎贡献！请随时提交问题和拉取请求。

## 许可证

本项目采用 GNU 通用公共许可证 v3.0 授权 - 详见 [LICENSE](LICENSE) 文件。

## 致谢

- 灵感来自 Vue.js 和 React 等现代 UI 组件框架
- 使用 Emacs Lisp 为 Emacs 文本编辑器构建
