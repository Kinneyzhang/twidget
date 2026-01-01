# twidget

一个为 Emacs 设计的声明式文本组件库，灵感来自现代 UI 组件框架。

[English](README.md) | 中文

## 概述

`twidget`（Text Widget）提供了一种强大而灵活的方式来定义 Emacs 中的可复用文本组件。它提供了类似于现代前端框架的声明式语法，支持：

- **属性系统** - 定义必选和可选属性，支持默认值
- **插槽系统** - 支持单一插槽和命名插槽，实现灵活的内容组合
- **组件继承** - 扩展父组件以创建特定变体
- **文本属性** - 与 Emacs 文本属性无缝集成
- **响应式数据** - 使用 `twidget-ref` 创建响应式 UI，自动更新
- **复合组件** - 使用 `:setup` 和 `:template` 构建复杂组件
- **事件系统** - 类似 Vue3 的声明式事件绑定，支持 `:on-click` 和内联表达式

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
| `:render` | 返回渲染字符串的 lambda 函数（用于简单组件） |
| `:setup` | 返回响应式绑定 plist 的 lambda 函数（用于复合组件） |
| `:template` | 组件结构的模板 sexp（用于复合组件） |

**两种定义组件的方式：**

1. **简单组件** - 使用 `:render` 直接渲染
2. **复合组件** - 使用 `:setup` 和 `:template` 组合响应式数据

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

### 使用响应式数据的复合组件

对于组合其他组件并具有响应式数据的复杂组件，使用 `:setup` 和 `:template`：

```elisp
;; 定义一个带响应式状态的计数器组件
(define-twidget my-counter
  :slot t
  :setup (lambda (_props slot)
           ;; 返回包含响应式绑定的 plist
           ;; slot 包含传递给组件的插槽内容
           (list :count (twidget-ref slot)))
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

;; 使用计数器，通过插槽传递初始值
(twidget-parse '(my-counter "0"))
```

**关键概念：**

- **`twidget-ref`** - 创建响应式引用。当值改变时，UI 自动更新。
- **`:setup`** - 接收 props 和 slot，返回响应式绑定 plist 的函数。
- **`:template`** - 定义组件结构的 quoted sexp。使用 `{varname}` 语法绑定响应式数据。

### 响应式数据 API

```elisp
;; 创建响应式引用
(twidget-ref "初始值")

;; 获取响应式值
(twidget-get 'varname)

;; 设置响应式值（触发 UI 更新）
(twidget-set 'varname "新值")

;; 增加/减少数值
(twidget-inc 'varname 1)
(twidget-dec 'varname 1)
```

## 实用函数

### `twidget-ref`

```elisp
(twidget-ref INITIAL-VALUE)
```

创建带有初始值 INITIAL-VALUE 的响应式引用。返回可在 `:setup` 函数中使用的 twidget-ref 对象。

### `twidget-get`

```elisp
(twidget-get SYM &optional KEY-OR-INDEX)
```

获取响应式变量 SYM 的当前值。

对于 plist/list 类型的值，可以访问嵌套值：
- 使用关键字（如 `:name`）访问 plist 属性
- 使用整数索引（从 0 开始）访问列表元素

```elisp
;; 获取整个值
(twidget-get 'user)

;; 从 plist 值中获取 :name
(twidget-get 'user :name)

;; 从列表值中获取第一个元素
(twidget-get 'items 0)
```

### `twidget-set`

```elisp
(twidget-set SYM VALUE &optional KEY-OR-INDEX)
```

将响应式变量 SYM 的值设置为 VALUE。这会触发缓冲区中的响应式更新。

对于 plist/list 类型的值，可以设置嵌套值：
- 使用关键字（如 `:name`）设置 plist 属性
- 使用整数索引（从 0 开始）设置列表元素

```elisp
;; 设置整个值
(twidget-set 'user new-user)

;; 设置 plist 值中的 :name
(twidget-set 'user "John" :name)

;; 设置列表值中的第一个元素
(twidget-set 'items "new-item" 0)
```

### `twidget-inc`

```elisp
(twidget-inc SYM NUM)
```

将响应式变量 SYM 中存储的数值增加 NUM。

### `twidget-dec`

```elisp
(twidget-dec SYM NUM)
```

将响应式变量 SYM 中存储的数值减少 NUM。

## 事件系统

Twidget 提供了类似 Vue3 的声明式事件系统，允许在组件模板中通过 `:on-*` 语法直接绑定事件处理器。

### 基本事件绑定

```elisp
;; 简单点击处理器
(define-twidget my-button
  :slot t
  :setup (lambda (_props slot)
           (list :label (twidget-ref slot)
                 :handleClick (lambda ()
                                (message "按钮被点击了！"))))
  :template '(span :on-click "handleClick" "{label}"))

(twidget-parse '(my-button "点击我"))
```

### 支持的表达式格式

| 格式 | 示例 | 说明 |
|------|------|------|
| 方法引用 | `"doSomething"` | 调用 `:setup` 中定义的方法 |
| 带参数方法调用 | `"doSomething(foo, 'bar')"` | 带参数调用方法 |
| 递增 | `"count++"` | 递增响应式变量 |
| 递减 | `"count--"` | 递减响应式变量 |
| 赋值 | `"count=10"` | 给变量赋值 |
| 取反 | `"flag=!flag"` | 切换布尔值 |
| 多语句 | `"a++ ; b++"` | 执行多个语句 |
| 三元表达式 | `"flag ? doA() : doB()"` | 条件执行 |
| 逻辑与 | `"enabled && doAction()"` | 条件为真时执行 |
| 逻辑或 | `"!enabled \|\| showWarning()"` | 条件为假时执行 |

### 事件系统示例

#### 计数器（递增/递减）

```elisp
(define-twidget counter
  :setup (lambda (_props _slot)
           (list :count (twidget-ref 0)))
  :template '(div
              (span "{count}")
              " "
              (span :on-click "count++" "[+]")
              " "
              (span :on-click "count--" "[-]")
              " "
              (span :on-click "count=0" "[重置]")))

(tp-pop-to-buffer "*counter-demo*"
  (twidget-insert '(counter)))
```

#### 开关切换

```elisp
(define-twidget toggle-switch
  :setup (lambda (_props _slot)
           (list :on (twidget-ref nil)
                 :notify (lambda ()
                           (message (if (twidget-get 'on) "开启！" "关闭！")))))
  :template '(div
              (span :on-click "on = !on ; notify()" "[切换: {on}]")))

(tp-pop-to-buffer "*toggle-demo*"
  (twidget-insert '(toggle-switch)))
```

#### 双计数器（多语句）

```elisp
(define-twidget dual-counter
  :setup (lambda (_props _slot)
           (list :a (twidget-ref 0)
                 :b (twidget-ref 0)))
  :template '(div
              (span "A: {a}, B: {b}")
              " "
              (span :on-click "a++;b++" "[同时+1]")))

(tp-pop-to-buffer "*dual-counter-demo*"
  (twidget-insert '(dual-counter)))
```

#### 条件执行

```elisp
(define-twidget conditional-action
  :setup (lambda (_props _slot)
           (list :enabled (twidget-ref t)
                 :doAction (lambda () (message "动作已执行！"))
                 :toggleEnabled (lambda ()
                                  (twidget-set 'enabled (not (twidget-get 'enabled))))))
  :template '(div
              (span :on-click "toggleEnabled" "[{enabled}]")
              " "
              (span :on-click "enabled && doAction()" "[启用时执行]")))

(tp-pop-to-buffer "*conditional-demo*"
  (twidget-insert '(conditional-action)))
```

#### 三元表达式

```elisp
(define-twidget ternary-demo
  :setup (lambda (_props _slot)
           (list :flag (twidget-ref t)
                 :showOn (lambda () (message "开启状态！"))
                 :showOff (lambda () (message "关闭状态！"))
                 :toggle (lambda ()
                           (twidget-set 'flag (not (twidget-get 'flag))))))
  :template '(div
              (span :on-click "toggle" "[切换]")
              " "
              (span :on-click "flag ? showOn() : showOff()" "[显示状态]")))

(tp-pop-to-buffer "*ternary-demo*"
  (twidget-insert '(ternary-demo)))
```

### 事件处理器参数类型

事件处理器支持多种参数类型：

| 类型 | 示例 | 说明 |
|------|------|------|
| 字符串 | `"greet('hello')"` | 单引号或双引号 |
| 数字 | `"setCount(42)"` | 整数或浮点数 |
| 布尔值 | `"setFlag(true)"` | `true`、`false` 或 `nil` |
| 变量 | `"greet(name)"` | 引用 setup 中的变量 |

### 比较运算符

条件表达式支持以下运算符：

| 运算符 | 示例 | 说明 |
|--------|------|------|
| `===` | `count === 0` | 严格相等 |
| `==` | `count == 0` | 相等 |
| `!=` | `count != 0` | 不相等 |
| `>` | `count > 10` | 大于 |
| `<` | `count < 10` | 小于 |
| `!` | `!flag` | 逻辑非 |

更多详情请参阅 [事件系统文档](docs/event-system.md)。

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
