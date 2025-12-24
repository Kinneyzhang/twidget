# Twidget Reactive System 文档

一个为 Emacs Lisp 实现的 Vue3 风格响应式系统。

## 目录

1. [设计概述](#设计概述)
2. [核心概念](#核心概念)
3. [API 参考](#api-参考)
4. [使用示例](#使用示例)
5. [最佳实践](#最佳实践)
6. [与 Vue3 的对比](#与-vue3-的对比)

---

## 设计概述

### 架构图

```
┌─────────────────────────────────────────────────────────────────┐
│                      Twidget Reactive System                     │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌─────────┐    ┌──────────┐    ┌──────────┐    ┌─────────┐    │
│  │   Ref   │    │ Reactive │    │ Computed │    │  Watch  │    │
│  │ 单值引用 │    │ 对象代理  │    │  计算属性 │    │  侦听器  │    │
│  └────┬────┘    └────┬─────┘    └────┬─────┘    └────┬────┘    │
│       │              │               │               │          │
│       └──────────────┴───────────────┴───────────────┘          │
│                              │                                   │
│                    ┌─────────▼─────────┐                        │
│                    │   Effect System   │                        │
│                    │    (副作用系统)    │                        │
│                    └─────────┬─────────┘                        │
│                              │                                   │
│              ┌───────────────┴───────────────┐                  │
│              │                               │                   │
│     ┌────────▼────────┐           ┌─────────▼─────────┐        │
│     │  Track (依赖收集) │           │ Trigger (触发更新) │        │
│     └────────┬────────┘           └─────────┬─────────┘        │
│              │                               │                   │
│              └───────────────┬───────────────┘                  │
│                              │                                   │
│                    ┌─────────▼─────────┐                        │
│                    │   Dependency Map  │                        │
│                    │ target → key → Set│                        │
│                    └───────────────────┘                        │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### 核心原理

1. **依赖收集 (Track)**：当在 effect 中访问响应式数据时，系统自动记录依赖关系
2. **依赖触发 (Trigger)**：当响应式数据变化时，自动执行所有依赖该数据的 effects
3. **惰性求值**：Computed 属性只在被访问且依赖变化时才重新计算

---

## 核心概念

### 1. Ref（响应式引用）

用于包装单个值，使其具有响应性。

```elisp
;; 创建 ref
(setq count (twidget-ref 0))

;; 读取值（自动追踪依赖）
(twidget-ref-get count)  ; => 0
(twidget-$ count)        ; => 0 (简写)

;; 设置值（自动触发更新）
(twidget-ref-set count 10)
(twidget-$! count 10)    ; (简写)
```

### 2. Reactive（响应式对象）

用于将整个对象变为响应式。

```elisp
;; 创建响应式对象
(setq state (twidget-reactive '(:name "Alice" :age 25)))

;; 读取属性
(twidget-reactive-get state :name)  ; => "Alice"
(twidget-.  state :name)             ; (简写)

;; 设置属性
(twidget-reactive-set state :age 26)
(twidget-.! state :age 26)          ; (简写)
```

### 3. Effect（副作用）

自动追踪依赖并在依赖变化时重新执行。

```elisp
(setq counter (twidget-ref 0))

(twidget-effect
 (lambda ()
   (message "Count is: %d" (twidget-ref-get counter))))
;; 立即输出:  "Count is: 0"

(twidget-ref-set counter 1)
;; 自动输出: "Count is: 1"
```

### 4. Computed（计算属性）

基于其他响应式数据派生的值，具有缓存。

```elisp
(setq price (twidget-ref 100))
(setq quantity (twidget-ref 2))

(setq total
      (twidget-computed
       (lambda ()
         (* (twidget-ref-get price)
            (twidget-ref-get quantity)))))

(twidget-computed-get total)  ; => 200
(twidget-c$ total)            ; => 200 (简写)

(twidget-ref-set quantity 3)
(twidget-computed-get total)  ; => 300 (自动更新)
```

### 5. Watch（侦听器）

监视特定数据的变化并执行回调。

```elisp
(setq name (twidget-ref "Alice"))

(setq stop-watch
      (twidget-watch
       name
       (lambda (new-val old-val)
         (message "Name changed from %s to %s" old-val new-val))))

(twidget-ref-set name "Bob")
;; 输出: "Name changed from Alice to Bob"

;; 停止监听
(funcall stop-watch)
```

---

## API 参考

### Set 数据结构

| 函数 | 描述 |
|------|------|
| `(twidget-set-make)` | 创建新的集合 |
| `(twidget-set-add set el)` | 添加元素 |
| `(twidget-set-has set el)` | 检查元素是否存在 |
| `(twidget-set-delete set el)` | 删除元素 |
| `(twidget-set-count set)` | 返回元素数量 |
| `(twidget-set-empty-p set)` | 检查是否为空 |
| `(twidget-set-to-list set)` | 转换为列表 |
| `(twidget-set-foreach set fn)` | 遍历每个元素 |
| `(twidget-set-clear set)` | 清空集合 |

### Map 数据结构

| 函数 | 描述 |
|------|------|
| `(twidget-map-make)` | 创建新的映射 |
| `(twidget-map-set map key value)` | 设置键值对 |
| `(twidget-map-get map key &optional default)` | 获取值 |
| `(twidget-map-has map key)` | 检查键是否存在 |
| `(twidget-map-delete map key)` | 删除键值对 |
| `(twidget-map-count map)` | 返回键值对数量 |
| `(twidget-map-keys map)` | 返回所有键 |
| `(twidget-map-values map)` | 返回所有值 |
| `(twidget-map-foreach map fn)` | 遍历每个键值对 |
| `(twidget-map-clear map)` | 清空映射 |

### Ref API

| 函数/宏 | 描述 |
|---------|------|
| `(twidget-ref value)` | 创建 ref |
| `(twidget-ref-get ref)` | 获取值（追踪依赖） |
| `(twidget-ref-set ref value)` | 设置值（触发更新） |
| `(twidget-ref-inc ref &optional delta)` | 递增 |
| `(twidget-ref-dec ref &optional delta)` | 递减 |
| `(twidget-$ ref)` | 获取值（简写） |
| `(twidget-$! ref value)` | 设置值（简写） |
| `(twidget-is-ref obj)` | 检查是否为 ref |
| `(twidget-unref ref)` | 解包 ref，非 ref 返回原值 |

### Reactive API

| 函数/宏 | 描述 |
|---------|------|
| `(twidget-reactive data)` | 创建响应式对象 |
| `(twidget-reactive-get obj key)` | 获取属性（追踪依赖） |
| `(twidget-reactive-set obj key value)` | 设置属性（触发更新） |
| `(twidget-reactive-has obj key)` | 检查属性是否存在 |
| `(twidget-reactive-delete obj key)` | 删除属性 |
| `(twidget-reactive-keys obj)` | 获取所有键 |
| `(twidget-reactive-to-plist obj)` | 转换为普通 plist |
| `(twidget-.  obj key)` | 获取属性（简写） |
| `(twidget-.! obj key value)` | 设置属性（简写） |
| `(twidget-is-reactive obj)` | 检查是否为响应式对象 |
| `(twidget-readonly obj)` | 创建只读版本 |
| `(twidget-is-readonly obj)` | 检查是否为只读 |

### Effect API

| 函数 | 描述 |
|------|------|
| `(twidget-effect fn &optional options)` | 创建副作用 |
| `(twidget-stop runner)` | 停止副作用 |
| `(twidget-pause-tracking)` | 暂停依赖追踪 |
| `(twidget-resume-tracking)` | 恢复依赖追踪 |
| `(twidget-without-tracking &rest body)` | 不追踪依赖执行代码 |

**Effect Options:**
- `:scheduler` - 自定义调度函数
- `:lazy` - 是否延迟首次执行
- `:on-stop` - 停止时的回调

### Computed API

| 函数 | 描述 |
|------|------|
| `(twidget-computed getter-or-options &optional setter)` | 创建计算属性 |
| `(twidget-computed-get computed)` | 获取值 |
| `(twidget-computed-set computed value)` | 设置值（需要 setter） |
| `(twidget-c$ computed)` | 获取值（简写） |
| `(twidget-is-computed obj)` | 检查是否为计算属性 |

**Computed Options (使用 plist 形式):**
- `:get` - getter 函数
- `:set` - setter 函数

### Watch API

| 函数 | 描述 |
|------|------|
| `(twidget-watch source callback &optional options)` | 创建侦听器 |
| `(twidget-watch-effect fn &optional options)` | 创建自动追踪的侦听器 |

**Watch Options:**
- `:immediate` - 是否立即执行
- `:deep` - 是否深度监听
- `:once` - 是否只触发一次

**Callback 签名:** `(lambda (new-value old-value on-cleanup) ... )`

### 批量更新 API

| 函数/宏 | 描述 |
|---------|------|
| `(twidget-batch &rest body)` | 批量执行更新，最后统一触发 |

### 工具函数

| 函数 | 描述 |
|------|------|
| `(twidget-to-raw obj)` | 获取底层原始数据 |
| `(twidget-to-ref reactive key)` | 将响应式对象的属性转为 ref |
| `(twidget-debug-deps target)` | 打印依赖信息 |
| `(twidget-debug-effect-count)` | 获取 effect 总数 |

### 定义宏

| 宏 | 描述 |
|----|------|
| `(twidget-define-reactive name data)` | 定义全局响应式变量 |
| `(twidget-define-ref name value)` | 定义全局 ref 变量 |
| `(twidget-with-reactive bindings &rest body)` | 创建局部响应式绑定 |

---

## 使用示例

### 示例 1：简单计数器

```elisp
;; 创建响应式计数器
(setq counter (twidget-ref 0))

;; 创建副作用，自动在计数变化时更新显示
(twidget-effect
 (lambda ()
   (message "Counter: %d" (twidget-$ counter))))

;; 更新计数器
(twidget-ref-inc counter)  ; 输出: Counter: 1
(twidget-ref-inc counter)  ; 输出:  Counter: 2
(twidget-$! counter 100)   ; 输出: Counter: 100
```

### 示例 2：购物车

```elisp
;; 创建购物车状态
(setq cart (twidget-reactive
            '(:items ((:name "Apple" :price 5 :qty 2)
                      (:name "Banana" :price 3 :qty 3))
              :discount 0.1)))

;; 计算总价
(setq total-price
      (twidget-computed
       (lambda ()
         (let* ((items (twidget-.  cart :items))
                (discount (twidget-. cart :discount))
                (subtotal (cl-reduce
                           (lambda (acc item)
                             (+ acc (* (plist-get item :price)
                                       (plist-get item :qty))))
                           items
                           :initial-value 0)))
           (* subtotal (- 1 discount))))))

;; 显示总价
(twidget-effect
 (lambda ()
   (message "Total: $%.2f" (twidget-c$ total-price))))
;; 输出:  Total: $17.10

;; 更新折扣
(twidget-. !  cart :discount 0.2)
;; 自动输出: Total: $15.20
```

### 示例 3：表单验证

```elisp
;; 表单状态
(setq form (twidget-reactive
            '(:username ""
              :email ""
              :password "")))

;; 验证规则
(setq username-valid
      (twidget-computed
       (lambda ()
         (let ((username (twidget-.  form :username)))
           (and (stringp username)
                (>= (length username) 3))))))

(setq email-valid
      (twidget-computed
       (lambda ()
         (let ((email (twidget-. form :email)))
           (and (stringp email)
                (string-match-p "@" email))))))

(setq form-valid
      (twidget-computed
       (lambda ()
         (and (twidget-c$ username-valid)
              (twidget-c$ email-valid)
              (>= (length (twidget-.  form :password)) 6)))))

;; 监听表单状态变化
(twidget-watch
 form-valid
 (lambda (is-valid _old)
   (if is-valid
       (message "Form is valid, submit button enabled")
     (message "Form is invalid")))
 '(:immediate t))

;; 更新表单
(twidget-.! form :username "alice")
(twidget-.!  form :email "alice@example.com")
(twidget-.! form :password "secret123")
;; 输出: Form is valid, submit button enabled
```

### 示例 4：深度监听

```elisp
(setq config (twidget-reactive
              '(:theme (:mode "dark" :primary "#007bff")
                : layout (:sidebar t :header t))))

;; 深度监听配置变化
(twidget-watch
 config
 (lambda (new-config _old)
   (message "Config changed: %S" new-config))
 '(:deep t))
```

### 示例 5：带清理的副作用

```elisp
(setq timer-ref (twidget-ref nil))
(setq interval (twidget-ref 1000))

(setq stop
      (twidget-watch-effect
       (lambda (on-cleanup)
         (let ((ms (twidget-$ interval))
               timer)
           (setq timer (run-at-time nil (/ ms 1000.0)
                                    (lambda () (message "Tick!"))))
           ;; 注册清理函数
           (funcall on-cleanup
                    (lambda ()
                      (when timer
                        (cancel-timer timer)
                        (message "Timer stopped"))))))))

;; 更改间隔会自动取消旧定时器并创建新的
(twidget-$! interval 2000)

;; 停止监听
(funcall stop)
```

### 示例 6：批量更新

```elisp
(setq state (twidget-reactive '(:a 1 :b 2 :c 3)))
(setq sum (twidget-computed
           (lambda ()
             (+ (twidget-.  state :a)
                (twidget-. state :b)
                (twidget-. state :c)))))

(setq update-count 0)
(twidget-effect
 (lambda ()
   (setq update-count (1+ update-count))
   (message "Sum: %d (update #%d)" (twidget-c$ sum) update-count)))

;; 不使用批量更新：会触发3次
(twidget-. ! state :a 10)
(twidget-.!  state :b 20)
(twidget-.! state : c 30)

;; 使用批量更新：只触发1次
(twidget-batch
 (twidget-. ! state :a 100)
 (twidget-.!  state :b 200)
 (twidget-.! state : c 300))
```

### 示例 7：toRef 转换

```elisp
(setq user (twidget-reactive '(:name "Alice" :age 25)))

;; 将响应式对象的属性转换为 ref
(setq name-ref (twidget-to-ref user :name))

;; 修改 ref 会同步到原对象
(twidget-computed-set name-ref "Bob")
(twidget-.  user :name)  ; => "Bob"

;; 修改原对象也会反映到 ref
(twidget-. ! user :name "Charlie")
(twidget-c$ name-ref)  ; => "Charlie"
```

---

## 最佳实践

### 1. 选择合适的响应式类型

```elisp
;; 单个值使用 ref
(setq count (twidget-ref 0))
(setq name (twidget-ref ""))
(setq enabled (twidget-ref t))

;; 对象/结构化数据使用 reactive
(setq user (twidget-reactive '(:id 1 :name "Alice" :email "...")))
(setq settings (twidget-reactive '(:theme "dark" :lang "en")))
```

### 2. 避免在 effect 中修改依赖

```elisp
;; ❌ 错误：可能导致无限循环
(twidget-effect
 (lambda ()
   (twidget-$! counter (1+ (twidget-$ counter)))))

;; ✅ 正确：使用 watch 或 without-tracking
(twidget-watch
 counter
 (lambda (new-val _old)
   (when (< new-val 10)
     (twidget-$!  counter (1+ new-val)))))
```

### 3. 使用 computed 缓存昂贵计算

```elisp
;; ✅ 使用 computed：结果被缓存
(setq filtered-list
      (twidget-computed
       (lambda ()
         (seq-filter #'expensive-predicate
                     (twidget-. state :items)))))

;; ❌ 在 effect 中重复计算
(twidget-effect
 (lambda ()
   ;; 每次依赖变化都重新计算
   (let ((result (seq-filter #'expensive-predicate
                             (twidget-. state :items))))
     (do-something result))))
```

### 4. 及时清理

```elisp
;; 保存停止函数以便清理
(setq cleanup-fns '())

(push (twidget-watch source callback) cleanup-fns)
(push (twidget-watch-effect effect-fn) cleanup-fns)

;; 清理时调用所有停止函数
(mapc #'funcall cleanup-fns)
```

### 5. 使用批量更新优化性能

```elisp
;; 当需要同时更新多个值时使用 batch
(twidget-batch
 (twidget-. ! state :loading t)
 (twidget-.!  state :data nil)
 (twidget-.! state :error nil))
```

---

## 与 Vue3 的对比

| Vue3 | Twidget | 说明 |
|------|---------|------|
| `ref(value)` | `(twidget-ref value)` | 创建响应式引用 |
| `ref.value` | `(twidget-ref-get ref)` | 获取 ref 值 |
| `ref.value = x` | `(twidget-ref-set ref x)` | 设置 ref 值 |
| `reactive(obj)` | `(twidget-reactive obj)` | 创建响应式对象 |
| `obj.key` | `(twidget-reactive-get obj :key)` | 获取属性 |
| `obj.key = x` | `(twidget-reactive-set obj :key x)` | 设置属性 |
| `computed(() => ...)` | `(twidget-computed (lambda () ...))` | 计算属性 |
| `watch(source, cb)` | `(twidget-watch source cb)` | 侦听器 |
| `watchEffect(fn)` | `(twidget-watch-effect fn)` | 自动追踪侦听器 |
| `readonly(obj)` | `(twidget-readonly obj)` | 只读包装 |
| `toRef(obj, key)` | `(twidget-to-ref obj key)` | 属性转 ref |
| `unref(ref)` | `(twidget-unref ref)` | 解包 ref |
| `isRef(x)` | `(twidget-is-ref x)` | 类型检查 |
| `isReactive(x)` | `(twidget-is-reactive x)` | 类型检查 |
| `toRaw(obj)` | `(twidget-to-raw obj)` | 获取原始数据 |

### 主要差异

1. **属性访问**：Vue3 使用 Proxy 自动拦截，Emacs 需要显式调用 getter/setter
2. **键名格式**：Vue3 使用字符串键，Twidget 推荐使用关键字（如 `:name`）
3. **模板集成**：Vue3 有模板系统，Twidget 需要手动创建 UI 更新 effects

---

## 许可证

MIT License
