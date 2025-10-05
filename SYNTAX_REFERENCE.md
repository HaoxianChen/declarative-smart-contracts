# 时序属性语法快速参考

## ✅ 正确语法

### 时序操作符（必须带括号）

```
✅ ONCE (withdraw())
✅ ALWAYS (balance(p, n) IMPLY n >= 0)
✅ NOT (ONCE (eventA()) AND ONCE (eventB()))
```

### 逻辑操作符

```
✅ expr1 AND expr2
✅ expr1 OR expr2
✅ expr1 IMPLY expr2
✅ expr1 IMPLIES expr2  (IMPLY 的别名)
```

### 比较操作符

```
✅ x == y    或  x = y
✅ x != y
✅ x < y
✅ x <= y
✅ x > y
✅ x >= y
```

---

## ❌ 错误语法

### 时序操作符缺少括号

```
❌ ONCE withdraw()           → 应改为: ONCE (withdraw())
❌ ALWAYS balance >= 0       → 应改为: ALWAYS (balance >= 0)
❌ NOT ONCE event()          → 应改为: NOT (ONCE (event()))
```

### 嵌套时序操作符缺少括号

```
❌ NOT (ONCE eventA() AND ONCE eventB())
   → 应改为: NOT (ONCE (eventA()) AND ONCE (eventB()))

❌ ALWAYS (ONCE close() IMPLY balance > 0)
   → 应改为: ALWAYS (ONCE (close()) IMPLY balance > 0)
```

---

## 完整示例

### 示例 1: 简单不变量

```
// 余额始终非负
ALWAYS (balance(p, n) IMPLY n >= 0)
```

### 示例 2: 事件互斥

```
// withdraw 和 refund 不能同时发生
NOT (ONCE (withdraw()) AND ONCE (refund()))
```

### 示例 3: 事件前提条件

```
// 只有在达到目标后才能提款
ALWAYS (ONCE (withdraw()) IMPLY raised(r) AND target(t) AND r >= t)
```

### 示例 4: 复杂组合

```
// 如果拍卖结束，则之后提款次数不超过1次
ONCE (end(true)) IMPLY ALWAYS (withdrawCount(p, c) IMPLY c <= 1)
```

---

## 记忆技巧

**规则**: 凡是 `ONCE`、`ALWAYS`、`NOT` 后面都要加括号，把整个子表达式括起来。

```
模式: OPERATOR (expression)
      ^^^^^^^^  ^^^^^^^^^^^
      操作符     必须有括号
```

**嵌套时**: 每一层都要加括号

```
NOT (ONCE (eventA()))
     ^^^^  ^^^^^^^^^
     外层  内层都要括号
```

---

## 快速检查清单

在写时序属性时，检查以下几点：

- [ ] 所有的 `ONCE` 后面都有 `(`
- [ ] 所有的 `ALWAYS` 后面都有 `(`
- [ ] 所有的 `NOT` 后面都有 `(`（当它是时序操作符时）
- [ ] 括号都匹配（开括号和闭括号数量相等）
- [ ] 所有逻辑关键字都是大写（`AND`、`OR`、`IMPLY`）
- [ ] 函数调用的参数用逗号分隔
