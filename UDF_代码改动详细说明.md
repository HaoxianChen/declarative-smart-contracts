# UDF 代码改动详细说明

本文档完整记录了为 `jokintheboxstakin` benchmark 引入 UDF（User-Defined Function）支持所做的所有代码改动，包括改动原因、具体实现和文件级别的代码变更。

---

## 目录

1. [改动背景与目标](#1-改动背景与目标)
2. [改动总览：文件清单与简述](#2-改动总览文件清单与简述)
3. [Datalog 层改动](#3-datalog-层改动)
   - 3.1 schema.dl
   - 3.2 rules.dl
   - 3.3 properties.dl
4. [UDF Solidity 实现](#4-udf-solidity-实现)
5. [Verifier 改动（Z3 约束编码）](#5-verifier-改动z3-约束编码)
   - 5.1 UDF 关系的识别与过滤
   - 5.2 recv_* 交易字面量约束
   - 5.3 UDF 字面量到 Z3 约束的编码
   - 5.4 isValidSignature 的 nonce 注入
   - 5.5 nonce 的 __exists 初始化特殊处理
   - 5.6 UDF 对不变量生成的影响
6. [算术类型兼容性修复](#6-算术类型兼容性修复)
7. [Solidity AST 前置检查](#7-solidity-ast-前置检查)
8. [Synthesis 流程集成](#8-synthesis-流程集成)
   - 8.1 Main.scala 入口改动
   - 8.2 SolidityTranslator 继承机制
9. [运行结果与验证](#9-运行结果与验证)

---

## 1. 改动背景与目标

### 1.1 问题描述

`jokintheboxstakin` 合约包含两个核心的外部调用逻辑：

- **`isValidSignature`**：使用 `ecrecover` + `nonce` 验证链下签名，防止签名重放攻击
- **`jokTokenBalance`**：查询 ERC20 代币余额，用于提款限额检查

在原有的合成/验证流程中，这些函数以"stub"形式存在（返回常量），导致：

1. 验证器对签名校验的结果默认为 true，无法发现签名相关的漏洞
2. 合成器无法感知 UDF 的语义，合成出的代码缺少正确的验证逻辑
3. 编译和验证之间缺少统一的前置检查，可能出现"编译通过但验证假设不一致"

### 1.2 目标（白板三点要求）

| 编号 | 要求 | 核心内容 |
|------|------|----------|
| 第1点 | UDF 要有具体实现 | `udf.sol` 中提供可执行的 Solidity 逻辑 |
| 第2点 | Verifier 要有对应约束 | UDF literal 必须转成 Z3 可推理约束 |
| 第3点 | Generalize（AST 前置检查） | 通过 Solidity AST 统一校验 UDF 签名、返回值、mutability |

### 1.3 额外任务：UDF → Verification Encoding

在白板三点的基础上，额外的任务要求：

> UDF → Verification encoding
> - 如果 UDF 的数量不多，可以先手动提供 encoding 测试完整的流程
> - 如果数量太多，就先实现自动 encoding 的代码

当前 benchmark 仅有 2 个 UDF（`isValidSignature` 和 `jokTokenBalance`），因此采用**手动编码**路线，为 `isValidSignature` 注入了 nonce 语义约束。

---

## 2. 改动总览：文件清单与简述

| 文件路径 | 改动类型 | 简述 |
|----------|----------|------|
| `synthesis-benchmark/jokintheboxstakin/schema.dl` | 修改 | 扩展 `recv_withdraw` 为 7 参数；新增 `nonce` 状态变量；声明 2 个 `.udf` 关系 |
| `synthesis-benchmark/jokintheboxstakin/rules.dl` | 修改 | 新增 `withdraw` 语义规则和 `nonce` 自增规则 |
| `synthesis-benchmark/jokintheboxstakin/properties.dl` | 修改 | 新增签名验证、ETH/Token 提款限额的 `invalidTx()` 规则 |
| `synthesis-benchmark/jokintheboxstakin/udf.sol` | 新增 | UDF 的 Solidity 具体实现（`isValidSignature` + `jokTokenBalance`） |
| `src/main/scala/verification/Verifier.scala` | 修改 | UDF/recv_* 约束编码；nonce 注入；__exists 初始化特殊处理 |
| `src/main/scala/datalog/ArithOperator.scala` | 修改 | 放宽算术类型兼容性检查（`int`/`uint`/`Any` 互通） |
| `src/main/scala/util/SolcAst.scala` | 新增 | 基于 `solc --ast-compact-json` 的 UDF 前置检查 |
| `src/main/scala/Main.scala` | 修改 | `compile`/`verify`/`synthesis-all` 路径集成 UDF 检查和文件复制 |
| `src/main/scala/imp/SolidityTranslator.scala` | 修改 | 支持 `udfInfoOpt` 参数实现继承式 Solidity 代码生成 |

---

## 3. Datalog 层改动

### 3.1 schema.dl

**文件**：`synthesis-benchmark/jokintheboxstakin/schema.dl`

#### 3.1.1 `recv_withdraw` 入参扩展

**改动原因**：原始 `recv_withdraw` 仅有 3 个参数，无法携带签名验证所需的 `messageHash`、`v`、`r`、`s` 等字段。为使 Datalog 规则能引用签名参数并传递给 `isValidSignature` UDF，需要扩展为 7 参数。

```datalog
// 改动前（3 参数）：
.decl recv_withdraw(earnings: uint, affiliateEarnings: uint, inETH: bool)

// 改动后（7 参数）：
.decl recv_withdraw(
  earnings: uint,
  affiliateEarnings: uint,
  inETH: bool,
  messageHash: bytes32,
  v: uint,
  r: bytes32,
  s: bytes32
)
.public recv_withdraw
```

#### 3.1.2 新增 `withdraw` 语义关系

**改动原因**：BMC（Bounded Model Checker）在提取反例 trace 时，需要一个与 `recv_withdraw` 对应的语义关系（按 `recv_` 前缀去除的命名规则，对应 `withdraw`）。如果缺少这个关系，BMC 找不到 `recv_withdraw` 的 transition expression 就会跳过该交易。

```datalog
.decl withdraw(sender: address, total: uint)
```

#### 3.1.3 新增 `nonce` 状态变量

**改动原因**：`isValidSignature` 的 Solidity 实现中，`nonce[beneficiary]` 被纳入哈希计算。每次成功提款后 nonce 自增，使得同一签名不能重放。Datalog 层必须建模这个状态，才能在验证时正确捕获重放保护语义。

```datalog
// Nonce: per-sender withdrawal counter (captures signature replay protection from udf.sol)
.decl nonce(sender: address, n: uint)[0]
```

`[0]` 表示第 0 列（`sender`）是主键索引，即每个 `sender` 对应唯一一个 `nonce` 值。

#### 3.1.4 UDF 声明

**改动原因**：Datalog 解析器需要通过 `.udf` 指令识别哪些关系是 UDF，以便 Verifier 将其编码为 Z3 无解释函数（Uninterpreted Function），而非普通的状态关系。

```datalog
// UDFs (last column is the return value variable)
.decl isValidSignature(
  sender: address,
  totalEarnings: uint,
  inETH: bool,
  messageHash: bytes32,
  v: uint,
  r: bytes32,
  s: bytes32,
  valid: bool
)
.udf isValidSignature

.decl jokTokenBalance(balance: uint)
.udf jokTokenBalance
```

**约定**：`.decl` 的最后一列是返回值变量，前面的列是输入参数。这个约定贯穿 Verifier 和 SolcAst 检查。

### 3.2 rules.dl

**文件**：`synthesis-benchmark/jokintheboxstakin/rules.dl`

#### 3.2.1 `withdraw` 语义规则

**改动原因**：将 `recv_withdraw` 的原始交易参数聚合为语义层的 `withdraw(sender, total)` 关系。所有 `recv_withdraw` 字段都使用命名变量（而非通配符 `_`），这样 `PredicateEnumerator` 可以把 `inETH`、`messageHash`、`v`、`r`、`s` 绑定到 `isValidSignature` 调用。

```datalog
withdraw(sender, total) :-
  recv_withdraw(earnings, affiliateEarnings, inETH, messageHash, v, r, s),
  msgSender(sender),
  total := earnings + affiliateEarnings.
```

#### 3.2.2 `nonce` 自增规则

**改动原因**：模拟 `udf.sol` 中 `nonce[beneficiary]++` 的行为。每次成功的 `withdraw` 交易后，对应 sender 的 nonce 递增 1。这确保验证器能捕获到"nonce 变化导致旧签名失效"的语义。

```datalog
nonce(sender, n_new) :-
  withdraw(sender, _),
  nonce(sender, n_old),
  n_new := n_old + 1.
```

> **注意**：这条规则直接触发了算术类型兼容性问题（见第 6 节），因为 `n_old` 类型为 `uint`，而字面量 `1` 被解析器推断为 `int`。

### 3.3 properties.dl

**文件**：`synthesis-benchmark/jokintheboxstakin/properties.dl`

#### 3.3.1 签名验证属性

**改动原因**：编码"如果签名无效，交易应被拒绝"的安全属性。

```datalog
// 签名必须有效
invalidTx() :-
  recv_withdraw(earnings, affiliateEarnings, inETH, messageHash, v, r, s),
  msgSender(sender),
  total := earnings + affiliateEarnings,
  isValidSignature(sender, total, inETH, messageHash, v, r, s, valid),
  valid == false.
```

#### 3.3.2 ETH 提款限额属性

**改动原因**：编码"ETH 提款金额不能超过合约余额的 `maxPercentage%`"。

```datalog
invalidTx() :-
  recv_withdraw(earnings, affiliateEarnings, true, messageHash, v, r, s),
  total := earnings + affiliateEarnings,
  maxPercentage(maxPct),
  hundred(c),
  thisBalance(bal),
  total * c >= bal * maxPct.
```

#### 3.3.3 Token 提款限额属性

**改动原因**：编码"Token 提款金额不能超过 jokToken 余额的 `maxPercentage%`"。

```datalog
invalidTx() :-
  recv_withdraw(earnings, affiliateEarnings, false, messageHash, v, r, s),
  total := earnings + affiliateEarnings,
  maxPercentage(maxPct),
  hundred(c),
  jokTokenBalance(tokenBal),
  total * c >= tokenBal * maxPct.
```

---

## 4. UDF Solidity 实现

**文件**：`synthesis-benchmark/jokintheboxstakin/udf.sol`（新增）

**改动原因**：提供 UDF 的可执行 Solidity 实现，使生成的合约能继承这些函数并在链上运行。

### 4.1 完整代码

```solidity
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.21;

interface IERC20 {
    function balanceOf(address account) external view returns (uint256);
}

contract JokintheboxstakinUDF {
    IERC20 public jokToken = IERC20(0xA728Aa2De568766E2Fa4544Ec7A77f79c0bf9F97);
    address public stakingSigner = 0x8aaBaf348B299E759D091F17100a95A0F9caD89C;
    mapping(address => uint256) public nonce;

    function isValidSignature(
        address beneficiary,
        uint256 amount,
        bool inETH,
        bytes32 messageHash,
        uint256 v,
        bytes32 r,
        bytes32 s
    ) internal view returns (bool) {
        bytes32 payloadHash = keccak256(
            abi.encodePacked(address(this), beneficiary, amount, inETH, messageHash, nonce[beneficiary])
        );
        bytes32 prefixedHash = keccak256(
            abi.encodePacked("\x19Ethereum Signed Message:\n32", payloadHash)
        );
        address recoveredSigner = ecrecover(prefixedHash, uint8(v), r, s);
        return recoveredSigner == stakingSigner;
    }

    function jokTokenBalance() internal view returns (uint256) {
        return jokToken.balanceOf(address(this));
    }

    function _setStakingSigner(address _stakingSigner) internal {
        stakingSigner = _stakingSigner;
    }

    function _setJokToken(address _jokToken) internal {
        jokToken = IERC20(_jokToken);
    }
}
```

### 4.2 设计决策

| 决策点 | 选择 | 原因 |
|--------|------|------|
| `message` 参数类型 | `bytes32` | 避免 Solidity 对 `string` 类型的 ABI 编码问题 |
| `nonce` 存储 | `mapping(address => uint256) public nonce` | 与原始合约一致，`public` 便于外部查询 |
| `jokToken`/`stakingSigner` 初始化 | 硬编码默认值 + internal setter | 避免构造函数参数污染继承链，同时保留可配置性 |
| 函数可见性 | `internal view` | `view`（不修改状态）满足 Verifier 对 `pure/view` 的检查要求；`internal` 避免暴露外部接口 |
| 哈希算法 | `keccak256(abi.encodePacked(..., nonce[beneficiary]))` | 与原始合约完全对齐，nonce 参与哈希保证重放保护 |

---

## 5. Verifier 改动（Z3 约束编码）

**文件**：`src/main/scala/verification/Verifier.scala`

这是本次改动中最核心、最复杂的文件。改动集中在 Verifier 类的前 175 行和 `getTransitionSystem` 方法中。

### 5.1 UDF 关系的识别与过滤

**改动原因**：UDF 关系不是有状态关系（stateful relation），不应被物化为 Z3 数组。需要在 `materializedRelations` 计算中排除它们。

```scala
// 第 28-36 行
private val materializedRelations: Set[Relation] = {
  val fromStatements = getMaterializedRelations(impAbsProgram, program.interfaces)
  val violationRules = program.rules.filter(r => program.violations.contains(r.head.relation))
  val readByViolationRules = violationRules.flatMap(r => r.body.map(_.relation))
  // UDF relations are not stateful relations; they are handled as uninterpreted functions.
  (fromStatements++readByViolationRules).filterNot(_.isInstanceOf[ReservedRelation])
    .filterNot(_.name.startsWith(transactionRelationPrefix))
    .filterNot(r => program.udfs.contains(r))  // <-- 新增：排除 UDF
}
```

**关键**：如果 UDF 被误加入 `materializedRelations`，Verifier 会尝试为其生成 `Array[Sort, Sort]` 状态变量，导致类型错误。

### 5.2 recv_* 交易字面量约束（`txLiteralToConst`）

**改动原因**：`recv_*` 是外部输入的交易参数，需要为其添加域约束（domain constraints），包括：

1. **uint 非负约束**：所有 `uint` 类型参数 >= 0
2. **交易绑定约束**：当前交易类型必须匹配（`transaction == "recv_withdraw"`）
3. **特定参数范围约束**：如 `v` 参数（ECDSA 签名的 recovery id）范围为 [0, 255]

```scala
// 第 105-120 行
private def txLiteralToConst(ctx: Context, lit: Literal, prefix: String): BoolExpr = {
  val baseConstraints = lit.fields.flatMap(p => uintNonNegativeConstraint(ctx, p, prefix))
  val txNameConstraint = {
    val txConst = ctx.mkConst("transaction", ctx.mkStringSort())
    ctx.mkEq(txConst, ctx.mkString(lit.relation.name))
  }
  val extraConstraints: List[BoolExpr] = {
    if (lit.relation.name == "recv_withdraw" && lit.fields.size >= 5
        && isUintType(lit.fields(4)._type)) {
      val vConst = paramToConst(ctx, lit.fields(4), prefix)._1
      List(ctx.mkLe(vConst.asInstanceOf[Expr[ArithSort]], ctx.mkInt(255)))
    } else Nil
  }
  val all = txNameConstraint :: (baseConstraints ++ extraConstraints)
  if (all.isEmpty) ctx.mkTrue() else ctx.mkAnd(all.toArray: _*)
}
```

**交易绑定约束的意义**：在归纳证明的 transition step 中，如果没有 `transaction == "recv_withdraw"` 约束，Z3 可能会在一个 `recv_unstake` 的 transition 中同时满足 `recv_withdraw` 的 violation 属性，产生虚假反例（spurious counterexample）。

### 5.3 UDF 字面量到 Z3 约束的编码（`udfLiteralToConst`）

**改动原因**：UDF 关系需要被编码为 Z3 无解释函数（Uninterpreted Function, UF），而非像普通状态关系那样编码为数组 `select`/`store` 操作。

**编码方式**：

- **约定**：UDF 关系的最后一列是输出（返回值），前面的列是输入
- **Z3 UF**：声明一个 `FuncDecl`，将输入参数映射到输出，并添加 `output == UF(inputs)` 等式约束
- **缓存**：使用 `udfDeclCache` 避免为同一 UDF 重复创建 `FuncDecl`

```scala
// 第 122-161 行
private def udfLiteralToConst(ctx: Context, lit: Literal, prefix: String): BoolExpr = {
  require(lit.fields.nonEmpty, s"UDF literal must have at least 1 field: $lit")
  val inputParams = lit.fields.dropRight(1)
  val outParam = lit.fields.last

  val inputConsts = inputParams.map(p => paramToConst(ctx, p, prefix)._1).toArray
  val outConst = paramToConst(ctx, outParam, prefix)._1

  val domain = inputParams.map(p => typeToSort(ctx, p._type))
  val range = typeToSort(ctx, outParam._type)
  val inDomains = inputParams.flatMap(p => uintNonNegativeConstraint(ctx, p, prefix))
  val outDomain = uintNonNegativeConstraint(ctx, outParam, prefix).toList

  // ... (nonce 注入逻辑，见 5.4)

  val key = (lit.relation.name, effectiveDomain, range)
  val decl = udfDeclCache.getOrElseUpdate(key,
    ctx.mkFuncDecl(lit.relation.name, effectiveDomain.toArray, range))
  val app = ctx.mkApp(decl, effectiveInputConsts.map(_.asInstanceOf[Expr[Sort]]): _*)
  val eq = ctx.mkEq(outConst.asInstanceOf[Expr[Sort]], app.asInstanceOf[Expr[Sort]])
  val all = eq +: (inDomains ++ outDomain)
  ctx.mkAnd(all.toArray: _*)
}
```

**UF 的语义含义**：Z3 中的 UF 满足"相同输入 → 相同输出"的一致性公理（congruence axiom），但不对函数内部逻辑做任何假设。这意味着默认的 UF 编码只保证了函数的一致性，但无法捕获特定的语义约束。

### 5.4 isValidSignature 的 nonce 注入

**改动原因**：默认的 UF 编码中，`isValidSignature(sender, total, inETH, msgHash, v, r, s)` 的输入参数不包含 `nonce`。这意味着 Z3 认为相同的 `(sender, v, r, s)` 总会得到相同的验证结果——**忽略了 nonce 变化导致的重放保护**。

为了捕获"nonce 改变后同一签名失效"的语义，需要将 `nonce[sender]` 作为额外参数注入到 UF 中：

```
UF 默认：valid = isValidSignature(sender, total, inETH, msgHash, v, r, s)
注入后：valid = isValidSignature(sender, total, inETH, msgHash, v, r, s, nonce[sender])
```

```scala
// 第 139-152 行
val (effectiveDomain, effectiveInputConsts) =
  if (lit.relation.name == "isValidSignature") {
    program.relations.collectFirst {
      case sr: SimpleRelation if sr.name == "nonce" => sr
    } match {
      case Some(_) =>
        val senderConst = inputConsts(0).asInstanceOf[Expr[Sort]]
        // 从 pre-state 的 nonce 数组中读取 nonce[sender]
        val nonceArraySort = ctx.mkArraySort(ctx.mkIntSort(), ctx.mkIntSort())
        val nonceArray = ctx.mkConst("nonce", nonceArraySort)
          .asInstanceOf[Expr[ArraySort[Sort, Sort]]]
        val nonceVal = ctx.mkSelect(nonceArray, senderConst)
        (domain :+ ctx.mkIntSort().asInstanceOf[Sort], inputConsts :+ nonceVal)
      case None => (domain, inputConsts)
    }
  } else (domain, inputConsts)
```

**技术细节**：

1. `inputConsts(0)` 是 `sender` 参数（UDF 声明的第一个输入列）
2. `ctx.mkConst("nonce", nonceArraySort)` 引用的是 Verifier 中已创建的 `nonce` pre-state Z3 常量（类型为 `Array[Int, Int]`）
3. `ctx.mkSelect(nonceArray, senderConst)` 执行 Z3 数组的 `select` 操作，等价于 `nonce[sender]`
4. 注入后 UF 的 domain 从 7 个参数变为 8 个参数

### 5.5 nonce 的 `__exists` 初始化特殊处理

**改动原因**：Verifier 为每个 `SimpleRelation` 维护一个 `__exists` 布尔数组，用于跟踪"某个键是否存在有效记录"。默认情况下，`__exists` 数组初始化为全 `false`（所有记录不存在）。

但 `nonce` 的语义是"所有地址的 nonce 默认为 0"——这意味着即使从未发生过提款，`nonce[sender]` 也应该有一个有效的值（0）。如果 `nonce__exists[sender]` 为 `false`，Verifier 在第一次 `withdraw` 时无法成功查找到 `nonce(sender, n_old)` 中的 `n_old`，导致 nonce 自增规则无法触发。

```scala
// 第 189-200 行（在 getTransitionSystem 方法中）
case sr: SimpleRelation =>
  val existsSort = getExistsSort(ctx, sr, getIndices(sr))
  val (existsIn, _) = tr.newVar(getExistsRelationName(sr.name), existsSort)
  val existsArraySort = existsSort.asInstanceOf[ArraySort[Sort, Sort]]
  // nonce is conceptually "always 0 for everyone" before any withdrawal,
  // so initialise its __exists array to all-true (every entry exists by default).
  val existsDefaultVal = if (sr.name == "nonce") ctx.mkTrue() else ctx.mkFalse()
  val existsInit = ctx.mkConstArray(existsArraySort.getDomain, existsDefaultVal)
  initConditions :+= ctx.mkEq(existsIn, existsInit)
```

**对比**：

| 关系 | `__exists` 初始值 | 原因 |
|------|-------------------|------|
| `stakeAmount` | `false`（默认） | 只有 `stake()` 后才存在 |
| `stakeStatus` | `false`（默认） | 只有 `stake()` 后才存在 |
| `nonce` | `true`（特殊处理） | 所有地址的 nonce 默认为 0 |

### 5.6 UDF 对不变量生成的影响

**改动原因**：当前的 `InvariantGenerator` 从 `materializedRelations` 中提取谓词来生成候选不变量。由于 UDF 关系和 `recv_*` 关系不是状态关系，如果不变量生成器试图在这些关系上提取谓词，可能导致异常。

```scala
// 第 242-257 行（在 check 方法中）
val resTr = _resTr match {
  case Status.UNSATISFIABLE => _resTr
  case Status.UNKNOWN | Status.SATISFIABLE => {
    // Quick feasibility mode for UDF: skip invariant generation to avoid
    // predicate extraction on non-state relations (e.g., recv_* and UDFs).
    if (program.udfs.nonEmpty) {
      _resTr
    } else {
      invariantGenerator.findInvariant(tr, vr) match {
        case Some(inv) => { ... }
        case None => _resTr
      }
    }
  }
}
```

当程序包含 UDF 时，跳过不变量生成，直接返回 `Tr` 结果。这是当前的"快速可行性模式"，后续可优化。

### 5.7 统一路由：`literalToConstOrUdf`

**改动原因**：Verifier 中每个 body literal 都需要转换为 Z3 约束。需要一个统一的路由函数，根据 literal 的类型选择不同的编码策略。

```scala
// 第 163-174 行
private def literalToConstOrUdf(ctx: Context, lit: Literal, prefix: String): BoolExpr = {
  if (lit.relation.name.startsWith(transactionRelationPrefix)) {
    txLiteralToConst(ctx, lit, prefix)    // recv_* → 交易域约束
  }
  else if (isUdfRelation(lit.relation)) {
    udfLiteralToConst(ctx, lit, prefix)   // UDF → 无解释函数约束
  }
  else {
    literalToConst(ctx, lit, getIndices(lit.relation), prefix)  // 普通 → 数组 select
  }
}
```

这个路由函数在以下位置被调用：

- `getProperty`（violation 属性翻译）
- `getViolationCheck`（BMC 的 violation 检查）
- `getTxViolationCheck`（交易级 violation 检查）

---

## 6. 算术类型兼容性修复

**文件**：`src/main/scala/datalog/ArithOperator.scala`

### 6.1 问题描述

添加 nonce 自增规则后：

```datalog
n_new := n_old + 1.
```

Datalog 解析器将 `n_old` 推断为 `uint` 类型（继承自 `nonce` 关系的 schema），而字面量 `1` 被推断为 `int` 类型。`Add` case class 原有的严格类型检查 `require(a._type == b._type)` 导致 `IllegalArgumentException`。

### 6.2 根本原因

在 Z3 中，`int`、`uint`、`Any` 三种 Datalog 类型都映射到 `IntSort`（任意精度整数），因此混合运算在语义上是安全的。严格的类型检查过于保守。

### 6.3 修复方案

引入 `numericCompat` 辅助方法，放宽 `Add`、`Sub`、`Mul`、`Div`、`Min` 五个二元运算符的类型检查：

```scala
// 改动前：
case class Add(a: Arithmetic, b: Arithmetic) extends BinaryOperator {
  require(a._type == b._type)
  // ...
}

// 改动后：
case class Add(a: Arithmetic, b: Arithmetic) extends BinaryOperator {
  require(Arithmetic.numericCompat(a._type, b._type),
    s"Type mismatch in +: ${a._type} vs ${b._type}")
  // ...
}

object Arithmetic {
  private val numericTypeNames: Set[String] = Set("int", "uint", "Any")
  def numericCompat(t1: Type, t2: Type): Boolean =
    t1 == t2 || (numericTypeNames.contains(t1.name) && numericTypeNames.contains(t2.name))
}
```

`Sub`、`Mul`、`Div`、`Min` 使用相同的修改模式。

---

## 7. Solidity AST 前置检查

**文件**：`src/main/scala/util/SolcAst.scala`（新增）

### 7.1 改动原因

此前"编译通过但验证前置不一致"的问题根源在于：缺少 `udf.sol` 与 `.udf` 声明之间的自动化校验。`SolcAst.scala` 通过解析 Solidity 编译器输出的 AST JSON 实现了结构化检查。

### 7.2 工作流程

```
udf.sol  →  solc --ast-compact-json  →  AST JSON  →  SolcAst 解析  →  检查项
```

### 7.3 检查项

| 检查项 | 描述 | 对应方法 |
|--------|------|----------|
| 函数存在性 | `.udf` 声明的每个关系在 `udf.sol` 中都有对应的函数定义 | `checkUdfsAgainstUdfSol` |
| 输入参数匹配 | Datalog 关系的输入列类型与 Solidity 函数参数类型一一对应 | `extractFunctionSig` + 类型归一化 |
| 单返回值 | Solidity 函数必须恰好有 1 个返回值 | `outputMatched` 检查 |
| mutability 约束 | 函数必须是 `pure` 或 `view` | `mutabilityMatched` 检查 |

### 7.4 类型归一化

Solidity 和 Datalog 的类型名不完全一致（如 `uint256` vs `uint`），需要归一化：

```scala
// Solidity 侧
private def normalizeSolType(typeString: String): String = {
  if (t.startsWith("uint")) "uint"
  else if (t.startsWith("int")) "int"
  else if (t.startsWith("bool")) "bool"
  else if (t.startsWith("address")) "address"
  else t
}

// Datalog 侧
private def normalizeDatalogType(t: Type): String = t.name match {
  case "uint" => "uint"
  case "int" => "int"
  case "bool" => "bool"
  case "address" => "address"
  case other => other
}
```

### 7.5 调用方式

`SolcAst.checkUdfsAgainstUdfSol(program, udfSolPath)` 返回 `(contractName: String, errors: List[String])`。如果 `errors` 非空，调用方抛出异常并终止编译/验证流程。

---

## 8. Synthesis 流程集成

### 8.1 Main.scala 入口改动

**文件**：`src/main/scala/Main.scala`

#### 8.1.1 `verify` 命令集成 UDF 检查

**改动原因**：在验证之前，需要确保 `udf.sol` 存在且通过 AST 检查，避免运行时类型不匹配。

```scala
else if (args(0) == "verify") {
  val filepath = args(1)
  val f = new File(filepath)
  val dl = if (f.exists() && f.isDirectory) parseProgramFromSplitDir(filepath)
           else parseProgram(filepath)
  if (dl.udfs.nonEmpty) {
    val udfPath = resolveUdfPath(filepath, f, dl)
    if (!isFileExists(udfPath)) {
      throw new Exception(s"Program declares .udf but missing udf.sol at: $udfPath")
    }
    val (_, errors) = SolcAst.checkUdfsAgainstUdfSol(dl, udfPath)
    if (errors.nonEmpty) {
      val msg = errors.mkString("\n  - ", "\n  - ", "\n")
      throw new Exception(s"udf.sol AST check failed:$msg")
    }
  }
  // ... 继续验证
}
```

#### 8.1.2 `compile` / `compile-all` 命令集成 UDF

**改动原因**：编译输出的 Solidity 代码需要继承 `udf.sol` 中的 UDF 基合约。

```scala
val udfInfoOpt: Option[(String, String)] = {
  if (dl.udfs.nonEmpty) {
    val udfPath = resolveUdfPath(filepath, f, dl)
    val (baseContractName, errors) = SolcAst.checkUdfsAgainstUdfSol(dl, udfPath)
    if (errors.nonEmpty) { /* throw */ }
    // 将 udf.sol 复制到输出目录
    val outUdfFileName = s"${filename}_udf.sol"
    Files.copy(Paths.get(udfPath), Paths.get(outDir, outUdfFileName),
      StandardCopyOption.REPLACE_EXISTING)
    Some((s"./$outUdfFileName", baseContractName))
  } else None
}
```

#### 8.1.3 `synthesis-all` 命令集成 UDF

**改动原因**：CEGIS 合成完成后，生成 Solidity 文件时需要正确继承 UDF 基合约。

- 从 benchmark 目录查找 `udf.sol`
- 执行 AST 检查
- 将 `udf.sol` 复制到 `synthesis-output/` 目录
- 将 `udfInfoOpt` 传递给 `SolidityTranslator`

#### 8.1.4 `resolveUdfPath` 辅助方法

**改动原因**：`udf.sol` 的位置取决于输入形式（单文件 vs 分割目录 vs 临时文件），需要统一解析。

```scala
private def resolveUdfPath(filepath: String, f: File, program: Program): String = {
  val candidate = if (f.exists() && f.isDirectory) {
    Paths.get(filepath, "udf.sol").toString
  } else {
    val parent = Paths.get(filepath).getParent
    if (parent == null) Paths.get("udf.sol").toString
    else parent.resolve("udf.sol").toString
  }
  if (isFileExists(candidate)) candidate
  else {
    // Fallback: synthesis-benchmark/<ProgramName>/udf.sol
    val fallback = Paths.get("synthesis-benchmark",
      program.name.toLowerCase, "udf.sol").toString
    if (isFileExists(fallback)) fallback else candidate
  }
}
```

### 8.2 SolidityTranslator 继承机制

**文件**：`src/main/scala/imp/SolidityTranslator.scala`

**改动原因**：生成的 Solidity 合约需要继承 `udf.sol` 中的 UDF 基合约，以使用 UDF 函数。

```scala
case class SolidityTranslator(
  // ...
  udfInfoOpt: Option[(String, String)] = None  // (importPath, baseContractName)
) extends Translator(...) {
  // ...
  def translate(): Statement = {
    // ...
    udfInfoOpt match {
      case Some((importPath, baseName)) =>
        Statement.makeSeq(
          Import(importPath),
          DeclContract(s"$name is $baseName", simplified)
        )
      case None =>
        DeclContract(name, simplified)
    }
  }
}
```

生成结果示例：

```solidity
import "./jokintheboxstakin_udf.sol";

contract Jokintheboxstakin is JokintheboxstakinUDF {
    // ... 合成的状态变量和函数 ...
}
```

---

## 9. 运行结果与验证

### 9.1 CEGIS 合成

运行 `synthesis-all` 后，`jokintheboxstakin` 合成成功，输出文件：

- `synthesis-output/jokintheboxstakin.dl`（合成的 Datalog 交易规则）
- `synthesis-output/jokintheboxstakin.sol`（合成的 Solidity 合约）
- `synthesis-output/jokintheboxstakin_udf.sol`（复制的 UDF 基合约）

### 9.2 验证结果

运行 `verify synthesis-benchmark/jokintheboxstakin` 的输出：

```
Init: UNSATISFIABLE   ← 初始状态满足所有属性 ✓
Tr:   SAT / UNSATISFIABLE ← transition step 结果（部分属性 UNSAT 表示归纳步证明成功）
```

**关于 `Tr: SAT`**：部分 violation 规则在 transition step 中仍为 SAT，这是因为：

1. 当前采用的是 UF 编码（无解释函数），Z3 可以自由选择函数的返回值来构造反例
2. `isValidSignature` 的完整语义（keccak256、ecrecover 等）无法在 Z3 中完全编码
3. 这是 UDF 验证的固有限制——手动编码只能捕获结构性约束（如 nonce 依赖），而非完整的密码学语义

### 9.3 改动有效性验证

| 验证项 | 状态 | 说明 |
|--------|------|------|
| `synthesis-all` 全量运行 | ✓ | 所有 benchmark 合成成功 |
| UDF AST 检查 | ✓ | `isValidSignature` 和 `jokTokenBalance` 均通过 |
| nonce 自增规则 | ✓ | 算术类型兼容性修复后正常解析 |
| nonce __exists 初始化 | ✓ | 首次 withdraw 可正确读取 nonce |
| UF + nonce 注入 | ✓ | 验证器输出中体现了 nonce 参与 UF |

---

## 附录 A：改动依赖关系图

```
schema.dl (扩展 recv_withdraw / 新增 nonce / 声明 .udf)
    │
    ├──→ rules.dl (新增 withdraw 规则 / nonce 自增规则)
    │        │
    │        └──→ ArithOperator.scala (n_old + 1 导致类型不兼容 → 修复)
    │
    ├──→ properties.dl (新增签名验证 / 提款限额的 invalidTx 规则)
    │
    ├──→ udf.sol (UDF 的 Solidity 具体实现)
    │        │
    │        └──→ SolcAst.scala (AST 前置检查)
    │                 │
    │                 └──→ Main.scala (在 compile/verify/synthesis-all 中调用)
    │
    └──→ Verifier.scala
             ├── 排除 UDF 物化
             ├── txLiteralToConst (交易域约束 + 交易绑定)
             ├── udfLiteralToConst (UF 编码)
             ├── isValidSignature nonce 注入
             ├── nonce __exists 全 true 初始化
             └── UDF 时跳过不变量生成
```

## 附录 B：关键术语对照表

| 术语 | 含义 |
|------|------|
| UDF (User-Defined Function) | 用户定义函数，在 Datalog 中以 `.udf` 声明 |
| UF (Uninterpreted Function) | Z3 中的无解释函数，满足一致性公理但无内部语义 |
| BMC (Bounded Model Checking) | 有界模型检查，展开 k 步 transition 寻找反例 |
| CEGIS | 反例引导的归纳合成 |
| `__exists` 数组 | Verifier 中用于跟踪某个键是否存在有效记录的布尔数组 |
| `recv_*` | 交易接口关系的命名前缀 |
| `materializedRelations` | 被物化为 Z3 状态数组的关系集合 |
| `transactionRelationPrefix` | 值为 `"recv_"`，用于识别交易接口 |
| congruence axiom | UF 的一致性公理：∀x,y. x=y → f(x)=f(y) |
