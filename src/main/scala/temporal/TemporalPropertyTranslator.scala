package temporal

import com.microsoft.z3.{ArithSort, BoolExpr, Context, Expr, Sort}
import datalog.{Constant, Parameter, Program, Relation, ReservedRelation, SimpleRelation, SingletonRelation, Type, Variable}
import verification.{TransitionSystem, Z3Helper}

import scala.collection.mutable

/**
 * 将时态属性 AST 翻译为 Z3 约束表达式。
 * 
 * 支持的时态运算符：
 * - ONCE φ: φ 在过去某个时刻（包括现在）成立过 (∃j≤i. T[j]⊨φ)
 * - ALWAYS φ: φ 在所有过去时刻（包括现在）都成立 (∀j≤i. T[j]⊨φ)
 * 
 * 实现策略：
 * 1. ONCE: 使用辅助布尔状态变量 once_φ，初始为 false，当 φ 成立时设为 true 并保持
 * 2. ALWAYS: 使用归纳证明，只需在当前状态验证 φ 成立
 */
class TemporalPropertyTranslator(
  val ctx: Context,
  val program: Program,
  val transitionSystem: TransitionSystem,
  val stateVarMap: Map[Relation, (Expr[_], Expr[_])], // 关系 -> (当前状态变量, 下一状态变量)
  val indices: Map[SimpleRelation, List[Int]], // 关系的索引
  val transactionVar: Option[Expr[_]] = None // 可选：transaction 变量（用于检查事务）
) {
  
  /**
   * 存储 ONCE 运算符对应的辅助状态变量
   * Key: ONCE 表达式的字符串表示（用于去重）
   * Value: (当前状态变量, 下一状态变量, 原始表达式)
   */
  val onceVars: mutable.Map[String, (Expr[_], Expr[_], TemporalExpr)] = mutable.Map()
  
  /**
   * 翻译时态属性为 Z3 约束
   * @param property 时态属性
   * @return 翻译后的 Z3 布尔表达式，带有适当的量化
   */
  def translate(property: TemporalProperty): BoolExpr = {
    // 收集自由变量
    val freeVars = collectFreeVariables(property.expr)
    
    if (freeVars.isEmpty) {
      // 没有自由变量，直接翻译
      translateExpr(property.expr, Map())
    } else {
      // 有自由变量，需要量化
      // 创建变量上下文，从表达式中推断类型
      val varContext = freeVars.map { varName =>
        // 从表达式中推断变量类型
        val inferredType = inferVariableType(varName, property.expr)
        val sort = Z3Helper.typeToSort(ctx, inferredType)
        val z3Var = ctx.mkConst(varName, sort)
        varName -> z3Var
      }.toMap
      
      // 翻译表达式
      val constraint = translateExpr(property.expr, varContext)
      
      // 根据表达式的最外层结构决定量化方式
      // 对于 ALWAYS，使用全称量化
      // 对于其他情况，也默认使用全称量化（安全属性）
      property.expr match {
        case TemporalExpr.Always(_) =>
          // ALWAYS 已经在 translateExpr 中处理，这里只需要对内部的自由变量量化
          if (varContext.nonEmpty) {
            ctx.mkForall(
              varContext.values.toArray,
              constraint,
              1, null, null,
              ctx.mkSymbol("Q_prop"),
              ctx.mkSymbol("sk_prop")
            ).asInstanceOf[BoolExpr]
          } else {
            constraint
          }
        
        case _ =>
          // 其他情况：默认全称量化
          if (varContext.nonEmpty) {
            ctx.mkForall(
              varContext.values.toArray,
              constraint,
              1, null, null,
              ctx.mkSymbol("Q_prop"),
              ctx.mkSymbol("sk_prop")
            ).asInstanceOf[BoolExpr]
          } else {
            constraint
          }
      }
    }
  }
  
  /**
   * 翻译时态表达式
   * @param expr 时态表达式 AST
   * @param variableContext 当前变量上下文（变量名 -> Z3 Expr）
   * @return 翻译后的 Z3 布尔表达式
   */
  private def translateExpr(expr: TemporalExpr, variableContext: Map[String, Expr[_]]): BoolExpr = {
    expr match {
      // 布尔字面量
      case TemporalExpr.BoolLiteral(value) => 
        ctx.mkBool(value)
      
      // 标识符（变量）
      case TemporalExpr.Identifier(name) => 
        variableContext.get(name) match {
          case Some(v: Expr[_]) if v.getSort.toString == "Bool" => 
            v.asInstanceOf[BoolExpr]
          case Some(v) => 
            throw new IllegalArgumentException(s"Variable $name is not boolean: ${v.getSort}")
          case None => 
            throw new IllegalArgumentException(s"Unbound variable: $name")
        }
      
      // 数字字面量（不应直接作为布尔表达式）
      case TemporalExpr.NumericLiteral(_) =>
        throw new IllegalArgumentException("Numeric literal cannot be used as boolean expression")
      
      // 函数调用（关系调用）
      case fc @ TemporalExpr.FunctionCall(name, args) =>
        translateFunctionCall(name, args, variableContext)
      
      // 逻辑运算符
      case TemporalExpr.Not(inner) =>
        ctx.mkNot(translateExpr(inner, variableContext))
      
      case TemporalExpr.And(left, right) =>
        ctx.mkAnd(
          translateExpr(left, variableContext),
          translateExpr(right, variableContext)
        )
      
      case TemporalExpr.Or(left, right) =>
        ctx.mkOr(
          translateExpr(left, variableContext),
          translateExpr(right, variableContext)
        )
      
      case TemporalExpr.Imply(left, right) =>
        ctx.mkImplies(
          translateExpr(left, variableContext),
          translateExpr(right, variableContext)
        )
      
      // 比较运算符
      case TemporalExpr.Eq(left, right) =>
        // 尝试智能翻译：如果是布尔类型，使用布尔翻译
        val leftExpr = translateGeneralExpr(left, variableContext)
        val rightExpr = translateGeneralExpr(right, variableContext)
        ctx.mkEq(leftExpr, rightExpr)
      
      case TemporalExpr.Neq(left, right) =>
        val leftExpr = translateGeneralExpr(left, variableContext)
        val rightExpr = translateGeneralExpr(right, variableContext)
        ctx.mkNot(ctx.mkEq(leftExpr, rightExpr))
      
      case TemporalExpr.Lt(left, right) =>
        ctx.mkLt(
          translateArithExpr(left, variableContext),
          translateArithExpr(right, variableContext)
        )
      
      case TemporalExpr.Le(left, right) =>
        ctx.mkLe(
          translateArithExpr(left, variableContext),
          translateArithExpr(right, variableContext)
        )
      
      case TemporalExpr.Gt(left, right) =>
        ctx.mkGt(
          translateArithExpr(left, variableContext),
          translateArithExpr(right, variableContext)
        )
      
      case TemporalExpr.Ge(left, right) =>
        ctx.mkGe(
          translateArithExpr(left, variableContext),
          translateArithExpr(right, variableContext)
        )
      
      // 时态运算符
      case TemporalExpr.Once(inner) =>
        translateOnce(inner, variableContext)
      
      case TemporalExpr.Always(inner) =>
        // ALWAYS φ: 使用归纳证明，只需在当前状态验证 φ
        // 归纳基础：初始状态满足 φ
        // 归纳步骤：如果当前状态满足 φ，则下一状态也满足 φ
        // 因此这里直接翻译 φ
        translateExpr(inner, variableContext)
    }
  }
  
  /**
   * 翻译通用表达式（自动识别类型）
   * 用于比较运算符，可以处理布尔或算术表达式
   */
  private def translateGeneralExpr(expr: TemporalExpr, variableContext: Map[String, Expr[_]]): Expr[_] = {
    expr match {
      case TemporalExpr.BoolLiteral(value) =>
        if (value) ctx.mkTrue() else ctx.mkFalse()
      
      case TemporalExpr.NumericLiteral(value) =>
        ctx.mkInt(value.toLong)
      
      case TemporalExpr.Identifier(name) =>
        variableContext.get(name) match {
          case Some(v) => v
          case None => throw new IllegalArgumentException(s"Unbound variable: $name")
        }
      
      case _ =>
        // 默认尝试作为算术表达式
        translateArithExpr(expr, variableContext)
    }
  }
  
  /**
   * 翻译算术表达式
   * 注意：布尔字面量会被转换为整数（true -> 1, false -> 0）
   */
  private def translateArithExpr(expr: TemporalExpr, variableContext: Map[String, Expr[_]]): Expr[ArithSort] = {
    expr match {
      case TemporalExpr.NumericLiteral(value) =>
        ctx.mkInt(value.toLong).asInstanceOf[Expr[ArithSort]]
      
      case TemporalExpr.BoolLiteral(value) =>
        // 将布尔字面量转换为整数：true -> 1, false -> 0
        ctx.mkInt(if (value) 1 else 0).asInstanceOf[Expr[ArithSort]]
      
      case TemporalExpr.Identifier(name) =>
        variableContext.get(name) match {
          case Some(v: Expr[_]) if v.getSort.toString == "Int" =>
            v.asInstanceOf[Expr[ArithSort]]
          case Some(v) =>
            throw new IllegalArgumentException(s"Variable $name is not numeric: ${v.getSort}")
          case None =>
            throw new IllegalArgumentException(s"Unbound variable: $name")
        }
      
      case TemporalExpr.FunctionCall(name, args) =>
        // 尝试作为返回数值的关系调用
        // 这需要更复杂的类型推断，暂时抛出异常
        throw new IllegalArgumentException(s"Function call in arithmetic context not yet supported: $name")
      
      case _ =>
        throw new IllegalArgumentException(s"Cannot translate as arithmetic expression: $expr")
    }
  }
  
  /**
   * 翻译函数调用（关系调用）
   * 
   * 两种情况：
   * 1. 有参数：例如 raised(r) 表示 "关系 raised 的当前值为 r"
   * 2. 无参数：例如 withdraw() 表示 "关系 withdraw 非空"（∃ vars. withdraw(vars)）
   */
  private def translateFunctionCall(
    name: String, 
    args: List[TemporalExpr], 
    variableContext: Map[String, Expr[_]]
  ): BoolExpr = {
    // 查找对应的关系
    val relation = findRelation(name)
    
    if (args.isEmpty) {
      // 无参数调用：表示关系非空，即 ∃ vars. relation(vars)
      translateRelationNonEmpty(relation, variableContext)
    } else {
      // 有参数调用：表示关系在当前状态的值
      translateRelationAccess(relation, args, variableContext)
    }
  }
  
  /**
   * 翻译关系非空检查
   * 例如：withdraw() 翻译为 ∃p,n. withdraw(p,n)
   * 
   * 特殊处理：
   * - 如果是事务/接口关系且提供了 transactionVar，则检查 transaction == "recv_XXX"
   * - 否则尝试标准的状态变量检查
   */
  private def translateRelationNonEmpty(
    relation: Relation,
    variableContext: Map[String, Expr[_]]
  ): BoolExpr = {
    
    // ⭐ 检查是否是事务/接口关系
    val isInterface = program.interfaces.exists(_.relation.name == relation.name)
    val hasRecvInterface = program.interfaces.exists(_.relation.name == s"recv_${relation.name}")
    
    if ((isInterface || hasRecvInterface) && transactionVar.isDefined) {
      // 使用事务变量检查
      val txName = if (isInterface) relation.name else s"recv_${relation.name}"
      val txNameExpr = ctx.mkString(txName)
      ctx.mkEq(transactionVar.get, txNameExpr)
    } else if (!stateVarMap.contains(relation)) {
      // 没有状态变量且不是事务关系
      throw new IllegalArgumentException(
        s"No state variable found for relation ${relation.name}. " +
        s"To use this relation in temporal properties, it must be either:\n" +
        s"  1. A materialized state relation (with indices defined), or\n" +
        s"  2. An interface/transaction (provide transactionVar to translator)"
      )
    } else {
      // 标准处理：对于状态关系，创建存在量化
      val quantVars = relation.sig.zipWithIndex.map { case (typ, idx) =>
        val sort = Z3Helper.typeToSort(ctx, typ)
        ctx.mkConst(s"${relation.name}_q${idx}", sort)
      }
      
      // 创建临时变量上下文
      val tempContext = variableContext ++ 
        relation.memberNames.zip(quantVars).toMap
      
      // 构造关系访问约束
      val tempArgs = quantVars.map { v =>
        // 为每个量化变量创建对应的 Identifier 表达式
        relation.memberNames.find(name => tempContext.get(name).contains(v)) match {
          case Some(name) => TemporalExpr.Identifier(name)
          case None => TemporalExpr.Identifier(v.toString)
        }
      }.toList
      
      val constraint = translateRelationAccess(relation, tempArgs, tempContext)
      
      // 存在量化
      if (quantVars.nonEmpty) {
        ctx.mkExists(
          quantVars.toArray,
          constraint,
          1, null, null, 
          ctx.mkSymbol(s"Q_${relation.name}"), 
          ctx.mkSymbol(s"sk_${relation.name}")
        ).asInstanceOf[BoolExpr]
      } else {
        constraint
      }
    }
  }
  
  /**
   * 翻译关系访问
   * 例如：raised(r) 翻译为 raised_state == r
   */
  private def translateRelationAccess(
    relation: Relation,
    args: List[TemporalExpr],
    variableContext: Map[String, Expr[_]]
  ): BoolExpr = {
    // 检查参数数量
    if (args.length != relation.arity) {
      throw new IllegalArgumentException(
        s"Relation ${relation.name} expects ${relation.arity} arguments, got ${args.length}"
      )
    }
    
    // 获取状态变量
    val (stateVar, _) = stateVarMap.getOrElse(
      relation,
      throw new IllegalArgumentException(s"No state variable found for relation ${relation.name}")
    )
    
    relation match {
      case _: SingletonRelation =>
        // 单例关系：直接比较值
        if (args.length == 1) {
          val argExpr = translateValueExpr(args.head, variableContext, stateVar.getSort)
          ctx.mkEq(stateVar, argExpr)
        } else {
          // 多字段单例关系：需要访问 tuple 的各个字段
          val constraints = args.zipWithIndex.map { case (arg, idx) =>
            // TODO: 实现 tuple 字段访问
            throw new IllegalArgumentException("Multi-field singleton relations not yet supported")
          }
          ctx.mkAnd(constraints: _*)
        }
      
      case sr: SimpleRelation =>
        // 简单关系（数组表示）
        val relIndices = indices.getOrElse(sr, List())
        
        if (relIndices.isEmpty) {
          throw new IllegalArgumentException(s"No indices defined for relation ${sr.name}")
        }
        
        // 分离键和值
        val keyArgs = relIndices.map(i => args(i))
        val valueIndices = args.indices.filterNot(relIndices.contains).toList
        val valueArgs = valueIndices.map(i => args(i))
        
        // 翻译键
        val keyExprs = keyArgs.map(arg => translateArithExpr(arg, variableContext)).toArray
        
        // 数组选择：array[key1, key2, ...]
        val selectedValue = if (keyExprs.length == 1) {
          ctx.mkSelect(
            stateVar.asInstanceOf[Expr[com.microsoft.z3.ArraySort[com.microsoft.z3.Sort, com.microsoft.z3.Sort]]],
            keyExprs(0).asInstanceOf[Expr[com.microsoft.z3.Sort]]
          )
        } else {
          ctx.mkSelect(
            stateVar.asInstanceOf[Expr[com.microsoft.z3.ArraySort[com.microsoft.z3.Sort, com.microsoft.z3.Sort]]],
            keyExprs.asInstanceOf[Array[Expr[_]]]
          )
        }
        
        // 翻译值（需要根据数组元素类型）
        if (valueArgs.length == 1) {
          val valueExpr = translateValueExpr(valueArgs.head, variableContext, selectedValue.getSort)
          ctx.mkEq(selectedValue, valueExpr)
        } else {
          // 多值：需要访问 tuple
          throw new IllegalArgumentException("Multi-value relations not yet supported")
        }
      
      case _: ReservedRelation =>
        // 保留关系（如 msgSender, msgValue）
        if (args.length == 1) {
          val argExpr = translateValueExpr(args.head, variableContext, stateVar.getSort)
          ctx.mkEq(stateVar, argExpr)
        } else {
          throw new IllegalArgumentException(s"Reserved relation ${relation.name} with multiple args not supported")
        }
    }
  }
  
  /**
   * 翻译值表达式（可以是算术或布尔）
   * 根据目标类型自动选择正确的翻译方式
   */
  private def translateValueExpr(expr: TemporalExpr, variableContext: Map[String, Expr[_]], targetSort: com.microsoft.z3.Sort): Expr[_] = {
    val sortStr = targetSort.toString
    
    if (sortStr == "Bool") {
      // 布尔类型
      expr match {
        case TemporalExpr.BoolLiteral(value) =>
          if (value) ctx.mkTrue() else ctx.mkFalse()
        case TemporalExpr.NumericLiteral(value) =>
          // 数字转布尔：0 -> false, 非0 -> true
          if (value == 0) ctx.mkFalse() else ctx.mkTrue()
        case TemporalExpr.Identifier(name) =>
          // 从变量上下文获取布尔变量
          variableContext.get(name) match {
            case Some(v: Expr[_]) if v.getSort.toString == "Bool" =>
              v.asInstanceOf[com.microsoft.z3.BoolExpr]
            case Some(v) =>
              throw new IllegalArgumentException(s"Variable $name is not boolean: ${v.getSort}")
            case None =>
              throw new IllegalArgumentException(s"Unbound variable: $name")
          }
        case _ =>
          throw new IllegalArgumentException(s"Cannot translate $expr as boolean value")
      }
    } else {
      // 算术类型（Int）
      translateArithExpr(expr, variableContext)
    }
  }
  
  /**
   * 翻译 ONCE 运算符
   * 
   * ONCE φ 的语义：∃j≤i. T[j]⊨φ
   * 
   * 实现：使用辅助布尔状态变量 once_φ
   * - 初始状态：once_φ = false
   * - 转换关系：once_φ' = once_φ ∨ φ
   * - 属性：once_φ
   */
  private def translateOnce(expr: TemporalExpr, variableContext: Map[String, Expr[_]]): BoolExpr = {
    val exprKey = expr.toString // 使用表达式的字符串表示作为唯一键
    
    // 获取或创建 ONCE 辅助变量
    val (onceVar, onceVarNext, _) = onceVars.getOrElseUpdate(exprKey, {
      val varName = s"once_${onceVars.size}" // 生成唯一的变量名
      val (v_in, v_out) = transitionSystem.newVar(varName, ctx.mkBoolSort())
      (v_in, v_out, expr) // 存储原始表达式
    })
    
    // 返回辅助变量（表示"曾经成立过"）
    onceVar.asInstanceOf[BoolExpr]
  }
  
  /**
   * 获取 ONCE 辅助变量的初始化约束
   * 所有 ONCE 变量初始为 false
   */
  def getOnceInitConstraints(): BoolExpr = {
    if (onceVars.isEmpty) {
      ctx.mkTrue()
    } else {
      val constraints = onceVars.values.map { case (onceVar, _, _) =>
        ctx.mkEq(onceVar, ctx.mkFalse())
      }
      ctx.mkAnd(constraints.toSeq: _*)
    }
  }
  
  /**
   * 获取 ONCE 辅助变量的更新约束
   * once_φ' = once_φ ∨ φ
   * 
   * 这个方法需要在转换关系中调用，以确保 ONCE 变量正确更新。
   */
  def getOnceUpdateConstraints(): BoolExpr = {
    if (onceVars.isEmpty) {
      ctx.mkTrue()
    } else {
      val constraints = onceVars.map { case (exprKey, (onceVar, onceVarNext, expr)) =>
        // once_φ' = once_φ ∨ φ
        // 翻译 φ（在当前状态）
        val phiConstraint = translateExpr(expr, Map())
        ctx.mkEq(
          onceVarNext,
          ctx.mkOr(onceVar.asInstanceOf[BoolExpr], phiConstraint)
        )
      }
      ctx.mkAnd(constraints.toSeq: _*)
    }
  }
  
  /**
   * 在 Program 中查找关系
   */
  private def findRelation(name: String): Relation = {
    program.relations.find(_.name == name).getOrElse(
      throw new IllegalArgumentException(s"Relation not found: $name")
    )
  }
  
  /**
   * 收集表达式中的自由变量（用于量化）
   */
  def collectFreeVariables(expr: TemporalExpr): Set[String] = {
    expr match {
      case TemporalExpr.Identifier(name) => Set(name)
      case TemporalExpr.NumericLiteral(_) => Set()
      case TemporalExpr.BoolLiteral(_) => Set()
      case TemporalExpr.FunctionCall(_, args) =>
        args.flatMap(collectFreeVariables).toSet
      case TemporalExpr.Not(inner) =>
        collectFreeVariables(inner)
      case TemporalExpr.And(left, right) =>
        collectFreeVariables(left) ++ collectFreeVariables(right)
      case TemporalExpr.Or(left, right) =>
        collectFreeVariables(left) ++ collectFreeVariables(right)
      case TemporalExpr.Imply(left, right) =>
        collectFreeVariables(left) ++ collectFreeVariables(right)
      case TemporalExpr.Eq(left, right) =>
        collectFreeVariables(left) ++ collectFreeVariables(right)
      case TemporalExpr.Neq(left, right) =>
        collectFreeVariables(left) ++ collectFreeVariables(right)
      case TemporalExpr.Lt(left, right) =>
        collectFreeVariables(left) ++ collectFreeVariables(right)
      case TemporalExpr.Le(left, right) =>
        collectFreeVariables(left) ++ collectFreeVariables(right)
      case TemporalExpr.Gt(left, right) =>
        collectFreeVariables(left) ++ collectFreeVariables(right)
      case TemporalExpr.Ge(left, right) =>
        collectFreeVariables(left) ++ collectFreeVariables(right)
      case TemporalExpr.Once(inner) =>
        collectFreeVariables(inner)
      case TemporalExpr.Always(inner) =>
        collectFreeVariables(inner)
    }
  }
  
  /**
   * 创建变量上下文（将变量名映射到 Z3 常量）
   * @param varNames 变量名集合
   * @param expr 用于推断类型的表达式
   */
  def createVariableContext(
    varNames: Set[String],
    expr: TemporalExpr
  ): Map[String, Expr[_]] = {
    varNames.map { name =>
      // 从表达式中推断变量类型
      val varType = inferVariableType(name, expr)
      val sort = Z3Helper.typeToSort(ctx, varType)
      val z3Var = ctx.mkConst(name, sort)
      name -> z3Var
    }.toMap
  }
  
  /**
   * 推断变量类型
   * 从表达式中的函数调用推断变量类型
   */
  private def inferVariableType(varName: String, expr: TemporalExpr): Type = {
    // 在表达式中查找使用该变量的函数调用
    findVariableTypeInExpr(varName, expr).getOrElse(Type.uintType) // 默认为 uint
  }
  
  /**
   * 在表达式中递归查找变量的类型
   * 通过查找 FunctionCall(name, args) 中 Identifier(varName) 的位置来推断类型
   */
  private def findVariableTypeInExpr(varName: String, expr: TemporalExpr): Option[Type] = {
    expr match {
      case TemporalExpr.FunctionCall(name, args) =>
        // 找到这个函数对应的关系
        program.relations.find(_.name == name) match {
          case Some(relation) =>
            // 在参数列表中查找变量的位置
            args.zipWithIndex.collectFirst {
              case (TemporalExpr.Identifier(vName), idx) if vName == varName =>
                // 返回对应位置的类型
                if (idx < relation.sig.length) relation.sig(idx) else Type.uintType
            }.orElse {
              // 递归查找嵌套表达式
              args.flatMap(arg => findVariableTypeInExpr(varName, arg)).headOption
            }
          case None =>
            // 关系未找到，递归查找参数
            args.flatMap(arg => findVariableTypeInExpr(varName, arg)).headOption
        }
      
      case TemporalExpr.Not(inner) =>
        findVariableTypeInExpr(varName, inner)
      
      case TemporalExpr.And(left, right) =>
        findVariableTypeInExpr(varName, left).orElse(findVariableTypeInExpr(varName, right))
      
      case TemporalExpr.Or(left, right) =>
        findVariableTypeInExpr(varName, left).orElse(findVariableTypeInExpr(varName, right))
      
      case TemporalExpr.Imply(left, right) =>
        findVariableTypeInExpr(varName, left).orElse(findVariableTypeInExpr(varName, right))
      
      case TemporalExpr.Eq(left, right) =>
        findVariableTypeInExpr(varName, left).orElse(findVariableTypeInExpr(varName, right))
      
      case TemporalExpr.Neq(left, right) =>
        findVariableTypeInExpr(varName, left).orElse(findVariableTypeInExpr(varName, right))
      
      case TemporalExpr.Lt(left, right) =>
        findVariableTypeInExpr(varName, left).orElse(findVariableTypeInExpr(varName, right))
      
      case TemporalExpr.Le(left, right) =>
        findVariableTypeInExpr(varName, left).orElse(findVariableTypeInExpr(varName, right))
      
      case TemporalExpr.Gt(left, right) =>
        findVariableTypeInExpr(varName, left).orElse(findVariableTypeInExpr(varName, right))
      
      case TemporalExpr.Ge(left, right) =>
        findVariableTypeInExpr(varName, left).orElse(findVariableTypeInExpr(varName, right))
      
      case TemporalExpr.Once(inner) =>
        findVariableTypeInExpr(varName, inner)
      
      case TemporalExpr.Always(inner) =>
        findVariableTypeInExpr(varName, inner)
      
      case _ =>
        None
    }
  }
}

object TemporalPropertyTranslator {
  /**
   * 工厂方法：创建翻译器实例
   */
  def apply(
    ctx: Context,
    program: Program,
    transitionSystem: TransitionSystem,
    stateVarMap: Map[Relation, (Expr[_], Expr[_])],
    indices: Map[SimpleRelation, List[Int]]
  ): TemporalPropertyTranslator = {
    new TemporalPropertyTranslator(ctx, program, transitionSystem, stateVarMap, indices)
  }
}

