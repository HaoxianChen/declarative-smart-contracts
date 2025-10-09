package temporal

import com.microsoft.z3.{ArithSort, BoolExpr, Context, Expr, Sort}
import datalog.{Constant, Parameter, Program, Relation, ReservedRelation, SimpleRelation, SingletonRelation, Type, Variable}
import verification.{TransitionSystem, Z3Helper}

import scala.collection.mutable

/**
 * Translates temporal property AST to Z3 constraint expressions.
 * 
 * Supported temporal operators:
 * - ONCE φ: φ held at some point in the past (including now) (∃j≤i. T[j]⊨φ)
 * - ALWAYS φ: φ has always held in the past (including now) (∀j≤i. T[j]⊨φ)
 * 
 * Implementation strategy:
 * 1. ONCE: Use auxiliary boolean state variable once_φ, initially false, set to true when φ holds and keep it true
 * 2. ALWAYS: Use inductive proof, only need to verify φ holds in current state
 */
class TemporalPropertyTranslator(
  val ctx: Context,
  val program: Program,
  val transitionSystem: TransitionSystem,
  val stateVarMap: Map[Relation, (Expr[_], Expr[_])], // Relation -> (current state variable, next state variable)
  val indices: Map[SimpleRelation, List[Int]], // Indices of relations
  val transactionVar: Option[Expr[_]] = None // Optional: transaction variable (for checking transactions)
) {
  
  /**
   * Store auxiliary state variables for ONCE operators
   * Key: String representation of ONCE expression (for deduplication)
   * Value: (current state variable, next state variable, original expression)
   */
  val onceVars: mutable.Map[String, (Expr[_], Expr[_], TemporalExpr)] = mutable.Map()
  
  /**
   * Quantifier ID counter for consistent naming with TransitionSystem
   */
  private var quantifierIdCounter = 0
  
  /**
   * Get next quantifier ID (consistent with TransitionSystem naming: Q1, Q2, Q3, ...)
   */
  private def nextQuantId(): Int = {
    quantifierIdCounter += 1
    quantifierIdCounter
  }
  
  /**
   * Get quantifier variable name (consistent with TransitionSystem naming: p, q, r, s, ...)
   * @param idx Index of the quantified variable
   * @return Variable name: p for 0, q for 1, r for 2, etc.
   */
  private def getQuantVarName(idx: Int): String = {
    if (idx < 26) {
      ('p' + idx).toChar.toString
    } else {
      // After 26 letters, use q26, q27, ...
      s"q${idx - 26}"
    }
  }
  
  /**
   * Translate temporal property to Z3 constraint
   * @param property Temporal property
   * @return Translated Z3 boolean expression with appropriate quantification
   *         Universal quantifiers are converted to existential quantifiers via formula transformation: ∀x.φ ≡ ¬∃x.¬φ
   */
  def translate(property: TemporalProperty): BoolExpr = {
    // Collect free variables
    val freeVars = collectFreeVariables(property.expr)
    
    if (freeVars.isEmpty) {
      // No free variables, translate directly
      translateExpr(property.expr, Map())
    } else {
      // Has free variables, need quantification
      // Create variable context, infer types from expression
      val varContext = freeVars.map { varName =>
        // Infer variable type from expression
        val inferredType = inferVariableType(varName, property.expr)
        val sort = Z3Helper.typeToSort(ctx, inferredType)
        val z3Var = ctx.mkConst(varName, sort)
        varName -> z3Var
      }.toMap
      
      // Translate expression
      val constraint = translateExpr(property.expr, varContext)
      
      // Convert universal quantification to existential quantification
      // Formula transformation: ∀x.φ ≡ ¬∃x.¬φ
      // This applies to all temporal properties for algorithm optimization
      if (varContext.nonEmpty) {
        val qid = nextQuantId()
        ctx.mkNot(
          ctx.mkExists(
            varContext.values.toArray,
            ctx.mkNot(constraint),
            1, null, null,
            ctx.mkSymbol(s"Q$qid"),
            ctx.mkSymbol(s"skid$qid")
          )
        ).asInstanceOf[BoolExpr]
      } else {
        constraint
      }
    }
  }
  
  /**
   * Translate temporal expression
   * @param expr Temporal expression AST
   * @param variableContext Current variable context (variable name -> Z3 Expr)
   * @return Translated Z3 boolean expression
   */
  private def translateExpr(expr: TemporalExpr, variableContext: Map[String, Expr[_]]): BoolExpr = {
    expr match {
      // Boolean literal
      case TemporalExpr.BoolLiteral(value) => 
        ctx.mkBool(value)
      
      // Identifier (variable)
      case TemporalExpr.Identifier(name) => 
        variableContext.get(name) match {
          case Some(v: Expr[_]) if v.getSort.toString == "Bool" => 
            v.asInstanceOf[BoolExpr]
          case Some(v) => 
            throw new IllegalArgumentException(s"Variable $name is not boolean: ${v.getSort}")
          case None => 
            throw new IllegalArgumentException(s"Unbound variable: $name")
        }
      
      // Numeric literal (should not be used directly as boolean expression)
      case TemporalExpr.NumericLiteral(_) =>
        throw new IllegalArgumentException("Numeric literal cannot be used as boolean expression")
      
      // Function call (relation call)
      case fc @ TemporalExpr.FunctionCall(name, args) =>
        translateFunctionCall(name, args, variableContext)
      
      // Logical operators
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
      
      // Comparison operators
      case TemporalExpr.Eq(left, right) =>
        // Try smart translation: if boolean type, use boolean translation
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
      
      // Temporal operators
      case TemporalExpr.Once(inner) =>
        translateOnce(inner, variableContext)
      
      case TemporalExpr.Always(inner) =>
        // ALWAYS φ: Use inductive proof, only need to verify φ holds in current state
        // Induction base: φ holds in initial state
        // Induction step: if φ holds in current state, it also holds in next state
        // Therefore, we directly translate φ here
        translateExpr(inner, variableContext)
    }
  }
  
  /**
   * Translate general expression (auto-detect type)
   * Used for comparison operators, can handle boolean or arithmetic expressions
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
        // Default: try as arithmetic expression
        translateArithExpr(expr, variableContext)
    }
  }
  
  /**
   * Translate arithmetic expression
   * Note: Boolean literals are converted to integers (true -> 1, false -> 0)
   */
  private def translateArithExpr(expr: TemporalExpr, variableContext: Map[String, Expr[_]]): Expr[ArithSort] = {
    expr match {
      case TemporalExpr.NumericLiteral(value) =>
        ctx.mkInt(value.toLong).asInstanceOf[Expr[ArithSort]]
      
      case TemporalExpr.BoolLiteral(value) =>
        // Convert boolean literal to integer: true -> 1, false -> 0
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
        // Try as relation call that returns numeric value
        // This requires more complex type inference, throw exception for now
        throw new IllegalArgumentException(s"Function call in arithmetic context not yet supported: $name")
      
      case _ =>
        throw new IllegalArgumentException(s"Cannot translate as arithmetic expression: $expr")
    }
  }
  
  /**
   * Translate function call (relation call)
   * 
   * Two cases:
   * 1. With arguments: e.g., raised(r) means "the current value of relation raised is r"
   * 2. Without arguments: e.g., withdraw() means "relation withdraw is non-empty" (∃ vars. withdraw(vars))
   */
  private def translateFunctionCall(
    name: String, 
    args: List[TemporalExpr], 
    variableContext: Map[String, Expr[_]]
  ): BoolExpr = {
    // Find corresponding relation
    val relation = findRelation(name)
    
    if (args.isEmpty) {
      // No arguments: relation is non-empty, i.e., ∃ vars. relation(vars)
      translateRelationNonEmpty(relation, variableContext)
    } else {
      // With arguments: value of relation in current state
      translateRelationAccess(relation, args, variableContext)
    }
  }
  
  /**
   * Translate relation non-empty check
   * Example: withdraw() translates to ∃p,n. withdraw(p,n)
   * 
   * Special handling:
   * - If it's a transaction/interface relation and transactionVar is provided, check transaction == "recv_XXX"
   * - Otherwise try standard state variable check
   */
  private def translateRelationNonEmpty(
    relation: Relation,
    variableContext: Map[String, Expr[_]]
  ): BoolExpr = {
    
    // Check if it's a transaction/interface relation
    val isInterface = program.interfaces.exists(_.relation.name == relation.name)
    val hasRecvInterface = program.interfaces.exists(_.relation.name == s"recv_${relation.name}")
    
    if ((isInterface || hasRecvInterface) && transactionVar.isDefined) {
      // Use transaction variable check
      val txName = if (isInterface) relation.name else s"recv_${relation.name}"
      val txNameExpr = ctx.mkString(txName)
      ctx.mkEq(transactionVar.get, txNameExpr)
    } else if (!stateVarMap.contains(relation)) {
      // No state variable and not a transaction relation
      throw new IllegalArgumentException(
        s"No state variable found for relation ${relation.name}. " +
        s"To use this relation in temporal properties, it must be either:\n" +
        s"  1. A materialized state relation (with indices defined), or\n" +
        s"  2. An interface/transaction (provide transactionVar to translator)"
      )
    } else {
      // Standard handling: create existential quantification for state relations
      // Use consistent naming with TransitionSystem: p, q, r, ...
      val quantVars = relation.sig.zipWithIndex.map { case (typ, idx) =>
        val sort = Z3Helper.typeToSort(ctx, typ)
        ctx.mkConst(getQuantVarName(idx), sort)
      }
      
      // Create temporary variable context
      val tempContext = variableContext ++ 
        relation.memberNames.zip(quantVars).toMap
      
      // Construct relation access constraint
      val tempArgs = quantVars.map { v =>
        // Create corresponding Identifier expression for each quantified variable
        relation.memberNames.find(name => tempContext.get(name).contains(v)) match {
          case Some(name) => TemporalExpr.Identifier(name)
          case None => TemporalExpr.Identifier(v.toString)
        }
      }.toList
      
      val constraint = translateRelationAccess(relation, tempArgs, tempContext)
      
      // Existential quantification
      // Use consistent quantifier ID naming with TransitionSystem: Q1, Q2, ...
      if (quantVars.nonEmpty) {
        val qid = nextQuantId()
        ctx.mkExists(
          quantVars.toArray,
          constraint,
          1, null, null, 
          ctx.mkSymbol(s"Q$qid"), 
          ctx.mkSymbol(s"skid$qid")
        ).asInstanceOf[BoolExpr]
      } else {
        constraint
      }
    }
  }
  
  /**
   * Translate relation access
   * Example: raised(r) translates to raised_state == r
   */
  private def translateRelationAccess(
    relation: Relation,
    args: List[TemporalExpr],
    variableContext: Map[String, Expr[_]]
  ): BoolExpr = {
    // Check argument count
    if (args.length != relation.arity) {
      throw new IllegalArgumentException(
        s"Relation ${relation.name} expects ${relation.arity} arguments, got ${args.length}"
      )
    }
    
    // Get state variable
    val (stateVar, _) = stateVarMap.getOrElse(
      relation,
      throw new IllegalArgumentException(s"No state variable found for relation ${relation.name}")
    )
    
    relation match {
      case _: SingletonRelation =>
        // Singleton relation: directly compare values
        if (args.length == 1) {
          val argExpr = translateValueExpr(args.head, variableContext, stateVar.getSort)
          ctx.mkEq(stateVar, argExpr)
        } else {
          // Multi-field singleton relation: need to access tuple fields
          val constraints = args.zipWithIndex.map { case (arg, idx) =>
            // TODO: implement tuple field access
            throw new IllegalArgumentException("Multi-field singleton relations not yet supported")
          }
          ctx.mkAnd(constraints: _*)
        }
      
      case sr: SimpleRelation =>
        // Simple relation (array representation)
        val relIndices = indices.getOrElse(sr, List())
        
        if (relIndices.isEmpty) {
          throw new IllegalArgumentException(s"No indices defined for relation ${sr.name}")
        }
        
        // Separate keys and values
        val keyArgs = relIndices.map(i => args(i))
        val valueIndices = args.indices.filterNot(relIndices.contains).toList
        val valueArgs = valueIndices.map(i => args(i))
        
        // Translate keys
        val keyExprs = keyArgs.map(arg => translateArithExpr(arg, variableContext)).toArray
        
        // Array select: array[key1, key2, ...]
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
        
        // Translate values (based on array element type)
        if (valueArgs.length == 1) {
          val valueExpr = translateValueExpr(valueArgs.head, variableContext, selectedValue.getSort)
          ctx.mkEq(selectedValue, valueExpr)
        } else {
          // Multiple values: need to access tuple
          throw new IllegalArgumentException("Multi-value relations not yet supported")
        }
      
      case _: ReservedRelation =>
        // Reserved relation (e.g., msgSender, msgValue)
        if (args.length == 1) {
          val argExpr = translateValueExpr(args.head, variableContext, stateVar.getSort)
          ctx.mkEq(stateVar, argExpr)
        } else {
          throw new IllegalArgumentException(s"Reserved relation ${relation.name} with multiple args not supported")
        }
    }
  }
  
  /**
   * Translate value expression (can be arithmetic or boolean)
   * Automatically select correct translation based on target type
   */
  private def translateValueExpr(expr: TemporalExpr, variableContext: Map[String, Expr[_]], targetSort: com.microsoft.z3.Sort): Expr[_] = {
    val sortStr = targetSort.toString
    
    if (sortStr == "Bool") {
      // Boolean type
      expr match {
        case TemporalExpr.BoolLiteral(value) =>
          if (value) ctx.mkTrue() else ctx.mkFalse()
        case TemporalExpr.NumericLiteral(value) =>
          // Convert number to boolean: 0 -> false, non-zero -> true
          if (value == 0) ctx.mkFalse() else ctx.mkTrue()
        case TemporalExpr.Identifier(name) =>
          // Get boolean variable from variable context
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
      // Arithmetic type (Int)
      translateArithExpr(expr, variableContext)
    }
  }
  
  /**
   * Translate ONCE operator
   * 
   * Semantics of ONCE φ: ∃j≤i. T[j]⊨φ
   * 
   * Implementation: Use auxiliary boolean state variable once_φ
   * - Initial state: once_φ = false
   * - Transition relation: once_φ' = once_φ ∨ φ
   * - Property: once_φ
   */
  private def translateOnce(expr: TemporalExpr, variableContext: Map[String, Expr[_]]): BoolExpr = {
    val exprKey = expr.toString // Use string representation of expression as unique key
    
    // Get or create ONCE auxiliary variable
    val (onceVar, onceVarNext, _) = onceVars.getOrElseUpdate(exprKey, {
      // Generate a descriptive variable name based on the expression
      val descriptiveName = generateOnceName(expr)
      val varName = s"once_${descriptiveName}"
      val (v_in, v_out) = transitionSystem.newVar(varName, ctx.mkBoolSort())
      (v_in, v_out, expr) // Store original expression
    })
    
    // Return auxiliary variable (represents "has held at some point")
    onceVar.asInstanceOf[BoolExpr]
  }
  
  /**
   * Generate a descriptive name for an ONCE variable based on the expression.
   * Examples:
   *   withdraw() -> "withdraw"
   *   transfer(from, to, n) -> "transfer"
   *   x > 0 -> "x_gt_0"
   *   complex expression -> "expr_N" (fallback to number)
   */
  private def generateOnceName(expr: TemporalExpr): String = {
    expr match {
      // Function call: use the function name
      case TemporalExpr.FunctionCall(name, _) => name
      
      // Simple identifier
      case TemporalExpr.Identifier(name) => name
      
      // Comparison with identifier on left
      case TemporalExpr.Eq(TemporalExpr.Identifier(name), _) => s"${name}_eq"
      case TemporalExpr.Neq(TemporalExpr.Identifier(name), _) => s"${name}_neq"
      case TemporalExpr.Lt(TemporalExpr.Identifier(name), _) => s"${name}_lt"
      case TemporalExpr.Le(TemporalExpr.Identifier(name), _) => s"${name}_le"
      case TemporalExpr.Gt(TemporalExpr.Identifier(name), _) => s"${name}_gt"
      case TemporalExpr.Ge(TemporalExpr.Identifier(name), _) => s"${name}_ge"
      
      // Boolean literal
      case TemporalExpr.BoolLiteral(value) => if (value) "true" else "false"
      
      // Complex expressions: fallback to numbered naming
      case _ => s"expr_${onceVars.size}"
    }
  }
  
  /**
   * Get initialization constraints for ONCE auxiliary variables
   * All ONCE variables are initially false
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
   * Get update constraints for ONCE auxiliary variables
   * once_φ' = once_φ ∨ φ
   * 
   * This method needs to be called in the transition relation to ensure ONCE variables are correctly updated.
   */
  def getOnceUpdateConstraints(): BoolExpr = {
    if (onceVars.isEmpty) {
      ctx.mkTrue()
    } else {
      val constraints = onceVars.map { case (exprKey, (onceVar, onceVarNext, expr)) =>
        // once_φ' = once_φ ∨ φ
        // Translate φ (in current state)
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
   * Find relation in Program
   */
  private def findRelation(name: String): Relation = {
    program.relations.find(_.name == name).getOrElse(
      throw new IllegalArgumentException(s"Relation not found: $name")
    )
  }
  
  /**
   * Collect free variables in expression (for quantification)
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
   * Create variable context (map variable names to Z3 constants)
   * @param varNames Set of variable names
   * @param expr Expression used for type inference
   */
  def createVariableContext(
    varNames: Set[String],
    expr: TemporalExpr
  ): Map[String, Expr[_]] = {
    varNames.map { name =>
      // Infer variable type from expression
      val varType = inferVariableType(name, expr)
      val sort = Z3Helper.typeToSort(ctx, varType)
      val z3Var = ctx.mkConst(name, sort)
      name -> z3Var
    }.toMap
  }
  
  /**
   * Infer variable type
   * Infer variable type from function calls in expression
   */
  private def inferVariableType(varName: String, expr: TemporalExpr): Type = {
    // Find function calls that use this variable in the expression
    findVariableTypeInExpr(varName, expr).getOrElse(Type.uintType) // Default to uint
  }
  
  /**
   * Recursively find variable type in expression
   * Infer type by finding position of Identifier(varName) in FunctionCall(name, args)
   */
  private def findVariableTypeInExpr(varName: String, expr: TemporalExpr): Option[Type] = {
    expr match {
      case TemporalExpr.FunctionCall(name, args) =>
        // Find the relation corresponding to this function
        program.relations.find(_.name == name) match {
          case Some(relation) =>
            // Find variable position in argument list
            args.zipWithIndex.collectFirst {
              case (TemporalExpr.Identifier(vName), idx) if vName == varName =>
                // Return the type at the corresponding position
                if (idx < relation.sig.length) relation.sig(idx) else Type.uintType
            }.orElse {
              // Recursively search nested expressions
              args.flatMap(arg => findVariableTypeInExpr(varName, arg)).headOption
            }
          case None =>
            // Relation not found, recursively search arguments
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
   * Factory method: create translator instance
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

