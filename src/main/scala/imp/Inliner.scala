package imp

import datalog.{Add, Arithmetic, Div, Expr, Min, Mul, Negative, One, Param, Parameter, Relation, ReservedRelation, SimpleRelation, SingletonRelation, Sub, Variable, Zero}
import imp.SolidityTranslator.transactionRelationPrefix

case class Inliner(solidityProgram: Statement,
                   interfaces: Set[Relation]) {
  val simplified = {
    val simplifier: Simplifier = new Simplifier()
    simplifier.simplify(solidityProgram)
  }
  val functionDefs: Map[String, DeclFunction] = collectFunctionDefs(simplified)
  val transactionNames: Set[String] = interfaces.flatMap { rel =>
    if (rel.name.startsWith(transactionRelationPrefix))
      Some(rel.name.stripPrefix(transactionRelationPrefix))
    else
      None
  }

  def run(): Statement = {
    var current: Statement = simplified
    var changed = true
    while (changed) {
      val before = current
      current = inlineFunctions(current)
      changed = before != current
    }
    val noUncalled = removeUncalledFunctions(current)
    removeEmptyStatements(noUncalled)
  }

  /**
    * Perform function inlining for a given Statement.
    * This method will replace function calls in the Statement with their bodies.
    *
    * @param stmt The Statement to inline.
    * @return The Statement with all function calls inlined.
    */
  def inlineFunctions(s: Statement): Statement = s match {
    case DeclContract(_name, _statement) => DeclContract(_name, inlineFunctions(_statement))
    case Seq(a, b) =>
      val inlinedA = inlineFunctions(a)
      val inlinedB = inlineFunctions(b)
      Seq(inlinedA, inlinedB)
    case If(condition, statement) =>
      val inlinedStmt = inlineFunctions(statement)
      If(condition, inlinedStmt)
    case statement: SolidityStatement => inlineSolidityStatement(statement)
    case _ => s
  }

  /**
    * Helper to inline SolidityStatement cases.
    */
  private def inlineSolidityStatement(statement: SolidityStatement): Statement = statement match {
    case ForLoop(iterator, initValue, loopCondition, nextValue, body) =>
      val inlinedBody = inlineFunctions(body)
      ForLoop(iterator, initValue, loopCondition, nextValue, inlinedBody)
    case Constructor(params, body) =>
      val inlinedBody = inlineFunctions(body)
      Constructor(params, inlinedBody)
    case Call(functionName, params, optReturnVar) =>
      inlineCall(functionName, params, optReturnVar)
    case CallObjectMethod(objectName, methodName, params, optRet) =>
      inlineCallObjectMethod(objectName, methodName, params, optRet)
    case df: DeclFunction =>
      val inlinedBody = inlineFunctions(df.stmt)
      df.copy(stmt = inlinedBody)
    case _ => statement
  }

  /**
    * Dedicated function to inline Call, now takes functionDefs as an explicit argument.
    */
  private def inlineCall(functionName: String, params: List[Parameter], optReturnVar: Option[Variable]): Statement = {
    // 1. Locate the function definition
    val funcOpt = functionDefs.get(functionName)
    require(funcOpt.isDefined, s"Function $functionName not found for inlining.")
    val func = funcOpt.get
    // 2. Substitute parameters
    /** todo: also need to rename all local variable in the body
     *    to avoid naming contention.
     *    Reuse the substituteParams method.
     */
    val paramMap: Map[Parameter, Parameter] = func.params.zip(params).toMap
    val inlinedBody = substituteParams(func.stmt, paramMap)
    // 3. Handle return value
    if (optReturnVar.isDefined) {
      def replaceReturn(stmt: Statement): Statement = stmt match {
        case Return(p) if optReturnVar.isDefined => Assign(Param(optReturnVar.get), Param(p))
        case Seq(a, b) => Seq(replaceReturn(a), replaceReturn(b))
        case If(cond, s) => If(cond, replaceReturn(s))
        case s: SolidityStatement => s
        case _ => stmt
      }
      replaceReturn(inlinedBody)
    } else {
      inlinedBody
    }
  }

  /**
    * Dedicated function to inline CallObjectMethod.
    */
  private def inlineCallObjectMethod(objectName: String, methodName: String, params: List[String], optRet: Option[Variable]): Statement = {
    // TODO: Implement the actual inlining logic for object method calls
    ???
  }

  /**
    * Scans the input statement and collects all DeclFunction definitions (SolidityStatement subtype) into a map.
    * Only traverses SolidityStatement nodes.
    */
  def collectFunctionDefs(stmt: Statement): Map[String, DeclFunction] = {
    def scan(s: Statement, acc: Map[String, DeclFunction]): Map[String, DeclFunction] = s match {
      case ss: SolidityStatement => ss match {
        case df: DeclFunction => acc + (df.name -> df)
        case Constructor(_, body) => scan(body, acc)
        case ForLoop(_, _, _, _, body) => scan(body, acc)
        case DeclModifier(_, _, before, after) => scan(before, acc) ++ scan(after, acc)
        case DeclContract(_, body) => scan(body, acc)
        case _ => acc
      }
      case Seq(a, b) => scan(a, acc) ++ scan(b, acc)
      case If(_, body) => scan(body, acc)
      case _ => acc
    }
    scan(stmt, Map.empty)
  }

  /**
    * Remove DeclFunction definitions that are not called in the contract.
    */
  private def removeUncalledFunctions(stmt: Statement): Statement = {
    // Collect all function names that are called
    def collectCalledNames(s: Statement, called: Set[String]): Set[String] = s match {
      case Call(name, _, _) => called + name
      case Seq(a, b) => collectCalledNames(a, called) ++ collectCalledNames(b, called)
      case If(_, body) => collectCalledNames(body, called)
      case ForLoop(_, _, _, _, body) => collectCalledNames(body, called)
      case Constructor(_, body) => collectCalledNames(body, called)
      case DeclModifier(_, _, before, after) => collectCalledNames(before, called) ++ collectCalledNames(after, called)
      case DeclContract(_, body) => collectCalledNames(body, called)
      case DeclFunction(_, _, _, body, _) => collectCalledNames(body, called)
      case _ => called
    }
    val calledNames = collectCalledNames(stmt, Set.empty)
    // Remove DeclFunction definitions not in calledNames or transactionNames
    def filterFunctions(s: Statement): Statement = s match {
      case Seq(a, b) => Seq(filterFunctions(a), filterFunctions(b))
      case If(cond, body) => If(cond, filterFunctions(body))
      case ForLoop(it, iv, lc, nv, body) => ForLoop(it, iv, lc, nv, filterFunctions(body))
      case Constructor(params, body) => Constructor(params, filterFunctions(body))
      case DeclModifier(n, p, before, after) => DeclModifier(n, p, filterFunctions(before), filterFunctions(after))
      case DeclContract(n, body) => DeclContract(n, filterFunctions(body))
      case df @ DeclFunction(name, _, _, _, _) =>
        if (calledNames.contains(name) || transactionNames.contains(name) ||
            name.startsWith("get")) df else Empty()
      case _ => s
    }
    filterFunctions(stmt)
  }

  def substituteParams(stmt: Statement, paramMap: Map[Parameter, Parameter]): Statement = stmt match {
    case Assign(p, expr) => Assign(Param(paramMap.getOrElse(p.p, p.p)), substituteParamsExpr(expr, paramMap))
    case Seq(a, b) => Seq(substituteParams(a, paramMap), substituteParams(b, paramMap))
    case If(cond, s) => If(substituteParamsCondition(cond, paramMap), substituteParams(s, paramMap))
    case us: UpdateStatement => us match {
      case Insert(literal) =>
        Insert(literal.copy(fields = literal.fields.map(p => paramMap.getOrElse(p, p))))
      case Delete(literal) =>
        Delete(literal.copy(fields = literal.fields.map(p => paramMap.getOrElse(p, p))))
      case DeleteByKeys(relation, keys, updateTarget) =>
        DeleteByKeys(relation, keys.map(p => paramMap.getOrElse(p, p)), updateTarget)
      case Increment(relation, literal, keyIndices, valueIndex, delta) =>
        Increment(
          relation,
          literal.copy(fields = literal.fields.map(p => paramMap.getOrElse(p, p))),
          keyIndices,
          valueIndex,
          substituteParamsArithmetic(delta, paramMap)
        )
      case IncrementAndInsert(increment) =>
        IncrementAndInsert(substituteParams(increment, paramMap).asInstanceOf[Increment])
    }
    case GroundVar(p, relation, keys, valueIndex, enableProjection) =>
      GroundVar(p, relation, keys.map(k => paramMap.getOrElse(k, k)), valueIndex, enableProjection)
    case s: SolidityStatement => s match {
      case ReadTuple(relation, keyList, outputVar) =>
        ReadTuple(relation, keyList.map(k => paramMap.getOrElse(k, k)), outputVar)
      case ReadArray(arrayName, iterator, outputVar) =>
        ReadArray(arrayName, paramMap.getOrElse(iterator, iterator), outputVar)
      case ReadValueFromMap(relation, keyList, output) =>
        ReadValueFromMap(relation, keyList.map(k => paramMap.getOrElse(k, k)), paramMap.getOrElse(output, output))
      case UpdateMap(name, keys, tupleTypeName, params) =>
        UpdateMap(name, keys.map(k => paramMap.getOrElse(k, k)), tupleTypeName, params.map(p => paramMap.getOrElse(p, p)))
      case UpdateMapValue(name, keys, fieldName, p) =>
        UpdateMapValue(name, keys.map(k => paramMap.getOrElse(k, k)), fieldName, paramMap.getOrElse(p, p))
      case SetTuple(relation, params) =>
        SetTuple(relation, params.map(p => paramMap.getOrElse(p, p)))
      case ConvertType(from, to) =>
        ConvertType(
          substituteParamsArithmetic(from, paramMap),
          paramMap.getOrElse(to, to).asInstanceOf[Variable]
        )
      case Call(functionName, params, optReturnVar) =>
        Call(
          functionName,
          params.map(p => paramMap.getOrElse(p, p)),
          optReturnVar
        )
      case ForLoop(iterator, initValue, loopCondition, nextValue, statement) =>
        ??? // throw exception here.
      case GetObjectAttribute(objectName, attributeName, ret) =>
        GetObjectAttribute(objectName, attributeName, paramMap.getOrElse(ret, ret))
      case CallObjectMethod(objectName, methodName, params, optRet) =>
        ??? // throw execption here
      case Return(p) => Return(paramMap.getOrElse(p, p))
      case SendEther(p, amount) => SendEther(paramMap.getOrElse(p, p), paramMap.getOrElse(amount, amount))
      case Emit(event, parameters) => Emit(event, parameters.map(p => paramMap.getOrElse(p, p)))
      case DeclFunction(name, params, returnType, stmt, metaData) =>
        DeclFunction(name, params.map(p => paramMap.getOrElse(p, p)), returnType, substituteParams(stmt, paramMap), metaData)
      case DeclEvent(name, params) =>
        DeclEvent(name, params.map(p => paramMap.getOrElse(p, p)))
      case DeclModifier(name, params, beforeStatement, afterStatement) =>
        DeclModifier(
          name,
          params.map(p => paramMap.getOrElse(p, p)),
          substituteParams(beforeStatement, paramMap),
          substituteParams(afterStatement, paramMap)
        )
      case DeclContract(name, statement) =>
        DeclContract(name, substituteParams(statement, paramMap))
      case DefineStruct(name, _type) => DefineStruct(name, _type)
      case DeclVariable(name, _type) => DeclVariable(name, _type)
      case Require(condition, msg) =>
        Require(substituteParamsCondition(condition, paramMap), msg)
      case ConvertType(from, to) =>  ConvertType(
        substituteParamsArithmetic(from, paramMap),
        paramMap.getOrElse(to, to).asInstanceOf[Variable] )
      case _ => s
    }
    case _ => stmt
  }
  /**
    * Substitute parameters in a datalog.Expr according to paramMap.
    */
  private def substituteParamsArithmetic(expr: Arithmetic, paramMap: Map[Parameter, Parameter]): Arithmetic = expr match {
    case Zero(t) => Zero(t)
    case One(t) => One(t)
    case Param(p) => Param(paramMap.getOrElse(p, p))
    case Negative(e) => Negative(substituteParamsArithmetic(e, paramMap))
    case Add(a, b) => Add(substituteParamsArithmetic(a, paramMap), substituteParamsArithmetic(b, paramMap))
    case Sub(a, b) => Sub(substituteParamsArithmetic(a, paramMap), substituteParamsArithmetic(b, paramMap))
    case Mul(a, b) => Mul(substituteParamsArithmetic(a, paramMap), substituteParamsArithmetic(b, paramMap))
    case Div(a, b) => Div(substituteParamsArithmetic(a, paramMap), substituteParamsArithmetic(b, paramMap))
    case Min(a, b) => Min(substituteParamsArithmetic(a, paramMap), substituteParamsArithmetic(b, paramMap))
    case _ => expr
  }

  private def substituteParamsExpr(expr: Expr, paramMap: Map[Parameter,Parameter]): Expr = expr match {
    case arithmetic: Arithmetic => substituteParamsArithmetic(arithmetic, paramMap)
  }

  /**
    * Substitute parameters in a Condition according to paramMap.
    */
  private def substituteParamsCondition(cond: Condition, paramMap: Map[Parameter, Parameter]): Condition = cond match {
    case True() => True()
    case False() => False()
    case Match(a, b) => Match(substituteParamsExpr(a, paramMap), substituteParamsExpr(b, paramMap))
    case Unequal(a, b) => Unequal(substituteParamsExpr(a, paramMap), substituteParamsExpr(b, paramMap))
    case Greater(a, b) => Greater(substituteParamsArithmetic(a, paramMap), substituteParamsArithmetic(b, paramMap))
    case Lesser(a, b) => Lesser(substituteParamsArithmetic(a, paramMap), substituteParamsArithmetic(b, paramMap))
    case Geq(a, b) => Geq(substituteParamsArithmetic(a, paramMap), substituteParamsArithmetic(b, paramMap))
    case Leq(a, b) => Leq(substituteParamsArithmetic(a, paramMap), substituteParamsArithmetic(b, paramMap))
    case And(a, b) => And(substituteParamsCondition(a, paramMap), substituteParamsCondition(b, paramMap))
    case Or(a, b) => Or(substituteParamsCondition(a, paramMap), substituteParamsCondition(b, paramMap))
    case BooleanFunction(name, parameters) => BooleanFunction(name, parameters.map(p => paramMap.getOrElse(p, p)))
    case _ => cond
  }

  /**
    * Recursively remove Empty() statements from the Statement tree.
    */
  private def removeEmptyStatements(stmt: Statement): Statement = stmt match {
    case Seq(a, b) =>
      (removeEmptyStatements(a), removeEmptyStatements(b)) match {
        case (Empty(), Empty()) => Empty()
        case (x, Empty()) => x
        case (Empty(), y) => y
        case (x, y) => Seq(x, y)
      }
    case If(cond, body) =>
      val newBody = removeEmptyStatements(body)
      if (newBody == Empty()) Empty() else If(cond, newBody)
    case ForLoop(it, iv, lc, nv, body) =>
      val newBody = removeEmptyStatements(body)
      if (newBody == Empty()) Empty() else ForLoop(it, iv, lc, nv, newBody)
    case Constructor(params, body) =>
      val newBody = removeEmptyStatements(body)
      if (newBody == Empty()) Empty() else Constructor(params, newBody)
    case DeclModifier(n, p, before, after) =>
      val newBefore = removeEmptyStatements(before)
      val newAfter = removeEmptyStatements(after)
      if (newBefore == Empty() && newAfter == Empty()) Empty()
      else DeclModifier(n, p, newBefore, newAfter)
    case DeclContract(n, body) =>
      val newBody = removeEmptyStatements(body)
      if (newBody == Empty()) Empty() else DeclContract(n, newBody)
    case df @ DeclFunction(_, _, _, _, _) => df
    case Empty() => Empty()
    case _ => stmt
  }

}
