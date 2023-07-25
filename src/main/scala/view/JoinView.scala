package view

import com.microsoft.z3.{ArithExpr, ArithSort, ArraySort, BoolExpr, Context, Expr, IntExpr, IntSort, Sort, TupleSort}
import datalog.{Add, AnyType, ArithOperator, Arithmetic, Assign, BinaryOperator, BooleanType, CompoundType, Constant, Equal, Functor, Geq, Greater, Leq, Lesser, Literal, MsgSender, MsgValue, Mul, Negative, Now, NumberType, One, Param, Parameter, Relation, ReservedRelation, Rule, Send, SimpleRelation, SingletonRelation, Sub, SymbolType, Type, Unequal, UnitType, Variable, Zero}
import imp.{BooleanFunction, Condition, Delete, DeleteTuple, Empty, GroundVar, If, Increment, IncrementAndInsert, IncrementValue, Insert, InsertTuple, Match, MatchRelationField, OnDelete, OnIncrement, OnInsert, OnStatement, ReadTuple, ReplacedByKey, Require, Return, Search, Statement, Trigger, True, UpdateDependentRelations, UpdateStatement}
import imp.SolidityTranslator.transactionRelationPrefix
import verification.RuleZ3Constraints
import verification.TransitionSystem.makeStateVar
import verification.Z3Helper.{fieldsToConst, functorToZ3, getArraySort, getSort, initValue, literalToConst, paramToConst}

case class JoinView(rule: Rule, primaryKeyIndices: List[Int], ruleId: Int, allIndices: Map[Relation, List[Int]],
                    functions: Set[Relation],
                    arithmeticOptimization: Boolean,
                    enableProjection: Boolean) extends View {
  require(rule.aggregators.isEmpty)
  val isTransaction: Boolean = rule.body.exists(_.relation.name.startsWith(transactionRelationPrefix))
  val functionLiterals = rule.body.filter(lit=>functions.contains(lit.relation))

  def deleteRow(deleteTuple: DeleteTuple): OnStatement = {
    val delete = getInsertedLiteral(deleteTuple.relation)
    val updateStatement: UpdateStatement = Delete(rule.head)
    val statement = getNewRowDerivationStatements(delete, updateStatement)
    OnDelete(delete, rule.head.relation, statement, ruleId)
  }

  def updateRow(incrementValue: IncrementValue): OnStatement = {

    val literal = getInsertedLiteral(incrementValue.relation)
    if (isUpdatable(incrementValue) && arithmeticOptimization) {
      val updates = updateOnIncrementValue(incrementValue)
      OnIncrement(literal = literal, keyIndices=incrementValue.keyIndices,
        updateIndex = incrementValue.valueIndex,
        updateTarget = rule.head.relation, statement = updates, ruleId)
    }
    else {
      /** todo: Check case where the incremented value directly matched to the head. */
      val delta = {
        val d = Param(literal.fields(incrementValue.valueIndex))
        d
      }
      val increment = Increment(incrementValue.relation, literal, incrementValue.keyIndices, incrementValue.valueIndex,
        delta = delta)
      val incrementAndInsert = IncrementAndInsert(increment)
      OnIncrement(literal = literal, keyIndices=incrementValue.keyIndices,
        updateIndex = incrementValue.valueIndex,
        updateTarget = rule.head.relation, statement = incrementAndInsert, ruleId)
    }
  }

  def insertRow(insertTuple: InsertTuple): OnStatement = {
    val insert = getInsertedLiteral(insertTuple.relation)
    val statement = {
      val delete = if (isDeleteBeforeInsert(insertTuple.relation, insertTuple.keyIndices)) {
        /** todo: this delete should only affect the current view, not others. */
        deleteByKeysStatement(insert, insertTuple.keyIndices)
      }
      else {
        Empty()
      }
      val updateStatement: Statement = if (isTransaction) {
        Statement.makeSeq(Insert(rule.head), Return(Constant(BooleanType(), "true")))
      }
      else {
        Insert(rule.head)
      }
      val deriveUpdate = getNewRowDerivationStatements(insert, updateStatement)
      val ret = if (isTransaction) Return(Constant(BooleanType(), "false")) else Empty()
      Statement.makeSeq(delete, deriveUpdate, ret)
    }
    OnInsert(insert, rule.head.relation, statement, ruleId)
  }

  private def getNewRowDerivationStatements(insert: Literal, updateStatement: Statement): Statement = {

    /** Generate assign statements for functors */
    val assignStatements = rule.functors.foldLeft[Statement](Empty())(
      (stmt, f) => f match {
        case datalog.Assign(p, a) => Statement.makeSeq(stmt,imp.Assign(p,a))
        case _ => stmt
      }
    )

    val condition = _getConditions()
    val IfStatement: If = If(condition, Statement.makeSeq(assignStatements,updateStatement))

    // Join
    val groundedParams: Set[Parameter] = insert.fields.toSet
    val sortedLiteral: List[Literal] = {
      val rest = rule.body.filterNot(_.relation==insert.relation).diff(functionLiterals)
      sortJoinLiterals(rest)
    }
    val updates = _getJoinStatements(groundedParams, sortedLiteral, IfStatement)
    // OnInsert(insert, rule.head.relation, updates)
    updates
  }

  def getQueryStatement(): Statement = {
    val innerStatement = If(condition = _getConditions(), Return(Constant.CTrue))
    val groundedParams: Set[Parameter] = rule.head.fields.toSet
    val sortedLiteral: List[Literal] = {
      val rest = rule.body.diff(functionLiterals)
      sortJoinLiterals(rest)
    }
    _getJoinStatements(groundedParams, sortedLiteral, innerStatement)
  }

  private def _getConditions(): Condition = {
    // Check conditions
    val functorCondition: Condition = getConditionsFromFunctors(rule.functors)
    // Check functions
    val functionCondition: Condition = getConditionFromBooleanFunctions(functionLiterals)
    Condition.conjunction(functorCondition,functionCondition)
  }


  def getInsertedLiteral(relation: Relation): Literal = {
    val _lits = rule.body.filter(_.relation == relation)
    require(_lits.size == 1, s"Only support rules where each relation appears at most once: $rule.")
    _lits.head
  }

  private def sortJoinLiterals(literals: Set[Literal]): List[Literal] = {
    /** Sort the list of join literals, by their relations:
     * 1. Singleton relations
     * 2. Other relations ...  */
    literals.toList.sortWith((a,b) => a.relation match {
      case _: SingletonRelation|_:ReservedRelation => true
      case _: SimpleRelation => b.relation match {
        case _:SingletonRelation|_:ReservedRelation => false
        case _: SimpleRelation => true
      }
    })
  }

  private def getConditionsFromFunctors(functors: Set[Functor]): Condition = {
    var cond: Condition = True()
    for (f <- functors) {
      val nextCond: Condition = f match {
        case g: datalog.Greater =>  imp.Greater(g.a,g.b)
        case l: datalog.Lesser => imp.Lesser(l.a,l.b)
        case l: datalog.Geq => imp.Geq(l.a,l.b)
        case l: datalog.Leq => imp.Leq(l.a,l.b)
        case l: datalog.Unequal => imp.Unequal(l.a,l.b)
        case l: datalog.Equal => imp.Match(l.a,l.b)
        case _: datalog.Assign => imp.True()
      }
      cond = Condition.conjunction(cond, nextCond)
    }
    cond
  }

  private def getConditionFromBooleanFunctions(functionLiterals: Set[Literal]): Condition = {
    val conditions: Set[Condition] = functionLiterals.map(l=>BooleanFunction(l.relation.name, l.fields))
    Condition.makeConjunction(conditions.toList:_*)
  }

  private def _getJoinStatements(groundedParams: Set[Parameter], remainingLiterals: List[Literal],
                                 innerStatement: Statement): Statement = {
    def _getCondition(grounded: Set[Parameter], literal: Literal): Set[MatchRelationField] = {
      val keys = allIndices(literal.relation).map(i=>literal.fields(i))
      literal.fields.zipWithIndex.flatMap {
        case (p, i) => p match {
          case v: Variable => if (grounded.contains(v)) {
            Some(MatchRelationField(literal.relation, keys, i, v, enableProjection))
          }
          else None
          case c: Constant => Some(MatchRelationField(literal.relation, keys, i, c, enableProjection))
        }
      }.toSet
    }
    def _groundVariables(groundVar: Set[Parameter], literal: Literal): Statement = {
      var stmt: Statement = Empty()
      for ((p,i) <- literal.fields.zipWithIndex) {
        if (!groundVar.contains(p)) {
          val newStmt = p match {
            case v: Variable => if (v.name != "_") {
              val keys = allIndices(literal.relation).map(i=>literal.fields(i))
              GroundVar(v, literal.relation, keys, i, enableProjection)
            } else {
              Empty()
            }
            case _: Constant => Empty()
          }
          stmt = Statement.makeSeq(stmt, newStmt)
        }
      }
      stmt
    }
    remainingLiterals match {
      case Nil => innerStatement
      case head::tail => {
        val newGroundedParams = (groundedParams ++ head.fields.toSet).filterNot(_.name=="_")
        val declareNewVars: Statement = _groundVariables(groundedParams, head)
        val nextStatements = _getJoinStatements(newGroundedParams, tail, innerStatement)
        val condition: Set[MatchRelationField] = _getCondition(groundedParams, head)
        Search(head.relation, condition, Statement.makeSeq(declareNewVars, nextStatements))
      }
    }
  }

  private def isUpdatable(incrementValue: IncrementValue): Boolean = {
    /** Check that the incremented value is not matched with other fields.
     * And is differentiable in another functor
     *  */
    val literal = getInsertedLiteral(incrementValue.relation)
    val newParam = literal.fields(incrementValue.valueIndex)
    val isMatchedInBody = (rule.body - literal).exists(_.fields.contains(newParam))
    val existDifferentiableFunctor = rule.functors.exists {
      case _:Greater|_:Lesser|_:datalog.Geq|_:datalog.Leq|_:datalog.Unequal|_:datalog.Equal => false
      case datalog.Assign(_, b) => b match {
        case arith: Arithmetic => !Arithmetic.derivativeOf(arith,Param(newParam)).isInstanceOf[Zero]
        case _ => ???
      }
    }
    existDifferentiableFunctor && !isMatchedInBody
  }

  private def updateOnIncrementValue(incrementValue: IncrementValue): Increment = {
    val assignment: datalog.Assign = {
      val assignments: Set[datalog.Assign] = rule.functors.flatMap{
        case a: datalog.Assign => Some(a)
        case _ => None
      }
      require(assignments.size == 1, s"$rule\n${incrementValue}")
      assignments.head
    }
    val x: Param = {
      val lits = rule.body.filter(_.relation == incrementValue.relation)
      require(lits.size==1)
      val lit = lits.head
      val p = lit.fields(incrementValue.valueIndex)
      Param(p)
    }
    // todo: support more general cases, where join exists.
    val resultIndex: Int = rule.head.fields.indexOf(assignment.a.p)
    val keyIndices: List[Int] = rule.head.fields.indices.toList.filterNot(_==resultIndex)
    /** Apply the chain rule. */
    val delta: Arithmetic = {
      val _d = assignment.b match {
        case arithmetic: Arithmetic => Mul(Arithmetic.derivativeOf(arithmetic, x), x)
        case _ => ???
      }
      val d = Arithmetic.simplify(_d)
      d
    }
    Increment(rule.head.relation, rule.head, keyIndices, resultIndex, delta)
  }


  def insertRowZ3(ctx: Context, insertTuple: InsertTuple, isMaterialized: Boolean, z3Prefix: String) = {

  val insert = getInsertedLiteral(insertTuple.relation)

    val sortedLiteral: List[Literal] = {
      val rest = rule.body.filterNot(_.relation==insert.relation).diff(functionLiterals)
      sortJoinLiterals(rest)
    }

    val exprs = sortedLiteral.map(lit => literalToConst(ctx,lit,allIndices(lit.relation),z3Prefix)).toArray
    val functorExprs = rule.functors.toList.map(f=>functorToZ3(ctx,f,z3Prefix)).toArray

    val updateExprs: Array[(Expr[Sort], Expr[Sort], Expr[_<:Sort])] = if(isMaterialized) {
      updateZ3ConstraintOnInsert(ctx, this.rule.head, z3Prefix)
    } else Array()
    val updateConstraint: BoolExpr = ctx.mkTrue()

    val bodyConstraint = ctx.mkAnd((exprs++functorExprs):_*)

    val insertConstraints = makeRuleZ3Constraints(ctx, bodyConstraint, updateConstraint, updateExprs,
      InsertTuple(this.relation, this.primaryKeyIndices))
    insertConstraints
  }

  def updateRowZ3(ctx: Context, incrementValue: IncrementValue, isMaterialized: Boolean, z3Prefix: String) = {
    val (resultIndex, delta) = getUpdate(incrementValue)
    val insertedLiteral = getInsertedLiteral(incrementValue.relation)
    if (isUpdatable(incrementValue)) {
      val (updateConstraint, updateExpr) = updateTargetRelationZ3(ctx, insertedLiteral, delta, resultIndex, isMaterialized, z3Prefix)

      /** todo: support more general cases, where join exists.
       * Now this function only propagates the update.
       * */
      val bodyConstraint = ctx.mkTrue()
      makeRuleZ3Constraints(ctx, bodyConstraint, updateConstraint, updateExpr,
        IncrementValue(relation, primaryKeyIndices, resultIndex, delta))
    }
    else {
      ???
    }
  }

  private def resetRelationZ3(ctx: Context, head: Literal, z3Prefix: String):
    Array[(Expr[Sort], Expr[Sort], Expr[_ <: Sort])]
  = {
    val sort = getSort(ctx, this.relation, this.primaryKeyIndices)
    val (v_in, v_out) = makeStateVar(ctx, this.relation.name, sort)
    val newValueExpr = relation match {
      case SimpleRelation(name, sig, memberNames) => {
        val keys = primaryKeyIndices.map(i=> head.fields(i))
        val keyConstArray: Array[Expr[_]] = keys.toArray.map(f => paramToConst(ctx, f, z3Prefix)._1)
        val valueParams: List[Parameter] = head.fields.filterNot(f => keys.contains(f))
        val newValueConst: Expr[_] = if (valueParams.size==1) {
          initValue(ctx,valueParams.head._type)
        }
        else {
          ???
        }
        ctx.mkStore(v_in.asInstanceOf[Expr[ArraySort[Sort, Sort]]], keyConstArray,
          newValueConst.asInstanceOf[Expr[Sort]])
      }
      case SingletonRelation(name, sig, memberNames) => {
        /** todo: should use all fields, instead of only the first field */
        if (sig.size==1) {
          initValue(ctx, sig.head)
        }
        else {
          val tupleSort: TupleSort = getSort(ctx, relation, primaryKeyIndices).asInstanceOf[TupleSort]
          val paramConst = head.fields.toArray.map(f => initValue(ctx,f._type))
          tupleSort.mkDecl().apply(paramConst:_*)
        }
      }
      case relation: ReservedRelation => {
        assert(false)
        ???
      }
    }
    Array(Tuple3(v_in, v_out, newValueExpr))
  }

  def deleteRowZ3(ctx: Context, deleteTuple: DeleteTuple, isMaterialized: Boolean, z3Prefix: String) = {
    val deletedLiteral = getInsertedLiteral(deleteTuple.relation)

    val readOldValue = if (deleteTuple.keyIndices.isEmpty) {
      ???
    }
    else {
      val keyParams = deleteTuple.keyIndices.map(i=>deletedLiteral.fields(i))
      val keyConsts: Array[Expr[_]] = keyParams.map(f => paramToConst(ctx,f, z3Prefix)._1).toArray
      val valueIndices = deleteTuple.relation.sig.indices.filterNot(i => deleteTuple.keyIndices.contains(i)).toList
      val (sort, _, valueSort) = getArraySort(ctx, deleteTuple.relation, deleteTuple.keyIndices)
      val relConst = ctx.mkConst(deleteTuple.relation.name, sort)
      val read = ctx.mkSelect(relConst.asInstanceOf[Expr[ArraySort[Sort,Sort]]], keyConsts)

      if (valueIndices.size==1) {
        val valueParam = deletedLiteral.fields(valueIndices.head)
        val (valueConst,_) = paramToConst(ctx, valueParam, z3Prefix)
        ctx.mkEq(read, valueConst)
      }
      else {
        val valueParams: List[Parameter] = valueIndices.map(i=>deletedLiteral.fields(i))
        val valueConsts = valueParams.map(p => paramToConst(ctx, p, z3Prefix)._1)
        val tuple = valueSort.asInstanceOf[TupleSort].mkDecl().apply(valueConsts:_*)
        ctx.mkEq(read, tuple)
      }
    }

    val sortedLiteral: List[Literal] = {
      val rest = rule.body.filterNot(_.relation==deletedLiteral.relation)
      sortJoinLiterals(rest)
    }

    val exprs = sortedLiteral.map(lit => literalToConst(ctx,lit,allIndices(lit.relation),z3Prefix)).toArray
    val functorExprs = rule.functors.toList.map(f=>functorToZ3(ctx,f,z3Prefix)).toArray

    val resetExprs: Array[(Expr[Sort], Expr[Sort], Expr[_<:Sort])] = if (isMaterialized) {
      resetRelationZ3(ctx, this.rule.head, z3Prefix)
    }
    else {
      Array()
    }
    val updateConstraint: BoolExpr = ctx.mkTrue()
    val bodyConstraint = ctx.mkAnd(((readOldValue +: exprs ) ++ functorExprs):_*)

    makeRuleZ3Constraints(ctx, bodyConstraint, updateConstraint, resetExprs,
      DeleteTuple(this.relation, this.primaryKeyIndices))
  }

  private def getUpdate(incrementValue: IncrementValue): (Int, Arithmetic) = {
    val assignment: datalog.Assign = {
      val assignments: Set[datalog.Assign] = rule.functors.flatMap{
        case a: datalog.Assign => Some(a)
        case _ => None
      }
      require(assignments.size == 1, s"$rule\n${incrementValue}")
      assignments.head
    }
    val x: Param = {
      val lits = rule.body.filter(_.relation == incrementValue.relation)
      require(lits.size==1)
      val lit = lits.head
      val p = lit.fields(incrementValue.valueIndex)
      Param(p)
    }
    val resultIndex: Int = rule.head.fields.indexOf(assignment.a.p)
    /** Apply the chain rule. */
    val delta: Arithmetic = {
      val _d = assignment.b match {
        case arithmetic: Arithmetic => Mul(Arithmetic.derivativeOf(arithmetic, x), x)
        case _ => ???
      }
      val d = Arithmetic.simplify(_d)
      d
    }
    (resultIndex, delta)
  }

  def getZ3QueryConstraint(ctx: Context, z3Prefix: String): BoolExpr = {
    val sortedLiteral = {
      val rest = rule.body.diff(functionLiterals)
      sortJoinLiterals(rest)
    }
    val exprs = sortedLiteral.map(lit => literalToConst(ctx,lit,allIndices(lit.relation),z3Prefix)).toArray
    val functorExprs = rule.functors.map(f => functorToZ3(ctx,f,z3Prefix)).toArray
    ctx.mkAnd(exprs++functorExprs:_*)
  }
}
