package view

import com.microsoft.z3.{ArithExpr, ArithSort, ArraySort, BoolExpr, Context, Expr, IntExpr, IntSort, Sort}
import datalog.{Add, AnyType, Arithmetic, Assign, BinFunctor, BinaryOperator, BooleanType, CompoundType, Constant, Equal, Geq, Greater, Leq, Lesser, Literal, MsgSender, MsgValue, Mul, Negative, Now, NumberType, One, Param, Parameter, Relation, ReservedRelation, Rule, Send, SimpleRelation, SingletonRelation, Sub, SymbolType, Type, Unequal, UnitType, Variable, Zero}
import imp.{Condition, Delete, DeleteTuple, Empty, GroundVar, If, Increment, IncrementAndInsert, IncrementValue, Insert, InsertTuple, MatchRelationField, OnDelete, OnIncrement, OnInsert, OnStatement, ReadTuple, ReplacedByKey, Return, Search, Statement, Trigger, True, UpdateDependentRelations, UpdateStatement}
import imp.SolidityTranslator.transactionRelationPrefix
import verification.Z3Helper.{literalToConst, functorToZ3}

case class JoinView(rule: Rule, primaryKeyIndices: List[Int], ruleId: Int, allIndices: Map[Relation, List[Int]]) extends View {
  require(rule.aggregators.isEmpty)
  val isTransaction: Boolean = rule.body.exists(_.relation.name.startsWith(transactionRelationPrefix))

  def deleteRow(deleteTuple: DeleteTuple): OnStatement = {
    val delete = getInsertedLiteral(deleteTuple.relation)
    val updateStatement: UpdateStatement = Delete(rule.head)
    val statement = getNewRowDerivationStatements(delete, updateStatement)
    OnDelete(delete, rule.head.relation, statement, ruleId)
  }

  def updateRow(incrementValue: IncrementValue): OnStatement = {

    val literal = getInsertedLiteral(incrementValue.relation)
    if (isUpdatable(incrementValue)) {
      val updates = updateOnIncrementValue(incrementValue)
      OnIncrement(literal = literal, keyIndices=incrementValue.keyIndices,
        updateIndex = incrementValue.valueIndex,
        updateTarget = rule.head.relation, statement = updates, ruleId)
    }
    else {
      /** todo: Check case where the incremented value directly matched to the head. */
      val delta = {
        val d = Param(literal.fields(incrementValue.valueIndex))
        Arithmetic.updateArithmeticType(d, View.getDeltaType(d._type))
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

    // Check conditions
    val condition: Condition = getConditionsFromFunctors(rule.functors)
    val IfStatement: If = If(condition, Statement.makeSeq(assignStatements,updateStatement))

    // Join
    val groundedParams: Set[Parameter] = insert.fields.toSet
    val sortedLiteral: List[Literal] = {
      val rest = rule.body.filterNot(_.relation==insert.relation)
      sortJoinLiterals(rest)
    }
    val updates = _getJoinStatements(rule.head, groundedParams, sortedLiteral, IfStatement)
    // OnInsert(insert, rule.head.relation, updates)
    updates
  }

  private def getInsertedLiteral(relation: Relation): Literal = {
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

  private def getConditionsFromFunctors(functors: Set[BinFunctor]): Condition = {
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
  private def _getJoinStatements(ruleHead: Literal, groundedParams: Set[Parameter], remainingLiterals: List[Literal],
                                 innerStatement: Statement): Statement = {
    def _getCondition(grounded: Set[Parameter], literal: Literal): Set[MatchRelationField] = {
      literal.fields.zipWithIndex.flatMap {
        case (p, i) => p match {
          case v: Variable => if (grounded.contains(v)) {
            Some(MatchRelationField(literal.relation, i, v))
          }
          else None
          case c: Constant => Some(MatchRelationField(literal.relation, i, c))
        }
      }.toSet
    }
    def _groundVariables(groundVar: Set[Parameter], literal: Literal): Statement = {
      var stmt: Statement = Empty()
      for ((p,i) <- literal.fields.zipWithIndex) {
        if (!groundVar.contains(p)) {
          val newStmt = p match {
            case v: Variable => if (v.name != "_") {
              GroundVar(v, literal.relation, i)
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
        val nextStatements = _getJoinStatements(ruleHead, newGroundedParams, tail, innerStatement)
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
      case datalog.Assign(_, b) => !Arithmetic.derivativeOf(b,Param(newParam)).isInstanceOf[Zero]
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
      val _d = Mul(Arithmetic.derivativeOf(assignment.b, x), x)
      val d = Arithmetic.simplify(_d)
      Arithmetic.updateArithmeticType(d, View.getDeltaType(x._type))
    }
    Increment(rule.head.relation, rule.head, keyIndices, resultIndex, delta)
  }

  def insertRowZ3(ctx: Context, insertTuple: InsertTuple, isMaterialized: Boolean, z3Prefix: String):
    (Array[BoolExpr], Array[(Expr[Sort], Expr[Sort], Expr[_<:Sort])]) = {

    val insert = getInsertedLiteral(insertTuple.relation)

    val sortedLiteral: List[Literal] = {
      val rest = rule.body.filterNot(_.relation==insert.relation)
      sortJoinLiterals(rest)
    }

    val exprs = sortedLiteral.map(lit => literalToConst(ctx,lit,allIndices(lit.relation),z3Prefix))
    val functorExprs = rule.functors.toList.map(f=>functorToZ3(ctx,f,z3Prefix))

    val updateExprs: Array[(Expr[Sort], Expr[Sort], Expr[_<:Sort])] = if(isMaterialized) ??? else Array()

    ((exprs++functorExprs).toArray, updateExprs)
  }

  def updateRowZ3(ctx: Context, incrementValue: IncrementValue, isMaterialized: Boolean, z3Prefix: String):
    (Array[BoolExpr], Array[(Expr[Sort], Expr[Sort], Expr[_<:Sort])]) = {
    /** todo: support more general cases, where join exists.
     * Now this function only propagates the update.
     * */
    val (resultIndex, delta) = getUpdate(incrementValue)
    val insertedLiteral = getInsertedLiteral(incrementValue.relation)
    updateTargetRelationZ3(ctx, insertedLiteral, delta, resultIndex, isMaterialized, z3Prefix)
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
      val _d = Mul(Arithmetic.derivativeOf(assignment.b, x), x)
      val d = Arithmetic.simplify(_d)
      Arithmetic.updateArithmeticType(d, View.getDeltaType(x._type))
    }
    (resultIndex, delta)
  }

  def getNextTriggers(trigger: Trigger): Set[Trigger] = trigger match {
    case InsertTuple(relation, keyIndices) => Set(InsertTuple(this.rule.head.relation, this.primaryKeyIndices))
    case DeleteTuple(relation, keyIndices) => ???
    case ReplacedByKey(relation, keyIndices, targetRelation) => ???
    case ic: IncrementValue => {
      val (valueIndex, delta) = getUpdate(ic)
      Set(IncrementValue(relation, primaryKeyIndices, valueIndex, delta))
    }
  }

}
