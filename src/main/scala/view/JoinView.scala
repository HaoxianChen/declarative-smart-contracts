package view

import datalog.{Arithmetic, Constant, Functor, Greater, Lesser, Literal, Mul, Param, Parameter, Relation, ReservedRelation, Rule, SimpleRelation, SingletonRelation, Variable, Zero}
import imp.{Condition, Delete, DeleteTuple, Empty, GroundVar, If, Increment, IncrementValue, Insert, InsertTuple, MatchRelationField, OnDelete, OnIncrement, OnInsert, OnStatement, ReadTuple, Search, Statement, True, UpdateDependentRelations, UpdateStatement}

case class JoinView(rule: Rule, primaryKeyIndices: List[Int], ruleId: Int) extends View {
  require(rule.aggregators.isEmpty)

  def deleteRow(deleteTuple: DeleteTuple): OnStatement = {
    val delete = getInsertedLiteral(deleteTuple.relation)
    val updateStatement: UpdateStatement = Delete(rule.head)
    val statement = getNewRowDerivationStatements(delete, updateStatement)
    OnDelete(delete, rule.head.relation, statement, ruleId)
  }

  def updateRow(incrementValue: IncrementValue): OnStatement = {

    if (isUpdatable(incrementValue)) {
      val updates = updateOnIncrementValue(incrementValue)
      val literal = getInsertedLiteral(incrementValue.relation)
      OnIncrement(literal = literal, keyIndices=incrementValue.keyIndices,
        updateIndex = incrementValue.valueIndex,
        updateTarget = rule.head.relation, statement = updates, ruleId)
    }
    else {
      /** todo: Check case where the incremented value directly matched to the head. */
      fallBackToInsertRow(incrementValue)
    }
  }

  def insertRow(insertTuple: InsertTuple): OnStatement = {
    val insert = getInsertedLiteral(insertTuple.relation)
    val statement = {
      val delete = if (isDeleteBeforeInsert(insertTuple.relation, insertTuple.keyIndices)) {
        deleteByKeysStatement(insert, insertTuple.keyIndices)
      }
      else {
        Empty()
      }
      val updateStatement: Statement = Insert(rule.head)
      val deriveUpdate = getNewRowDerivationStatements(insert, updateStatement)
      Statement.makeSeq(delete, deriveUpdate)
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

  private def getConditionsFromFunctors(functors: Set[Functor]): Condition = {
    var cond: Condition = True()
    for (f <- functors) {
      val nextCond: Condition = f match {
        case g: datalog.Greater =>  imp.Greater(g.a,g.b)
        case l: datalog.Lesser => imp.Lesser(l.a,l.b)
        case l: datalog.Geq => imp.Geq(l.a,l.b)
        case l: datalog.Leq => imp.Leq(l.a,l.b)
        case l: datalog.Unequal => imp.Unequal(l.a,l.b)
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
        val newGroundedParams = groundedParams ++ head.fields.toSet
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
      case _:Greater|_:Lesser|_:datalog.Geq|_:datalog.Leq|_:datalog.Unequal => false
      case datalog.Assign(_, b) => !Arithmetic.derivativeOf(b,Param(newParam)).isInstanceOf[Zero]
    }
    existDifferentiableFunctor && !isMatchedInBody
  }

  private def updateOnIncrementValue(incrementValue: IncrementValue): UpdateStatement = {
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
      Arithmetic.simplify(_d)
    }
    Increment(rule.head.relation, rule.head, keyIndices, resultIndex, delta)
  }

  private def fallBackToInsertRow(incrementValue: IncrementValue): OnStatement = {
    val literal = getInsertedLiteral(incrementValue.relation)
    val updates: Statement = {
      val tupleName = "toInsert"
      val readTuple = {
        val keyList = incrementValue.keyIndices.map(i=>literal.fields(i))
        ReadTuple(incrementValue.relation, keyList, tupleName)
      }
      val targetField: String = incrementValue.relation.memberNames(incrementValue.valueIndex)
      val deltaParam: Parameter = literal.fields(incrementValue.valueIndex)
      val toInsert: Literal = {
        val rel = incrementValue.relation
        val fields = rel.sig.zip(rel.memberNames).map {
          case (t,n) => if (n==targetField) {
            Variable(t, s"$tupleName.$n+$deltaParam")
          }
          else {
            Variable(t, s"$tupleName.$n")
          }
        }
        Literal(incrementValue.relation, fields)
      }
      val insert = Insert(toInsert)
      val updateDependentRelations = UpdateDependentRelations(insert)
      Statement.makeSeq(readTuple, updateDependentRelations)
    }
    OnIncrement(literal = literal, keyIndices=incrementValue.keyIndices,
      updateIndex = incrementValue.valueIndex,
      updateTarget = rule.head.relation, statement = updates, ruleId)
  }
}
