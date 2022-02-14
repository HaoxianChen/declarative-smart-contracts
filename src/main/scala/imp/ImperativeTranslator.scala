package imp

import datalog._

/** Generate imperative program from Datalog rules
 * */
case class ImperativeTranslator() {
  private val transactionPrefix = "recv_"

  def translate(program: Program): ImperativeAbstractProgram = {
    def isTransactionRule(rule: Rule): Boolean = rule.body.exists(_.relation.name.startsWith(transactionPrefix))
    def isTransactionTrigger(trigger: Trigger): Boolean = trigger.relation.name.startsWith(transactionPrefix)

    var triggers: Set[Trigger] = {
      val relationsToTrigger = program.interfaces.map(_.relation).filter(r => r.name.startsWith(transactionPrefix))
      val relationsInBodies = program.rules.flatMap(_.body).map(_.relation)
      val toTrigger = relationsInBodies.intersect(relationsToTrigger)
      toTrigger.map(rel => InsertTuple(rel))
    }
    var dependencies: Set[(Relation, Relation)] = Set()

    var triggered: Set[Trigger] = Set()
    var impProgram: Statement = Empty()
    while (triggers.nonEmpty) {
      for (trigger <- triggers) {

        val triggeredRules: Set[Rule] = program.rules.filter(
          r => r.body.map(_.relation).contains(trigger.relation) || r.aggregators.exists(_.relation==trigger.relation)
        ).filterNot( /** transaction rules are only triggered by new transaction.  */
          r => isTransactionRule(r) && !isTransactionTrigger(trigger)
        )

        for (rule <- triggeredRules) {
          val updateProgram = getUpdateStatements(rule, trigger)
          impProgram = Statement.makeSeq(impProgram, updateProgram)
          val allNextTriggers = getTrigger(updateProgram)
          /** Update dependencies */
          for (nt <- allNextTriggers) {
            dependencies += Tuple2(trigger.relation, nt.relation)
          }
          val nextTriggers = allNextTriggers.
            filterNot(t => program.interfaces.map(_.relation).contains(t.relation))
          assert(nextTriggers.size <= 1)
          /** Check no recursion */
          assert(triggered.intersect(nextTriggers).isEmpty)
          triggers ++= nextTriggers
        }

        triggered += trigger
        triggers -= trigger
      }
    }
    val constructor = {
      val constructorRel = program.relations.find(_.name=="constructor").get
      val (constuctorDefinition, dependentRelations) = getConstructor(constructorRel, program.rules)
      for (r <- dependentRelations) dependencies += Tuple2(constructorRel, r)
      constuctorDefinition
    }
    val statements = Statement.makeSeq(constructor,impProgram)
    val dependencyMap: Map[Relation, Set[Relation]] = {
      dependencies.groupBy(_._1).map{
        case (k,v) => k -> v.map(_._2)
      }
    }
    ImperativeAbstractProgram(program.name, program.relations, program.relationIndices, statements, dependencyMap)
  }

  private def getConstructor(constructorRel: Relation, rules: Set[Rule]): (Statement, Set[Relation]) = {
    val dependentRules = rules.filter(_.body.exists(_.relation.name=="constructor"))
    val allUpdates = dependentRules.map {
      r => getUpdateStatements(r, InsertTuple(constructorRel))
    }
    (Statement.makeSeq(allUpdates.toList:_*), dependentRules.map(_.head.relation))
  }

  private def getUpdateStatements(rule: Rule, trigger: Trigger): OnStatement = {
    def _updateStatementFromInsert(rule: Rule, insertedLiteral: Literal): OnStatement = {
      val updates = getInsertStatement(rule, insertedLiteral)
      OnInsert(insertedLiteral, rule.head.relation, updates)
    }
    def _getUpdateStatementsFromIncrement(rule: Rule, literal: Literal, incrementValue: IncrementValue): OnStatement = {
      val updates = getUpdateNonAgg(rule, incrementValue)
      OnIncrement(literal = literal, keyIndices=incrementValue.keyIndices,
        updateIndex = incrementValue.valueIndex,
        updateTarget = rule.head.relation, statement = updates)
    }
    val literal: Literal = {
      val aggs = rule.aggregators.filter(_.literal.relation == trigger.relation)
      assert(aggs.size <= 1)
      if (aggs.size == 1) {
        aggs.head.literal
      }
      else {
        val _lits = rule.body.filter(_.relation == trigger.relation)
        require(_lits.size == 1)
        _lits.head
      }
    }
    trigger match {
      case _: InsertTuple => _updateStatementFromInsert(rule, literal)
      case iv: IncrementValue => _getUpdateStatementsFromIncrement(rule, literal, iv)
    }
  }

  private def getIncrementFromAgg(rule: Rule): UpdateStatement = {
    require(rule.aggregators.size == 1)
    val agg = rule.aggregators.head
    require(rule.head.fields.contains(agg.aggResult))
    val resultIndex = rule.head.fields.indexOf(agg.aggResult)
    val keyIndices = rule.head.fields.indices.toList.filterNot(_==resultIndex)
    val delta: Arithmetic = agg match {
      case _: Sum => Param(agg.aggParam)
    }
    // Increment(rule.head.relation, agg.literal, keyIndices,resultIndex, delta = delta)
    Increment(rule.head.relation, rule.head, keyIndices,resultIndex, delta = delta)
  }

  private def getUpdateNonAgg(rule: Rule, incrementValue: IncrementValue): UpdateStatement = {
    /** todo: check that the increment value is not matched with other fields.
     *  Otherwise, fall back to generate update as if a new tuple is inserted.
     */
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

  private def getInsertStatement(rule: Rule, insert: Literal): Statement = {

    /** Generate assign statements for functors */
    val assignStatements = rule.functors.foldLeft[Statement](Empty())(
      (stmt, f) => f match {
        case Assign(p, a) => Statement.makeSeq(stmt,imp.Assign(p,a))
        case _ => stmt
      }
    )

    // Insert / Increment
    val updateStatement: UpdateStatement = if (rule.aggregators.isEmpty) {
      Insert(rule.head)
    }
    else {
      getIncrementFromAgg(rule)
    }

    // Check conditions
    val condition: Condition = getConditionsFromFunctors(rule.functors)
    val IfStatement: If = If(condition, Statement.makeSeq(assignStatements,updateStatement))

    // Join
    val groundedParams: Set[Parameter] = insert.fields.toSet
    val sortedLiteral: List[Literal] = {
      val rest = rule.body.filterNot(_.relation==insert.relation)
      sortJoinLiterals(rest)
    }
    _getJoinStatements(rule.head, groundedParams, sortedLiteral, IfStatement)
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
        case _: datalog.Assign => imp.True()
      }
      cond = Condition.conjunction(cond, nextCond)
    }
    cond
  }

  private def _getJoinStatements(ruleHead: Literal, groundedParams: Set[Parameter], remainingLiterals: List[Literal],
                         innerStatement: Statement): Statement = {
    def _getCondition(grounded: Set[Parameter], literal: Literal): Set[Match] = {
      literal.fields.zipWithIndex.flatMap {
        case (p, i) => p match {
            case v: Variable => if (grounded.contains(v)) {
              Some(Match(literal.relation, i, v))
            }
            else None
            case c: Constant => Some(Match(literal.relation, i, c))
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
        val condition: Set[Match] = _getCondition(groundedParams, head)
        Search(head.relation, condition, Statement.makeSeq(declareNewVars, nextStatements))
      }
    }
  }

  def getTrigger(statement: Statement): Set[Trigger] = statement match {
    case _:Empty | _:GroundVar | _:imp.Assign | _:ReadTuple | _:SolidityStatement => Set()
    case Seq(a,b) => getTrigger(a) ++ getTrigger(b)
    case If(_,s) => getTrigger(s)
    case o: OnStatement => getTrigger(o.statement)
    case Search(_, _, stmt) => getTrigger(stmt)
    case Insert(lit) => Set(InsertTuple(lit.relation))
    case Increment(rel,lit,keys,vid,delta) => Set(IncrementValue(rel,keys,vid,delta))
  }
}
