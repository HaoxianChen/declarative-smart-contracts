package imp

import datalog._

/** Generate imperative program from Datalog rules
 * */
case class Translator() {
  def translate(program: Program): Statement = {
    var triggers: Set[Trigger] = {
      val relationsInBodies = program.rules.flatMap(_.body).map(_.relation)
      val toTrigger = relationsInBodies.intersect(program.interfaces.map(_.relation))
      toTrigger.map(rel => InsertTuple(rel))
    }

    var triggered: Set[Trigger] = Set()
    var impProgram: Statement = Empty()
    while (triggers.nonEmpty) {
      for (trigger <- triggers) {

        val triggeredRules: Set[Rule] = program.rules.filter(
          _.body.map(_.relation).contains(trigger.relation)
        )

        for (rule <- triggeredRules) {
          val updateProgram = getUpdateStatements(rule, trigger)
          impProgram = Statement.makeSeq(impProgram, updateProgram)
          val nextTriggers = getTrigger(updateProgram)
          assert(nextTriggers.size==1)
          /** Check no recursion */
          assert(triggered.intersect(nextTriggers).isEmpty)
          triggers ++= nextTriggers
        }

        triggered += trigger
        triggers -= trigger
      }
    }
    impProgram
  }

  def getUpdateStatements(rule: Rule, trigger: Trigger): Statement = {
    val updates = trigger match {
      case it: InsertTuple => {
        val insert: Literal = {
          val lits =  rule.body.filter(_.relation == it.relation)
          assert(lits.size==1)
          lits.head
        }
        getInsertStatement(rule, insert)
      }
      case _: IncrementValue => ???
    }
    On(trigger, updates)
  }

  def getInsertStatement(rule: Rule, insert: Literal): Statement = {
    // Insert / Increment
    val updateStatement: UpdateStatement = Insert(rule.head)

    // Check conditions
    val condition: Condition = getConditionsFromFunctors(rule.functors)
    val IfStatement: If = If(condition, updateStatement)

    // Join
    val groundedParams: Set[Parameter] = insert.fields.toSet
    val sortedLiteral: List[Literal] = {
      val rest = rule.body.diff(Set(insert))
      sortJoinLiterals(rest)
    }
    _getJoinStatements(rule.head, groundedParams, sortedLiteral, IfStatement)
  }

  def sortJoinLiterals(literals: Set[Literal]): List[Literal] = {
    /** Sort the list of join literals, by their relations:
     * 1. Singleton relations
     * 2. Other relations ...  */
    literals.toList.sortWith((a,b) => a.relation match {
      case _: SingletonRelation => true
      case _: SimpleRelation => b.relation match {
        case _: SingletonRelation => false
        case _: SimpleRelation => true
      }
    })

  }

  def getConditionsFromFunctors(functors: Set[Functor]): Condition = {
    var cond: Condition = True()
    for (f <- functors) {
      val nextCond: Condition = f match {
        case g: datalog.Greater =>  imp.Greater(g.a,g.b)
        case l: datalog.Lesser => imp.Lesser(l.a,l.b)
        case l: datalog.Geq => imp.Geq(l.a,l.b)
        case l: datalog.Leq => imp.Leq(l.a,l.b)
      }
      cond = Condition.conjunction(cond, nextCond)
    }
    cond
  }

  def _getJoinStatements(ruleHead: Literal, groundedParams: Set[Parameter], remainingLiterals: List[Literal],
                         innerStatement: Statement): Statement = {
    def _getCondition(grounded: Set[Parameter], literal: Literal): Condition = {
      var cond: Condition = True()
      for ((p,i) <- literal.fields.zipWithIndex) {
        val nextCond: Condition = p match {
          case v: Variable => if (grounded.contains(v)) {
              Match(literal.relation, i, v)
            }
            else {
              True()
            }
          case c: Constant => Match(literal.relation, i, c)
        }
        cond = Condition.conjunction(cond, nextCond)
      }
      cond
    }
    def _groundVariables(groundVar: Set[Parameter], literal: Literal): Statement = {
      var stmt: Statement = Empty()
      for ((p,i) <- literal.fields.zipWithIndex) {
        if (!groundVar.contains(p)) {
          val newStmt = p match {
            case v: Variable => GroundVar(v, literal.relation, i)
            case c: Constant => Empty()
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
        val condition: Condition = _getCondition(groundedParams, head)
        Search(head.relation, condition, Statement.makeSeq(declareNewVars, nextStatements))
      }
    }
  }

  def getTrigger(statement: Statement): Set[Trigger] = statement match {
    case _: Empty => Set()
    case _ :GroundVar => Set()
    case Seq(a,b) => getTrigger(a) ++ getTrigger(b)
    case If(_,s) => getTrigger(s)
    case On(_,s) => getTrigger(s)
    case Insert(lit) => Set(InsertTuple(lit.relation))
    case Search(_, _, stmt) => getTrigger(stmt)
    case Increment(rel,keys,vid,delta) => ???
  }
}
