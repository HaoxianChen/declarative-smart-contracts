package imp

import datalog._
import view.View

/** Generate imperative program from Datalog rules
 * */
case class ImperativeTranslator(program: Program) {
  private val transactionPrefix = "recv_"

  private val views: Map[Rule, View] = program.rules.map(r => (r->View(r))).toMap

  def translate(): ImperativeAbstractProgram = {
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
    var allUpdates: Set[Statement] = Set()
    var hasUpdate: Boolean = true
    while (hasUpdate) {
      hasUpdate = false
      for (trigger <- triggers) {

        val triggeredRules: Set[Rule] = program.rules.filter(
          r => r.body.map(_.relation).contains(trigger.relation) || r.aggregators.exists(_.relation==trigger.relation)
        ).filterNot( /** transaction rules are only triggered by new transaction.  */
          r => isTransactionRule(r) && !isTransactionTrigger(trigger)
        )

        for (rule <- triggeredRules) {
          val updateProgram = views(rule).getUpdateStatement(trigger)
          if (!allUpdates.contains(updateProgram)) {
            hasUpdate=true
            allUpdates += updateProgram
          }

          val allNextTriggers = getTrigger(updateProgram)
          /** Update dependencies */
          for (nt <- allNextTriggers) {
            dependencies += Tuple2(trigger.relation, nt.relation)
          }
          val nextTriggers = allNextTriggers
            // .filterNot(t => program.interfaces.map(_.relation).contains(t.relation))
          assert(nextTriggers.size <= 1)
          /** todo:Check no recursion */
          // assert(triggered.intersect(nextTriggers).isEmpty, s"$rule\n$nextTriggers\n$triggered")
          triggers ++= nextTriggers
        }

        triggered += trigger
        triggers -= trigger
      }
    }
    val constructor: Statement = program.relations.find(_.name=="constructor") match {
        case Some(constructorRel) => {
          val (constuctorDefinition, dependentRelations) = getConstructor(constructorRel, program.rules)
          for (r <- dependentRelations) dependencies += Tuple2(constructorRel, r)
          constuctorDefinition
        }
        case None => Empty()
      }
    val statements = Statement.makeSeq((constructor::allUpdates.toList):_*)
    val dependencyMap: Map[Relation, Set[Relation]] = {
      dependencies.groupBy(_._1).map{
        case (k,v) => k -> v.map(_._2)
      }
    }
    /** todo: check recursions on the dependency map */
    ImperativeAbstractProgram(program.name, program.relations, program.relationIndices, statements, dependencyMap)
  }

  private def getConstructor(constructorRel: Relation, rules: Set[Rule]): (Statement, Set[Relation]) = {
    val dependentRules = rules.filter(_.body.exists(_.relation.name=="constructor"))
    val allUpdates = dependentRules.map {
      r => views(r).getUpdateStatement(InsertTuple(constructorRel))
    }
    (Statement.makeSeq(allUpdates.toList:_*), dependentRules.map(_.head.relation))
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
