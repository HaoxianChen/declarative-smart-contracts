package imp

import datalog._
import imp.SolidityTranslator.transactionRelationPrefix
import view.View

/** Generate imperative program from Datalog rules
 * */

abstract class AbstractImperativeTranslator(program: Program, isInstrument: Boolean, monitorViolations: Boolean) {
  protected val primaryKeyIndices: Map[Relation, List[Int]] = program.relations.map {
    case rel: SimpleRelation => rel -> program.relationIndices.getOrElse(rel, List())
    case rel: SingletonRelation => rel -> List()
    case rel: ReservedRelation => rel -> List()
  }.toMap

  protected val views: Map[Rule, View] = program.rules.toList.zipWithIndex.map {
    case (r, i) => (r -> View(r, primaryKeyIndices(r.head.relation), i, primaryKeyIndices, program.functions))
  }.toMap

  protected val rulesToEvaluate: Set[Rule] = getRulesToEvaluate()

  protected def getRulesToEvaluate() : Set[Rule] = {
    val targetRelations: Set[Relation] = {
      val _v = if (isInstrument) program.violations else Set()
      val sendRelation = program.relations.filter(_ == Send())
      val transactionRules = program.rules.filter(_.body.exists(_.relation.name.startsWith(transactionRelationPrefix)))
      val viewRelations = program.interfaces.map(_.relation)
      viewRelations ++ transactionRules.map(_.head.relation) ++ _v  ++ sendRelation
    }
    var toEvaluate: Set[Rule] = Set()
    var _nextTargetRelations: Set[Relation] = targetRelations
    while (_nextTargetRelations.nonEmpty) {
      val _dependentRules = program.rules.filter(r => _nextTargetRelations.contains(r.head.relation))
      _nextTargetRelations = _dependentRules
        .flatMap(r=>r.body.map(_.relation)++r.aggregators.map(_.relation))
        .diff(toEvaluate.map(_.head.relation))
      toEvaluate ++= _dependentRules
    }
    if (monitorViolations) {
      toEvaluate
    }
    else {
      toEvaluate.filterNot(r => program.violations.contains(r.head.relation))
    }
  }

  protected def getTrigger(statement: Statement): Set[Trigger] = statement match {
    case _:Empty | _:GroundVar | _:imp.Assign | _:ReadTuple | _:SolidityStatement | _:Query => Set()
    case Seq(a,b) => getTrigger(a) ++ getTrigger(b)
    case If(_,s) => getTrigger(s)
    case o: OnStatement => getTrigger(o.statement)
    case Search(_, _, stmt) => getTrigger(stmt)
    case UpdateDependentRelations(update) => getTrigger(update)
    case IncrementAndInsert(increment) => Set(InsertTuple(increment.relation, primaryKeyIndices(increment.relation)))
    case Insert(lit) => Set(InsertTuple(lit.relation, primaryKeyIndices(lit.relation)))
    case Delete(lit) => Set(DeleteTuple(lit.relation, primaryKeyIndices(lit.relation)))
    case DeleteByKeys(rel,keys, updateTarget) => Set(ReplacedByKey(rel, primaryKeyIndices(rel), updateTarget))
    case Increment(rel,lit,keys,vid,delta) => Set(IncrementValue(rel,keys,vid,delta))
  }

  protected def getTriggeredRules(trigger: Trigger): Set[Rule] = {
    def isTransactionRule(rule: Rule): Boolean = rule.body.exists(_.relation.name.startsWith(transactionRelationPrefix))
    def isTransactionTrigger(trigger: Trigger): Boolean = trigger.relation.name.startsWith(transactionRelationPrefix)

    // val triggeredRules: Set[Rule] = program.rules.filter(
    val triggeredRules: Set[Rule] = rulesToEvaluate.filter(
      r => r.body.map(_.relation).contains(trigger.relation) || r.aggregators.exists(_.relation==trigger.relation)
    ).filterNot( /** transaction rules are only triggered by new transaction.  */
      r => isTransactionRule(r) && !isTransactionTrigger(trigger)
    ).filterNot( /** relations that declared as functions are not triggered */
      r=>program.functions.contains(r.head.relation)
    )

    trigger match {
      case ReplacedByKey(_, _, targetRelation) => triggeredRules.filter(_.head.relation==targetRelation)
      case _:InsertTuple|_:DeleteTuple|_:IncrementValue => triggeredRules
    }
  }


}

case class ImperativeTranslator(program: Program, isInstrument: Boolean, monitorViolations: Boolean)
    extends AbstractImperativeTranslator(program, isInstrument, monitorViolations: Boolean) {
  def ruleSize: Int = rulesToEvaluate.size

  def translate(): ImperativeAbstractProgram = {
    var triggers: Set[Trigger] = {
      val relationsToTrigger = program.interfaces.map(_.relation).filter(r => r.name.startsWith(transactionRelationPrefix))
      val relationsInBodies = program.rules.flatMap(_.body).map(_.relation)
      val toTrigger = relationsInBodies.intersect(relationsToTrigger)
      toTrigger.map(rel => InsertTuple(rel,primaryKeyIndices(rel)))
    }
    var dependencies: Set[(Relation, Relation)] = Set()

    var triggered: Set[Trigger] = Set()
    var allUpdates: Set[OnStatement] = Set()
    var hasUpdate: Boolean = true
    while (hasUpdate) {
      hasUpdate = false
      for (trigger <- triggers) {

        val triggeredRules: Set[Rule] = getTriggeredRules(trigger)

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
          assert(nextTriggers.size <= 2)
          /** todo:Check no recursion */
          // assert(triggered.intersect(nextTriggers).isEmpty, s"$rule\n$nextTriggers\n$triggered")
          triggers ++= nextTriggers
        }

        triggered += trigger
        triggers -= trigger
      }
    }
    val constructors: Set[OnStatement] = program.relations.find(_.name=="constructor") match {
        case Some(constructorRel) => {
          val (constuctorDefinition, dependentRelations) = getConstructor(constructorRel, program.rules)
          for (r <- dependentRelations) dependencies += Tuple2(constructorRel, r)
          constuctorDefinition
        }
        case None => Set()
      }
    // val statements = Statement.makeSeq((constructor::allUpdates.toList):_*)
    val dependencyMap: Map[Relation, Set[Relation]] = {
      dependencies.groupBy(_._1).map{
        case (k,v) => k -> v.map(_._2)
      }
    }

    val queryDefs = program.functions.map(getQueryDef)

    /** todo: check recursions on the dependency map */
    ImperativeAbstractProgram(program.name, program.relations, program.relationIndices,
      constructors++allUpdates,
      queryDefs,
      dependencyMap, program.rules)
  }

  private def getQueryDef(relation: Relation): Query = {
    require(program.functions.contains(relation))

    val defRules = program.rules.filter(_.head.relation==relation)

    val ruleStatements = Statement.makeSeq(
      defRules.map(r => views(r).getQueryStatement()).toSeq:_*
    )
    val statement = Statement.makeSeq(ruleStatements,Return(Constant.CFalse))

    Query(defRules.head.head, statement)
  }

  private def getConstructor(constructorRel: Relation, rules: Set[Rule]): (Set[OnStatement], Set[Relation]) = {
    val dependentRules = rules.filter(_.body.exists(_.relation.name=="constructor"))
    val allUpdates = dependentRules.map {
      r => views(r).getUpdateStatement(InsertTuple(constructorRel, primaryKeyIndices(constructorRel)))
    }
    (allUpdates, dependentRules.map(_.head.relation))
  }

}
