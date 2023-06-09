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

  protected val simplifier = new Simplifier()

  protected val views: Map[Rule, View] = program.rules.toList.zipWithIndex.map {
    case (r, i) => (r -> View(r, primaryKeyIndices(r.head.relation), i, primaryKeyIndices, program.functions))
  }.toMap

  protected def isTransactionRule(rule: Rule): Boolean = {
    rule.body.exists(_.relation.name.startsWith(transactionRelationPrefix))
  }

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


  protected def getQueryDef(relation: Relation): Query = {
    require(program.functions.contains(relation))

    val defRules = program.rules.filter(_.head.relation==relation)

    val ruleStatements = Statement.makeSeq(
      defRules.map(r => views(r).getQueryStatement()).toSeq:_*
    )
    val statement = Statement.makeSeq(ruleStatements,Return(Constant.CFalse))

    Query(defRules.head.head, statement)
  }

  protected def getConstructor(constructorRel: Relation, rules: Set[Rule]): (Set[OnStatement], Set[Relation]) = {
    val dependentRules = rules.filter(_.body.exists(_.relation.name=="constructor"))
    val allUpdates = dependentRules.map {
      r => views(r).getUpdateStatement(InsertTuple(constructorRel, primaryKeyIndices(constructorRel)))
    }
    (allUpdates, dependentRules.map(_.head.relation))
  }

  def getRelationDependencies(): Set[(Relation, Relation, Int, Boolean, Boolean)] = {
    /** Each 5Tuple represent one relation dependency (body, head, ruleID, isAggregationRule, isTransactionRule) */
    def getDependencies(rule: Rule): Set[((Relation, Relation, Int, Boolean, Boolean))] = {
      val isTx = isTransactionRule(rule)
      val isAgg = rule.aggregators.nonEmpty
      val ruleId = views(rule).ruleId
      val fromBody = rule.body.filterNot(_.relation.isInstanceOf[ReservedRelation])
                               .filterNot(_.relation.name.contains(transactionRelationPrefix))
                               .map(lit => Tuple5(lit.relation, rule.head.relation, ruleId, isAgg, isTx))
      val fromAggregator = rule.aggregators.map(agg =>
                              Tuple5(agg.literal.relation, rule.head.relation, ruleId, isAgg, isTx))
      fromBody++fromAggregator
    }
    program.rules.filterNot(_.body.exists(_.relation.name=="constructor")).flatMap(getDependencies)

  }
}

class ImperativeTranslator(program: Program, isInstrument: Boolean, monitorViolations: Boolean)
    extends AbstractImperativeTranslator(program, isInstrument, monitorViolations: Boolean) {
  def ruleSize: Int = rulesToEvaluate.size

  def translate(): ImperativeAbstractProgram = {
    var triggers: Set[Trigger] = {
      val relationsToTrigger = program.interfaces.map(_.relation).filter(r => r.name.startsWith(transactionRelationPrefix))
      val relationsInBodies = program.rules.flatMap(_.body).map(_.relation)
      val toTrigger = relationsInBodies.intersect(relationsToTrigger)
      toTrigger.map(rel => InsertTuple(rel,primaryKeyIndices(rel)))
    }

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
          constuctorDefinition
        }
        case None => Set()
      }
    // val statements = Statement.makeSeq((constructor::allUpdates.toList):_*)

    val queryDefs = program.functions.map(getQueryDef)

    /** todo: check recursions on the dependency map */
    ImperativeAbstractProgram(program.name, program.relations, program.relationIndices,
      constructors++allUpdates,
      queryDefs,
      program.rules)
  }

}

case class ImperativeTranslatorWithUpdateFusion(program: Program, isInstrument: Boolean, monitorViolations: Boolean)
    extends ImperativeTranslator(program, isInstrument, monitorViolations) {

  override def translate(): ImperativeAbstractProgram = {
    val triggers: Set[Trigger] = {
      val relationsToTrigger = program.interfaces.map(_.relation).filter(r => r.name.startsWith(transactionRelationPrefix))
      val relationsInBodies = program.rules.flatMap(_.body).map(_.relation)
      val toTrigger = relationsInBodies.intersect(relationsToTrigger)
      toTrigger.map(rel => InsertTuple(rel,primaryKeyIndices(rel)))
    }
    var statements: List[OnStatement] = List()
    for (t <- triggers) {
      val triggeredRules: Set[Rule] = getTriggeredRules(t)
      for (r <- triggeredRules) {
        val update = getUpdate(t,r)
        val simplified = simplifier.simplify(update).asInstanceOf[OnStatement]
        statements +:= simplified
      }
    }

    val queryDefs = program.functions.map(getQueryDef)

    val constructors: Set[OnStatement] = program.relations.find(_.name=="constructor") match {
      case Some(constructorRel) => {
        val (constuctorDefinition, _) = getConstructor(constructorRel, program.rules)
        constuctorDefinition
      }
      case None => Set()
    }

    /** todo: check recursions on the dependency map */
    ImperativeAbstractProgram(program.name, program.relations, program.relationIndices,
      constructors++statements,
      queryDefs,
      program.rules)
  }

  def getUpdate(trigger: Trigger, rule: Rule): OnStatement = {
    val updateProgram = views(rule).getUpdateStatement(trigger)
    val nextTriggers = getTrigger(updateProgram)
    var updateDependents: List[Statement] = List()
    for (nt <- nextTriggers) {
      val triggeredRules: Set[Rule] = getTriggeredRules(nt)
      for (nr <- triggeredRules) {
        val nextUpdates = getUpdate(nt, nr)
        val renamed = renameUpdate(nt, rule.head, nr, nextUpdates)
        updateDependents :+= renamed
      }
    }

    /** Replace the statement with the update dependent statements. */
    val fused = replaceUpdateStatement(updateProgram.statement, Statement.makeSeq(updateDependents:_*))

    trigger match {
      case InsertTuple(relation, keyIndices) => OnInsert(updateProgram.literal, updateProgram.updateTarget, fused,
        ruleId = views(rule).ruleId)
      case DeleteTuple(relation, keyIndices) => OnDelete(updateProgram.literal,updateProgram.updateTarget, fused,
        ruleId = views(rule).ruleId)
      case ReplacedByKey(relation, keyIndices, targetRelation) => {
        OnDelete(updateProgram.literal, updateProgram.updateTarget, fused,
          ruleId = views(rule).ruleId)
      }
      case IncrementValue(relation, keyIndices, valueIndex, delta) => OnIncrement(updateProgram.literal, keyIndices,
        valueIndex, updateTarget = updateProgram.updateTarget, fused, ruleId = views(rule).ruleId)
    }
  }

  private def renameUpdate(trigger: Trigger, literal: Literal, rule: Rule, statement: OnStatement): Statement = {
    val updatedLiteral = views(rule).getInsertedLiteral(trigger.relation)
    trigger match {
      case InsertTuple(relation, keyIndices) => {
        var mapping: Map[Parameter, Parameter] = Map()
        for ((from,to) <- updatedLiteral.fields.zip(literal.fields)) {
          mapping += from -> to
        }
        val renamed = renameParameters(statement.statement,mapping)
        renamed
      }
      case DeleteTuple(relation, keyIndices) => ???
      case ReplacedByKey(relation, keyIndices, targetRelation) => {
        /** todo: Fix this on benchmark NFT. */
        statement.statement
        ???
      }
      case IncrementValue(relation, keyIndices, valueIndex, delta) => {
        var mapping: Map[Parameter, Parameter] = Map()
        for (i<-keyIndices) {
          val from = updatedLiteral.fields(i)
          val to = literal.fields(i)
          mapping += from->to
        }
        val renamed = renameParameters(statement.statement,mapping)
        val renamed2 = replaceArithmetic(renamed, Map(Param(updatedLiteral.fields(valueIndex)) -> delta))
        renamed2
      }
    }
  }

  def replaceArithmetic(statement: Statement, mapping: Map[Param, Arithmetic]): Statement = statement match {
    case s: Empty  => s
    case g: GroundVar => g
    case imp.Assign(p, expr) => expr match {
      case a: Arithmetic => imp.Assign(p, Arithmetic.replace(a,mapping))
    }
    case Seq(a, b) => Seq(replaceArithmetic(a,mapping),replaceArithmetic(b,mapping))
    case If(condition, _statement) => If(Condition.replaceArithmetic(condition,mapping),
                                          replaceArithmetic(_statement,mapping))
    case statement: OnStatement => ???
    case Query(literal, statement) => ???
    case _update: UpdateStatement => _update match {
      case _:Insert|_:Delete|_:DeleteByKeys => _update
      case IncrementAndInsert(increment) => ???
      case Increment(relation, literal, keyIndices, valueIndex, delta) =>
        Increment(relation,literal, keyIndices, valueIndex, Arithmetic.replace(delta,mapping))
    }
    case UpdateDependentRelations(update) => ???
    case Search(relation, conditions, _statement) => {
      val newConds = conditions.map{
        case m: MatchRelationField => Condition.replaceArithmetic(m,mapping).asInstanceOf[MatchRelationField]
        case _ => ???
      }
      Search(relation, newConds, replaceArithmetic(_statement,mapping))
    }
    case statement: SolidityStatement => ???
  }

  def renameParameters(statement: Statement, mapping: Map[Parameter, Parameter]): Statement = statement match {
    case Empty() => Empty()
    case GroundVar(p, relation, index) => GroundVar(mapping.getOrElse(p,p), relation, index)
    case imp.Assign(p, expr) => {
      val newP = Param(mapping.getOrElse(p.p, p.p))
      imp.Assign(newP, Arithmetic.rename(expr, mapping))
    }
    case Seq(a, b) => Seq(renameParameters(a,mapping), renameParameters(b,mapping))
    case If(condition, statement) => If(Condition.rename(condition,mapping), renameParameters(statement, mapping))
    case statement: OnStatement => ???
    case Query(literal, statement) => ???
    case statement: UpdateStatement => statement match {
      case Insert(literal) => Insert(literal.rename(mapping))
      case Delete(literal) => Delete(literal.rename(mapping))
      case DeleteByKeys(relation, keys, updateTarget) => {
        val newKeys = keys.map(k=>mapping.getOrElse(k,k))
        DeleteByKeys(relation, newKeys, updateTarget)
      }
      case IncrementAndInsert(increment) => {
        /** todo: implement this for benchmark voting.dl */
        ???
      }
      case Increment(relation, literal, keyIndices, valueIndex, delta) => Increment(relation, literal.rename(mapping),
        keyIndices, valueIndex, delta)
    }
    case UpdateDependentRelations(update) => UpdateDependentRelations(
      renameParameters(update,mapping).asInstanceOf[UpdateStatement])
    case Search(relation, conditions, statement) => {
      val newConds = conditions.map(c=>Condition.rename(c,mapping).asInstanceOf[MatchRelationField])
      Search(relation, newConds, renameParameters(statement,mapping))
    }
    case solidityStatement: SolidityStatement => solidityStatement match {
      case ReadTuple(relation, keyList, outputVar) => {
        val newKeys = keyList.map(k=>mapping.getOrElse(k,k))
        ReadTuple(relation, newKeys, outputVar)
      }
      case _ => {
        ???
      }
    }
  }

  def replaceUpdateStatement(statement: Statement, updates: Statement): Statement = statement match {
    case _:Empty | _:GroundVar | _:imp.Assign => statement
    case Seq(a, b) => Seq(replaceUpdateStatement(a,updates), replaceUpdateStatement(b,updates))
    case If(condition, _statement) => If(condition, replaceUpdateStatement(_statement, updates))
    case statement: OnStatement => ???
    case Query(literal, statement) => ???
    case _update: UpdateStatement => Statement.makeSeq(_update, updates)
    case UpdateDependentRelations(update) => ???
    case Search(relation, conditions, _statement) => Search(relation, conditions,
                                                        replaceUpdateStatement(_statement,updates))
    case _statement: SolidityStatement => _statement
  }
}