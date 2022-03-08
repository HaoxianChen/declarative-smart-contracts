package imp

import datalog.{Relation, Rule}

case class EventHelper(rules: Set[Rule]) {
  private val relationsToEmit: Set[Relation] = {
    val txRules = rules.filter(isTransactionRule)
    txRules.map(_.head.relation)
  }
  def eventName(relation: Relation): String = relation.name.capitalize
  private def isTransactionRule(rule: Rule): Boolean = rule.body.exists(
    _.relation.name.startsWith(SolidityTranslator.transactionRelationPrefix))

  def getAllEventDeclarations(): Statement = {
    val decls = relationsToEmit.map(rel => DeclEvent(eventName(rel), rel.paramList))
    Statement.makeSeq(decls.toList:_*)
  }

  def emitEvent(update: UpdateStatement): Statement = update match {
    case Insert(literal) => {
      val rel = literal.relation
      if (relationsToEmit.contains(rel)) {
        Emit(eventName(rel), literal.fields)
      }
      else {
        Empty()
      }
    }
    case _:Delete => Empty()
    case _:DeleteByKeys => Empty()
    case _:Increment => Empty()
    case _:IncrementAndInsert => Empty()
  }
}
