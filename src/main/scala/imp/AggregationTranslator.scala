package imp

import datalog.{Arithmetic, Literal, Max, Param, Parameter, Rule, Sum, Variable}

case class AggregationTranslator() {
  def translateSumRule(rule: Rule, sum: Sum): OnInsert = {
    require(rule.aggregators.size == 1)
    require(rule.aggregators.contains(sum))
    require(rule.body.size<=1 && rule.body.forall(_.relation==sum.relation))
    require(rule.head.fields.contains(sum.aggResult))

    val insertedLiteral = sum.literal
    val resultIndex = rule.head.fields.indexOf(sum.aggResult)
    val keyIndices = rule.head.fields.indices.toList.filterNot(_==resultIndex)
    val delta: Arithmetic = Param(sum.aggParam)
    val increment =  Increment(rule.head.relation, rule.head, keyIndices,resultIndex, delta = delta)
    OnInsert(insertedLiteral, rule.head.relation, increment)
  }

  def translateMaxRule(rule: Rule, max: Max): OnInsert = {
    require(rule.aggregators.contains(max))
    require(rule.body.size<=1 && rule.body.forall(_.relation==max.relation),
      s"Unspported aggregation rule $rule, " +
        s"Only support rules that perform groupBy and projection on a single relation.")
    val insertedLiteral: Literal = rule.body.head
    val newValue: Param = Param(insertedLiteral.fields(max.valueIndex))
    val groupKeys: List[Parameter] = {
      val allKeys = max.literal.fields.filterNot(_==max.aggParam).filterNot(_.name=="_")
      rule.head.fields.intersect(allKeys)
    }
    val readTuple: ReadTuple = ReadTuple(rule.head.relation, groupKeys)
    val oldValue: Param = Param(Variable(max.aggResult._type,"_max"))
    val groundVar: GroundVar = {
      val valueIndexInHead: Int = rule.head.fields.indexOf(max.aggResult)
      GroundVar(oldValue.p,rule.head.relation,valueIndexInHead)
    }
    val condition = imp.Greater(newValue,oldValue)
    val insert: Insert = Insert(rule.head)
    val stmt = Statement.makeSeq(readTuple, groundVar, If(condition, insert))
    OnInsert(literal = insertedLiteral, updateTarget = rule.head.relation, statement = stmt)
  }
}
