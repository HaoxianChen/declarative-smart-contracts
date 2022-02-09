package imp

import datalog.{Parameter, Relation, SimpleRelation, SingletonRelation, UnitType}

case class SolidityTranslator(program: ImperativeAbstractProgram) {
  private val indices: Map[SimpleRelation, Int] = program.indices
  private val dependencies = program.dependencies

  private def getFunName(src: Relation, target: Relation): String = {
    s"update${target.name.capitalize}On${src.name.capitalize}"
  }

  def translate(): Statement = translateStatement(program.statement)

  /** Translate abstract imperative program into Solidity statements */
  private def translateStatement(statement: Statement): Statement = statement match {
    case s: Search => translateStatement(translateSearchStatement(s))
    case If(cond,stmt) => If(cond, translateStatement(stmt))
    case Seq(a,b) => Seq(translateStatement(a), translateStatement(b))
    case o: OnStatement => translateStatement(translateOnStatement(o))
    case DeclFunction(name,lit,target,stmt) => DeclFunction(name,lit,target,translateStatement(stmt))
    case u: UpdateStatement => translateUpdateStatement(u)
    case _:Empty|_:Assign|_:GroundVar|_:ReadTuple|_:SolidityStatement => statement
  }

  private def translateUpdateStatement(update: UpdateStatement): Statement = {
    val increments = update match {
      case i: Increment => i
      case _: Insert => Empty()
    }
    var stmt: Statement = increments

    val params: List[Parameter] = update.literal.fields
    val targetRels = dependencies.getOrElse(update.relation, Set())
    for (rel <- targetRels) {
      val functionName = getFunName(update.relation, rel)
      stmt = Statement.makeSeq(stmt,Call(functionName, params))
    }
    stmt
  }

  private def translateOnStatement(on: OnStatement): Statement = {
    val (funcName,params) = on match {
      case OnInsert(literal, updateTarget, statement) => {
        (getFunName(literal.relation, updateTarget), literal.fields)
      }
      case OnIncrement(relation, keys, updateValue, updateTarget, statement) => {
        val ps = keys :+ updateValue.p
        (getFunName(relation, updateTarget), ps)
      }
    }
    DeclFunction(funcName, params, returnType = UnitType(), on.statement)
  }

  private def translateSearchStatement(search: Search): Statement = {
    search.relation match {
      case SingletonRelation(_, _) => {
        val condition = search.conditions.foldLeft[Condition](True())(Condition.conjunction)
        If(condition, search.statement)
      }
      case rel: SimpleRelation => {
        val key: Parameter = {
          require(indices.contains(rel), s"$rel\n$search")
          val index = indices(rel)
          val keyConditions = search.conditions.filter(_.index==index)
          require(keyConditions.size == 1)
          keyConditions.head.p
        }
        val readTuple: ReadTuple = {
          ReadTuple(rel, key)
        }
        val condition = search.conditions.filterNot(_.p==key).foldLeft[Condition](True())(Condition.conjunction)
        Statement.makeSeq(readTuple, If(condition, search.statement))
      }
    }
  }
}
