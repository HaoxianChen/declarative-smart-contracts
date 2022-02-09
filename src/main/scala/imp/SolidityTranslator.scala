package imp

import datalog.{Interface, MapType, Parameter, Relation, SimpleRelation, SingletonRelation, StructType, Type, UnitType, Variable}

case class SolidityTranslator(program: ImperativeAbstractProgram, interfaces: Set[Interface]) {
  private val relations: Set[Relation] = program.relations
  private val indices: Map[SimpleRelation, Int] = program.indices
  private val dependencies = program.dependencies

  private val tupleTypes :Map[Relation, Type] = {
    relations.filterNot(_.name.startsWith(s"recv_"))
      .map(rel => rel -> getStructType(rel)).toMap
  }

  private def getStructName(relation: Relation): String = s"${relation.name.capitalize}Tuple"

  private def getStructType(relation: Relation) = {
    val structName = getStructName(relation)
    val params = relation.sig.zip(relation.memberNames).map{
      case (t,n)=> Variable(t,n)
    }
    StructType(structName, params)
  }

  def translate(): Statement = {
    val definitions: Statement = getDefinitions()
    val declarations: Statement = getRelationDeclartions()
    val functions = translateStatement(program.statement)
    Statement.makeSeq(definitions, declarations, functions)
  }

  private def getRelationDeclartions(): Statement = {
    var stmt: Statement = Empty()
    for ((rel, i) <- indices) {
      val mapType = MapType(rel.sig(i), tupleTypes(rel))
      val declRelation = DeclRelation(rel, mapType)
      stmt = Statement.makeSeq(stmt, declRelation)
    }
    stmt
  }

  private def getDefinitions(): Statement = {
    val allDefs = tupleTypes.map{
      case (rel, _type)=> _type match {
        case st: StructType => DefineStruct(getStructName(rel), st)
        case _ => Empty()
      }
    }.toList
    Statement.makeSeq(allDefs:_*)

  }

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

  private def getFunName(src: Relation, target: Relation): String = {
    s"update${target.name.capitalize}On${src.name.capitalize}"
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
      case SingletonRelation(_, _, _) => {
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
