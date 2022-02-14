package imp

import datalog._

case class SolidityTranslator(program: ImperativeAbstractProgram, interfaces: Set[Interface]) {
  val name: String = program.name
  private val transactionRelationPrefix = "recv_"
  private val relations: Set[Relation] = program.relations
  private val indices: Map[SimpleRelation, Int] = program.indices
  private val materializedRelations: Set[Relation] = {
    val fromStatements = relationsToMaterialize(program.statement)
    val viewRelations = interfaces.filterNot(_.relation.name.startsWith(transactionRelationPrefix)).map(_.relation)
    fromStatements ++ viewRelations
  }
  private val dependentFunctions: Map[Relation, Set[FunctionHelper]] = {
    initFunctionHelpers(program.statement).groupBy(_.inRel)
  }

  private val tupleTypes :Map[Relation, Type] = {
    relations.filterNot(_.name.startsWith(transactionRelationPrefix))
      .map(rel => rel -> getStructType(rel)).toMap
  }

  private def initFunctionHelpers(statement: Statement): Set[FunctionHelper] = statement match {
    case Seq(a, b) => initFunctionHelpers(a) ++ initFunctionHelpers(b)
    case If(_, statement) => initFunctionHelpers(statement)
    case on: OnStatement => Set(FunctionHelper(on))
    case _:Empty|_:GroundVar|_:imp.Assign|_:UpdateStatement|_:Search|_:SolidityStatement=> Set()
  }

  private def getStructName(relation: Relation): String = s"${relation.name.capitalize}Tuple"

  private def getStructType(relation: Relation) = {
    val structName = getStructName(relation)
    val params = interfaceRelationToParams(relation)
    StructType(structName, params)
  }

  def translate(): Statement = {
    val structDefinitions: Statement = makeStructDefinitions()
    val declarations: Statement = getRelationDeclartions()
    val interfaces: Statement = makeInterfaces()
    val functions = translateStatement(program.statement)
    val definitions = Statement.makeSeq(structDefinitions, declarations, interfaces, functions)
    DeclContract(name, definitions)
  }

  private def getRelationDeclartions(): Statement = {
    var stmt: Statement = Empty()
    for (rel <- materializedRelations) {
      rel match {
        case _: SingletonRelation => {
          val declRelation = DeclRelation(rel, getStructType(rel))
          stmt = Statement.makeSeq(stmt, declRelation)
        }
        case sr: SimpleRelation => {
          val i = indices(sr)
          val mapType = MapType(rel.sig(i), tupleTypes(sr))
          val declRelation = DeclRelation(rel, mapType)
          stmt = Statement.makeSeq(stmt, declRelation)
        }
        case _: ReservedRelation => Empty()
      }
    }
    stmt
  }

  private def makeStructDefinitions(): Statement = {
    val allDefs = tupleTypes.map{
      case (rel, _type)=> _type match {
        case st: StructType => if (materializedRelations.contains(rel)) DefineStruct(getStructName(rel), st) else Empty()
        case _ => Empty()
      }
    }.toList
    Statement.makeSeq(allDefs:_*)
  }

  /** Translate abstract imperative program into Solidity statements */
  private def translateStatement(statement: Statement): Statement = statement match {
    case s: Search => translateStatement(translateSearchStatement(s))
    case i: If => translateStatement(flattenIfStatement(i))
    case Seq(a,b) => Seq(translateStatement(a), translateStatement(b))
    case o: OnStatement => translateStatement(translateOnStatement(o))
    case DeclFunction(name,lit,target,stmt, publicity) => DeclFunction(name,lit,target,translateStatement(stmt), publicity)
    case u: UpdateStatement => translateUpdateStatement(u)
    case _:Empty|_:imp.Assign|_:GroundVar|_:ReadTuple|_:SolidityStatement => statement
  }

  private def flattenIfStatement(ifStatement: If): Statement = {
    val cond = ifStatement.condition
    cond match {
      case _: True => ifStatement.statement
      case _: False => throw new Exception(s"False condition in if statement ${ifStatement}")
      case _ => {
        val req = Require(cond, s"condition $cond is false.")
        Statement.makeSeq(req, ifStatement.statement)
      }
    }
  }

  private def translateUpdateStatement(update: UpdateStatement): Statement = {
    val params: List[Parameter] = update.literal.fields
    val newUpdates = update match {
      case i: Increment => if (materializedRelations.contains(i.relation)) i else Empty()
      case ins: Insert => if (materializedRelations.contains(ins.relation)) {
        ins.relation match {
          case rel:SingletonRelation => SetTuple(rel, params)
          case rel @ (_:SimpleRelation|_:ReservedRelation) => throw new Exception(
            s"Do not support insert tuple of ${rel.getClass}: $rel")
        }
      }
      else Empty()
    }
    var stmt: Statement = newUpdates

    /** Call functions to update dependent relations */
    for (fh <- dependentFunctions.getOrElse(update.relation, Set())) {
      stmt = Statement.makeSeq(stmt, fh.getCallStatement(update))
    }
    stmt
  }

  private def translateOnStatement(on: OnStatement): Statement = {
    FunctionHelper.getFunctionDeclaration(on)
  }

  private def translateSearchStatement(search: Search): Statement = {
    search.relation match {
      case _:SingletonRelation|_:ReservedRelation => {
        val condition = search.conditions.foldLeft[Condition](True())(Condition.conjunction)
        If(condition, search.statement)
      }
      case rel: SimpleRelation => {
        val key: Parameter = {
          require(indices.contains(rel), s"$rel\n$search")
          val index = indices(rel)
          val keyConditions = search.conditions.filter(_.index==index)
          require(keyConditions.size == 1, s"$search\nkey: $keyConditions")
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

  private def relationsToMaterialize(statement: Statement): Set[Relation] = statement match {
    case ReadTuple(rel, _) => Set(rel)
    case GroundVar(_, rel, _) => Set(rel)
    case Search(_, _, stmt) => relationsToMaterialize(stmt)
    case If(_,stmt) => relationsToMaterialize(stmt)
    case Seq(a,b) => relationsToMaterialize(a) ++ relationsToMaterialize(b)
    case on: OnStatement => relationsToMaterialize(on.statement)
    case _:Empty|_:imp.Assign|_:UpdateStatement|_:SolidityStatement => Set()
  }

  private def makeInterfaces(): Statement = {
    def interfaceIO(iface: Interface): (List[Parameter], Option[Parameter]) = {
      val rel = iface.relation
      val params: List[Parameter] = {
        iface.inputIndices.map(i=>{
          val _type = rel.sig(i)
          val name = rel.memberNames(i)
          Variable(_type,name)
        })
      }
      val optOutput: Option[Parameter] = iface.optReturnIndex match {
        case Some(i) => {
          val n = rel.memberNames(i)
          val t = iface.returnType
          Some(Variable(t,n))
        }
        case None => None
      }
      (params, optOutput)
    }
    def _declInterfaceFunction(iface: Interface): DeclFunction = {
      if (iface.relation.name.startsWith(transactionRelationPrefix)) _declTxFunction(iface)
      else _declViewFunction(iface)
    }
    def _declViewFunction(iface: Interface): DeclFunction = {
      val funcName: String = s"get${iface.relation.name.capitalize}"
      val outIndex = iface.optReturnIndex.get
      val rel = iface.relation
      val (params, optOutput) = interfaceIO(iface)
      val statement: Statement = {
        val outputVar = optOutput.get
        val groundVar = GroundVar(outputVar, rel, outIndex)
        val ret = Return(outputVar)
        iface.relation match {
          case _rel:SimpleRelation => if (indices.contains(_rel)) {
            val idx = indices(_rel)
            val key = {
              val n = _rel.memberNames(idx)
              val t = _rel.sig(idx)
              Variable(t,n)
            }
            val readTuple = ReadTuple(_rel, key)
            Statement.makeSeq(readTuple,groundVar,ret)
          }
          else {
            throw new Exception(s"Do not support simple relation without indices: ${rel}")
          }
          case _:SingletonRelation => Statement.makeSeq(groundVar,ret)
          case _:ReservedRelation => throw new Exception(s"Do not support interface on reserved relation: $rel")
        }
      }
      DeclFunction(funcName, params, returnType = iface.returnType, statement,
        metaData=FunctionMetaData(Publicity.Public, true))
    }
    def _declTxFunction(iface: Interface): DeclFunction = {
      val funcName: String = {
        val relName = iface.relation.name
        relName.substring(transactionRelationPrefix.length, relName.length)
      }
      val params: List[Parameter] = interfaceRelationToParams(iface.relation)
      var statement: Statement = Empty()
      for (fh <- dependentFunctions.getOrElse(iface.relation, Set())) {
        statement = Statement.makeSeq(statement,fh.getCallStatementFromInterface(params))
      }
      DeclFunction(funcName, params, returnType = iface.returnType, statement,
        metaData=FunctionMetaData(Publicity.Public, false)
      )
    }
    val allInterfaceFunctions = interfaces.map(_declInterfaceFunction).toList
    val constructor = makeConstructor()
    Statement.makeSeq((constructor +: allInterfaceFunctions):_*)
  }

  private def interfaceRelationToParams(rel: Relation): List[Parameter] = {
    rel.sig.zip(rel.memberNames).map{ case (t,n) => Variable(t,n) }
  }

  private def makeConstructor(): Statement = {
    val constructorRel = program.relations.find(_.name=="constructor")
    require(constructorRel.isDefined, s"Constructor undefined")
    val rel = constructorRel.get
    val params = interfaceRelationToParams(rel)

    /** Relevant update functions */
    var statement: Statement = Empty()
    for (fh <- dependentFunctions.getOrElse(rel, Set())) {
      statement = Statement.makeSeq(statement,fh.getCallStatementFromInterface(params))
    }
    Constructor(params = params, statement = statement)
  }
}
