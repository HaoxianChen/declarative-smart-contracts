package imp

import datalog._
import imp.SolidityTranslator.transactionRelationPrefix
import imp.Translator.getMaterializedRelations
import view.View
import viewMaterializer.BaseViewMaterializer

abstract class Translator(program: ImperativeAbstractProgram, interfaces: Set[Interface],
                          violations: Set[Relation], isInstrument: Boolean) {

  protected val relations: Set[Relation] = program.relations
  protected val indices: Map[SimpleRelation, List[Int]] = program.indices

}

object Translator {
  val viewMaterializer = new BaseViewMaterializer()
  def getMaterializedRelations(program: ImperativeAbstractProgram, interfaces: Set[Interface]): Set[Relation] = {
    viewMaterializer.getMaterializedRelations(program,interfaces)
  }

}

case class SolidityTranslator(program: ImperativeAbstractProgram, interfaces: Set[Interface],
                              violations: Set[Relation],
                              _materializedRelations: Set[Relation],
                              isInstrument: Boolean,
                              monitorViolation: Boolean,
                              enableProjection: Boolean)
      extends Translator(program, interfaces, violations, monitorViolation) {
  val name: String = program.name
  private val eventHelper = EventHelper(program.rules)
  private val payableRelations: Set[Relation] = {
    val payableRules = program.rules.filter(_.body.exists(_.relation==MsgValue()))
    payableRules.flatMap{
      case rule => rule.body.map(_.relation).filter(_.name.startsWith(transactionRelationPrefix))
    }
  }
  private val simplifier = new Simplifier()
  private val dataStructureHelper: Map[Relation, DataStructureHelper] = relations.map{
    case rel: SimpleRelation => {
      /** todo: handle situation with no indices */
      rel -> DataStructureHelper(rel, indices.getOrElse(rel, List()),enableProjection)
    }
    case rel @ (_:SingletonRelation|_:ReservedRelation) => rel -> DataStructureHelper(rel, List(),enableProjection)
  }.toMap
  private val materializedRelations: Set[Relation] = {
    val sendRelation = program.relations.filter(_ == Send())
    val _v = if (monitorViolation) violations else Set()
    if (_materializedRelations.nonEmpty) {
      _materializedRelations ++ _v ++ sendRelation
    }
    else {
      getMaterializedRelations(program,interfaces) ++ _v ++ sendRelation
    }
  }
  private val functionHelpers: Map[OnStatement,FunctionHelper] = program.onStatements.map(
    on=>on->FunctionHelper(on)).toMap
  private val dependentFunctions: Map[Relation, Set[FunctionHelper]] = functionHelpers.values.toSet.groupBy(_.inRel)

  private val violationHelper = ViolationHelper(violations, program.indices)

  private val tupleTypes :Map[Relation, Type] = {
    relations.filterNot(_.name.startsWith(transactionRelationPrefix)).diff(Relation.reservedRelations)
      .map(rel => rel -> getStructType(rel)).toMap
  }

  private def getStructName(relation: Relation): String = s"${relation.name.capitalize}Tuple"

  private def getStructType(relation: Relation) = dataStructureHelper(relation).valueType

  def translate(): Statement = {
    val structDefinitions: Statement = makeStructDefinitions()
    val declarations: Statement = getRelationDeclarations()
    val eventDeclarations = eventHelper.getAllEventDeclarations()
    val interfaces: Statement = makeInterfaces()
    val functions = {
      val decls = program.onStatements.map(on => functionHelpers(on).getFunctionDeclaration())
      val queryDecls = program.queryDefs.map(queryToDecl)
      val translatedDecls = (decls++queryDecls).map(translateStatement)
      val updateFunctions = getUpdateFunctionDeclarations()
        // .map(flattenIfStatement)
      Statement.makeSeq((translatedDecls++updateFunctions).toList:_*)
    }
    val checkViolations = if (monitorViolation) {
      val _all = violations.map(violationHelper.getViolationCheckingFunction)
      val declModifier = violationHelper.getViolationCheckingModifier()
      Statement.makeSeq(_all.toList:+declModifier:_*)
    }
    else Empty()
    val definitions = Statement.makeSeq(structDefinitions, declarations, eventDeclarations, interfaces, checkViolations, functions)
    val simplified = simplifier.simplify(definitions)
    DeclContract(name, simplified)
  }

  private def queryToDecl(query: Query): DeclFunction = {
    DeclFunction(query.relation.name, query.relation.paramList, BooleanType(), query.statement,
        FunctionMetaData(Publicity.Private, isView = true, isTransaction = false,
        modifiers = Set()))

  }

  private def getUpdateFunctionDeclarations(): Set[DeclFunction] = {
    /** todo: make this more dynamic */
    val all = program.onStatements.flatMap {
      case _:OnInsert | _:OnDelete => None
      case onIncrement: OnIncrement => {
        val xType = onIncrement.relation.sig(onIncrement.updateIndex)
        val deltaType = View.getDeltaType(xType)
        Some(DataStructureHelper.updateFunctionDecl(xType,deltaType))
      }
    }
    all + DataStructureHelper.updateFunctionDecl(Type.uintType, Type.integerType)
  }

  private def getRelationDeclarations(): Statement = {
    var stmt: Statement = Empty()
    for (rel <- materializedRelations) {
      rel match {
        case _: SingletonRelation => {
          val declRelation = DeclVariable(rel.name, getStructType(rel))
          stmt = Statement.makeSeq(stmt, declRelation)
        }
        case sr: SimpleRelation => {
          val mapType = dataStructureHelper(sr)._type
          val declRelation = DeclVariable(rel.name, mapType)
          stmt = Statement.makeSeq(stmt, declRelation)
        }
        case _: ReservedRelation => Empty()
      }
    }
    stmt = if (monitorViolation) {
      Statement.makeSeq(stmt, violationHelper.getViolationKeyArrayDecl())
    }
    else {
      stmt

    }
    stmt
  }

  private def makeStructDefinitions(): Statement = {
    val tupleStructDefs  = tupleTypes.map{
      case (rel, _type)=> _type match {
        case st: StructType => if (materializedRelations.contains(rel)) DefineStruct(getStructName(rel), st) else Empty()
        case _ => Empty()
      }
    }.toList
    val violationKeyStructDefs = if (monitorViolation) {
      violationHelper.getViolationKeyStructTypes().map {
        case st: StructType => DefineStruct(st.name, st)
        case _ => Empty()
      }.toList
    }
    else {
      List()
    }
    Statement.makeSeq((tupleStructDefs++violationKeyStructDefs):_*)
  }

  /** Translate abstract imperative program into Solidity statements */
  private def translateStatement(statement: Statement): Statement = statement match {
    case s: Search => translateStatement(dataStructureHelper(s.relation).translateSearchStatement(s))
    case If(condition, statement) => If(condition,translateStatement(statement))
    case Seq(a,b) => Seq(translateStatement(a), translateStatement(b))
    // case o: OnStatement => translateStatement(translateOnStatement(o))
    case o: OnStatement => throw new Exception(s"Cannot translate OnStatement:\n$o")
    case DeclFunction(name,lit,target,stmt, publicity) => DeclFunction(name,lit,target,translateStatement(stmt), publicity)
    case u: UpdateStatement => translateUpdateStatement(u)
    case UpdateDependentRelations(u) => getCallDependentFunctionsStatement(u)
    case query: Query => ???
    case _:Empty|_:imp.Assign|_:GroundVar|_:ReadTuple|_:SolidityStatement => statement
  }

  private def _flattenIfStatement(ifStatement: If): Statement = {
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

  private def flattenIfStatement(statement: Statement): Statement = statement match {
    case Seq(a, b) => Seq(flattenIfStatement(a),flattenIfStatement(b))
    case i: If => flattenIfStatement(_flattenIfStatement(i))
    case DeclFunction(name,params,returnType,stmt,metaData) => if (metaData.isTransaction) {
      DeclFunction(name,params,returnType,flattenIfStatement(stmt),metaData)
      } else {
      DeclFunction(name,params,returnType,stmt,metaData)
      }
    case o @ (_:Empty|_:GroundVar|_:imp.Assign|_:OnStatement|_:UpdateStatement|_:UpdateDependentRelations|_:Search|_:SolidityStatement) => o
  }


  private def translateUpdateStatement(update: UpdateStatement): Statement = {
    val dsHelper = dataStructureHelper(update.relation)
    val isInsertKey = violations.contains(update.relation)
    val newUpdates = if (materializedRelations.contains(update.relation)) {
      dsHelper.getUpdateStatement(update, isInsertKey)
    }
    else {
      Empty()
    }
    /** Emit events */
    val emitEvent = eventHelper.emitEvent(update)
    val callDependentFunctions = getCallDependentFunctionsStatement(update)
    Statement.makeSeq(callDependentFunctions, newUpdates, emitEvent)
  }

  private def getCallDependentFunctionsStatement(update: UpdateStatement): Statement = {
    val dsHelper = dataStructureHelper(update.relation)
    dependentFunctions.get(update.relation) match {
      case Some(dependents) => dsHelper.callDependentFunctions(update, dependents)
      case None => Empty()
    }
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
        val ret = Return(outputVar)
        iface.relation match {
          case _rel:SimpleRelation => if (indices.contains(_rel)) {
            val keys = indices(_rel).map(i=> Variable(_rel.sig(i), _rel.memberNames(i)))
            val groundVar = GroundVar(outputVar, rel, keys, outIndex, enableProjection)
            val readTuple = if (!enableProjection) ReadTuple(_rel, keys) else Empty()
            Statement.makeSeq(readTuple, groundVar,ret)
          }
          else {
            throw new Exception(s"Do not support simple relation without indices: ${rel}")
          }
          case _:SingletonRelation => {
            val groundVar = GroundVar(outputVar, rel, List(), outIndex, enableProjection)
            Statement.makeSeq(groundVar,ret)
          }
          case _:ReservedRelation => throw new Exception(s"Do not support interface on reserved relation: $rel")
        }
      }
      DeclFunction(funcName, params, returnType = iface.returnType, statement,
        metaData=FunctionMetaData(Publicity.Public, isView = true, isTransaction = false,
          modifiers = Set()))
    }
    def _declTxFunction(iface: Interface): DeclFunction = {
      val funcName: String = {
        val relName = iface.relation.name
        relName.substring(transactionRelationPrefix.length, relName.length)
      }
      val params: List[Parameter] = interfaceRelationToParams(iface.relation)
      var statement: Statement = Empty()
      var returnVars: Set[Variable] = Set()
      for (fh <- dependentFunctions.getOrElse(iface.relation, Set())) {
        val callStatement: Call = fh.getCallStatementFromInterface(params)
        if (callStatement.optReturnVar.isDefined) returnVars += callStatement.optReturnVar.get
        statement = Statement.makeSeq(statement,callStatement)
      }

      val checkResults = {
        val allConditions = returnVars.map(v => Match(Param(v), Param(Constant(BooleanType(),"false"))))
        val condition = Condition.makeConjunction(allConditions.toList:_*)
        If(condition, Revert("Rule condition failed"))
      }
      statement = Statement.makeSeq(statement, checkResults)

      var modifiers: Set[String] = Set()
      if (monitorViolation) modifiers += ViolationHelper.violationCheckingFunctionName
      if (payableRelations.contains(iface.relation)) modifiers += "payable"

      DeclFunction(funcName, params, returnType = iface.returnType, statement,
        metaData=FunctionMetaData(Publicity.Public, isView = false, isTransaction = true,
          modifiers = modifiers)
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
    program.relations.find(_.name=="constructor") match {
      case Some(rel) => {
        val params = interfaceRelationToParams(rel)
        /** Relevant update functions */
        var statement: Statement = Empty()
        for (fh <- dependentFunctions.getOrElse(rel, Set())) {
          statement = Statement.makeSeq(statement, fh.getCallStatementFromInterface(params))
        }
        Constructor(params = params, statement = statement)
      }
      case None => Empty()
    }
  }
}

object SolidityTranslator {
  val transactionRelationPrefix = "recv_"
}