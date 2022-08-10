package view

import com.microsoft.z3.{ArithSort, ArraySort, BitVecSort, BoolExpr, Context, Expr, Sort, TupleSort}
import datalog.{AnyType, BooleanType, CompoundType, Literal, Max, NumberType, Param, Parameter, Relation, Rule, SymbolType, UnitType, Variable}
import imp.{DeleteTuple, GroundVar, If, IncrementValue, Insert, InsertTuple, OnInsert, OnStatement, ReadTuple, ReplacedByKey, Statement, Trigger}
import verification.TransitionSystem.makeStateVar
import verification.Z3Helper.{getArraySort, getSort, paramToConst}

case class MaxView(rule: Rule, primaryKeyIndices: List[Int], ruleId: Int) extends View {
  require(rule.aggregators.size==1)
  require(rule.aggregators.head.isInstanceOf[Max])
  val max: Max = rule.aggregators.head.asInstanceOf[Max]
  val relValueIndex: Int = rule.head.fields.indexOf(max.aggResult)

  /** Interfaces */
  def insertRow(insertTuple: InsertTuple): OnStatement = {
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
    OnInsert(literal = insertedLiteral, updateTarget = rule.head.relation, statement = stmt, ruleId)
  }

  def deleteRow(deleteTuple: DeleteTuple): OnStatement = ???

  def updateRow(incrementValue: IncrementValue): OnStatement = ???

  protected def getInsertedLiteral(relation: Relation): Literal = {
    val lit = rule.body.head
    require(lit.relation==relation)
    lit
  }

  /** Interfaces to genreate Z3 constraints */
  def getNextTriggers(trigger: Trigger): Set[Trigger] = trigger match {
    case InsertTuple(relation, keyIndices) => Set(InsertTuple(this.relation, this.primaryKeyIndices))
    case DeleteTuple(relation, keyIndices) => ???
    case ReplacedByKey(relation, keyIndices, targetRelation) => ???
    case IncrementValue(relation, keyIndices, valueIndex, delta) => ???
  }

  /** Interfaces to generate Z3 constraints */
  def insertRowZ3(ctx: Context, insertTuple: InsertTuple, isMaterialized: Boolean, z3Prefix: String):
      (BoolExpr, BoolExpr, Array[(Expr[Sort], Expr[Sort], Expr[_ <: Sort])]) = {
    val insertedLiteral: Literal = this.max.literal
    require(insertTuple.relation == insertedLiteral.relation)
    val newValueParam: Parameter = insertedLiteral.fields(max.valueIndex)
    /** Read the old value */
    val oldValue: Expr[_] = oldValueZ3Const(ctx, z3Prefix)
    val (newValue, _) = paramToConst(ctx, newValueParam, prefix = z3Prefix)
    val bodyConstraint = newValueParam._type.name match {
      case "int" => ctx.mkGt(newValue.asInstanceOf[Expr[ArithSort]], oldValue.asInstanceOf[Expr[ArithSort]])
      case "uint" => ctx.mkBVSGT(newValue.asInstanceOf[Expr[BitVecSort]], oldValue.asInstanceOf[Expr[BitVecSort]])
    }

    val updateExprs: Array[(Expr[Sort], Expr[Sort], Expr[_<:Sort])] = if(isMaterialized) {
      updateZ3ConstraintOnInsert(ctx, this.rule.head, z3Prefix)
    } else Array()
    val updateConstraint: BoolExpr = ctx.mkTrue()

    (bodyConstraint, updateConstraint, updateExprs)
  }

  private def oldValueZ3Const(ctx: Context, z3Prefix: String): Expr[_] = {
    val groupKeys: List[Parameter] = {
      val allKeys = max.literal.fields.filterNot(_==max.aggParam).filterNot(_.name=="_")
      rule.head.fields.intersect(allKeys)
    }
    val sort = getSort(ctx, this.relation, this.primaryKeyIndices)
    val relConst = ctx.mkConst(this.relation.name, sort)
    val oldValue: Expr[_] = if (groupKeys.isEmpty) {
      relConst
    }
    else {
      val keyConstArray: Array[Expr[_]] = groupKeys.toArray.map(f => paramToConst(ctx, f, z3Prefix)._1)
      ctx.mkSelect(relConst.asInstanceOf[Expr[ArraySort[Sort,Sort]]], keyConstArray)
    }
    /** This relation only has one row */
    val valueIndices = this.relation.sig.indices.filterNot(i => primaryKeyIndices.contains(i))
    if (valueIndices.size==1) {
      oldValue
    }
    else {
      val (_,_,rangeSort) = getArraySort(ctx, this.relation, primaryKeyIndices)
      val valueParams = valueIndices.map(i=>this.relation.memberNames(i))
      val i = valueParams.indexOf(this.relation.memberNames(this.relValueIndex))
      rangeSort.asInstanceOf[TupleSort].getFieldDecls.apply(i).apply(oldValue)
    }
  }

  def updateRowZ3(ctx: Context, incrementValue: IncrementValue, isMaterialized: Boolean, z3Prefix: String): (BoolExpr, BoolExpr, Array[(Expr[Sort], Expr[Sort], Expr[_ <: Sort])]) = ???
}
