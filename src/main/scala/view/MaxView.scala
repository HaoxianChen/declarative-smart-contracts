package view

import com.microsoft.z3.{ArithSort, ArraySort, BitVecSort, BoolExpr, Context, Expr, Sort, TupleSort}
import datalog.{AnyType, BooleanType, CompoundType, Literal, Max, NumberType, Param, Parameter, Relation, Rule, SymbolType, UnitType, Variable}
import imp.{DeleteTuple, Empty, GroundVar, If, IncrementValue, Insert, InsertTuple, OnInsert, OnStatement, ReadTuple, ReplacedByKey, Statement, Trigger}
import verification.RuleZ3Constraints
import verification.TransitionSystem.makeStateVar
import verification.Z3Helper.{getArraySort, getSort, makeTupleSort, paramToConst}

case class MaxView(rule: Rule, primaryKeyIndices: List[Int], ruleId: Int, enableProjection:Boolean) extends View {
  require(rule.aggregators.size==1)
  require(rule.aggregators.head.isInstanceOf[Max])
  val max: Max = rule.aggregators.head.asInstanceOf[Max]
  val relValueIndex: Int = rule.head.fields.indexOf(max.aggResult)

  /** Interfaces */
  def insertRow(insertTuple: InsertTuple): OnStatement = {
    val insertedLiteral: Literal = getInsertedLiteral(insertTuple.relation)
    val newValue: Param = Param(insertedLiteral.fields(max.valueIndex))
    val groupKeys: List[Parameter] = {
      val allKeys = max.literal.fields.filterNot(_==max.aggParam).filterNot(_.name=="_")
      rule.head.fields.intersect(allKeys)
    }
    val readTuple = if (!enableProjection) ReadTuple(rule.head.relation, groupKeys) else Empty()
    val oldValue: Param = Param(Variable(max.aggResult._type,"_max"))
    val groundVar: GroundVar = {
      val valueIndexInHead: Int = rule.head.fields.indexOf(max.aggResult)
      GroundVar(oldValue.p,rule.head.relation,groupKeys,valueIndexInHead,enableProjection)
    }
    val condition = imp.Greater(newValue,oldValue)
    val insert: Insert = Insert(rule.head)
    val stmt = Statement.makeSeq(readTuple, groundVar, If(condition, insert))
    OnInsert(literal = insertedLiteral, updateTarget = rule.head.relation, statement = stmt, ruleId)
  }

  def deleteRow(deleteTuple: DeleteTuple): OnStatement = ???

  def updateRow(incrementValue: IncrementValue): OnStatement = ???

  def getInsertedLiteral(relation: Relation): Literal = {
    require(relation==max.relation)
    if (rule.body.nonEmpty) {
      val _lits = rule.body.filter(_.relation == max.relation)
      require(_lits.size==1)
      _lits.head
    } else {
      max.literal
    }
  }

  /** Interfaces to generate Z3 constraints */
  def insertRowZ3(ctx: Context, insertTuple: InsertTuple, isMaterialized: Boolean, z3Prefix: String) = {
    val insertedLiteral: Literal = this.max.literal
    require(insertTuple.relation == insertedLiteral.relation)
    val newValueParam: Parameter = insertedLiteral.fields(max.valueIndex)
    /** Read the old value */
    val oldValue: Expr[_] = oldValueZ3Const(ctx, z3Prefix)
    val (newValue, _) = paramToConst(ctx, newValueParam, prefix = z3Prefix)
    val bodyConstraint = ctx.mkGt(newValue.asInstanceOf[Expr[ArithSort]], oldValue.asInstanceOf[Expr[ArithSort]])

    val updateExprs: Array[(Expr[Sort], Expr[Sort], Expr[_<:Sort])] = if(isMaterialized) {
      updateZ3ConstraintOnInsert(ctx, this.rule.head, z3Prefix)
    } else Array()
    val updateConstraint: BoolExpr = ctx.mkTrue()

    makeRuleZ3Constraints(ctx, bodyConstraint, updateConstraint, updateExprs,
      InsertTuple(this.relation, this.primaryKeyIndices))
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
      //  val (_,_,rangeSort) = getArraySort(ctx, this.relation, primaryKeyIndices)
      val valueTypes = valueIndices.map(i=>relation.sig(i))
      val fieldNames = valueIndices.map(i=>relation.memberNames(i))
      val rangeSort = makeTupleSort(ctx, relation, valueTypes.toArray, fieldNames.toArray)
      val valueParams = valueIndices.map(i=>this.relation.memberNames(i))
      val i = valueParams.indexOf(this.relation.memberNames(this.relValueIndex))
      rangeSort.asInstanceOf[TupleSort].getFieldDecls.apply(i).apply(oldValue)
    }
  }

  def updateRowZ3(ctx: Context, incrementValue: IncrementValue, isMaterialized: Boolean, z3Prefix: String) = ???

  def deleteRowZ3(ctx: Context, deleteTuple: DeleteTuple, isMaterialized: Boolean, z3Prefix: String) = ???

  def getQueryStatement(): Statement = ???

  def getZ3QueryConstraint(ctx: Context, z3Prefix: String): BoolExpr = ???
}
