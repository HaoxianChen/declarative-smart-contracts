package view

import com.microsoft.z3.{ArithExpr, ArithSort, ArraySort, BitVecSort, BoolExpr, Context, Expr, Sort}
import datalog.{AnyType, Arithmetic, BooleanType, CompoundType, Count, Literal, Negative, NumberType, One, Param, Parameter, Relation, Rule, SymbolType, UnitType, Variable}
import imp.{DeleteTuple, Empty, Increment, IncrementValue, Insert, InsertTuple, OnDelete, OnInsert, OnStatement, ReplacedByKey, Statement, Trigger}
import verification.RuleZ3Constraints
import verification.TransitionSystem.makeStateVar
import verification.Z3Helper.{getSort, paramToConst, uintSize}

case class CountView(rule: Rule, primaryKeyIndices: List[Int], ruleId: Int) extends View {
  require(rule.aggregators.size==1)
  require(rule.aggregators.head.isInstanceOf[Count])
  val count: Count = rule.aggregators.head.asInstanceOf[Count]
  val relValueIndex: Int = rule.head.fields.indexOf(count.aggResult)


  /** Interfaces */
  def insertRow(insertTuple: InsertTuple): OnStatement = {
    val insertedLiteral = getInsertedLiteral(insertTuple.relation)
    val resultIndex = rule.head.fields.indexOf(count.aggResult)
    val delta: Arithmetic = {
      val d = One(count.aggResult._type)
      // Arithmetic.updateArithmeticType(d, View.getDeltaType(d._type))
      d
    }
    val statement = {
      val delete = if (isDeleteBeforeInsert(insertTuple.relation, insertTuple.keyIndices)) {
        deleteByKeysStatement(insertedLiteral, insertTuple.keyIndices)
      }
      else {
        Empty()
      }
      val increment = Increment(rule.head.relation, rule.head, primaryKeyIndices, resultIndex, delta = delta)
      Statement.makeSeq(delete, increment)
    }
    OnInsert(insertedLiteral, rule.head.relation, statement, ruleId)
  }

  def deleteRow(deleteTuple: DeleteTuple): OnStatement = {
    val deletedLiteral = getInsertedLiteral(deleteTuple.relation)
    val resultIndex = rule.head.fields.indexOf(count.aggResult)
    val delta: Arithmetic = {
      val d = Negative(One(count.aggResult._type))
      d
    }
    val decrement = Increment(rule.head.relation, rule.head, primaryKeyIndices, resultIndex, delta = delta)
    OnDelete(deletedLiteral, rule.head.relation, statement = decrement, ruleId)
  }

  def updateRow(incrementValue: IncrementValue): OnStatement = ???

  def getInsertedLiteral(relation: Relation): Literal = {
    require(relation==count.relation)
    val memberNames = relation.memberNames
    val fields = count.literal.fields.zipWithIndex.map{
      case (p,i) => {
        val name = if (p.name == "_") s"_${memberNames(i)}$i" else p.name
        Variable(p._type,name)
      }
    }
    Literal(relation, fields)
  }

  /** Interfaces to generate Z3 constraints */
  private def getNextTrigger(trigger: Trigger): Set[Trigger] = trigger match {
    case InsertTuple(relation, keyIndices) => Set(InsertTuple(this.relation, this.primaryKeyIndices))
    case DeleteTuple(relation, keyIndices) => ???
    case ReplacedByKey(relation, keyIndices, targetRelation) => ???
    case IncrementValue(relation, keyIndices, valueIndex, delta) => ???
  }

  /** Interfaces to generate Z3 constraints */
  def insertRowZ3(ctx: Context, insertTuple: InsertTuple, isMaterialized: Boolean, z3Prefix: String) =
    _insertOrDeleteRowZ3(ctx, z3Prefix, isInsert = true)

  def updateRowZ3(ctx: Context, incrementValue: IncrementValue, isMaterialized: Boolean, z3Prefix: String) = ???

  def deleteRowZ3(ctx: Context, deleteTuple: DeleteTuple, isMaterialized: Boolean, z3Prefix: String) =
    _insertOrDeleteRowZ3(ctx, z3Prefix, isInsert = false)

  def _insertOrDeleteRowZ3(ctx: Context, z3Prefix: String, isInsert: Boolean) = {
    val bodyConstraint = ctx.mkTrue()
    val sort = getSort(ctx, this.relation, this.primaryKeyIndices)
    val (v_in, v_out) = makeStateVar(ctx, this.relation.name, sort)
    val updateExpr = {
      val relConst = ctx.mkConst(this.relation.name, sort)
      val keyParams = primaryKeyIndices.map(i=>this.rule.head.fields(i))
      val keyConsts: Array[Expr[_]] = keyParams.map(f => paramToConst(ctx,f,z3Prefix)._1).toArray
      val oldCountConst = if (primaryKeyIndices.isEmpty) {
        /** Only one row */
        relConst
      }
      else {
        /** Read by keys */
        ctx.mkSelect(relConst.asInstanceOf[Expr[ArraySort[Sort,Sort]]], keyConsts)
      }
      val newValueExpr = if (isInsert) {
        /** Increment count by one on insertion */
        ctx.mkAdd(oldCountConst.asInstanceOf[Expr[ArithSort]], ctx.mkInt(1))
      }
      else {
        /** Decrement count by one on deletion */
        ctx.mkSub(oldCountConst.asInstanceOf[Expr[ArithSort]], ctx.mkInt(1))
      }

      if (primaryKeyIndices.isEmpty) {
        newValueExpr
      }
      else {
        ctx.mkStore(relConst.asInstanceOf[Expr[ArraySort[Sort,Sort]]], keyConsts, newValueExpr.asInstanceOf[Expr[Sort]])
      }

    }
    val updateConstraint = ctx.mkTrue()
    makeRuleZ3Constraints(ctx, bodyConstraint, updateConstraint, Array(Tuple3(v_in, v_out, updateExpr)),
      InsertTuple(this.relation, this.primaryKeyIndices))
  }

  def getQueryStatement(): Statement = ???

  def getZ3QueryConstraint(ctx: Context, z3Prefix: String): BoolExpr = ???
}
