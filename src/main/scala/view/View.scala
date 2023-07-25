package view

import com.microsoft.z3.{ArithExpr, ArithSort, ArraySort, BitVecSort, BoolExpr, Context, Expr, Sort, TupleSort}
import datalog.Arithmetic.updateArithmeticType
import datalog._
import imp._
import verification.RuleZ3Constraints
import verification.TransitionSystem.makeStateVar
import verification.Z3Helper.{fieldsToConst, functorExprToZ3, getSort, literalToConst, matchFieldstoTuple, paramToConst, typeToSort}

abstract class View {
  def rule: Rule
  def primaryKeyIndices: List[Int]
  def ruleId: Int
  val relation: Relation = rule.head.relation

  /** Interfaces */
  def insertRow(insertTuple: InsertTuple): OnStatement
  def deleteRow(deleteTuple: DeleteTuple): OnStatement
  def updateRow(incrementValue: IncrementValue): OnStatement

  def getQueryStatement(): Statement

  def getUpdateStatement(trigger: Trigger): OnStatement = trigger match {
    case it: InsertTuple => insertRow(it)
    case dt: DeleteTuple => deleteRow(dt)
    case iv: IncrementValue => updateRow(iv)
    case ReplacedByKey(relation, keyIndices, targetRelation) => {
      require(targetRelation==this.relation)
      deleteRow(DeleteTuple(relation,keyIndices))
    }
  }

  /** Interfaces to generate Z3 constraints */
  def insertRowZ3(ctx: Context, insertTuple: InsertTuple, isMaterialized: Boolean, z3Prefix: String)
                 :(RuleZ3Constraints, RuleZ3Constraints)

  def updateRowZ3(ctx: Context, incrementValue: IncrementValue, isMaterialized: Boolean, z3Prefix: String)
    :(RuleZ3Constraints, RuleZ3Constraints)

  def deleteRowZ3(ctx: Context, deleteTuple: DeleteTuple, isMaterialized: Boolean, z3Prefix: String)
    :(RuleZ3Constraints, RuleZ3Constraints)

  def getZ3Constraint(ctx: Context, trigger: Trigger, isMaterialized: Boolean, z3Prefix: String):
      (RuleZ3Constraints, RuleZ3Constraints) = {
    /** Return two branch, one is rule body evaluates to true, the other is false */
    trigger match {
      case it: InsertTuple => insertRowZ3(ctx, it, isMaterialized, z3Prefix)
      case dt: DeleteTuple => deleteRowZ3(ctx, dt, isMaterialized, z3Prefix)
      case ReplacedByKey(relation, keyIndices, targetRelation) => ???
      case ic: IncrementValue => updateRowZ3(ctx, ic, isMaterialized, z3Prefix)
    }
  }

  def getZ3QueryConstraint(ctx: Context, z3Prefix: String): BoolExpr

  def getInsertedLiteral(relation: Relation): Literal

  protected def makeRuleZ3Constraints(ctx: Context, bodyConstraint: BoolExpr, updateConstraint: BoolExpr,
                                      updateExprs: Array[(Expr[Sort], Expr[Sort], Expr[_<:Sort])],
                                      nextTrigger: Trigger):
  (RuleZ3Constraints, RuleZ3Constraints) = {
    val trueBranch = RuleZ3Constraints(bodyConstraint, updateConstraint, updateExprs,
      Set(nextTrigger))
    val falseBranch = {
      val _updateExprs = updateExprs.map(t => Tuple3(t._1, t._2, t._1.asInstanceOf[Expr[_<:Sort]]))
      RuleZ3Constraints(ctx.mkNot(bodyConstraint), ctx.mkTrue(), _updateExprs, Set())
    }
    (trueBranch, falseBranch)
  }

  def isDeleteBeforeInsert(relation: Relation, keyIndices: List[Int]): Boolean = {
    /** Conditions to delete a head tuple before inserting a new tuple P(X):
     * 1. The relation P has index, or is a singleton relation:
     *        this means P(X) may overwritten an old entry of P.
     * todo: 2. If the new derived head tuple H'(Y) has the *same keys* as the old tuple H(Y),
     *  skip deletion. Because H(Y) will be implicitly overwritten by H'(Y).
     * */
    val insertedLiteral = getInsertedLiteral(relation)
    val insertedLiteralKeys = keyIndices.map(i=>insertedLiteral.fields(i)).toSet
    val headLiteralKeys = this.primaryKeyIndices.map(i=>this.rule.head.fields(i)).toSet

    val isOverwritten = headLiteralKeys.subsetOf(insertedLiteralKeys) && rule.functors.isEmpty

    // keyIndices.nonEmpty || relation.isInstanceOf[SingletonRelation]
    // (keyIndices.nonEmpty && !headLiteralKeys.subsetOf(insertedLiteralKeys)) || relation.isInstanceOf[SingletonRelation]
    keyIndices.nonEmpty && !isOverwritten
  }

  def getTriggersForView(trigger: Trigger): Array[Trigger] = trigger match {
    case InsertTuple(relation, keyIndices) => if (isDeleteBeforeInsert(relation, keyIndices)) {
      Array(DeleteTuple(relation, keyIndices), InsertTuple(relation, keyIndices))
    }
    else {
      Array(InsertTuple(relation, keyIndices))
    }
    case _ => Array(trigger)
  }

  protected def deleteByKeysStatement(literal: Literal, keyIndices: List[Int]): Statement = {
      val keys = keyIndices.map(i=>literal.fields(i))
      DeleteByKeys(literal.relation, keys, updateTarget = this.relation)
  }

  protected def updateTargetRelationZ3(ctx: Context, insertedLiteral: Literal, delta: Arithmetic, resultIndex: Int,
                                       isMaterialized: Boolean,
                                       z3Prefix: String): (BoolExpr, Array[(Expr[Sort], Expr[Sort], Expr[_<:Sort])]) = {

    val keys = primaryKeyIndices.map(i=>rule.head.fields(i))
    val valueType = this.relation.sig(resultIndex)
    val deltaz3 = functorExprToZ3(ctx, updateArithmeticType(delta, valueType), z3Prefix)
    val (diffConst,_) = {
      val _y = this.rule.head.fields(resultIndex)
      paramToConst(ctx, _y, z3Prefix)
    }
    /** todo: the diffConst should have a better name, now it is the same as the param in rule head.
     * H(x,s) = p(x), s = sum y: p(x,y).
     * now the diff eq is: s = y.
     * should be ds = y.
     * But then need to add different constraints on the naming across dependent rules.
     * */
    val diffEq = ctx.mkEq(diffConst, deltaz3)

    val sort = getSort(ctx, this.relation, primaryKeyIndices)
    val (v_in, v_out) = makeStateVar(ctx, relation.name, sort)

    val updateExpr = if (primaryKeyIndices.nonEmpty) {
      val keyConstArray: Array[Expr[_]] = keys.toArray.map(f => paramToConst(ctx, f, z3Prefix)._1)
      val valueConst = ctx.mkSelect(v_in.asInstanceOf[Expr[ArraySort[Sort, Sort]]], keyConstArray)
      val newValue = ctx.mkAdd(valueConst.asInstanceOf[Expr[ArithSort]], diffConst.asInstanceOf[Expr[ArithSort]])
      ctx.mkStore(v_in.asInstanceOf[Expr[ArraySort[Sort, Sort]]], keyConstArray, newValue.asInstanceOf[Expr[Sort]])
    }
    else {
      assert(this.relation.isInstanceOf[SingletonRelation] || this.relation.isInstanceOf[ReservedRelation])
      ctx.mkAdd(v_in.asInstanceOf[Expr[ArithSort]], diffConst.asInstanceOf[Expr[ArithSort]])
    }

    if (isMaterialized) (diffEq, Array(Tuple3(v_in, v_out, updateExpr)))
    else (diffEq, Array())
  }

  protected def updateZ3ConstraintOnInsert(ctx: Context, head: Literal, z3Prefix: String):
  Array[(Expr[Sort], Expr[Sort], Expr[_<:Sort])] = {
    val sort = getSort(ctx, this.relation, primaryKeyIndices)
    val (v_in, v_out) = makeStateVar(ctx, relation.name, sort)
    val newValueExpr = relation match {
      case SimpleRelation(name, sig, memberNames) => {
        val keys = primaryKeyIndices.map(i=>head.fields(i))
        val keyConstArray: Array[Expr[_]] = keys.toArray.map(f => paramToConst(ctx, f, z3Prefix)._1)
        val valueParams: List[Parameter] = head.fields.filterNot(f => keys.contains(f))
        val valueIndices = sig.indices.filterNot(i=>primaryKeyIndices.contains(i)).toList
        val fieldNames = valueIndices.map(i=>memberNames(i))
        val newValueConst: Expr[_] = fieldsToConst(ctx, relation, valueParams, fieldNames, z3Prefix)._1
        ctx.mkStore(v_in.asInstanceOf[Expr[ArraySort[Sort, Sort]]], keyConstArray,
          newValueConst.asInstanceOf[Expr[Sort]])
      }
      case SingletonRelation(name, sig, memberNames) => {
        /** todo: should use all fields, instead of only the first field */
        if (sig.size==1) {
          paramToConst(ctx,head.fields.head, z3Prefix)._1
        }
        else {
          val tupleSort: TupleSort = getSort(ctx, relation, primaryKeyIndices).asInstanceOf[TupleSort]
          val paramConst = head.fields.toArray.map(f => paramToConst(ctx,f,z3Prefix)._1)
          tupleSort.mkDecl().apply(paramConst:_*)
        }
      }
      case relation: ReservedRelation => {
        assert(false)
        ???
      }
    }
    Array(Tuple3(v_in, v_out,newValueExpr))
  }

}
object View {
  def apply(rule: Rule, primaryKeyIndices: List[Int], ruleId: Int, allIndices: Map[Relation, List[Int]],
            functions: Set[Relation], arithmeticOptimization: Boolean, enableProjection: Boolean): View = {
    require(rule.aggregators.size <= 1)
    if (rule.aggregators.isEmpty) {
      JoinView(rule, primaryKeyIndices, ruleId, allIndices, functions, arithmeticOptimization,
        enableProjection=enableProjection)
    }
    else {
      rule.aggregators.head match {
        case _:Sum => SumView(rule, primaryKeyIndices, ruleId)
        case _:Max => MaxView(rule, primaryKeyIndices, ruleId, enableProjection)
        case _:Count => CountView(rule, primaryKeyIndices, ruleId)
      }
    }
  }

  def getDeltaType(t: Type): Type = t match {
      case NumberType(name) => {
        if (name == "uint") Type.integerType
        else if (name == "int") Type.integerType
        else {
          ???
        }
      }
      case _ => ???
    }
}

