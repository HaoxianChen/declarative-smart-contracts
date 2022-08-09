package verification

import com.microsoft.z3.{ArrayExpr, BoolExpr, Context, Expr, IntSort, Sort, TupleSort}
import datalog.{Constant, Program, Relation, ReservedRelation, Rule, SimpleRelation, SingletonRelation, Type, Variable}
import imp.SolidityTranslator.transactionRelationPrefix
import imp.Translator.getMaterializedRelations
import imp.{AbstractImperativeTranslator, DeleteTuple, ImperativeAbstractProgram, IncrementValue, InsertTuple, ReplacedByKey, Trigger}
import util.Misc.crossJoin
import verification.TransitionSystem.makeStateVar
import verification.Z3Helper.{addressSize, extractEq, functorToZ3, getSort, literalToConst, makeTupleSort, paramToConst, typeToSort, uintSize}
import view.{CountView, JoinView, MaxView, SumView, View}

case class RuleZ3Constraints(ruleConstraints: BoolExpr,
                             updateConstraint: BoolExpr,
                             updateExprs: Array[(Expr[Sort], Expr[Sort], Expr[_<:Sort])]) {
  def allConstraints(ctx: Context): BoolExpr = ctx.mkAnd(ruleConstraints, updateConstraint)
}

class Verifier(program: Program, impAbsProgram: ImperativeAbstractProgram)
  extends AbstractImperativeTranslator(program, isInstrument = true) {

  private val ctx: Context = new Context()

  protected val relations: Set[Relation] = program.relations
  protected val indices: Map[SimpleRelation, List[Int]] = impAbsProgram.indices

  private val materializedRelations: Set[Relation] = getMaterializedRelations(impAbsProgram, program.interfaces)
       .filterNot(_.isInstanceOf[ReservedRelation])
  override val rulesToEvaluate: Set[Rule] = getRulesToEvaluate().filterNot(r => program.violations.contains(r.head.relation))

  val stateVars: Set[(Expr[_], Expr[_])] = materializedRelations.map(rel => {
    val sort = getSort(ctx, rel, getIndices(rel))
    makeStateVar(ctx, rel.name, sort)
  })

  private def getIndices(relation: Relation): List[Int] = relation match {
    case sr:SimpleRelation => indices(sr)
    case SingletonRelation(name, sig, memberNames) => List()
    case relation: ReservedRelation => ???
  }

  def check(): Unit = {
    val violationRules: Set[Rule] = program.rules.filter(r => program.violations.contains(r.head.relation))
    val tr = TransitionSystem(program.name, ctx)

    var initConditions: List[BoolExpr] = List()
    for (rel <- materializedRelations) {
      val sort = getSort(ctx, rel, getIndices(rel))
      val (v_in, _) = tr.newVar(rel.name, sort)
      initConditions :+= getInitConstraints(rel, v_in)
    }
    tr.setInit(ctx.mkAnd(initConditions.toArray:_*))

    val transitionConditions = getTransitionConstraints()
    tr.setTr(transitionConditions)

    for (vr <- violationRules) {
      val property = getProperty(ctx, vr)
      val (resInit, resTr) = tr.inductiveProve(ctx, property)
      println(property)
      println(s"Init: $resInit")
      println(s"Tr: $resTr")
    }
  }

  private def initValue(_type: Type): Expr[_<:Sort] = _type.name match {
    case "address" => ctx.mkBV(0, addressSize)
    case "int" => ctx.mkInt(0)
    case "uint" => ctx.mkBV(0, uintSize)
    case "bool" => ctx.mkBool(false)
  }

  private def getInitConstraints(relation: Relation, const: Expr[Sort]): BoolExpr = relation match {
    case sr: SimpleRelation => {
      val keyTypes: List[Type] = indices(sr).map(i=>relation.sig(i))
      val valueTypes: List[Type] = relation.sig.filterNot(t => keyTypes.contains(t))
      val initValues = if (valueTypes.size == 1) {
        initValue(valueTypes.head)
      }
      else {
        val _initValues = valueTypes.toArray.map(initValue)
        val tupleSort = makeTupleSort(ctx, s"${sr.name}Tuple", valueTypes.toArray, relation.memberNames.toArray)
        tupleSort.mkDecl().apply(_initValues:_*)
      }

      val keyConstArray: Array[Expr[_]] = keyTypes.toArray.zipWithIndex.map{
          case (t,i) => ctx.mkConst(s"${t.name.toLowerCase()}$i", typeToSort(ctx,t))
      }

      ctx.mkForall(keyConstArray, ctx.mkEq(
            ctx.mkSelect(const.asInstanceOf[ArrayExpr[Sort,Sort]], keyConstArray),
            initValues),
        1, null, null, ctx.mkSymbol(s"Q${sr.name}"), ctx.mkSymbol(s"skid${sr.name}"))
    }
    case SingletonRelation(name, sig, memberNames) => {
      if (sig.size==1) {
        ctx.mkEq(const, initValue(sig.head))
      }
      else {
        val tupleSort = makeTupleSort(ctx, name, sig.toArray, memberNames.toArray)
        val tupleConst = ctx.mkConst(name, tupleSort)
        val eqs = sig.zip(tupleSort.getFieldDecls).map {
          case (t, decl) => ctx.mkEq(decl.apply(tupleConst), initValue(t))
        }.toArray
        ctx.mkAnd(eqs: _*)
      }
    }
    case rel: ReservedRelation => ???
  }

  private def getProperty(ctx: Context, rule: Rule): BoolExpr = {
    /** Each violation query rule is translated into a property as follows:
     *  ! \E (V), P1 /\ P2 /\ ...
     *  where V is the set of variable appears in the rule body,
     *  P1, P2, ... are predicates translated from each rule body literal.
     *  */
    val prefix = "p"
    val _vars: Array[Expr[_]] = rule.body.flatMap(_.fields).toArray.flatMap(p => p match {
      case Constant(_type, name) => None
      case Variable(_type, name) => Some(paramToConst(ctx,p,prefix)._1)
    })
    val bodyConstraints = rule.body.map(lit => literalToConst(ctx, lit, getIndices(lit.relation), prefix)).toArray
    val functorConstraints = rule.functors.map(f => functorToZ3(ctx,f, prefix)).toArray
    ctx.mkNot(ctx.mkExists(
        _vars,
        ctx.mkAnd(bodyConstraints++functorConstraints:_*),
        1, null, null, ctx.mkSymbol("Q"), ctx.mkSymbol("skid2")
      )
    )
  }

  private def getTransitionConstraints(): BoolExpr = {

    val triggers: Set[Trigger] = {
      val relationsToTrigger = program.interfaces.map(_.relation).filter(r => r.name.startsWith(transactionRelationPrefix))
      val relationsInBodies = program.rules.flatMap(_.body).map(_.relation)
      val toTrigger = relationsInBodies.intersect(relationsToTrigger)
      toTrigger.map(rel => InsertTuple(rel,primaryKeyIndices(rel)))
    }

    var transactionConstraints: List[BoolExpr] = List()
    var transactionConst: List[Expr[IntSort]] = List()
    for (t <- triggers) {
      val triggeredRules: Set[Rule] = getTriggeredRules(t)
      for (rule <- triggeredRules) {
        for ((eachBranch,i) <- ruleToExpr(rule, t).zipWithIndex) {
          /** Add the "unchanged" constraints */
          val unchangedConstraints = getUnchangedConstraints(eachBranch)
          /** A boolean value indicating which transaction branch gets evaluate to true */
          val trConst = ctx.mkIntConst(s"${t.relation.name}$i")
          transactionConst +:= trConst
          transactionConstraints +:= ctx.mkAnd((ctx.mkEq(trConst, ctx.mkInt(1)) :: eachBranch :: unchangedConstraints).toArray: _*)
        }
      }
    }

    ctx.mkAnd(
      ctx.mkAnd(transactionConst.map(c => ctx.mkOr(ctx.mkEq(c,ctx.mkInt(0)), ctx.mkEq(c,ctx.mkInt(1)))).toArray:_*),
      ctx.mkEq(ctx.mkAdd(transactionConst.toArray:_*), ctx.mkInt(1)),
      ctx.mkOr(transactionConstraints.toArray:_*)
    )
  }

  private def getUnchangedConstraints(_expr: BoolExpr): List[BoolExpr] = {
    var unchangedConstraints: List[BoolExpr] = List()
    for (rel <- materializedRelations) {
      val sort = getSort(ctx, rel, getIndices(rel))
      val (v_in, v_out) = makeStateVar(ctx, rel.name, sort)
      val allVars = Prove.get_vars(_expr)
      if (!allVars.contains(v_out)) {
        unchangedConstraints :+= ctx.mkEq(v_out, v_in)
      }
    }
    unchangedConstraints
  }

  private def ruleToExpr(rule: Rule, trigger: Trigger): Array[BoolExpr] = {
    var id = 0

    def _ruleToExpr(rule: Rule, trigger: Trigger, upstreamRule: Option[Rule], upstreamId: Option[Int]):
      List[(BoolExpr, Array[(Expr[Sort], Expr[Sort], Expr[_<:Sort])])] = {
      val thisId = id
      id += 1

      val view = views(rule)
      val isMaterialized = materializedRelations.contains(rule.head.relation)
      val (trueBranch, falseBranch) = view.getZ3Constraint(ctx, trigger, isMaterialized, getPrefix(thisId))

      /** Derive dependent constraints */
      val nextTriggers = view.getNextTriggers(trigger)

      val dependentRules: List[(Trigger,Rule)] = nextTriggers.flatMap(t => getTriggeredRules(t).map(r => (t,r))).toList
      val dependentConstraints = dependentRules.map(t => _ruleToExpr(t._2, t._1, Some(rule), Some(thisId)))

      /** Merge local branch and dependent branches via cross join */
      var allBranches: List[(BoolExpr, Array[(Expr[Sort], Expr[Sort], Expr[_<:Sort])])] = List()
      for (eachBranch <- crossJoin(
                  List(Tuple2(trueBranch.allConstraints(ctx), trueBranch.updateExprs)) +: dependentConstraints)
           ) {
        val bodyConstraints: BoolExpr = ctx.mkAnd(eachBranch.map(_._1).toArray:_*)
        val updateConstraints: Array[(Expr[Sort], Expr[Sort], Expr[_<:Sort])] = eachBranch.flatMap(_._2).toArray
        allBranches +:= Tuple2(bodyConstraints, updateConstraints)
      }
      /** If this rule evaluates to false, do not involve further dependent rules. */
      if (!falseBranch.ruleConstraints.simplify().isFalse
          && thisId > 0
      ) {
        allBranches +:= Tuple2(falseBranch.allConstraints(ctx), falseBranch.updateExprs)
      }

      /** Add naming constraints  */
      def _renameConstraints(_body: BoolExpr, _updates: Array[(Expr[Sort], Expr[Sort], Expr[_<:Sort])]):
      (BoolExpr, Array[(Expr[Sort], Expr[Sort], Expr[_<:Sort])]) = {
        /** Do the renaming here. */
        val (_, from, to) = getNamingConstraints(upstreamRule.get, rule, upstreamId.get, thisId)
        val _renamed: BoolExpr = _body.substitute(from, to).asInstanceOf[BoolExpr]
        val _renamedUpdates = _updates.map(t => (t._1, t._2, t._3.substitute(from, to)))
        (_renamed, _renamedUpdates)
      }

      val renamedBranches = if (upstreamRule.isDefined) {
        allBranches.map(t => _renameConstraints(t._1, t._2))
      }
      else allBranches

      renamedBranches
    }

    var allExprs: Array[BoolExpr] = Array()
    for ((body, updates) <- _ruleToExpr(rule,trigger, None, None)) {
      val merged = mergeUpdates(updates)
      val expr = ctx.mkAnd(body+:merged:_*)
      val simplified = simplifyByRenamingConst(expr)
      allExprs +:= simplified.asInstanceOf[BoolExpr]
    }
    allExprs
  }

  private def mergeUpdates(updates: Array[(Expr[Sort], Expr[Sort], Expr[_<:Sort])]): Array[BoolExpr] = {
    /** Each tuple: (v_in, v_out, v_in + delta )
     * */
    def _mergeUpdates(_updates: Array[(Expr[Sort], Expr[Sort], Expr[_<:Sort])]): BoolExpr = {
      require(_updates.size > 1)
      val v_out = _updates.head._2
      var updateExpr = _updates(1)._3
      for (i <- 0 to _updates.size-2) {
        val from = _updates(i+1)._1
        val to = _updates(i)._3
        updateExpr = updateExpr.substitute(from,to)
      }
      ctx.mkEq(v_out, updateExpr)
    }
    updates.groupBy(_._1).map {
      case (_,v) => if (v.size > 1) {
        _mergeUpdates(v)
      } else {
        ctx.mkEq(v.head._2, v.head._3)
      }
    }.toArray
  }

  private def getNamingConstraints(rule: Rule, dependentRule: Rule, id1: Int, id2: Int): (BoolExpr, Array[Expr[_]], Array[Expr[_]]) = {
    val headLiteral = rule.head
    val bodyLiteral = views(dependentRule) match {
      case _: JoinView => {
        val _s = dependentRule.body.filter(_.relation == rule.head.relation)
        assert(_s.size==1)
        _s.head
      }
      case cv: CountView =>  cv.count.literal
      case mv: MaxView => mv.max.literal
      case sv: SumView => sv.sum.literal
      case _ => ???
    }

    var expr: List[BoolExpr] = List()
    var from: List[Expr[_]] = List()
    var to: List[Expr[_]] = List()
    for (v <- headLiteral.fields.zip(bodyLiteral.fields)) {
      if (v._1.name != "_" && v._2.name != "_") {
        val (x1,_) = paramToConst(ctx, v._1, getPrefix(id1))
        val (x2,_) = paramToConst(ctx, v._2, getPrefix(id2))
        expr +:= ctx.mkEq(x1,x2)
        to +:= x1
        from +:= x2
      }
    }
    (ctx.mkAnd(expr.toArray:_*), from.toArray, to.toArray)
  }

  private def simplifyByRenamingConst[T<:Sort](expr: Expr[T]): Expr[T] = {
    val exceptions = stateVars.flatMap(t=>Set(t._1,t._2))
    var pairs = extractEq(expr, exceptions)
    var newExpr = expr
    while (pairs.nonEmpty) {
      val renamed = newExpr.substitute(pairs.map(_._1), pairs.map(_._2))
      newExpr = renamed.simplify()
      pairs = extractEq(newExpr, exceptions)
    }
    newExpr
  }


  private def getPrefix(id: Int): String = s"i$id"

}
