package verification

import com.microsoft.z3.{BoolExpr, Context, Expr, Sort, Status}
import datalog.{Program, Relation, ReservedRelation, Rule, SimpleRelation, SingletonRelation, Type}
import imp.SolidityTranslator.transactionRelationPrefix
import verification.Prove.prove
import verification.TransitionSystem.makeStateVar
import verification.Z3Helper.{getSort, literalToConst, paramToConst}
import Verifier.{_getDefaultConstraints, simplifyByRenamingConst}

case class InvariantGenerator(ctx: Context, program: Program,
                              materializedRelations: Set[Relation],
                              indices: Map[SimpleRelation, List[Int]],
                              _getInitConstraints: (Relation, Expr[Sort]) => (BoolExpr, Array[Expr[_]], Array[Type]),
                              debug: Boolean=false) {

  private val predicateExtractor = PredicateExtractor(program.rules, indices, program.functions)

  private def _refuteInvariant(inv: BoolExpr, candidates: Set[BoolExpr], tr: TransitionSystem): Boolean = {
    val f = ctx.mkImplies(ctx.mkAnd((tr.getTr() +: candidates.toArray):_*), tr.toPost(inv))
    val (res,_) = prove(ctx, f)
    res != Status.UNSATISFIABLE
  }

  private def getIndices(relation: Relation): List[Int] = relation match {
    case sr:SimpleRelation => indices.getOrElse(sr, List())
    case SingletonRelation(name, sig, memberNames) => List()
    case relation: ReservedRelation => List()
  }

  def findInvariant(tr: TransitionSystem, propertyRule: Rule): Option[BoolExpr] = {
    val prefix = "i"
    val candidates = generateCandidateInvariants(ctx, propertyRule, prefix)
    if (debug) {
      println(s"${candidates.size} candidate invariants.")
    }

    // debug
    // val lemma: Set[BoolExpr] = {
    //   val addrSort = ctx.mkBitVecSort(addressSize)
    //   val uintSort = ctx.mkBitVecSort(uintSize)
    //   val p = ctx.mkConst("p", addrSort)
    //   val n = ctx.mkConst("n", uintSort)
    //   val balance = ctx.mkConst("balance", ctx.mkArraySort(addrSort, uintSort))
    //   val highestBidTupleSort = ctx.mkTupleSort(ctx.mkSymbol("highestBidTuple"),
    //                           Array(ctx.mkSymbol("bidder"), ctx.mkSymbol("amount")), Array(addrSort, uintSort))
    //   val highestBid = ctx.mkConst("highestBid", highestBidTupleSort)
    //   val withdrawCount = ctx.mkConst("withdrawCount", ctx.mkArraySort(addrSort, uintSort))
    //   val premise = ctx.mkNot(ctx.mkEq(ctx.mkSelect(withdrawCount, p), ctx.mkBV(0,uintSize)))
    //   val _l1 = ctx.mkForall(Array(p), ctx.mkImplies(
    //     ctx.mkAnd(premise, ctx.mkEq(p, highestBidTupleSort.getFieldDecls.apply(0).apply(highestBid))),
    //     ctx.mkEq(ctx.mkSelect(balance, p), highestBidTupleSort.getFieldDecls.apply(1).apply(highestBid))
    //     ),
    //     1, null, null, ctx.mkSymbol("Q1"), ctx.mkSymbol("skid1"))

    //   val _l2 = ctx.mkForall(Array(p), ctx.mkImplies(
    //     ctx.mkAnd(premise, ctx.mkNot(ctx.mkEq(p, highestBidTupleSort.getFieldDecls.apply(0).apply(highestBid)))),
    //     ctx.mkEq(ctx.mkSelect(balance, p), ctx.mkBV(0,uintSize))
    //     ),
    //     1, null, null, ctx.mkSymbol("Q1"), ctx.mkSymbol("skid1"))

    //   val end = ctx.mkConst("end", ctx.mkBoolSort())
    //   val _l3 = ctx.mkForall(Array(p), ctx.mkImplies(
    //     ctx.mkNot(ctx.mkEq(ctx.mkSelect(withdrawCount, p), ctx.mkBV(0,uintSize))),
    //     end),
    //     1, null, null, ctx.mkSymbol("Q2"), ctx.mkSymbol("skid2"))
    //   Set(_l1,_l2, _l3)
    // }
    // val inv = findInvariant(tr, lemma)
    // val inv = findInvariant(tr, candidates ++ lemma)
    val inv = findInvariant(tr, candidates)
    inv
  }


  private def findInvariant(tr: TransitionSystem, candidates: Set[BoolExpr]): Option[BoolExpr] = {

    if (debug) {
      println(s"${candidates.size} candidate invariants remain.")
    }
    if (candidates.isEmpty) return None
    for (inv <- candidates) {
      if (_refuteInvariant(inv, candidates, tr)) {
        return findInvariant(tr, candidates - inv)
      }
    }
    Some(ctx.mkAnd(candidates.toArray:_*))
  }

  private def generateCandidateInvariants(ctx: Context, propertyRule: Rule, prefix: String,
                                         ): Set[BoolExpr] = {
    val transactionRules: Set[Rule] = program.rules.filter(r => r.body.exists(_.relation.name.startsWith(transactionRelationPrefix)))

    var candidates: Set[BoolExpr] = Set()

    def getPremisesFromInitConstraint(): (Array[BoolExpr], Array[Expr[_]], Array[Type]) = {
      var initConditions: Array[BoolExpr] = Array()
      var _keyConsts: Array[Expr[_]] = Array()
      var _keyTypes: Array[Type] = Array()
      for (rel <- materializedRelations.intersect(propertyRule.body.map(_.relation))) {
        val sort = getSort(ctx, rel, getIndices(rel))
        val (v_in, _) = makeStateVar(ctx, rel.name, sort)

        // val (_init, _keys, _kts) = _getDefaultConstraints(ctx,rel, v_in, indices, isQuantified = false)
        val (_init, _keys, _kts) = _getInitConstraints(rel, v_in)
        initConditions :+= _init
        _keyConsts ++= _keys
        _keyTypes ++= _kts
      }
      // (ctx.mkNot(ctx.mkAnd(initConditions:_*)), _keyConsts, _keyTypes)
      (initConditions.map(i => ctx.mkNot(i)), _keyConsts, _keyTypes)
    }

    for (rule <- transactionRules) {
      val _preds = predicateExtractor.extractPredicates(ctx,rule,prefix)
      val predicates: Set[BoolExpr] = _preds.map(
        p=>simplifyByRenamingConst(p,constOnly = false).simplify().asInstanceOf[BoolExpr])

      val (premises, keyConsts, keyTypes) = getPremisesFromInitConstraint()

      val premisesFromProperty = {
        val _lits = propertyRule.body.filter(_.relation.isInstanceOf[SingletonRelation])
        _lits.map(_lit=>literalToConst(ctx, _lit, List(), prefix=""))
      }

      /** Conjunct the premise with predicates form rules */
      val predicatesOnKeys: Set[BoolExpr] = {
        val _preds = keyTypes.flatMap(kt =>
          predicateExtractor.extractPredicates(ctx, rule, kt, prefix)).toSet
        _preds.map(p=>simplifyByRenamingConst(p,constOnly = false).simplify().asInstanceOf[BoolExpr])
      }

      val conditionalPremises: Set[BoolExpr] = {
        // predicatesOnKeys.map(p => ctx.mkAnd(premise, p))
        predicatesOnKeys.flatMap(
          p => premises.map(i=> ctx.mkAnd(i, p)))
      }

      /** Bind the key variable to predicates */
      var from: Array[Expr[_]] = Array()
      var to: Array[Expr[_]] = Array()
      for ((k,t) <- keyConsts.zip(keyTypes)) {
        val params = rule.body.flatMap(_.fields).filter(_._type == t)
        for (p <- params) {
          val (_const, _) = paramToConst(ctx,p, prefix)
          from :+= _const
          to :+= k
        }
      }

      /** Generate invariants in the form of implications */
      // for (eachPremise <- conditionalPremises+premise) {
      // for (eachPremise <- conditionalPremises++premises++premisesFromProperty) {
      for (eachPremise <- predicatesOnKeys++premises++premisesFromProperty) {
        for (eachPred <- predicates) {
          val conclusion = ctx.mkNot(eachPred).simplify()
          val inv = if (keyConsts.nonEmpty) {
            val renamedPremise = eachPremise.substitute(from, to)
            val renamedConclusion = conclusion.substitute(from, to)
            ctx.mkForall(
              keyConsts,
              ctx.mkImplies(renamedPremise, renamedConclusion),
              1, null, null, ctx.mkSymbol(s"Qinv${candidates.size}"), ctx.mkSymbol(s"skidinv${candidates.size}")
            )
          }
          else {
            ctx.mkImplies(eachPremise, conclusion)
          }
          candidates += inv
        }
      }
    }
    candidates
  }

}
