package verification

import com.microsoft.z3.{BoolExpr, Context, Expr}
import datalog.{Constant, Functor, Literal, Parameter, Relation, ReservedRelation, Rule, SimpleRelation, SingletonRelation, Type, Variable}
import Z3Helper.{functorToZ3, literalToConst, paramToConst}
import imp.SolidityTranslator.transactionRelationPrefix
import verification.Prove.get_vars

case class PredicateExtractor(rules: Set[Rule], indices: Map[SimpleRelation, List[Int]],
                              functions: Set[Relation]) {

  private def getIndices(relation: Relation): List[Int] = relation match {
    case sr:SimpleRelation => indices.getOrElse(sr, List())
    case SingletonRelation(name, sig, memberNames) => List()
    case relation: ReservedRelation => List()
  }

  def extractPredicates(ctx: Context, rule: Rule, _type: Type, prefix: String): Set[BoolExpr] = {
    val consts: Set[Expr[_]] = {
      val params = rule.body.flatMap(_.fields).filter(p => p._type==_type && p.name != "_")
      params.map(p=>paramToConst(ctx, p,prefix)._1)
    }
    val allPreds: Set[BoolExpr] = extractPredicates(ctx,rule,prefix)
    allPreds.filter(f => get_vars(f).intersect(consts).nonEmpty)
  }

  def extractPredicates(ctx: Context, rule: Rule, prefix: String): Set[BoolExpr] = {
    val candidateLiterals = rule.body.filterNot(_.relation.name.startsWith(transactionRelationPrefix)).
                              filterNot(lit=>functions.contains(lit.relation)).
                              toList
    val _p = extractMatchingPredicates(ctx, candidateLiterals, prefix)
    val _m = rule.functors.flatMap(f => extractComparisonPredicates(ctx,f,candidateLiterals,prefix))
    /** todo: should extract predicates from function relations as well */
    _p ++ _m
  }

  def extractPredicates(ctx: Context, prefix: String): Set[BoolExpr] = {
    val triggerRules = rules.filter(r => r.body.exists(_.relation.name.startsWith(transactionRelationPrefix)))
    triggerRules.flatMap(r => extractPredicates(ctx,r,prefix))
  }

  private def extractComparisonPredicates(ctx: Context, functor: Functor, literals: List[Literal], prefix: String): Set[BoolExpr] = {
    val p = functorToZ3(ctx, functor, prefix)
    val params: Set[Parameter] = functor.args.flatMap(_.getParameters()).toSet

    val freeVars: List[Variable] = params.flatMap {
      case _:Constant => None
      case v:Variable => Some(v)
    }.toList

    if (freeVars.isEmpty) {
      Set(p)
    }
    else {
      val matchingPredicates = _getMatchingLiteralPredicates(ctx, freeVars, literals, prefix)
      matchingPredicates.map(f => ctx.mkAnd(p, f))
    }
  }

  private def extractMatchingPredicates(ctx: Context, literals: List[Literal], prefix: String): Set[BoolExpr] = literals match {
    case Nil => Set()
    case head::next => {
      _extractMatchingPredicates(ctx, head, next, prefix) ++ extractMatchingPredicates(ctx, next, prefix)
    }
  }

  private def _extractMatchingPredicates(ctx: Context, literal: Literal, body: List[Literal], prefix: String): Set[BoolExpr] = {
    val thisIndices = getIndices(literal.relation)
    val keys = thisIndices.map(i=>literal.fields(i))
    val values: List[Parameter] = literal.fields.diff(keys)

    val p1 = literalToConst(ctx, literal, thisIndices, prefix)

    val freeVars: List[Variable] = values.flatMap{
      case v:Variable => if (v.name!="_") Some(v) else None
      case _:Constant => None
    }

    if (freeVars.isEmpty) {
      Set(p1)
    }
    else {
      val matchingPredicates = freeVars.flatMap(v=>_getMatchingLiteralPredicates(ctx,v,body,prefix)).toSet
      matchingPredicates.map(f => ctx.mkAnd(p1, f))
    }
  }

  private def _getMatchingLiteralPredicates(ctx: Context, freeVars: List[Variable], literals: List[Literal],
                                              prefix: String): Set[BoolExpr] = freeVars match {
    case Nil => Set(ctx.mkTrue())
    case head::next => {
      val preds = _getMatchingLiteralPredicates(ctx, head, literals, prefix)
      val tails = _getMatchingLiteralPredicates(ctx, next, literals, prefix)
      preds.flatMap(p => tails.map(f => ctx.mkAnd(p,f)))
    }
  }

  private def _getMatchingLiteralPredicates(ctx: Context, v: Variable, literals: List[Literal], prefix: String): Set[BoolExpr] = {
    val matchedLiterals = literals.filter(_.fields.contains(v))
    matchedLiterals.map (_lit =>
      literalToConst(ctx, _lit, getIndices(_lit.relation), prefix)
    ).toSet
  }

}
