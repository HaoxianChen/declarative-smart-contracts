// Scala
package synthesis

import datalog.{Constant, Equal, Functor, Literal, Param, Parameter, Program, Relation, Rule, Unequal, Variable}
import imp.SolidityTranslator.isTransactionTriggerRelation

import scala.collection.mutable


case class Context(tx: Literal, bindingLiterals: Set[Literal]) {
  override def toString: String = {
    val txStr = tx.toString
    val bindingsStr = bindingLiterals.map(_.toString).mkString(", ")
    s"tx: $txStr, bindings: [$bindingsStr]"
  }
}
case class Predicate(context: Context, functor: Functor) {
  override def toString: String = {
    s"Predicate(context: $context, functor: $functor)"
  }
}

/**
 * Enumerates candidate Datalog literals directly from the program schema.
 *
 * Produces:
 *  - Relation atoms with fresh variables per instance.
 *  - Simple guards for each bound variable (e.g., x >= 0).
 *  - Cross-relation equalities between variables at corresponding positions.
 */
object PredicateEnumerator {

  def enumerate(program: Program): Set[Functor] = {
    val insts = makeLiterals(program.relations)

    val singles: Set[Functor] = insts.flatMap(singleAtomCandidates)

    val pairwiseEqs: Set[Functor] =
      pairwise(insts.toVector).flatMap(crossRelationEqualities)

    (singles ++ pairwiseEqs)
  }

  /** Alternative enumerate method.
   *  For each transaction relation, return a set of Predicates:
   *   -   */
  def enumeratePredicates(program: Program): Map[Rule,Set[Predicate]] = {
    val txRules = program.transactionRules()

    // init a mutable mapping from rule to set of predicate objects
    val predicates: mutable.Map[Rule, Set[Predicate]] = mutable.Map.empty


    /** 1. Simply comparing parameter in the txRule  */
    for (txRule <- txRules) {
      val txLiteral = extractTxLiteral(txRule)
      val preds = singlePredicate(txLiteral)
      predicates.update(txRule, preds)
    }
    predicates.toMap
  }

  private def singlePredicate(txLiteral: Literal): Set[Predicate] = {
    val context = Context(txLiteral, Set.empty)
    val functors: Set[Functor] = singleAtomCandidates(txLiteral)
    functors.map(f => Predicate(context, f))
  }


  /** For each relation, return a type consistent literal,
   * and each literal uses unique variable name */
  private def makeLiterals(rels: Set[Relation]): Set[Literal] = {
    rels.flatMap { r =>
      val arity = relationArity(r)
      val vars = List.tabulate(arity)(i => Variable(r.sig(i), s"${r.name}_x$i"))
      Set(Literal(r, vars))
    }
  }

  // ----- helpers -----
  private def relationArity(r: Relation): Int = r.sig.length
  /**
   * Emit for each variable `x`:
   * - If `x` has type `int`, generate: `x == 0`, `x != 0`
   */
  private def singleAtomCandidates(literal: Literal): Set[Functor] = {
    literal.fields.collect {
      case v: Variable =>
        v._type match {
          case datalog.NumberType(_) =>
            Set(
              Equal(Constant(v._type, "0"), v),
              Unequal(Constant(v._type, "0"), v)
            )
          case _ => Set.empty[Functor]
        }
    }.flatten.toSet
  }

  private def isTxnRelation(literal: Literal): Boolean = {
    literal.relation.name.startsWith("transaction")
  }

  /** Pair-wise relation comparison, avoid two transaction
   * relation (with the transaction relation prefix in name)
   * directly comparing though.
   * */
  private def pairwise(insts: Vector[Literal]): Set[(Literal, Literal)] = {
    (for {
      i <- insts.indices
      j <- (i + 1) until insts.size
      if !(isTxnRelation(insts(i)) && isTxnRelation(insts(j)))
    } yield (insts(i), insts(j))).toSet
  }

  /** For each pair produce equalities x_i == y_i at matching positions,
   only when x_i and y_i has the same type
  */
  private def crossRelationEqualities(p: (Literal, Literal)): Set[Functor] = {
    val (a, b) = p
    val m = math.min(a.fields.length, b.fields.length)
    Vector.tabulate(m) { i =>
      if (a.fields(i)._type == b.fields(i)._type) {
        Equal(a.fields(i), b.fields(i))
      } else {
        None
      }
    }.collect { case eq: Equal => eq }.toSet
  }

  def extractTxLiteral(txRule: Rule): Literal = {
    val txLits: Seq[Literal] =
      txRule.body.collect { case lit: Literal
        if isTransactionTriggerRelation(lit.relation) => lit
      }.toSeq

    txLits match {
      case Seq(lit) => lit
      case Seq() =>
        throw new IllegalArgumentException(s"No transaction literal found in rule: $txRule")
      case _ =>
        throw new IllegalArgumentException(s"Multiple transaction literals found in rule: $txRule")
    }
  }

}