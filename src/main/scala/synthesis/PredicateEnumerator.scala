// Scala
package synthesis

import datalog.{ArithOperator, Arithmetic, Assign, Constant, Equal, Functor, Geq, Greater, Leq, Lesser, Literal, Param, Parameter, Program, Relation, Rule, SimpleRelation, Unequal, Variable}
import imp.{ImperativeAbstractProgram, ImperativeTranslator}
import Arithmetic.extractParameters
import viewMaterializer.BaseViewMaterializer

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

case class InterpreterContext(relationIndices: Map[SimpleRelation, List[Int]],
                              materializedRelations: Set[Relation])

object InterpreterContext {
  def makeContext(program: Program): InterpreterContext = {
    val relationIndices = program.relationIndices

    val impTranslator = new ImperativeTranslator(program, Set(),
      isInstrument=true, enableProjection=true,
      monitorViolations = false, arithmeticOptimization = true)
    val imperative = impTranslator.translate()

    val baseViewMaterializer = new BaseViewMaterializer()
    val materializedRelations = baseViewMaterializer.getMaterializedRelations(
      imperative, program.interfaces).toSet

    InterpreterContext(relationIndices, materializedRelations)
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
case class PredicateEnumerator(interpreterContext: InterpreterContext) {
  /** keep only materialized relation in the map */
  private val relationIndices = interpreterContext.relationIndices.filter {
    case (rel, _) => interpreterContext.materializedRelations.contains(rel)
  }

  def enumeratePredicates(program: Program): Map[Rule,Set[Predicate]] = {
    val txRules = program.transactionRules()

    // init a mutable mapping from rule to set of predicate objects
    val predicates: mutable.Map[Rule, Set[Predicate]] = mutable.Map.empty


    for (txRule <- txRules) {
      val txLiteral = PredicateEnumerator.extractTxLiteral(txRule)
      /** 1. Simply comparing parameter in the txRule  */
      val singles = singlePredicate(txLiteral)

      /** 2. Binding one indexed relation, then add one predicate. */
      val indexedRelations = program.relations.collect {
        case r: SimpleRelation if ( relationIndices.contains(r)) => r
      }
      val withBindings: Set[Predicate] = withOneBinding(txLiteral, indexedRelations)

      predicates.update(txRule, singles ++ withBindings)
    }
    predicates.toMap
  }

  private def singlePredicate(txLiteral: Literal): Set[Predicate] = {
    val context = Context(txLiteral, Set.empty)
    val functors: Set[Functor] = singleAtomCandidates(txLiteral)
    functors.map(f => Predicate(context, f))
  }

  // Helper to extract all parameters from a Functor
  private def functorParams(f: Functor): Seq[Parameter] = f match {
    case operator: ArithOperator =>
      extractParameters(operator.a) ++ extractParameters(operator.b)
    case Equal(lhs, rhs) => extractParameters(lhs) ++ extractParameters(rhs)
    case Unequal(lhs, rhs) => extractParameters(lhs) ++ extractParameters(rhs)
    case Assign(a, b) => throw new UnsupportedOperationException("Assign functor is not supported")
  }

  /** For each relation in bindingRels, lookup its index;
   invoke makeOneBinding. For each output Literal,
   make a Predicate. */
  private def withOneBinding(txLiteral: Literal, bindingRels: Set[SimpleRelation]): Set[Predicate] = {
    bindingRels.flatMap { rel =>
      val indices = relationIndices.getOrElse(rel, Nil)
      val bindingLiterals = makeOneBinding(txLiteral, rel, indices)
      bindingLiterals.flatMap { bindingLiteral =>
        val context = Context(txLiteral, Set(bindingLiteral))

        val bindingVars = bindingLiteral.fields.collect { case v: Variable => v }.toSet

        // Only keep functors that refer to at least one variable in bindingLiteral
        val singles = singleAtomCandidates(bindingLiteral)
        val crossRelation = crossRelationComparison(txLiteral, bindingLiteral)
        val functors = (singles ++ crossRelation)
          .filter { f => functorParams(f).exists {
            case v: Variable => bindingVars.contains(v)
            case _ => false
          }
        }
        functors.map(f => Predicate(context, f))
      }
    }
  }


  /** Given a txLiteral with parameters a,b,c...,
   *  make a set of literals of the indexed relation
   *  where each literal has the indexed parameter bind to one of the
   *  type consistent parameter in txLiteral. */
  private def makeOneBinding(txLiteral: Literal, indexedRelation: Relation,
                             indices: List[Int]): Set[Literal] = {
    val txParams = txLiteral.fields

    // For each index, find all type-consistent txParams
    val bindings = indices.flatMap { idx =>
      val relType = indexedRelation.sig(idx)
      txParams.collect {
        case p if p._type == relType => (idx, p)
      }
    }

    if (bindings.isEmpty) Set.empty
    else {
      // Group bindings by index, so we can replace all indexed parameters at once
      val grouped: Map[Int, Seq[(Int, Parameter)]] = bindings.groupBy(_._1)
      // For each combination of parameters for all indices, create a new literal
      val allIdxs = indices
      val allParams = allIdxs.map(idx => bindings.filter(_._1 == idx).map(_._2)).filter(_.nonEmpty)
      // Cartesian product of all possible parameter choices for each index
      val combos = allParams.foldLeft(Seq(Seq.empty[Parameter])) { (acc, params) =>
        for (a <- acc; p <- params) yield a :+ p
      }
      combos.map { paramsForIndices =>
        // Build fields for the new literal
        val fields = indexedRelation.sig.zipWithIndex.map { case (t, i) =>
          val idxInIndices = allIdxs.indexOf(i)
          if (idxInIndices >= 0) paramsForIndices(idxInIndices) else Variable(t, s"${indexedRelation.name}_x$i")
        }
        Literal(indexedRelation, fields)
      }.toSet
    }
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

  /** For each pair produce equalities x_i == y_j at matching positions,
   only when x_i and y_j has the same type
  */
  private def crossRelationComparison(p: (Literal, Literal)): Set[Functor] = {
    val (a, b) = p
    (for {
      i <- a.fields.indices
      j <- b.fields.indices
      val fieldA = a.fields(i)
      val fieldB = b.fields(j)
      if fieldA._type == fieldB._type && fieldA != fieldB // avoid reflexive comparison
    } yield Set(
      Equal(a.fields(i), b.fields(j)),
      Greater(Param(a.fields(i)), Param(b.fields(j))),
      Geq(Param(a.fields(i)), Param(b.fields(j))),
      Lesser(Param(a.fields(i)), Param(b.fields(j))),
      Leq(Param(a.fields(i)), Param(b.fields(j))),
    )).flatten.toSet
  }
}

object PredicateEnumerator {
  def extractTxLiteral(txRule: Rule): Literal = {
    val txLits: Seq[Literal] =
      txRule.body.collect { case lit: Literal
        if imp.SolidityTranslator.isTransactionTriggerRelation(lit.relation) => lit
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