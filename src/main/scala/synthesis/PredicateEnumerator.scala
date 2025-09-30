// Scala
package synthesis

import datalog.{Constant, Equal, Functor, Greater, Literal, Param, Parameter, Program, Relation, Unequal, Variable}

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
}