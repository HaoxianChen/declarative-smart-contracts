package synthesis

import datalog.{Parameter, Constant, Program, Relation, Type}

/** Given a trace, and a set of predicates, this represents the
 * bit-vector (List[Boolean]) value of each predicate (Transaction).
 * Each Transaction maps to a List[Boolean] where index = time step.
 * */
case class EvaluatedTrace(
  // Map from transaction to its bit-vector across time steps
  data: Map[Transaction, List[Boolean]]
) {
  // Number of time steps in the trace (max length of bit-vectors; 0 if empty)
  def length: Int = data.size

  // Get the truth value of a transaction at a specific step
  def get(step: Int, tx: Transaction): Option[Boolean] =
    data.get(tx).flatMap(_.lift(step))

  // Get all predicates (transactions) tracked
  def predicates: Set[Transaction] = data.keySet

  override def toString: String =
    s"EvaluatedTrace(steps=$length, predicates=${predicates.size})"
}

object EvaluatedTrace {
  // Create an empty EvaluatedTrace
  def empty: EvaluatedTrace = EvaluatedTrace(Map.empty)

  // Create some test evaluated trace.
  def testTrace1(program: Program): EvaluatedTrace = {
    // Extract relations from the program
    val rels = program.relations.toList
    require(rels.length >= 2, "Program must contain at least two relations for the test.")
    val rel1 = rels(0)
    val rel2 = rels(1)

    // Create parameters matching the relation signatures
    val params1 = rel1.sig.zipWithIndex.map { case (t, i) => Constant(t, s"p${i+1}") }.toList
    val params2 = rel2.sig.zipWithIndex.map { case (t, i) => Constant(t, s"p${i+1}") }.toList

    // Transactions using extracted relations and parameters
    val tx1: Transaction = Transaction(rel1, params1, ImplicitParameters())
    val tx2: Transaction = Transaction(rel2, params2, ImplicitParameters())

    val tx1Bits: List[Boolean] = List(true, false, true)
    val tx2Bits: List[Boolean] = List(false, true, true)
    EvaluatedTrace(Map(tx1 -> tx1Bits, tx2 -> tx2Bits))
  }
}