package synthesis

import datalog.{Parameter, Constant, Program, Relation, Type}

/**
 * Represents the execution trace of a sequence of transactions:
 * - initialState: the state before any transaction
 * - steps: a sequence of (Transaction, State) pairs, where State is the result after executing the transaction
 */
case class EvaluatedTrace(
  initialState: State,
  steps: Seq[(Transaction, State)]
) {
  // Number of steps in the trace
  def length: Int = steps.length

  override def toString: String =
    s"EvaluatedTrace(initialState=$initialState, steps=$length)"
}

object EvaluatedTrace {
  // Create an empty EvaluatedTrace
  def empty: EvaluatedTrace = EvaluatedTrace(State(), Seq.empty)

  // Create a test evaluated trace
  def testTrace1(program: Program): EvaluatedTrace = {
    val rels = program.relations.toList
    require(rels.length >= 2, "Program must contain at least two relations for the test.")
    val rel1 = rels(0)
    val rel2 = rels(1)
    val params1 = rel1.sig.zipWithIndex.map { case (t, i) => Constant(t, i.toString) }.toList
    val params2 = rel2.sig.zipWithIndex.map { case (t, i) => Constant(t, i.toString) }.toList
    val tx1: Transaction = Transaction(rel1, params1, ImplicitParameters())
    val tx2: Transaction = Transaction(rel2, params2, ImplicitParameters())
    val initialState = State()
    // For demonstration, just use the same state (no real execution)
    val state1 = State()
    val state2 = State()
    EvaluatedTrace(initialState, Seq((tx1, state1), (tx2, state2)))
  }
}