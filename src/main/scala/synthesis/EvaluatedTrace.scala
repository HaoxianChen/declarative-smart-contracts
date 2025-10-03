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

  /**
   * Returns a sequence of (state, transaction) pairs, where each transaction
   * is paired with the previous state in the trace.
   * Example: Seq((init state, tx1), (state1, tx2), ...)
   */
  def shiftTrace(trace: EvaluatedTrace): Seq[(State, Transaction)] = {
    val states = trace.initialState +: trace.steps.map(_._2)
    val txs = trace.steps.map(_._1)
    states.zip(txs)
  }

  // Create a test evaluated trace
  def testTrace1(program: Program): EvaluatedTrace = {
    val txRules = program.transactionRules().toList
    require(txRules.nonEmpty, "Program must contain at least one transaction rule for the test.")
    val txLiterals = txRules.map(PredicateEnumerator.extractTxLiteral)
    val txs = txLiterals.map { txLit =>
      val params = txLit.relation.sig.zipWithIndex.map { case (t, i) => Constant(t, i.toString) }.toList
      Transaction(txLit.relation, params, ImplicitParameters())
    }
    val initialState = State()
    // For demonstration, just use the same state (no real execution)
    val states = List.fill(txs.size)(State())
    EvaluatedTrace(initialState, txs.zip(states))
  }
}