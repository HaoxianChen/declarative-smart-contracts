package synthesis

import datalog.Functor

/** Given an EvaluatedTrace object, a set of predicates, return
 * a mapping, each transaction type to a bit vector encoding,
 * indicating which predicate is selected at each transaction's
 * condition guard. */
case class InductiveSynthesis(evaluatedTrace: EvaluatedTrace, predicates: Set[Functor]) {

  def makeEncoding(): Map[Transaction, List[Boolean]] = ???

  /** Perform the synthesis given an EvaluatedTrace and predicates. */
  def synthesize(evaluatedTrace: EvaluatedTrace, predicates: Set[Functor]): Map[Transaction, List[Boolean]] = {
    /** Step 1, generate encoding of the condition.
     *          For each transaction step:
     *            -  */
    ???
  }

  /** Validate the synthesis results. */
  def validate(): Boolean = {
    ???
  }
}
