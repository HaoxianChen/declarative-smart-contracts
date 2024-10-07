package synthesis

import datalog.Program
import verification.{TransitionSystem, Verifier}

case class Synthesizer(dl: Program, example_traces: Set[ExampleTrace],
                       temporalProperties: Set[TemporalProperty]) {

  val verifier: Verifier = ???
  def go(): Program = {

    /** Translate dl into a transition system. */
    val transitionSystem: TransitionSystem = ???

    /** The CEGIS loop.  */
    ???
  }
}
