package synthesis

import datalog.{Program,Rule}
import verification.{TransitionSystem, Verifier}

case class Synthesizer(dl: Program, violationRules: Set[Rule]) {

  val verifier: Verifier = ???
  def go(): Program = {

    /** Translate dl into a transition system. */
    val transitionSystem: TransitionSystem = ???

    /** The CEGIS loop.  */
    ???
  }
}
