package synthesis

import datalog.{Program, Rule}

case class BoundedModelChecker() {
  /** Input:
   *    - A datalog program
   *    - A set of rules that are query violation instance
   *  Output:
   *    - Result: Boolean
   *    - Counter example if result is false. */
  def check(program: Program, violationRules: Set[Rule], bound: Int): (Boolean, Option[Trace]) = {
    ???
  }
}

object BoundedModelChecker {
  /** Prepare some unit tests here.
   *  - Read the program and violation rules from a file.
   *  - Return counter example when it violates the property.
   * */
  def unitTest1(): Unit = {
    ???
  }
  
}
