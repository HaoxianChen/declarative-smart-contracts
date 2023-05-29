package viewMaterializer

import datalog.{Interface, Program, Relation}
import imp.ImperativeAbstractProgram

abstract class ViewMaterializer {
  /** Input:
   * - program: the imperative abstract program,
   *             where program.rules store the original Datalog rules.
   * - interfaces: the public interfaces of the smart contracts.
   *
   * Output: set of relations to be materialized
   *  */
  def getMaterializedRelations(program: ImperativeAbstractProgram, interfaces: Set[Interface]): Set[Relation]
}