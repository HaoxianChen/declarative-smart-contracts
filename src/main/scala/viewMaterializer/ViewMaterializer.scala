package viewMaterializer

import datalog.{Interface, Program, Relation}
import imp.ImperativeAbstractProgram

abstract class ViewMaterializer {
  def getMaterializedRelations(program: ImperativeAbstractProgram, interfaces: Set[Interface]): Set[Relation]
}