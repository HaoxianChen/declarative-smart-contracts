package viewMaterializer
import datalog.{Interface, Relation}
import imp.ImperativeAbstractProgram

class MinimalViewMaterializer extends ViewMaterializer {
  def getMaterializedRelations(program: ImperativeAbstractProgram, interfaces: Set[Interface]): Set[Relation] = ???
}
