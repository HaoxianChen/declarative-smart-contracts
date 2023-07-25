package viewMaterializer
import datalog.{Interface, Program, Relation}
import imp.{Empty, GroundVar, If, ImperativeAbstractProgram, IncrementAndInsert, OnStatement, Query, ReadTuple, Search, Seq, SolidityStatement, Statement, UpdateDependentRelations, UpdateStatement}
import imp.SolidityTranslator.transactionRelationPrefix

class BaseViewMaterializer extends ViewMaterializer {
  /** Materializes all relations that are directly read by an update program. */

  def getMaterializedRelations(program: ImperativeAbstractProgram, interfaces: Set[Interface]): Set[Relation] = {

    val fromStatements = program.onStatements.flatMap(relationsToMaterialize)
    val fromQueryDefs = program.queryDefs.flatMap(relationsToMaterialize)
    val viewRelations = interfaces.filterNot(_.relation.name.startsWith(transactionRelationPrefix)).map(_.relation)
    fromStatements ++ fromQueryDefs ++ viewRelations
  }

  protected def relationsToMaterialize(statement: Statement): Set[Relation] = statement match {
    case ReadTuple(rel, _, _) => Set(rel)
    case GroundVar(_, rel, _, _, _) => Set(rel)
    case Search(_, matches, stmt) => matches.map(_.relation) ++ relationsToMaterialize(stmt)
    case If(_,stmt) => relationsToMaterialize(stmt)
    case Seq(a,b) => relationsToMaterialize(a) ++ relationsToMaterialize(b)
    case on: OnStatement => relationsToMaterialize(on.statement)
    case inc: IncrementAndInsert => Set(inc.relation)
    case Query(_,_statement) => relationsToMaterialize(_statement)
    case _:Empty|_:imp.Assign|_:UpdateStatement|_:UpdateDependentRelations|_:SolidityStatement => Set()
  }

}
