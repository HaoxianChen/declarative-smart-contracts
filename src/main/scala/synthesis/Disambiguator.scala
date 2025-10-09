package synthesis

import datalog.{Constant, NumberType, Program, SimpleRelation, SymbolType, Type}
import imp.SolidityStatement

import scala.util.Random
import imp.SolidityTranslator.transactionRelationPrefix

case class Disambiguator(sketch: Program,
                         solInterpreter: SolidityInterpreter,
                         txDefs: Map[String, SolidityStatement]) {

  val interfaceRelations = sketch.interfaces.map(_.relation)
    .filter(_.name.startsWith(transactionRelationPrefix)).toList

  val txRelations = interfaceRelations.collect {
    case sr: SimpleRelation => sr.copy(name = sr.name.stripPrefix(transactionRelationPrefix))
    // Extend here for other relation types if needed
  }

  val addresses = List("1", "2", "3")
  val numberRange = 3

  private def randomConstant(t: Type): Constant = t match {
      case SymbolType(_) =>
        datalog.Constant(t, addresses(Random.nextInt(addresses.length)))
      case _: NumberType =>
        datalog.Constant(t, (Random.nextInt(10) + 1).toString)
      case _ =>
        datalog.Constant(t, Random.nextInt(3).toString)
  }


  private def randomTransaction(rel: datalog.Relation): Transaction = {
    val params = rel.sig.map(randomConstant)
    val msgSender = Random.nextInt(addresses.size)
    val msgValue = Random.nextInt(numberRange)
    Transaction(rel, params, ImplicitParameters(msgSender,msgValue))
  }

  def makeTracesHeuristic(numTraces: Int = 1000, txsPerTrace: Int = 3): Set[EvaluatedTrace] = {
    // Helper to get possible values for a type
    def paramDomain(t: Type): Seq[String] = t match {
      case SymbolType(_) => addresses
      case _: NumberType => (1 to numberRange).map(_.toString)
      case _ => (0 until numberRange).map(_.toString)
    }

    def allTransactions(rel: datalog.Relation): Seq[Transaction] = {
      val domains = rel.sig.map(paramDomain)
      val combos = domains.foldLeft(Seq(Seq.empty[String])) { (acc, dom) =>
        for { a <- acc; d <- dom } yield a :+ d
      }
      for {
        params <- combos
        senderIdx <- addresses.indices
        // Optionally enumerate msgValue as well
        msgValue <- 0 until numberRange
        // msgValue <- Seq(0)
      } yield {
        val constants = rel.sig.zip(params).map { case (t, v) => datalog.Constant(t, v) }
        Transaction(rel, constants, ImplicitParameters(senderIdx, msgValue))
      }
    }

    val setupTxs: List[Transaction] = txRelations.flatMap { rel =>
      // Heuristic: if relation name contains "mint" (case-insensitive), produce per-address setup
      if (rel.name.toLowerCase.contains("mint")) {
        addresses.map { addr =>
          val params = rel.sig.zipWithIndex.map { case (t, idx) =>
            t match {
              case SymbolType(_) =>
                // first param assumed to be address
                datalog.Constant(t, addr)
              case _ =>
                datalog.Constant(t, "10")
            }
          }
          val implicitParameters = ImplicitParameters(0,0)
          Transaction(rel, params, implicitParameters)
        }
      } else Nil
    }

    // Build all possible transactions for all relations
    val allTxs: Seq[Transaction] = txRelations.flatMap(allTransactions)

    val traces = allTxs.map(tx => Trace(setupTxs :+ tx))

    traces.map(t => solInterpreter.interpret(txDefs, t)).toSet
  }

}
