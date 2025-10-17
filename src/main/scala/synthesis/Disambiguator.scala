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

  // Make a constructor transaction, with address field randomly drawn from the
  // address universe, and others from the numberRange
  def makeConstructorTransaction(): Transaction = {
    // find the relation called constructor.
    val rel = {
      val constructorRelOpt = sketch.relations.find(_.name.equalsIgnoreCase("constructor"))
      constructorRelOpt.get
    }

    val params = rel.sig.zipWithIndex.map { case (t, idx) =>
      t match {
        case SymbolType(_) =>
          datalog.Constant(t, addresses(Random.nextInt(addresses.length)))
        case _: NumberType =>
          datalog.Constant(t, (Random.nextInt(numberRange) + 1).toString)
        case _ =>
          datalog.Constant(t, Random.nextInt(numberRange).toString)
      }
    }
    val implicitParameters = ImplicitParameters(Random.nextInt(addresses.length),
              Random.nextInt(numberRange))
    Transaction(rel, params, implicitParameters)
  }

  def makeTracesHeuristic(numTraces: Int): Set[EvaluatedTrace] = {
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
      // if (rel.name.toLowerCase.contains("mint")) {
      val lname = rel.name.toLowerCase
      if (lname.contains("mint")
        || lname.contains("increaseallowance")
        || lname.contains("increaseapproval")
        || lname.contains("invest")
        || lname.contains("issue")
        || lname.contains("issueByPartition")
        || lname.contains("addVoter")
      ) {
        val symbolIndices = rel.sig.zipWithIndex.collect { case (SymbolType(_), idx) => idx }
        val addressCombos = List.fill(symbolIndices.size)(addresses).foldLeft(Seq(Seq.empty[String])) {
          (acc, addrs) => for { a <- acc; addr <- addrs } yield a :+ addr
        }//.filter(combo => combo.distinct.size == combo.size) // skip combos with duplicate addresses

        addressCombos.map { combo =>
          val params = rel.sig.zipWithIndex.map { case (t, idx) =>
            t match {
              case SymbolType(_) =>
                val comboIdx = symbolIndices.indexOf(idx)
                datalog.Constant(t, combo(comboIdx))
              case _ =>
                datalog.Constant(t, "10")
            }
          }
          val implicitParameters = ImplicitParameters(0, 0)
          Transaction(rel, params, implicitParameters)
        }
          // addresses.map { addr =>
          // val params = rel.sig.zipWithIndex.map { case (t, idx) =>
          //   t match {
          //     case SymbolType(_) =>
          //       // first param assumed to be address
          //       datalog.Constant(t, addr)
          //     case _ =>
          //       datalog.Constant(t, "10")
          //   }
          // }
          // val implicitParameters = ImplicitParameters(0,0)
          // Transaction(rel, params, implicitParameters)
        // }
      } else Nil
    }

    println(s"Setup trace length: ${setupTxs.size}.")

    val constructorTx = makeConstructorTransaction()

    // Build all possible transactions for all relations
    // val allTxs: Seq[Transaction] = txRelations.flatMap(allTransactions)
    // val traces = allTxs.map(tx => Trace( constructorTx +: setupTxs :+ tx))


    // // sample traces when exceeds quota
    // val sampledTraces =
    //   if (traces.size > numTraces) {
    //     println(s"[Disambiguation trace] Sampling ${numTraces} traces out of ${traces.size}.")
    //     Random.shuffle(traces).take(numTraces)
    //   } else
    //     traces

    // sampledTraces.map(t => solInterpreter.interpret(txDefs, t)).toSet

    // build per-relation traces
    val perRelTraces: Seq[Seq[Trace]] = txRelations.map { rel =>
      val relTxs = allTransactions(rel)
      Random.shuffle(relTxs).map(tx => Trace(constructorTx +: setupTxs :+ tx))
    }

    // take up to per-transaction quota per relation
    val perTransactionQuota = Math.ceil(numTraces.toDouble / txRelations.size).toInt
    val selectedBuilder = Vector.newBuilder[Trace]
    val takenCounts = perRelTraces.map { traces =>
      val takeN = Math.min(traces.size, perTransactionQuota)
      selectedBuilder ++= traces.take(takeN)
      takeN
    }

    var selected = selectedBuilder.result()

    // if we still need more to reach numTraces, fill from leftovers across relations
    if (selected.size < numTraces) {
      val needed = numTraces - selected.size
      val leftovers: Seq[Trace] = perRelTraces.zip(takenCounts).flatMap { case (traces, taken) =>
        traces.drop(taken)
      }
      selected ++= leftovers.take(needed)
    }

    // cap to numTraces in case per-transaction quotas exceeded global budget
    val finalSampled = if (selected.size > numTraces) selected.take(numTraces) else selected

    val evaluatedTrace = finalSampled.map(t => solInterpreter.interpret(txDefs, t)).toSet
    evaluatedTrace
    val takeLastTxTrace = evaluatedTrace.map(_.takeLast())
    takeLastTxTrace
  }

}
