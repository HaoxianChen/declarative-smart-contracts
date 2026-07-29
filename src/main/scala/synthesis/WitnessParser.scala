package synthesis

import datalog.{BooleanType, Constant, NumberType, Program, Relation, SimpleRelation, SymbolType}

/**
 * Parses a `witness.dl` file into a list of Traces.
 *
 * File format:
 *   // Line comments are ignored
 *   relName arg1 arg2 ... [| senderIdx msgValue]
 *   ---                           <- separates scenarios (each becomes its own Trace)
 *
 * Arguments are bare strings (no quotes).
 * If the `| senderIdx msgValue` suffix is omitted, defaults are sender=0, value=0.
 *
 * Example:
 *   constructor
 *   issueByPartition 1 0 5
 *   redeemByPartition 1 0 3
 *   ---
 *   constructor
 *   issueByPartition 1 0 5
 *   transferByPartition 1 2 0 3 | 1 0
 */
object WitnessParser {

  /**
   * Parse `filePath` against the relation declarations in `program`.
   * Returns one Trace per `---`-separated scenario.
   * Returns an empty list if the file does not exist or is empty.
   */
  def parse(filePath: String, program: Program): List[Trace] = {
    val file = new java.io.File(filePath)
    if (!file.exists()) return List.empty

    val relByName: Map[String, Relation] = program.relations.map(r => r.name -> r).toMap

    val lines = scala.io.Source.fromFile(file).getLines().toList

    // Split on "---" separator lines to get individual scenario blocks
    val scenarios: List[List[String]] = lines
      .map(_.trim)
      .foldLeft(List(List.empty[String])) { (acc, line) =>
        if (line == "---") acc :+ List.empty[String]
        else {
          val stripped = line.takeWhile(_ != '/').trim  // strip inline // comments
          if (stripped.isEmpty) acc
          else acc.init :+ (acc.last :+ stripped)
        }
      }
      .filter(_.nonEmpty)

    scenarios.flatMap { scenarioLines =>
      val txs = scenarioLines.flatMap { line =>
        parseStep(line, relByName)
      }
      if (txs.isEmpty) None else Some(Trace(txs))
    }
  }

  private def parseStep(line: String, relByName: Map[String, Relation]): Option[Transaction] = {
    // Split off implicit parameters suffix after '|'
    val (txPart, implicitPart) = line.split('|') match {
      case Array(tx)           => (tx.trim, None)
      case Array(tx, impl, _*) => (tx.trim, Some(impl.trim))
    }

    val tokens = txPart.split("\\s+").toList.filter(_.nonEmpty)
    if (tokens.isEmpty) return None

    val relName = tokens.head
    val argTokens = tokens.tail

    val rel = relByName.get(relName).orElse {
      // Also try without the recv_ prefix (witnesses may use the logical name)
      relByName.get(s"recv_$relName")
    }

    rel match {
      case None =>
        println(s"[WitnessParser] Warning: relation '$relName' not found in program schema. Skipping line: $line")
        None
      case Some(r) =>
        if (argTokens.length != r.arity) {
          println(s"[WitnessParser] Warning: relation '${r.name}' expects ${r.arity} args but got ${argTokens.length}. Skipping line: $line")
          None
        } else {
          val params = r.sig.zip(argTokens).map { case (t, v) => Constant(t, v) }

          val impl = implicitPart match {
            case None => ImplicitParameters(0, 0)
            case Some(s) =>
              s.split("\\s+").map(_.trim).filter(_.nonEmpty) match {
                case Array(sender, value) => ImplicitParameters(sender.toInt, value.toInt)
                case Array(sender)        => ImplicitParameters(sender.toInt, 0)
                case _                    => ImplicitParameters(0, 0)
              }
          }

          Some(Transaction(r, params, impl))
        }
    }
  }
}
