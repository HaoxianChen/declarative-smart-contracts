package synthesis

import datalog.{Relation, Constant}

/** Transaction: relation + concrete list of parameters. */
case class Transaction(relation: Relation, parameters: List[Constant]) {
  def arity: Int = relation.arity
  override def toString: String = s"${relation.name}(${parameters.mkString(",")})"
}

object Transaction {
  /** helper constructor from varargs parameters */
  def apply(relation: Relation, parameters: Constant*): Transaction = Transaction(relation, parameters.toList)
}

/** Trace: consist of a sequence of transactions. */
case class Trace(steps: Seq[Transaction]) {

  /** number of steps in the trace */
  def length: Int = steps.length
  def isEmpty: Boolean = steps.isEmpty

  /** append a new transaction (validates parameter arity and types against relation signature) */
  def append(tx: Transaction): Trace = {
    require(tx != null, "transaction must not be null")
    val rel: Relation = tx.relation
    val parameters = tx.parameters

    // check arity
    require(parameters.length == rel.arity,
      s"Transaction '${rel.name}' expects ${rel.arity} parameters but got ${parameters.length}")

    // check per-parameter types; allow wildcard parameters named "_" to skip type-check
    parameters.zip(rel.sig).zipWithIndex.foreach { case ((param, expT), idx) =>
      if (param.name != "_") {
        require(param._type == expT,
          s"Parameter at position $idx for transaction '${rel.name}' has type ${param._type} but expected $expT")
      }
    }

    Trace(steps :+ tx)
  }

  override def toString: String = {
    steps.zipWithIndex.map { case (tx, i) => s"[$i] ${tx.relation.name}(${tx.parameters.mkString(",")})" }.mkString("Trace:\n", "\n", "")
  }
}

object Trace {
  /** empty trace */
  val empty: Trace = Trace(Seq.empty)
}
