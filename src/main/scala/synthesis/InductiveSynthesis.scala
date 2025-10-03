package synthesis

import com.microsoft.z3.{BoolExpr, Context, Model, Solver}
import datalog.{Relation, Rule}
import synthesis.EvaluatedTrace.shiftTrace

/** Given an EvaluatedTrace object, a set of predicates, return
 * a mapping, each transaction type to a bit vector encoding,
 * indicating which predicate is selected at each transaction's
 * condition guard. */
case class InductiveSynthesis(
  predicatesPerRule: Map[Rule,Set[Predicate]],
  interpreterContext: InterpreterContext
) {
  val interpreter: Interpreter = Interpreter(interpreterContext)

  /** Reorganize and make the predicate lookup by relation efficient. */
  val predicates: Map[Relation, Set[Predicate]] = {
    predicatesPerRule.map { case (rule, preds) =>
      val txLiteral = PredicateEnumerator.extractTxLiteral(rule)
      txLiteral.relation -> preds
    }
  }

  // Initialize Z3 context as a member
  val z3ctx: Context = new Context()

  // Initialize encoding as a member using makeEncoding
  val encodings: Map[Relation, List[BoolExpr]] = makeEncoding(z3ctx)

  /** For each Rule, create a list of Z3 Bool variables, one for each predicate.
    * The length of the list matches the number of predicates for that Rule. */
  def makeEncoding(z3ctx: Context): Map[Relation, List[BoolExpr]] = {
    predicates.map { case (rel, preds) =>
      val boolVars = preds.toList.zipWithIndex.map { case (_, i) =>
        z3ctx.mkBoolConst(s"pred_${rel.name}_$i")
      }
      rel -> boolVars
    }
  }

  /**
    * For a given EvaluatedTrace, return a sequence of (Transaction, List[Boolean])
    * where each transaction is paired with the previous state and the predicate
    * evaluation results for that state and transaction.
    */
  def evaluatePredicates(evaluatedTrace: EvaluatedTrace): Seq[(Transaction, List[Boolean])] = {
    val pairs = shiftTrace(evaluatedTrace)
    pairs.map { case (state, tx) =>
      val preds = predicates(tx.relation)
      val results = preds.toList.map(p => interpreter.evaluate(state, tx, p))
      (tx, results)
    }
  }

  /**
    * For each (tx, boolList) in evalResults:
    *   - Fetch the list of Z3 Bool variables from encodings for that relation.
    *   - For each i, assert boolVar[i] implies boolList[i].
    *   - Conjunct all assertions for the transaction.
    * Conjunct all transaction constraints into a single Z3 BoolExpr and return.
   *  Assert that the trace cannot go through.
    */
  def makeConstraints(evalResults: Seq[(Transaction, List[Boolean])]): BoolExpr = {
    val txConstraints = evalResults.map { case (tx, boolList) =>
      val boolVars = encodings(tx.relation)
      val assertions = boolList.zipWithIndex.map { case (b, i) =>
        val premise = boolVars(i)
        val conclusion = z3ctx.mkBool(b)
        z3ctx.mkImplies(premise, conclusion)
      }
      z3ctx.mkAnd(assertions: _*)
    }
    val acceptTrace = z3ctx.mkAnd(txConstraints: _*)
    z3ctx.mkNot(acceptTrace)
  }

  /**
    * Given a Z3 model, extract for each relation the list of predicate assignments
    * (true if the predicate variable is true in the model, false otherwise).
    * Also print the selected predicates for each relation.
    */
  private def interpretModel(model: Model): Map[Relation, List[Boolean]] = {
    encodings.map { case (rel, boolVars) =>
      val preds = predicates(rel).toList
      val assignments = boolVars.map { v =>
        val value = model.eval(v, true)
        value.isTrue
      }
      val selectedPreds = preds.zip(assignments).collect {
        case (p, true) => p
      }
      println(s"Relation: ${rel.name}")
      selectedPreds.foreach(p => println(s"  Selected: ${p}"))
      rel -> assignments
    }
  }

  /** Perform the synthesis given an EvaluatedTrace and predicates. */
  def synthesize(evaluatedTrace: EvaluatedTrace): Map[Relation, List[Boolean]] = {
    val evalResults = evaluatePredicates(evaluatedTrace)
    val constraint = makeConstraints(evalResults)
    val solver = z3ctx.mkSolver()
    solver.add(constraint)
    val status = solver.check()
    if (status == com.microsoft.z3.Status.SATISFIABLE) {
      val model = solver.getModel
      interpretModel(model)
    } else {
      Map.empty
    }
  }

  /** Validate the synthesis results. */
  def validate(): Boolean = {
    ???
  }
}