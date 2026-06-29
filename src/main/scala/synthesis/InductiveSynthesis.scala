package synthesis

import com.microsoft.z3.{ArraySort, BoolExpr, BoolSort, Context, Expr, IntExpr, Model, Quantifier, Sort}
import datalog.{Constant, Functor, Literal, Parameter, Program, Relation, ReservedRelation, Rule, SimpleRelation, SingletonRelation, Variable}
import synthesis.EvaluatedTrace.shiftTrace
import imp.ImperativeTranslator
import imp.SolidityTranslator.transactionRelationPrefix
import verification.{TransitionSystem, Verifier}
import verification.Verifier.indicatorConstForTransactionTriggerRelation
import verification.Z3Helper.{fieldsToConst, functorToZ3, getSort, literalToConst, mkTupleKey, paramToConst, typeToSort}
import scala.collection.mutable

/** Given an EvaluatedTrace object, a set of predicates, return
 * a mapping, each transaction type to a bit vector encoding,
 * indicating which predicate is selected at each transaction's
 * condition guard. */
case class InductiveSynthesis(
  predicatesPerRule: Map[Rule,Set[Predicate]],
  interpreterContext: InterpreterContext,
  programForEncoding: Program,
  encodingMode: EncodingMode = EncodingMode.Concrete
) {

  case class Representation(map: Map[Relation, Set[Predicate]]) {
    def getPredicates(rel: Relation): Set[Predicate] = map.getOrElse(rel, Set())

    override def toString: String =
      map.map { case (rel, preds) => s"${rel.name}: ${preds.mkString("\n")}" }.mkString("\n")
  }

  val interpreter: Interpreter = Interpreter(interpreterContext)

  private val synthesisCache: mutable.Map[List[EvaluatedTrace], List[Representation]] = mutable.Map.empty

  /** Reorganize and make the predicate lookup by relation efficient. */
  val predicates: Map[Relation, Set[Predicate]] = {
    predicatesPerRule.map { case (rule, preds) =>
      val txLiteral = PredicateEnumerator.extractTxLiteral(rule)
      txLiteral.relation -> preds
    }
  }

  private val orderedPredicates: Map[Relation, List[Predicate]] =
    predicates.map { case (rel, preds) => rel -> preds.toList.sortBy(_.toString) }

  private lazy val symbolicVerifier: Verifier = {
    val impTranslator = new ImperativeTranslator(programForEncoding, Set(),
      isInstrument = true, monitorViolations = false,
      arithmeticOptimization = true, enableProjection = true)
    new Verifier(programForEncoding, impTranslator.translate())
  }

  private lazy val symbolicTransitionSystem: TransitionSystem =
    symbolicVerifier.getTransitionSystem()

  // Symbolic mode must use the verifier context that owns the transition-system expressions.
  val z3ctx: Context = encodingMode match {
    case EncodingMode.SymbolicCexBlocking => symbolicVerifier.context
    case EncodingMode.Concrete => new Context()
  }

  var traceBlockingTimeMs: Long = 0L
  var inductiveSolverTimeMs: Long = 0L

  // Initialize encoding as a member using makeEncoding
  val encodings: Map[Relation, List[BoolExpr]] = makeEncoding(z3ctx)

  /** For each Rule, create a list of Z3 Bool variables, one for each predicate.
   * The length of the list matches the number of predicates for that Rule. */
  def makeEncoding(z3ctx: Context): Map[Relation, List[BoolExpr]] = {
    orderedPredicates.map { case (rel, preds) =>
      val boolVars = preds.zipWithIndex.map { case (_, i) =>
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
      val preds = orderedPredicates(tx.relation)
      val results = preds.map(p => interpreter.evaluate(state, tx, p))
      (tx, results)
    }
  }

  /**
   * For each (tx, boolList) in evalResults:
   *   - Fetch the list of Z3 Bool variables from encodings for that relation.
   *   - For each i, assert boolVar[i] implies boolList[i].
   *   - Conjunct all assertions for the transaction.
   *     Conjunct all transaction constraints into a single Z3 BoolExpr and return.
   *     Assert that the trace cannot go through.
   */
  private def makeAcceptExpr(evalResults: Seq[(Transaction, List[Boolean])]): BoolExpr = {
    val txConstraints = evalResults.map { case (tx, boolList) =>
      val boolVars = encodings(tx.relation)
      val assertions = boolList.zipWithIndex.map { case (b, i) =>
        val premise = boolVars(i)
        val conclusion = z3ctx.mkBool(b)
        z3ctx.mkImplies(premise, conclusion)
      }
      z3ctx.mkAnd(assertions: _*)
    }
    z3ctx.mkAnd(txConstraints: _*)
  }

  def makeConstraints(evalResults: Seq[(Transaction, List[Boolean])]): BoolExpr = {
    z3ctx.mkNot(makeAcceptExpr(evalResults))
  }

  /**
   * Given a Z3 model, extract for each relation the list of predicate assignments
   * (true if the predicate variable is true in the model, false otherwise).
   * Also print the selected predicates for each relation.
   */
  private def interpretModel(model: Model): Representation = {
    val mapping = encodings.map { case (rel, boolVars) =>
      val preds = orderedPredicates(rel)
      val assignments = boolVars.map { v =>
        val value = model.eval(v, true)
        value.isTrue
      }
      val selectedPreds = preds.zip(assignments).collect {
        case (p, true) => p
      }
      // println(s"Relation: ${rel.name}")
      // selectedPreds.foreach(p => println(s"  Selected: ${p}"))
      //rel -> assignments
      rel -> selectedPreds.toSet
    }
    Representation(mapping)
  }

  /** Rename relation in trace with the recv_ prefix */
  private def renameTxRelationInTrace(old: EvaluatedTrace, keepConstructor: Boolean = false): EvaluatedTrace = {

    def toTxTriggerRelation(relation: Relation): Relation = {
      require(!relation.name.startsWith(transactionRelationPrefix), "Assuming non tx relation")
      relation match {
        case SimpleRelation(name, sig, memberNames) =>
          SimpleRelation(s"$transactionRelationPrefix$name", sig, memberNames)
        case SingletonRelation(name, sig, memberNames) => ???
        case relation: ReservedRelation => ???
      }
    }

    val newSteps = old.steps.map { case (tx, state) =>
      if (keepConstructor && tx.relation.name == "constructor") {
        (tx, state)
      } else {
        val triggerRelation = toTxTriggerRelation(tx.relation)
        (tx.updateRelation(triggerRelation), state)
      }
    }
    old.copy(steps = newSteps)
  }

  private case class EncodedTrace(pathConstraints: Seq[BoolExpr], acceptTrace: BoolExpr)
  private case class SymbolicPredicateEncoding(facts: Seq[BoolExpr], truth: BoolExpr)

  private def relationIndices(relation: Relation): List[Int] = relation match {
    case sr: SimpleRelation => interpreterContext.relationIndices.getOrElse(sr, List())
    case _: SingletonRelation => List()
    case _: ReservedRelation => List()
  }

  private def safeZ3Name(raw: String): String =
    raw.replaceAll("[^A-Za-z0-9_]", "_")

  private def traceConstName(traceId: String, orig: String, step: Int): String =
    s"${safeZ3Name(traceId)}_${safeZ3Name(orig)}_s$step"

  private def isBuiltinConstName(name: String): Boolean =
    name == "true" || name == "false" || name.matches("-?\\d+")

  private def collectConstsFrom(root: Expr[_]): Set[Expr[_]] = {
    val acc = mutable.HashSet.empty[Expr[_]]
    val visited = mutable.HashSet.empty[Expr[_]]
    val stack = mutable.Stack[(Expr[_], Int)]((root, 0))
    val maxDepth = 200
    while (stack.nonEmpty) {
      val (x, depth) = stack.pop()
      if (!visited.contains(x)) {
        visited += x
        try {
          if (x.isConst) {
            val name = x.getSExpr
            if (!isBuiltinConstName(name)) acc += x
          } else if (x.isQuantifier && depth < maxDepth) {
            stack.push((x.asInstanceOf[Quantifier].getBody, depth + 1))
          } else if (depth < maxDepth) {
            val args = try x.getArgs catch { case _: Throwable => Array.empty[Expr[_]] }
            if (args != null) {
              args.reverse.foreach(arg => stack.push((arg, depth + 1)))
            }
          }
        } catch {
          case _: Throwable =>
        }
      }
    }
    acc.toSet
  }

  private lazy val symbolicStateVars: Seq[(Expr[_], Expr[_])] =
    symbolicTransitionSystem.getVariables().toSeq

  private lazy val symbolicOtherConsts: Set[Expr[_]] = {
    val initConsts = collectConstsFrom(symbolicTransitionSystem.getInit())
    val trConsts = collectConstsFrom(symbolicTransitionSystem.getTr())
    val stateConsts = symbolicStateVars.flatMap { case (a, b) => Seq(a, b) }.toSet
    (initConsts ++ trConsts).filterNot(stateConsts.contains)
  }

  private val symbolicStepSubstCache =
    mutable.Map.empty[(String, Int), (Array[Expr[_]], Array[Expr[_]], Map[String, Expr[_]])]

  private def getStepSubst(traceId: String, step: Int): (Array[Expr[_]], Array[Expr[_]], Map[String, Expr[_]]) = {
    symbolicStepSubstCache.getOrElseUpdate((traceId, step), {
      var from = List.empty[Expr[_]]
      var to = List.empty[Expr[_]]
      val names = mutable.Map.empty[String, Expr[_]]

      for ((vIn, vOut) <- symbolicStateVars) {
        val inVar = z3ctx.mkConst(traceConstName(traceId, vIn.getSExpr, step), vIn.getSort.asInstanceOf[Sort])
        val outVar = z3ctx.mkConst(traceConstName(traceId, vIn.getSExpr, step + 1), vOut.getSort.asInstanceOf[Sort])
        from ::= vIn
        to ::= inVar
        from ::= vOut
        to ::= outVar
        names += (vIn.getSExpr -> inVar)
        names += (vOut.getSExpr -> outVar)
      }

      for (c <- symbolicOtherConsts) {
        val orig = c.getSExpr
        val renamed = z3ctx.mkConst(traceConstName(traceId, orig, step), c.getSort.asInstanceOf[Sort])
        from ::= c
        to ::= renamed
        names += (orig -> renamed)
      }

      (from.reverse.toArray, to.reverse.toArray, names.toMap)
    })
  }

  private def renameForTraceStep(e: Expr[_], traceId: String, step: Int): Expr[_] = {
    val (fromArr, toArr, _) = getStepSubst(traceId, step)
    e.substitute(fromArr, toArr)
  }

  private def stepConst(traceId: String, step: Int, originalName: String, sort: Sort): Expr[_] = {
    val (_, _, names) = getStepSubst(traceId, step)
    names.getOrElse(originalName, z3ctx.mkConst(traceConstName(traceId, originalName, step), sort))
  }

  private def constantExpr(c: Constant): Expr[_] =
    paramToConst(z3ctx, c, "")._1

  private lazy val symbolicTriggerIndicators: Map[Relation, Set[(IntExpr, Literal)]] = {
    val txInterfaces = programForEncoding.interfaces
      .filter(_.relation.name.startsWith(transactionRelationPrefix))
    txInterfaces.map { iface =>
      val triggeredRules = programForEncoding.rules.diff(programForEncoding.violationRules)
        .filter(r => r.body.exists(lit => lit.relation == iface.relation))
      val indicators = triggeredRules.zipWithIndex.map { case (triggeredRule, i) =>
        val const = indicatorConstForTransactionTriggerRelation(z3ctx, iface.relation, i)
        val triggerLiteral = triggeredRule.body
          .find(_.relation.name.startsWith(transactionRelationPrefix))
          .getOrElse(throw new IllegalArgumentException(s"No transaction literal found in $triggeredRule"))
        (const, triggerLiteral)
      }
      iface.relation -> indicators
    }.toMap
  }

  private def bindTxInputs(traceId: String, step: Int, tx: Transaction): Seq[BoolExpr] = {
    val indicators = symbolicTriggerIndicators.getOrElse(tx.relation, Set.empty)
    if (indicators.isEmpty) {
      throw new UnsupportedOperationException(s"Symbolic CEX blocking has no verifier trigger for ${tx.relation.name}")
    }

    val activeIndicator = z3ctx.mkOr(indicators.toSeq.map { case (indicator, _) =>
      z3ctx.mkEq(renameForTraceStep(indicator, traceId, step), z3ctx.mkInt(1))
    }: _*)

    val fieldBindings = indicators.toSeq.flatMap { case (_, triggerLiteral) =>
      triggerLiteral.fields.zip(tx.parameters).flatMap { case (field, value) =>
        if (field.name == "_") None
        else {
          val variable = stepConst(traceId, step, s"i0_${field.name}", typeToSort(z3ctx, field._type))
          Some(z3ctx.mkEq(variable, constantExpr(value)))
        }
      }
    }

    val msgSender = z3ctx.mkEq(
      stepConst(traceId, step, "msgSender", z3ctx.getIntSort),
      z3ctx.mkInt(tx.implicitParameters.msgSender))
    val msgValue = z3ctx.mkEq(
      stepConst(traceId, step, "msgValue", z3ctx.getIntSort),
      z3ctx.mkInt(tx.implicitParameters.value))

    activeIndicator +: (fieldBindings :+ msgSender :+ msgValue)
  }

  private def bindConstructorInputs(traceId: String, tx: Transaction): Seq[BoolExpr] = {
    val fieldBindings = tx.relation.paramList.zip(tx.parameters).flatMap { case (field, value) =>
      if (field.name == "_") None
      else {
        val variable = stepConst(traceId, 0, s"_${field.name}", typeToSort(z3ctx, field._type))
        Some(z3ctx.mkEq(variable, constantExpr(value)))
      }
    }
    val msgSender = z3ctx.mkEq(
      stepConst(traceId, 0, "msgSender", z3ctx.getIntSort),
      z3ctx.mkInt(tx.implicitParameters.msgSender))
    val msgValue = z3ctx.mkEq(
      stepConst(traceId, 0, "msgValue", z3ctx.getIntSort),
      z3ctx.mkInt(tx.implicitParameters.value))
    fieldBindings :+ msgSender :+ msgValue
  }

  private def symbolicPathConstraints(trace: EvaluatedTrace, traceId: String): Seq[BoolExpr] = {
    val init = renameForTraceStep(symbolicTransitionSystem.getInit(), traceId, 0).asInstanceOf[BoolExpr]
    val (constructorBindings, nonConstructorSteps) = trace.steps.headOption match {
      case Some((tx, _)) if tx.relation.name == "constructor" =>
        (bindConstructorInputs(traceId, tx), trace.steps.tail)
      case _ =>
        (Seq.empty[BoolExpr], trace.steps)
    }
    val transitions = nonConstructorSteps.zipWithIndex.flatMap { case ((tx, _), step) =>
      val tr = renameForTraceStep(symbolicTransitionSystem.getTr(), traceId, step).asInstanceOf[BoolExpr]
      tr +: bindTxInputs(traceId, step, tx)
    }
    (init +: constructorBindings) ++ transitions
  }

  private def constToInt(c: Constant): Int = c.name match {
    case "true" | "1" => 1
    case "false" | "0" => 0
    case other => other.toInt
  }

  private def bindPredicateInputs(prefix: String, tx: Transaction, predicate: Predicate): Seq[BoolExpr] = {
    val txBindings = predicate.context.tx.fields.zip(tx.parameters).flatMap { case (field, value) =>
      if (field.name == "_") None
      else Some(z3ctx.mkEq(paramToConst(z3ctx, field, prefix)._1, constantExpr(value)))
    }

    val msgSender = z3ctx.mkEq(
      paramToConst(z3ctx, synthesis.Context.msgSender.fields.head, prefix)._1,
      z3ctx.mkInt(tx.implicitParameters.msgSender))
    val msgValue = z3ctx.mkEq(
      paramToConst(z3ctx, synthesis.Context.msgValue.fields.head, prefix)._1,
      z3ctx.mkInt(tx.implicitParameters.value))

    txBindings :+ msgSender :+ msgValue
  }

  private def concreteScalarBindings(state: State, tx: Transaction, predicate: Predicate): Map[String, Int] = {
    val txBindings = predicate.context.tx.fields.zip(tx.parameters).collect {
      case (field, value) if field.name != "_" => field.name -> constToInt(value)
    }.toMap
    val implicitBindings = Map(
      synthesis.Context.msgSender.fields.head.name -> tx.implicitParameters.msgSender,
      synthesis.Context.msgValue.fields.head.name -> tx.implicitParameters.value)
    val singletonBindings = predicate.context.bindingLiterals.collect {
      case lit if lit.relation.isInstanceOf[SingletonRelation] && lit.fields.nonEmpty =>
        lit.fields.head.name -> state.lookupSingleton(lit.relation.name)
    }.toMap

    txBindings ++ implicitBindings ++ singletonBindings
  }

  private def resolveConcreteInt(param: Parameter,
                                 state: State,
                                 scalars: Map[String, Int]): Int = param match {
    case c: Constant => constToInt(c)
    case Variable(_, name) => scalars.getOrElse(name, state.lookup(name))
  }

  private def concreteStateFacts(prefix: String,
                                 state: State,
                                 predicate: Predicate,
                                 traceId: String,
                                 step: Int,
                                 tx: Transaction): Seq[BoolExpr] = {
    val scalars = concreteScalarBindings(state, tx, predicate)
    predicate.context.bindingLiterals.flatMap {
      case lit@Literal(sr: SimpleRelation, _) =>
        val indices = relationIndices(sr)
        val (keyParams, valueParam) = interpreter.extractKeyValueVar(lit)
        val keyValues = keyParams.map(p => resolveConcreteInt(p, state, scalars))
        val concreteValue = state.lookup(sr, keyValues)

        val arraySort = getSort(z3ctx, sr, indices).asInstanceOf[ArraySort[Sort, Sort]]
        val arrayConst = stepConst(traceId, step, sr.name, arraySort).asInstanceOf[Expr[ArraySort[Sort, Sort]]]
        val keyConsts = keyParams.zip(keyValues).map { case (p, value) =>
          constantExpr(Constant(p._type, value.toString))
        }.toArray
        val keyExpr = mkTupleKey(z3ctx, arraySort.getDomain, keyConsts)
        val valueExpr = constantExpr(Constant(valueParam._type, concreteValue.toString))
        Seq(z3ctx.mkEq(
          z3ctx.mkSelect(arrayConst, keyExpr.asInstanceOf[Expr[Sort]]),
          valueExpr))

      case lit if lit.relation.isInstanceOf[SingletonRelation] && lit.fields.nonEmpty =>
        val value = state.lookupSingleton(lit.relation.name)
        val sort = getSort(z3ctx, lit.relation, relationIndices(lit.relation))
        val relationConst = stepConst(traceId, step, lit.relation.name, sort)
        Seq(z3ctx.mkEq(relationConst, constantExpr(Constant(lit.fields.head._type, value.toString))))

      case _ =>
        Seq.empty
    }.toSeq
  }

  private def predicateLiteralToConst(lit: Literal,
                                      indices: List[Int],
                                      prefix: String): BoolExpr = {
    lit.relation match {
      case sr: SimpleRelation =>
        val keys = indices.map(i => lit.fields(i))
        val valueIndices = lit.fields.indices.filterNot(i => indices.contains(i)).toList
        val values = valueIndices.map(i => lit.fields(i))
        val fieldNames = valueIndices.map(i => lit.relation.memberNames(i))

        if (keys.nonEmpty && values.nonEmpty) {
          val (valueConst, _) = fieldsToConst(z3ctx, lit.relation, values, fieldNames, prefix)
          val sort = getSort(z3ctx, lit.relation, indices).asInstanceOf[ArraySort[Sort, Sort]]
          val arrayConst = z3ctx.mkConst(sr.name, sort).asInstanceOf[Expr[ArraySort[Sort, Sort]]]
          val keyConsts = keys.toArray.map(f => paramToConst(z3ctx, f, prefix)._1)
          val keyExpr = mkTupleKey(z3ctx, sort.getDomain, keyConsts)
          z3ctx.mkEq(
            z3ctx.mkSelect(arrayConst, keyExpr.asInstanceOf[Expr[Sort]]),
            valueConst)
        } else {
          literalToConst(z3ctx, lit, indices, prefix)
        }

      case _ =>
        literalToConst(z3ctx, lit, indices, prefix)
    }
  }

  private def symbolicPredicateEncoding(predicate: Predicate,
                                        stateBefore: State,
                                        tx: Transaction,
                                        traceId: String,
                                        step: Int,
                                        predIdx: Int): SymbolicPredicateEncoding = {
    val prefix = s"${safeZ3Name(traceId)}_t${step}_p$predIdx"
    val inputBindings = bindPredicateInputs(prefix, tx, predicate)
    val bindingExprs = predicate.context.bindingLiterals.map { lit =>
      val expr = predicateLiteralToConst(lit, relationIndices(lit.relation), prefix)
      renameForTraceStep(expr, traceId, step).asInstanceOf[BoolExpr]
    }.toSeq
    val functorExpr = functorToZ3(z3ctx, predicate.functor, prefix)
    SymbolicPredicateEncoding(inputBindings ++ bindingExprs, functorExpr)
  }

  private def encodeConcreteTrace(trace: EvaluatedTrace): EncodedTrace = {
    val evalResults = evaluatePredicates(trace)
    EncodedTrace(Seq.empty, makeAcceptExpr(evalResults))
  }

  private def encodeSymbolicTrace(trace: EvaluatedTrace, traceId: String): EncodedTrace = {
    val path = symbolicPathConstraints(trace, traceId)
    val predicateFacts = mutable.ListBuffer.empty[BoolExpr]
    val txAndStates = trace.iterateTxAndStateBefore.toSeq
      .filterNot { case (_, tx) => tx.relation.name == "constructor" }
    val accepts = txAndStates.zipWithIndex.map { case ((stateBefore, tx), step) =>
      val preds = orderedPredicates.getOrElse(tx.relation, List.empty)
      val boolVars = encodings.getOrElse(tx.relation, List.empty)
      val assertions = preds.zipWithIndex.map { case (predicate, i) =>
        val encoded = symbolicPredicateEncoding(predicate, stateBefore, tx, traceId, step, i)
        predicateFacts ++= encoded.facts
        z3ctx.mkImplies(boolVars(i), encoded.truth)
      }
      if (assertions.nonEmpty) z3ctx.mkAnd(assertions: _*) else z3ctx.mkTrue()
    }
    val acceptTrace = if (accepts.nonEmpty) z3ctx.mkAnd(accepts: _*) else z3ctx.mkTrue()
    EncodedTrace(path ++ predicateFacts, acceptTrace)
  }

  private def encodeSafetyTrace(trace: EvaluatedTrace, traceId: String): EncodedTrace = {
    encodingMode match {
      case EncodingMode.Concrete => encodeConcreteTrace(trace)
      case EncodingMode.SymbolicCexBlocking => encodeSymbolicTrace(trace, traceId)
    }
  }

  private def blockTraceConstraint(encoded: EncodedTrace): BoolExpr =
    z3ctx.mkNot(encoded.acceptTrace)

  private def fixedSelectionConstraints(selection: Representation): Seq[BoolExpr] = {
    encodings.toSeq.flatMap { case (rel, boolVars) =>
      val selected = selection.getPredicates(rel)
      val preds = orderedPredicates.getOrElse(rel, List.empty)
      preds.zip(boolVars).map { case (predicate, boolVar) =>
        if (selected.contains(predicate)) boolVar else z3ctx.mkNot(boolVar)
      }
    }
  }

  private def candidateBlocksAllSafetyTraces(selection: Representation,
                                             traces: List[EvaluatedTrace],
                                             rel: Relation): Boolean = {
    if (encodingMode == EncodingMode.Concrete) return true

    traces.zipWithIndex.forall { case (trace, i) =>
      val encoded = encodeSafetyTrace(trace, s"validate_${safeZ3Name(rel.name)}_$i")
      val solver = z3ctx.mkSolver()
      val params = z3ctx.mkParams()
      params.add("timeout", 10000)
      params.add("smt.mbqi", true)
      solver.setParameters(params)
      encoded.pathConstraints.foreach(c => solver.add(c))
      fixedSelectionConstraints(selection).foreach(c => solver.add(c))
      solver.add(encoded.acceptTrace)
      val status = solver.check()
      status match {
        case com.microsoft.z3.Status.UNSATISFIABLE => true
        case com.microsoft.z3.Status.SATISFIABLE =>
          println(s"[candidateValidation] rejected candidate for ${rel.name}: still accepts CEX trace $i")
          false
        case other =>
          println(s"[candidateValidation] rejected candidate for ${rel.name}: validation returned $other on CEX trace $i")
          false
      }
    }
  }

  def synthesize(sketch: Program,
                 evaluatedTraces: List[EvaluatedTrace],
                 maxSolutions: Int,
                 disambiguationTraces: Set[EvaluatedTrace]): Program = {
    // Remove the first constructor transaction from each trace if present
    def stripConstructor(trace: EvaluatedTrace): EvaluatedTrace = {
      val steps = trace.steps
      if (steps.nonEmpty && steps.head._1.relation.name == "constructor") {
        val initState = steps.head._2
        trace.copy(initialState = initState, steps = steps.tail)
      } else
        trace
    }

    val renamedDisambiguationTrace = {
      val strippedDisambiguationTraces = disambiguationTraces.map(stripConstructor)
      strippedDisambiguationTraces.map(t => renameTxRelationInTrace(t))
    }
    val renamedSafetyTrace = {
      encodingMode match {
        case EncodingMode.SymbolicCexBlocking =>
          evaluatedTraces.map(t => renameTxRelationInTrace(t, keepConstructor = true))
        case EncodingMode.Concrete =>
          val strippedSafetyTraces = evaluatedTraces.map(stripConstructor)
          strippedSafetyTraces.map(renameTxRelationInTrace(_))
      }
    }

    // val renamedDisambiguationTrace = disambiguationTraces.map(renameTxRelationInTrace)
    // val renamedSafetyTrace = evaluatedTraces.map(renameTxRelationInTrace)

    // val candidates = synthesizeMultiSolution(renamedSafetyTrace, maxSolutions, renamedDisambiguationTrace)
    // val selection = disambiguate(renamedDisambiguationTrace, candidates)
    // Build map of already-baked-in functors per tx relation (from augmented sketch rules)
    val existingFunctorsPerRel: Map[Relation, Set[Functor]] = {
      sketch.rules.diff(sketch.violationRules).flatMap { r =>
        val txLitOpt = try { Some(PredicateEnumerator.extractTxLiteral(r)) } catch { case _: Throwable => None }
        txLitOpt.map(lit => lit.relation -> r.functors)
      }.toMap
    }

    val selection = synthesizePerRelation(renamedSafetyTrace, maxSolutions,
      renamedDisambiguationTrace, existingFunctorsPerRel)
    makeProgram(sketch, selection)
  }

  // debug info
  private def filterAndRankCandidates(
                                       evaluatedTraces: List[EvaluatedTrace],
                                       disambiguationTraces: Set[EvaluatedTrace]
                                     ): Map[Rule, List[(Predicate, Boolean, Int)]] = {

    predicatesPerRule.map { case (rule, preds) =>
      // Safely extract the transaction relation for the rule; if not present, treat as no occurrences.
      val txRelationOpt = try {
        Some(PredicateEnumerator.extractTxLiteral(rule).relation)
      } catch {
        case _: Throwable => None
      }

      val infos = preds.toList.flatMap { p =>
        val (total, trueCount) = txRelationOpt match {
          case Some(txRel) =>
            val evalResults: Seq[Boolean] = evaluatedTraces.toSeq.flatMap { trace =>
              trace.iterateTxAndStateBefore.collect {
                case (st, tx) if tx.relation == txRel => interpreter.evaluate(st, tx, p) }
            }
            (evalResults.size, evalResults.count(identity))
          case None =>
            (0, 0)
        }

        // Predicate blocks at least one occurrence iff it's observed and not always true
        val blocksEvaluatedTrace = total > 0 && trueCount != total

        val permissiveness = txRelationOpt match {
          case Some(txRel) =>
            disambiguationTraces.count { trace =>
              val occ = trace.iterateTxAndStateBefore.collect {
                case (st, tx) if tx.relation == txRel => (st, tx)
              }
              occ.nonEmpty && occ.forall {
                case (st, tx) => interpreter.evaluate(st, tx, p)
              }
            }
          case None => 0
        }
        Some((p, blocksEvaluatedTrace, permissiveness))
      }.sortBy { case (_, _, perm) => -perm }

      rule -> infos
    }
  }


  /** Synthesize predicates for each relation independently, then combine best selections. */
  private def synthesizePerRelation(evaluatedTraces: List[EvaluatedTrace],
                                       maxSolutions: Int,
                                       disambiguationTraces: Set[EvaluatedTrace],
                                       existingFunctorsPerRel: Map[Relation, Set[Functor]] = Map.empty
                                     ): Representation = {
    // Group traces by last transaction relation
    val grouped: Map[Relation, List[EvaluatedTrace]] =
      evaluatedTraces.groupBy(_.steps.last._1.relation)

    // For each relation, synthesize and disambiguate
    val allSolutions: Map[Relation, List[Representation]] = grouped.map { case (rel, traces) =>
      synthesisCache.get(traces) match {
        case Some(cached) =>
          println(s"[synthesizePerRelation] Using cached synthesis for relation ${rel.name}")
          rel -> cached
        case None => {
          val solver = z3ctx.mkOptimize()
          val traceBlockingStart = System.currentTimeMillis()
          val safetyEncodings = traces.zipWithIndex.map { case (t, i) =>
            encodeSafetyTrace(t, s"cex_${rel.name}_$i")
          }
          safetyEncodings.flatMap(_.pathConstraints).foreach(c => solver.Add(c))
          val traceConstraints = safetyEncodings.map(blockTraceConstraint)
          val constraint = z3ctx.mkAnd(traceConstraints.toSeq: _*)
          solver.Add(constraint)
          traceBlockingTimeMs += System.currentTimeMillis() - traceBlockingStart

          // Block all other relations
          val otherRelations = encodings.keySet - rel
          val blockOthers = otherRelations.flatMap(encodings).map(z3ctx.mkNot)
          if (blockOthers.nonEmpty) solver.Add(z3ctx.mkAnd(blockOthers.toSeq: _*))

          // Anti-contradiction constraints: two predicates that are logically contradictory
          // cannot both be selected in the same rule.
          def areContradictory(f1: Functor, f2: Functor): Boolean = {
            // Check exact negation pairs (a > b / a <= b, a == b / a != b, etc.)
            val isNegation = try { Functor.negate(f1) == f2 } catch { case _: Exception => false }
            if (isNegation) return true
            // Opposing strict inequalities on same operands: a > b AND a < b
            (f1, f2) match {
              case (datalog.Greater(a1, b1), datalog.Lesser(a2, b2)) if a1 == a2 && b1 == b2 => return true
              case (datalog.Lesser(a1, b1), datalog.Greater(a2, b2)) if a1 == a2 && b1 == b2 => return true
              case _ =>
            }
            // Strict inequality with equality on same operands (symmetric): a > b AND a == b, a < b AND a == b
            (f1, f2) match {
              case (datalog.Greater(a1, b1), datalog.Equal(a2, b2)) if (a1==a2&&b1==b2)||(a1==b2&&b1==a2) => return true
              case (datalog.Equal(a1, b1),   datalog.Greater(a2, b2)) if (a2==a1&&b2==b1)||(a2==b1&&b2==a1) => return true
              case (datalog.Lesser(a1, b1),  datalog.Equal(a2, b2)) if (a1==a2&&b1==b2)||(a1==b2&&b1==a2) => return true
              case (datalog.Equal(a1, b1),   datalog.Lesser(a2, b2)) if (a2==a1&&b2==b1)||(a2==b1&&b2==a1) => return true
              case _ =>
            }
            false
          }
          val predsForRel = orderedPredicates.getOrElse(rel, List.empty)
          val boolVarsForRel = encodings.getOrElse(rel, List.empty)
          val predsWithVars = predsForRel.zip(boolVarsForRel)
          // Candidate-vs-candidate: no two contradictory candidates can both be selected
          for {
            ((pi, vi), i) <- predsWithVars.zipWithIndex
            ((pj, vj), j) <- predsWithVars.zipWithIndex
            if i < j
            if areContradictory(pi.functor, pj.functor)
          } {
            solver.Add(z3ctx.mkNot(z3ctx.mkAnd(vi, vj)))
          }
          // Candidate-vs-existing: exclude any candidate that contradicts a functor
          // already baked into the sketch rule for this relation.
          val existingFunctors = existingFunctorsPerRel.getOrElse(rel, Set.empty)
          for {
            (pi, vi) <- predsWithVars
            ef <- existingFunctors
            if areContradictory(pi.functor, ef)
          } {
            solver.Add(z3ctx.mkNot(vi))
          }

          // Maximize permissiveness for this relation
          val permissivenessObjective = makePermissivenessObjective(disambiguationTraces, encodings, z3ctx)
          solver.MkMaximize(permissivenessObjective)

          var selections = List.empty[Representation]
          var found = 0
          def timedCheck(): com.microsoft.z3.Status = {
            val solverStart = System.currentTimeMillis()
            val status = solver.Check()
            inductiveSolverTimeMs += System.currentTimeMillis() - solverStart
            status
          }
          var solverStatus = timedCheck()
          while (found < maxSolutions && solverStatus == com.microsoft.z3.Status.SATISFIABLE) {
            val model = solver.getModel
            val selection = interpretModel(model)

            // Block this model for next candidate
            val block = encodings.flatMap { case (_, boolVars) =>
              boolVars.map { v =>
                val value = model.eval(v, true)
                if (value.isTrue) z3ctx.mkNot(v) else v
              }
            }.toSeq
            solver.Add(z3ctx.mkOr(block: _*))

            if (candidateBlocksAllSafetyTraces(selection, traces, rel)) {
              selections :+= selection
              found += 1
            }
            if (found < maxSolutions) solverStatus = timedCheck()
          }
          synthesisCache.update(traces,selections)
          rel -> selections
        }
      }
    }

    // Disambiguate for each relation after collecting all solutions
    val bestSelections: Map[Relation, Set[Predicate]] = allSolutions.map { case (rel, candidates) =>
      if (candidates.isEmpty) {
        println(s"[synthesizePerRelation] No validated candidates for relation ${rel.name}")
        rel -> Set.empty[Predicate]
      } else {
        val best = disambiguate(disambiguationTraces, candidates)
        rel -> best.getPredicates(rel)
      }
    }

    // Combine best selections for all relations, defaulting to empty set for missing keys
    val allRelations = encodings.keySet
    val completeSelections: Map[Relation, Set[Predicate]] = allRelations.map { rel =>
      rel -> bestSelections.getOrElse(rel, Set.empty[Predicate])
    }.toMap

    /** Turn on for debugging. */
    val debugInfo = filterAndRankCandidates(evaluatedTraces, disambiguationTraces)
    // println(debugInfo)


    Representation(completeSelections)
  }

  private def permissiveness(disambiguationTraces: Set[EvaluatedTrace], repr: Representation): Int = {

    def accept(trace: EvaluatedTrace, repr: Representation): Boolean = {
      trace.iterateTxAndStateBefore.forall {
        case (state, tx) =>
          val predicates = repr.getPredicates(tx.relation)
          val accepts = predicates.map(p => p -> interpreter.evaluate(state, tx, p)).toMap
          predicates.forall(accepts)
      }
    }

    // val renamedTrace = disambiguationTraces.map(renameTxRelationInTrace)
    disambiguationTraces.count(t => accept(t, repr))
  }

  private def disambiguate(disambiguationTraces: Set[EvaluatedTrace],
                           candidates: List[Representation]): Representation = {

    val permissivenessScores: Map[Representation, Int] = {
      candidates.map(c => c -> permissiveness(disambiguationTraces, c)).toMap
    }
    // val best = candidates.maxBy(permissivenessScores)
    // val bestScore = permissivenessScores(best)
    // if (bestScore == 0) {
    //   println(s"[Warning]")
    // }
    // println(s"Selected $best with permissive score: $bestScore / ${disambiguationTraces.size}.")
    val maxScore = permissivenessScores.values.max
    val bestCandidates = candidates.filter(c => permissivenessScores(c) == maxScore)

    def numPredicates(repr: Representation): Int =
      repr.map.values.map(_.size).sum

    val minPredCount = bestCandidates.map(numPredicates).min
    val finalCandidates = bestCandidates.filter(c => numPredicates(c) == minPredCount)
    val best = finalCandidates.head
    println(s"Selected $best with permissive score: $maxScore / ${disambiguationTraces.size}, min predicates: $minPredCount.")

    best
  }

  /** Perform the synthesis given EvaluatedTraces and predicates, returning up to maxSolutions programs. */
  private def synthesizeMultiSolution(evaluatedTraces: Set[EvaluatedTrace],
                                       maxSolutions: Int,
                                       disambiguationTraces: Set[EvaluatedTrace]
                                     ): List[Representation] = {
    val traceConstraints = evaluatedTraces.map(t => {
      val evalResults = evaluatePredicates(t)
      makeConstraints(evalResults).asInstanceOf[Expr[BoolSort]]
    })
    val constraint = z3ctx.mkAnd(traceConstraints.toSeq: _*)
    val solver = z3ctx.mkOptimize()
    solver.Add(constraint)

    /** Metric: maximize permissiveness */
    val permissivenessObjective = makePermissivenessObjective(disambiguationTraces, encodings, z3ctx)
    solver.MkMaximize(permissivenessObjective)

    var solutions = List.empty[Representation]
    var found = 0

    while (found < maxSolutions && solver.Check() == com.microsoft.z3.Status.SATISFIABLE) {
      val model = solver.getModel
      val selection = interpretModel(model)

      val block = encodings.flatMap { case (_, boolVars) =>
        boolVars.map { v =>
          val value = model.eval(v, true)
          if (value.isTrue) z3ctx.mkNot(v) else v
        }
      }.toSeq
      solver.Add(z3ctx.mkOr(block: _*))

      solutions = solutions :+ selection
      found += 1
    }
    solutions
  }

  /** Perform the synthesis given EvaluatedTraces and predicates, returning up to maxSolutions programs. */
  private def synthesizeMultiSolutionBatch(evaluatedTraces: Set[EvaluatedTrace], maxSolutions: Int, disambiguationTraces: Set[EvaluatedTrace]): List[Representation] = {
    val traceConstraints = evaluatedTraces.map(t => {
      val evalResults = evaluatePredicates(t)
      makeConstraints(evalResults).asInstanceOf[Expr[BoolSort]]
    })
    val constraint = z3ctx.mkAnd(traceConstraints.toSeq: _*)
    // val solver = z3ctx.mkSolver()
    val solver = z3ctx.mkOptimize()
    solver.Add(constraint)

    /** Metric: minimize the number of selected predicats */
   //  val numSelection = {
   //    def boolToInt(b: BoolExpr): IntExpr = z3ctx.mkITE(b, z3ctx.mkInt(1), z3ctx.mkInt(0)).asInstanceOf[IntExpr]

   //    val boolVars = encodings.flatMap(_._2).toSeq
   //    z3ctx.mkAdd(boolVars.map(boolToInt): _*)
   //  }
   //
   //  solver.MkMinimize(numSelection)

    /** New metric: maximize permissiveness */
    val permissivenessObjective = makePermissivenessObjective(disambiguationTraces, encodings, z3ctx)
    solver.MkMaximize(permissivenessObjective)

    val batchSize = 50
    var solutions = List.empty[Representation]
    var found = 0

    while (found < maxSolutions && solver.Check() == com.microsoft.z3.Status.SATISFIABLE) {
      var batch = List.empty[Representation]
      var batchCount = 0

      // Collect a batch of candidate models
      while (batchCount < batchSize && solver.Check() == com.microsoft.z3.Status.SATISFIABLE) {
        val model = solver.getModel
        val selection = interpretModel(model)
        batch +:= selection

        // Block this model for next candidate
        val block = encodings.flatMap { case (_, boolVars) =>
          boolVars.map { v =>
            val value = model.eval(v, true)
            if (value.isTrue) z3ctx.mkNot(v) else v
          }
        }.toSeq
        solver.Add(z3ctx.mkOr(block: _*))

        batchCount += 1
      }

      // Validate and block common predicates in zero-score batch
      val (validSelections, blockClause) =
        validateAndBlockZeroScore(disambiguationTraces, batch, encodings, z3ctx)

      if (blockClause != null && !blockClause.isFalse) {
        solver.Add(blockClause)
      }

      // Add valid selections to solutions
      solutions ++= validSelections
      found += validSelections.size
    }

    if (solutions.isEmpty) {
      println(s"[synthesize] No solution found.")
    }

    /** single mode */

    // val (_, blockAlwaysFalsePredicates) = constantFalsePredicates(disambiguationTraces)
    // solver.Add(blockAlwaysFalsePredicates)

    // var solutions = List.empty[Representation]
    // var found = 0

    // while (found < maxSolutions && solver.Check() == com.microsoft.z3.Status.SATISFIABLE) {
    //   val model = solver.getModel
    //   val selection = interpretModel(model)

    //   // Add blocking clause to prevent finding the same model again
    //   val block = encodings.flatMap { case (_, boolVars) =>
    //     boolVars.map { v =>
    //       val value = model.eval(v, true)
    //       if (value.isTrue) z3ctx.mkNot(v) else v
    //     }
    //   }.toSeq
    //   solver.Add(z3ctx.mkOr(block: _*))

    //   val validated: Boolean = validate(disambiguationTraces, selection)
    //   if (validated) {
    //     println(s"Found ${found} solutions.")
    //     solutions = solutions :+ selection
    //     found += 1
    //   }
    // }
    solutions
  }

  private def makePermissivenessObjective(
                                           disambiguationTraces: Set[EvaluatedTrace],
                                           encodings: Map[Relation, List[BoolExpr]],
                                           z3ctx: Context
                                         ): IntExpr = {
    def boolToInt(b: BoolExpr): IntExpr = z3ctx.mkITE(b, z3ctx.mkInt(1), z3ctx.mkInt(0)).asInstanceOf[IntExpr]

    val disambigAcceptExprs: Seq[BoolExpr] = disambiguationTraces.toSeq.map { trace =>
      val evalResults = evaluatePredicates(trace)
      val txAccepts = evalResults.map { case (tx, boolList) =>
        val boolVars = encodings(tx.relation)
        val assertions = boolList.zipWithIndex.map { case (b, i) =>
          val premise = boolVars(i)
          val conclusion = z3ctx.mkBool(b)
          z3ctx.mkImplies(premise, conclusion)
        }
        z3ctx.mkAnd(assertions: _*)
      }
      z3ctx.mkAnd(txAccepts: _*)
    }

    val permissiveness: Seq[IntExpr] = disambigAcceptExprs.map(boolToInt)

    // z3ctx.mkAdd(permissiveness: _*).asInstanceOf[IntExpr]
    val permissivenessSum = z3ctx.mkAdd(permissiveness: _*).asInstanceOf[IntExpr]

    // Penalize by number of binding literals in each predicate
    val allBoolVars = encodings.values.flatten.toSeq
    // val numSelectedPredicates = z3ctx.mkAdd(allBoolVars.map(boolToInt): _*).asInstanceOf[IntExpr]
    // val penalty = z3ctx.mkMul(z3ctx.mkInt(1), numSelectedPredicates).asInstanceOf[IntExpr]
    val penaltyTerms = allBoolVars.map { b =>
      val boolExprToPredicate: Map[BoolExpr, Predicate] = encodings.flatMap { case (rel, boolVars) =>
        val preds = orderedPredicates(rel)
        boolVars.zip(preds)
      }.toMap
      val predicate = boolExprToPredicate(b)
      val bindingCount = predicate.context.bindingLiterals.size
      z3ctx.mkMul(z3ctx.mkInt(bindingCount), boolToInt(b)).asInstanceOf[IntExpr]
    }
    val penalty = z3ctx.mkAdd(penaltyTerms: _*).asInstanceOf[IntExpr]


    // Objective: maximize permissiveness - penalty
    z3ctx.mkSub(permissivenessSum, penalty).asInstanceOf[IntExpr]
  }


  private def makeProgram(sketch: Program, repr: Representation): Program = {
    // For each rule in the sketch, if it is a transaction rule, replace it with a rule
    // that includes the selected predicates' binding literals in the body and predicate functors
    // in the rule's functors set. Non-transaction rules are kept as-is.

    val newRules: Set[Rule] = sketch.rules.diff(sketch.violationRules).map { r =>
      // Check if this rule is a transaction rule by finding its transaction literal (if any)
      val txLiteralOpt = try {
        Some(PredicateEnumerator.extractTxLiteral(r))
      } catch {
        case _: Throwable => None
      }

      txLiteralOpt match {
        case Some(txLit) => {
          // // Find selected predicates for the transaction relation
          // val rel = txLit.relation
          // val selection: List[Boolean] = predicateSelection.getOrElse(rel, List.empty)
          // val candidates: List[Predicate] = predicates.getOrElse(rel, Set.empty).toList

          // // Pair candidates with selection booleans; if selection shorter than candidates, treat missing as false
          // val selectedPreds: Set[Predicate] = candidates.zipAll(selection, null, false)
          //   .collect { case (p: Predicate, true) => p }.toSet
          val selectedPreds = repr.getPredicates(txLit.relation)
          val newRule = makeRule(r, selectedPreds)
          // println(s"[makeProgram] selected predicates: $selectedPreds")
          println(s"[makeProgram] new rule: $newRule")
          newRule
        }
        case None => r
      }
    }

    // Reuse program metadata from sketch
    // datalog.Program(newRules, sketch.interfaces, sketch.relationIndices, sketch.functions, sketch.violations, sketch.name)
    sketch.copy(rules = newRules++sketch.violationRules)
  }

  def augmentSketchWithPredicates(sketch: Program,
                                  candidates: Map[Rule, Predicate],
                                 ): Program = {
    val predicatesPerRelation: Map[Relation, Set[Predicate]] = candidates.groupBy {
      case (rule, predicate) => PredicateEnumerator.extractTxLiteral(rule).relation
    }.mapValues(_.values.toSet).toMap

    val newProgram = makeProgram(sketch, Representation(predicatesPerRelation))
    newProgram
  }

  private def makeRule(sketchRule: Rule, predicates: Set[Predicate]): Rule = {

    def _resolveCollision(_preds: Set[Predicate]): Set[Predicate] = {
      val groups = _preds.groupBy(_.context.bindingLiterals)
      if (groups.size == 1) return _preds

      var renamedPredicates = Set[Predicate]()
      // using one idx per group
      for ((group, idx) <- groups.zipWithIndex) {
        val (bindingLits, predGroup) = group
        val renameMapping = bindingLits.map(lit => {
          val (_, valueParam) = interpreter.extractKeyValueVar(lit)
          val newName: String = s"${valueParam.name}_$idx"
          val newParameter = valueParam match {
            case _: Constant => throw new Exception(s"Expected variable at binding literal: $lit")
            case v: Variable => v.copy(name = newName)
          }
          valueParam -> newParameter
        }).toMap

        val renamedGroup = predGroup.map(_.rename(renameMapping))
        renamedPredicates ++= renamedGroup
      }
      renamedPredicates
    }

    val renamedPredicates = _resolveCollision(predicates)
    val bindingLits = renamedPredicates.flatMap(p => p.context.bindingLiterals)

    // Collect all predicate functors
    val predicateFunctors: Set[datalog.Functor] = renamedPredicates.map(_.functor)

    // New body: original body plus binding literals (avoid duplicates)
    val newBody: Set[datalog.Literal] = sketchRule.body ++ bindingLits

    // New functors: original functors plus selected predicate functors
    val newFunctors: Set[datalog.Functor] = sketchRule.functors ++ predicateFunctors

    // if predicate refer to variable in the context literals,
    // add those literal to the rule as well.
    val addMsgSender: Set[datalog.Literal] = if (renamedPredicates.exists(_.referredMsgSender())) Set(synthesis.Context.msgSender) else Set.empty
    val addMsgValue: Set[datalog.Literal] = if (renamedPredicates.exists(_.referredMsgValue())) Set(synthesis.Context.msgValue) else Set.empty

    // Combine bodies: original body + binding literals + possible implicit context literals
    val finalBody: Set[datalog.Literal] = newBody ++ addMsgSender ++ addMsgValue

    // Keep aggregators unchanged
    val newAggregators = sketchRule.aggregators

    Rule(sketchRule.head, finalBody, newFunctors, newAggregators)
  }

  /** Validate the synthesis results. */
  def validate(disambiguationTrace: Set[EvaluatedTrace], selection: Representation): Boolean = {
    val score = permissiveness(disambiguationTrace, selection)
    score > 0
  }

  def validateAndBlockZeroScore(
    disambiguationTraces: Set[EvaluatedTrace],
    selectionBatch: List[Representation],
    encodings: Map[Relation, List[BoolExpr]],
    z3ctx: Context
  ): (List[Representation], BoolExpr) = {
    // Compute scores
    val scores = selectionBatch.map(sel => sel -> permissiveness(disambiguationTraces, sel)).toMap
    val nonZeroSelections = scores.filter(_._2 > 0).keys
    val zeroSelections = scores.filter(_._2 == 0).keys

    println(s"${zeroSelections.size} 0 permissive program found.")

    // For each zero-permissive selection, find minimal blocking subset
    val minimalBlocks = zeroSelections.toList.flatMap { sel =>
      val allSelected: Set[(Relation, Predicate)] = encodings.keys.flatMap { rel =>
        sel.getPredicates(rel).map(p => (rel, p))
      }.toSet

      // Greedy minimization: try removing each predicate and check permissiveness
      def isStillZero(subset: Set[(Relation, Predicate)]): Boolean = {
        val testSel = new Representation(encodings.keys.map { rel =>
          val preds = subset.collect { case (`rel`, p) => p }
          rel -> preds
        }.toMap)
        permissiveness(disambiguationTraces, testSel) == 0
      }

      // Try to minimize the subset
      var minimal = allSelected
      for ((rel, p) <- allSelected) {
        val candidate = minimal - ((rel, p))
        if (candidate.nonEmpty && isStillZero(candidate)) {
          minimal = candidate
        }
      }
      if (minimal.nonEmpty) Some(minimal) else None
    }

    // Create blocking clauses for each minimal subset
    val blockClauses = minimalBlocks.map { subset =>
      val vars = subset.flatMap { case (rel, p) =>
        val idx = orderedPredicates(rel).indexOf(p)
        if (idx >= 0) Some(encodings(rel)(idx)) else None
      }
      // At least one must be false
      // z3ctx.mkOr(vars.map(z3ctx.mkNot).toSeq: _*)
      z3ctx.mkOr(vars.map(z3ctx.mkNot).toSeq: _*)
    }

    println(s"Block ${blockClauses.size} claues.")

    val block = if (blockClauses.nonEmpty) z3ctx.mkAnd(blockClauses: _*) else z3ctx.mkFalse()

    (nonZeroSelections.toList, block)
  }

  def constantFalsePredicates(disambiguationTrace: Set[EvaluatedTrace]): (Set[Predicate], BoolExpr) = {
    // For each relation, check which predicates are always false
    val alwaysFalsePredicates = predicates.flatMap { case (rel, preds) =>
      preds.filter { p =>
        disambiguationTrace.forall { trace =>
          // For each step, check if predicate is always false
          trace.iterateTxAndStateBefore.forall { case (state, tx) =>
            if (tx.relation == rel) !interpreter.evaluate(state, tx, p) else true
          }
        }
      }.map(p => (rel, p))
    }.toSet

    // Get the corresponding BoolExpr variables for these predicates
    val falseVars = alwaysFalsePredicates.flatMap { case (rel, p) =>
      val idx = orderedPredicates(rel).indexOf(p)
      if (idx >= 0) Some(encodings(rel)(idx)) else None
    }

    val alwaysFalsePairsPerRelation: Map[Relation, Set[(Predicate, Predicate)]] = predicates.map { case (rel, preds) =>
      val predList = preds.toList
      val pairs = (for {
        i <- predList.indices
        j <- (i + 1) until predList.size
        p1 = predList(i)
        p2 = predList(j)
        if disambiguationTrace.forall { trace =>
          trace.iterateTxAndStateBefore.forall { case (state, tx) =>
            tx.relation != rel || !(interpreter.evaluate(state, tx, p1) && interpreter.evaluate(state, tx, p2))
          }
        }
      } yield (p1, p2)).toSet
      rel -> pairs
    }

    val alwaysFalsePairs: Set[(Relation, Predicate, Predicate)] =
      alwaysFalsePairsPerRelation.flatMap { case (rel, pairs) =>
        pairs.map { case (p1, p2) => (rel, p1, p2) }
      }.toSet

    // Get corresponding BoolExpr variables for these pairs
    val falsePairVars = alwaysFalsePairs.flatMap { case (rel, p1, p2) =>
      val idx1 = orderedPredicates(rel).indexOf(p1)
      val idx2 = orderedPredicates(rel).indexOf(p2)
      if (idx1 >= 0 && idx2 >= 0) Some((encodings(rel)(idx1), encodings(rel)(idx2))) else None
    }

    // Block clause: all these must be false
    val blockSingle: BoolExpr =
      if (falseVars.nonEmpty) z3ctx.mkAnd(falseVars.map(z3ctx.mkNot).toSeq: _*)
      else z3ctx.mkTrue()

    // Block clause: for each pair, at least one must be false
    val blockParis: BoolExpr =
      if (falsePairVars.nonEmpty)
        z3ctx.mkAnd(falsePairVars.map { case (v1, v2) => z3ctx.mkOr(z3ctx.mkNot(v1), z3ctx.mkNot(v2)) }.toSeq: _*)
      else z3ctx.mkTrue()

    val block = z3ctx.mkAnd(blockSingle,blockParis)


    println(s"Block always false predicates: ${alwaysFalsePredicates.mkString("\n")}")
    println(s"Block always false predicates pairs: ${alwaysFalsePairs.size}")

    // Return the set of predicates and the blocking clause
    (alwaysFalsePredicates.map(_._2), block)
  }


}
