package synthesis

import com.microsoft.z3.{BoolExpr, Context, Expr, Model, Sort, Status, Z3Exception}
import datalog.{Constant, Program, Relation, Rule}
import imp.ImperativeTranslator
import util.Misc
import verification.{Prove, TransitionSystem, Verifier}

import scala.collection.mutable

/**
 * BoundedModelChecker performs bounded model checking on Datalog programs.
 * 
 * This checker translates Datalog programs into transition systems and uses
 * SMT solving (via Z3) to search for violations of safety properties within
 * a bounded number of execution steps.
 * 
 * The approach follows the classical BMC algorithm:
 * 1. Encode the initial state and transition relation
 * 2. Unroll the transition relation up to a bound k
 * 3. Check if a violation is reachable within k steps
 * 4. If SAT, extract a counterexample trace; if UNSAT, property holds up to bound k
 */
case class BoundedModelChecker() {
  
  /**
   * Performs bounded model checking on a Datalog program.
   * 
   * @param program The Datalog program representing a smart contract
   * @param violationRules Set of rules that define violation conditions.
   *                       If empty, uses program.violationRules
   * @param bound Maximum number of execution steps to explore (must be >= 0)
   * @return A tuple of:
   *         - Boolean: true if no violation found within bound, false otherwise
   *         - Option[Trace]: counterexample trace if violation found, None otherwise
   * @throws IllegalArgumentException if bound is negative
   */
  def check(program: Program, violationRules: Set[Rule], bound: Int): (Boolean, Option[Trace]) = {
    require(bound >= 0, s"Bound must be non-negative, got $bound")
    
    // Early termination: no violations to check or bound is zero
    if (violationRules.isEmpty || bound == 0) {
      return (true, None)
    }

    // Step 1: Translate Datalog program to imperative representation
    // No relations are materialized; instrumentation is enabled for verification
    val materializedRelations: Set[Relation] = Set()
    val translator = new ImperativeTranslator(
      program,
      materializedRelations,
      isInstrument = true,           // Enable instrumentation for verification
      monitorViolations = false,
      arithmeticOptimization = true,  // Use optimized arithmetic operations
      enableProjection = true         // Enable projection optimization
    )
    val imperative = translator.translate()

    // Step 2: Create verifier and extract transition system using reflection
    // The verifier encapsulates the Z3 context and transition system
    val verifier = new Verifier(program, imperative)
    val ctx = BoundedModelChecker.ReflectionUtils.getField[Context](verifier, "ctx")
    val ts = BoundedModelChecker.ReflectionUtils.invokeNoArg[TransitionSystem](verifier, "getTransitionSystem")

    // Step 3: Extract transition relation and initial state formula
    val combinedTrans = ts.getTr()  // Combined transition relation for all transactions
    val init = ts.getInit()         // Initial state formula

    // Step 4: Collect state variables (current and next-state pairs)
    // 需要看一下这里的reflection
    val variables: Seq[(Expr[_], Expr[_])] = BoundedModelChecker.ReflectionUtils.getField[Set[(Expr[_], Expr[_])]](ts, "variables").toSeq
    val (xs, xns) = BoundedModelChecker.BmcUtilities.splitStateVariables(variables)

    // Step 5: Prepare auxiliary data structures
    val stateExprs: Set[Expr[_]] = variables.flatMap { case (cur, nxt) => Seq(cur, nxt) }.toSet
    val relationByName: Map[String, Relation] = program.relations.map(r => r.name -> r).toMap

    // Step 6: Use reflection to access the private getProperty method from Verifier
    val propertyMethod = classOf[Verifier].getDeclaredMethod("getProperty", classOf[Context], classOf[Rule])
    propertyMethod.setAccessible(true)

    // Step 7: Determine which violation rules to check
    val allViolationRules = if (violationRules.nonEmpty) violationRules else program.violationRules

    // Step 8: Check each violation rule separately
    for (rule <- allViolationRules) {
      // Convert the violation rule to a Z3 property formula
      val property = propertyMethod.invoke(verifier, ctx, rule).asInstanceOf[BoolExpr]
      val violationGoal = ctx.mkNot(property)  // We search for states where property is violated

      // Collect free variables (those not in state variables)
      // These represent input parameters, nondeterministic choices, etc.
      val fvsSet = (Prove.get_vars(init) ++ Prove.get_vars(combinedTrans) ++ Prove.get_vars(violationGoal))
        .diff(stateExprs)
      val fvs = BoundedModelChecker.BmcUtilities.orderExprArray(fvsSet)

      // Print the five parameters
      println("=== BMC Parameters ===")
      println(s"init: $init")
      println(s"combinedTrans: $combinedTrans")
      println(s"violationGoal: $violationGoal")
      println(s"fvs: ${fvs.mkString("[", ", ", "]")}")
      println(s"xs: ${xs.mkString("[", ", ", "]")}")
      println(s"xns: ${xns.mkString("[", ", ", "]")}")
      println(s"bound: $bound")
      println("====================")

      // Run bounded model checking for this violation rule
      BoundedModelChecker.SimpleBMC.bmc(ctx, init, combinedTrans, violationGoal, fvs, xs, xns, bound) match {
        case Some(model) if model.nonEmpty =>
          // Violation found! Extract counterexample trace from the model
          // Build helper maps for enriched trace printing
          val interfacesByName: Map[String, datalog.Interface] = program.interfaces.map(i => i.relation.name -> i).toMap
          // Derive the set of state variable base names (normalized names) from (cur,next) pairs
          val stateVarBaseNames: Set[String] = variables.map { case (cur, _) =>
            BoundedModelChecker.BmcUtilities.pureName(cur.toString)
          }.toSet
          val traceOpt = BoundedModelChecker.BmcUtilities.modelToTrace(model, relationByName, interfacesByName, stateVarBaseNames)
          return (false, traceOpt)
        case _ =>
          // No violation found for this rule within the bound
      }
    }

    // All violation rules checked: no counterexample found within bound
    (true, None)
  }
}

object BoundedModelChecker {
  
  /**
   * Unit test suite for the bounded model checker.
   * 
   * This test contains two scenarios:
   * 1. A simple bit-vector increment test
   * 2. A crowdsale smart contract mutual exclusion property test
   * 
   * The tests demonstrate BMC's ability to find counterexamples for
   * reachable violation states within a given bound.
   */
  def unitTest1(): Unit = {
    
    // ===== Test 1: Simple bit-vector increment test =====
    println("[BMC][unitTest1] ===== testbmc =====")
    val ctx1 = new Context()
    
    // Setup: 4-bit bitvector that increments by 3 each step
    val bvSize = 4
    val bvSort = ctx1.mkBitVecSort(bvSize)
    val x = ctx1.mkBVConst("x", bvSize)           // Current state variable
    val xNext = ctx1.mkBVConst("x_next", bvSize)  // Next state variable
    
    // Initial state: x = 0
    val init1 = ctx1.mkEq(x, ctx1.mkBV(0, bvSize))
    
    // Transition relation: x' = x + 3 (with overflow)
    val trans1 = ctx1.mkEq(xNext, ctx1.mkBVAdd(x, ctx1.mkBV(3, bvSize)))
    
    // Goal: reach state where x = 15
    // Note: With 4-bit arithmetic, sequence is 0, 3, 6, 9, 12, 15 (6 steps, but modulo 16)
    val goal1 = ctx1.mkEq(x, ctx1.mkBV(15, bvSize))

    // Run BMC with bound=8 to search for a trace reaching x=15
    SimpleBMC
      .bmc(ctx1, init1, trans1, goal1, Array.empty, Array(x), Array(xNext), bound = 8)
      .foreach { model =>
        // Print the counterexample trace (should show states 0, 3, 6, 9, 12, 15)
        var id = 0
        model.foreach { elem =>
          print(s"$id: ")
          println(elem.mkString(", "))
          id += 1
        }
      }
    ctx1.close()

    // ===== Test 2: Crowdsale smart contract mutual exclusion test =====
    println("[BMC][unitTest1] ===== testbmc2 =====")
    val ctx2 = new Context()
    val ts = TransitionSystem("Crowdsale", ctx2)

    // Crowdsale contract parameters
    val GOAL = 10000       // Fundraising goal
    val CLOSETIME = 10000  // Maximum time before refund is allowed

    val addrSort = ctx2.mkBitVecSort(256)  // Ethereum address type (256-bit)

    // State variable: crowdsale state (OPEN, SUCCESS, or REFUND)
    val (state, stateOut) = ts.newVar("state", ctx2.mkBitVecSort(2))
    val OPEN = ctx2.mkBV(0, 2)     // State 0: accepting investments
    val SUCCESS = ctx2.mkBV(1, 2)  // State 1: goal reached, funds can be withdrawn
    val REFUND = ctx2.mkBV(2, 2)   // State 2: goal not reached, refunds available

    // State variables for the crowdsale contract
    val (deposits, depositsOut) = ts.newVar("deposits", ctx2.mkArraySort(addrSort, ctx2.mkBitVecSort(256)))  // Map: address -> deposit amount
    val (totalDeposits, totalDepositsOut) = ts.newVar("totalDeposits", ctx2.mkBitVecSort(256))               // Sum of all deposits
    val (raised, raisedOut) = ts.newVar("raised", ctx2.mkBitVecSort(256))                                    // Total amount raised
    val (auxWithdraw, auxWithdrawOut) = ts.newVar("aux_withdraw", ctx2.mkBoolSort())                         // Auxiliary: has withdraw occurred?
    val (auxRefund, auxRefundOut) = ts.newVar("aux_refund", ctx2.mkBoolSort())                               // Auxiliary: has refund occurred?
    val (func, funcOut) = ts.newVar("func", ctx2.mkStringSort())                                             // Current transaction name
    val (timestamp, timestampOut) = ts.newVar("now", ctx2.mkBitVecSort(256))                                 // Current timestamp

    // Free variables representing transaction inputs
    val p = ctx2.mkConst("p", addrSort)      // Participant address
    val amount = ctx2.mkBVConst("amount", 256)  // Investment amount

    // Initial state: all deposits are zero, state is OPEN, no transactions executed yet
    val init2 = ctx2.mkAnd(
      ctx2.mkForall(Array(p), ctx2.mkEq(ctx2.mkSelect(deposits, p), ctx2.mkBV(0, 256)), 1, null, null, ctx2.mkSymbol("Q1"), ctx2.mkSymbol("skid1")),  // All deposits = 0
      ctx2.mkEq(raised, ctx2.mkBV(0, 256)),           // No funds raised yet
      ctx2.mkEq(totalDeposits, ctx2.mkBV(0, 256)),    // Total deposits = 0
      ctx2.mkEq(auxWithdraw, ctx2.mkFalse()),         // Withdraw has not occurred
      ctx2.mkEq(auxRefund, ctx2.mkFalse()),           // Refund has not occurred
      ctx2.mkEq(state, OPEN),                          // Initial state is OPEN
      ctx2.mkEq(func, ctx2.mkString("init")),         // Initial transaction
      ctx2.mkEq(timestamp, ctx2.mkBV(0, 256))         // Start time is 0
    )

    // Transition: invest - participant deposits funds (only allowed when raised < GOAL)
    val trInvest = ctx2.mkAnd(
      ctx2.mkBVULT(raised, ctx2.mkBV(GOAL, 256)),     // Guard: raised < GOAL
      ctx2.mkEq(stateOut, state),                      // State unchanged
      ctx2.mkEq(raisedOut, ctx2.mkBVAdd(raised, amount)),  // Increase raised amount
      ctx2.mkEq(depositsOut, ctx2.mkStore(deposits, p, ctx2.mkBVAdd(ctx2.mkSelect(deposits, p), amount))),  // Record deposit for participant p
      ctx2.mkEq(totalDepositsOut, ctx2.mkBVAdd(totalDeposits, amount)),  // Increase total deposits
      ctx2.mkEq(auxRefundOut, auxRefund),              // Auxiliary flags unchanged
      ctx2.mkEq(auxWithdrawOut, auxWithdraw),
      ctx2.mkEq(funcOut, ctx2.mkString("invest")),    // Transaction name
      ctx2.mkBVSGT(timestampOut, timestamp)            // Time advances
    )

    // Transition: close_success - close crowdsale successfully when goal is reached
    val trCloseSuccess = ctx2.mkAnd(
      ctx2.mkBVUGE(raised, ctx2.mkBV(GOAL, 256)),     // Guard: raised >= GOAL
      ctx2.mkEq(stateOut, SUCCESS),                    // Transition to SUCCESS state
      ctx2.mkEq(depositsOut, deposits),                // All other state variables unchanged
      ctx2.mkEq(raisedOut, raised),
      ctx2.mkEq(totalDepositsOut, totalDeposits),
      ctx2.mkEq(auxRefundOut, auxRefund),
      ctx2.mkEq(auxWithdrawOut, auxWithdraw),
      ctx2.mkEq(funcOut, ctx2.mkString("close_success")),  // Transaction name
      ctx2.mkBVSGT(timestampOut, timestamp)            // Time advances
    )

    // Transition: close_refund - close crowdsale for refund when time expired and goal not reached
    val trCloseRefund = ctx2.mkAnd(
      ctx2.mkBVSGT(timestamp, ctx2.mkBV(CLOSETIME, 256)),  // Guard: time > CLOSETIME
      ctx2.mkBVULT(raised, ctx2.mkBV(GOAL, 256)),          // Guard: raised < GOAL
      ctx2.mkEq(stateOut, REFUND),                          // Transition to REFUND state
      ctx2.mkEq(depositsOut, deposits),                     // All other state variables unchanged
      ctx2.mkEq(raisedOut, raised),
      ctx2.mkEq(totalDepositsOut, totalDeposits),
      ctx2.mkEq(auxRefundOut, auxRefund),
      ctx2.mkEq(auxWithdrawOut, auxWithdraw),
      ctx2.mkEq(funcOut, ctx2.mkString("close_refund")),   // Transaction name
      ctx2.mkBVSGT(timestampOut, timestamp)                 // Time advances
    )

    // Transition: claimrefund - participant claims their refund (only in REFUND state)
    val trClaimRefund = ctx2.mkAnd(
      ctx2.mkEq(state, REFUND),                             // Guard: must be in REFUND state
      ctx2.mkBVULT(raised, ctx2.mkBV(GOAL, 256)),          // Guard: raised < GOAL
      ctx2.mkEq(depositsOut, ctx2.mkStore(deposits, p, ctx2.mkBV(0, 256))),  // Set participant's deposit to 0
      ctx2.mkEq(totalDepositsOut, ctx2.mkBVSub(totalDeposits, ctx2.mkSelect(deposits, p))),  // Decrease total deposits
      ctx2.mkEq(raisedOut, raised),                         // Raised amount unchanged
      ctx2.mkEq(stateOut, state),                           // State unchanged
      ctx2.mkEq(auxRefundOut, ctx2.mkTrue()),              // Mark that a refund has occurred
      ctx2.mkEq(auxWithdrawOut, auxWithdraw),
      ctx2.mkEq(funcOut, ctx2.mkString("claimrefund")),    // Transaction name
      ctx2.mkBVSGT(timestampOut, timestamp)                 // Time advances
    )

    // Transition: withdraw - owner withdraws all funds (only in SUCCESS state)
    val trWithdraw = ctx2.mkAnd(
      ctx2.mkEq(state, SUCCESS),                            // Guard: must be in SUCCESS state
      ctx2.mkEq(totalDepositsOut, ctx2.mkBV(0, 256)),      // Withdraw all deposits (set to 0)
      ctx2.mkEq(depositsOut, deposits),                     // Individual deposits unchanged
      ctx2.mkEq(raisedOut, raised),
      ctx2.mkEq(stateOut, state),                           // State unchanged
      ctx2.mkEq(auxWithdrawOut, ctx2.mkTrue()),            // Mark that withdraw has occurred
      ctx2.mkEq(auxRefundOut, auxRefund),
      ctx2.mkEq(funcOut, ctx2.mkString("withdraw")),       // Transaction name
      ctx2.mkBVSGT(timestampOut, timestamp)                 // Time advances
    )

    println(funcOut.getClass)
    ts.setInit(init2)

    // Combined transition relation: nondeterministically choose one of the five transitions
    val combinedTransition = ctx2.mkOr(trInvest, trCloseRefund, trCloseSuccess, trClaimRefund, trWithdraw)

    // Property to check: mutual exclusion - withdraw and refund should never both occur
    // r2 = ¬(auxWithdraw ∧ auxRefund), which should always hold
    val r2 = ctx2.mkNot(ctx2.mkAnd(auxWithdraw, auxRefund))
    val goal2 = ctx2.mkNot(r2)  // Goal: find a state where both withdraw and refund happened (violation)
    
    // Free variables: participant address and investment amount
    val fvs2: Array[Expr[_]] = Array(p, amount)
    
    // Current state variables
    val xs2: Array[Expr[_]] = Array(deposits, totalDeposits, raised, state, auxWithdraw, auxRefund, func, timestamp)
    
    // Next state variables
    val xns2: Array[Expr[_]] = Array(depositsOut, totalDepositsOut, raisedOut, stateOut, auxWithdrawOut, auxRefundOut, funcOut, timestampOut)

    // Run BMC to search for a violation of mutual exclusion
    SimpleBMC
      .bmc(ctx2, ts.getInit(), combinedTransition, goal2, fvs2, xs2, xns2, bound = 8)
      .foreach { model =>
        // If a counterexample is found, print the trace
        var id = 0
        model.foreach { elem =>
          print(s"$id: ")
          println(elem.mkString(", "))
          id += 1
        }
      }
    ctx2.close()
  }

  /**
   * SimpleBMC implements the core bounded model checking algorithm.
   * 
   * The algorithm works by:
   * 1. Starting with the initial state formula
   * 2. Unrolling the transition relation k times
   * 3. At each step, checking if the goal (violation) is reachable
   * 4. If reachable, extracting and returning a counterexample model
   * 
   * The implementation uses variable renaming to create fresh copies
   * of state variables at each step of the unrolling.
   */
  private object SimpleBMC {
    // Counter for generating fresh variable names
    private var freshIndex: Int = 0

    /**
     * Extracts the pure name from a Z3 variable name.
     * Z3 may wrap names in |...| format; this extracts the inner name.
     * 
     * @param name The variable name (possibly wrapped)
     * @return The pure name without wrapper symbols
     */
    /**
     * Extract the pure name for a Z3 symbol.
     * Z3 may wrap names with vertical bars like |name|; this removes the wrappers.
     */
    private def pureName(name: String): String = {
      if (name.contains("|")) {
        val first = name.indexOf("|")
        val second = name.indexOf("|", first + 1)
        if (first >= 0 && second > first) name.substring(first + 1, second)
        else name
      } else name
    }

    /**
     * Creates a fresh variable for a specific unrolling round.
     * 
     * @param round The BMC unrolling round number
     * @param ctx Z3 context
     * @param name Base variable name
     * @param sort Variable sort (type)
     * @return A fresh Z3 expression with unique name
     */
    private def fresh(round: Int, ctx: Context, name: String, sort: Sort): Expr[_] = {
      freshIndex += 1
      ctx.mkConst(s"${name}:r${round}:i${freshIndex}", sort)
    }

    /**
     * Performs bounded model checking to search for a state satisfying the goal.
     * 
     * @param ctx Z3 context
     * @param init Initial state formula
     * @param trans Transition relation formula
     * @param goal Goal formula (typically a violation condition)
     * @param fvs Free variables (inputs, parameters)
     * @param xs Current state variables
     * @param xns Next state variables
     * @param bound Maximum number of steps to unroll
     * @return Some(model) if goal is reachable, None otherwise
     */
    def bmc(
      ctx: Context,
      init: BoolExpr,
      trans: BoolExpr,
      goal: BoolExpr,
      fvs: Array[Expr[_]],
      xs: Array[Expr[_]],
      xns: Array[Expr[_]],
      bound: Int
    ): Option[Array[mutable.Map[String, Expr[_]]]] = {
      // Create a Z3 solver and assert the initial state
      val solver = ctx.mkSolver()
      solver.add(init)

      // Variables that will be updated during unrolling
      var currentTrans = trans   // Transition relation (will be renamed each round)
      var currentGoal = goal      // Goal formula (will be renamed each round)
      var currentXs = xs          // Current state variables (renamed each round)
      var currentXns = xns        // Next state variables (renamed each round)
      var currentFvs = fvs        // Free variables (renamed each round)

      freshIndex = 0  // Reset fresh variable counter

      // Unroll the transition relation up to 'bound' steps
      for (round <- 0 until bound) {
        // Create a guard variable for activation literals (for better SMT performance)
        val guard = fresh(round, ctx, "P", ctx.getBoolSort).asInstanceOf[BoolExpr]
        
        // Check if goal is reachable at this step
        solver.push()  // Save current solver state
        solver.add(guard)
        solver.add(ctx.mkImplies(guard, currentGoal))  // If guard, then goal must hold
        val res = solver.check(guard)  // Check satisfiability with guard enabled
        solver.pop()   // Restore solver state

        // If SAT, we found a trace that reaches the goal
        if (res == Status.SATISFIABLE) {
          // 打印出当前找到的可满足模型（counterexample trace），便于调试和分析
          // println("=== SAT Model (Counterexample Trace) ===")
          // println(solver.getModel)
          // println("========================================")
          return Some(extractModel(solver.getModel))
        }

        // Add transition relation for this step
        solver.add(currentTrans)

        // Create fresh variables for the next round
        val newXs = currentXs.map { x => fresh(round, ctx, pureName(x.toString), x.getSort.asInstanceOf[Sort]) }
        val newFvs = currentFvs.map { f => fresh(round, ctx, pureName(f.toString), f.getSort.asInstanceOf[Sort]) }

        // Rename variables in transition and goal for next round
        // Substitution: replace xns with newXs, xs with xns, fvs with newFvs
        val substitutions = (BmcUtilities.toArray(currentXns ++ currentXs ++ currentFvs), BmcUtilities.toArray(newXs ++ currentXns ++ newFvs))
        currentTrans = substituteSafe(currentTrans, substitutions)
        currentGoal = substituteSafe(currentGoal, (currentXs, currentXns))

        // Update current variables for next iteration
        currentXs = currentXns
        currentXns = BmcUtilities.toArray(newXs)
        currentFvs = BmcUtilities.toArray(newFvs)
      }

      // No counterexample found within bound
      None
    }

    /**
     * Safely substitutes variables in an expression.
     * Only substitutes variables with compatible sorts to avoid Z3 errors.
     * 
     * @param expr Expression to substitute in
     * @param substitution Pair of (from, to) arrays for variable substitution
     * @return Expression with substitutions applied
     */
    private def substituteSafe(expr: BoolExpr, substitution: (Array[Expr[_]], Array[Expr[_]])): BoolExpr = {
      val (from, to) = substitution
      // Filter out incompatible variable pairs to avoid Z3 exceptions
      val compatible = from.zip(to).filter { case (f, t) =>
        try {
          f.getSort == t.getSort
        } catch {
          case _: Throwable => false
        }
      }
      expr.substitute(compatible.map(_._1), compatible.map(_._2)).asInstanceOf[BoolExpr]
    }

    /**
     * Extracts variable assignments from a Z3 model.
     * 
     * @param model Z3 model containing satisfying assignment
     * @return Array of maps, one per execution step, mapping variable names to values
     */
    private def extractModel(model: Model): Array[mutable.Map[String, Expr[_]]] = {
      // Extract all constant interpretations from the model
      val entries = model.getDecls.map { decl =>
        val name = decl.getName.toString
        val value = try {
          model.getConstInterp(decl)
        } catch {
          case _: Z3Exception => decl.getRange  // Fallback if interpretation not available
        }
        name -> value.asInstanceOf[Expr[_]]
      }.toMap
      categorize(entries)
    }

    /**
     * Categorizes model entries by execution round.
     * Variables are grouped into buckets based on their round number (extracted from name).
     * 
     * @param raw Raw map of variable names to values
     * @return Array of maps, indexed by round number
     */
    private def categorize(raw: Map[String, Expr[_]]): Array[mutable.Map[String, Expr[_]]] = {
      // Determine the maximum round number in the model
      var maxRound = 0
      raw.foreach { case (key, _) =>
        val round =
          if (key.contains(":r")) {
            // Extract round number from variable name format "name:rN:iM"
            key.substring(key.indexOf(":r") + 2, key.indexOf(":i")).toInt
          } else if (key.contains("_next")) 1 else 0
        maxRound = math.max(maxRound, round)
      }

      // Create buckets for each round
      val buckets = Array.fill(math.max(1, maxRound + 2))(mutable.Map[String, Expr[_]]())
      
      // Distribute variables into appropriate buckets
      raw.foreach { case (key, value) =>
        val pname = pureName(key)
        if (pname != "P") {  // Skip guard variables
          val round =
            if (key.contains(":r")) {
              key.substring(key.indexOf(":r") + 2, key.indexOf(":i")).toInt
            } else if (key.contains("_next")) 1 else 0
          
          // Normalize variable name (remove "_next" suffix)
          val normalized = if (pname.endsWith("_next")) pname.substring(0, pname.length - 5) else pname
          
          if (round >= 0 && round < buckets.length) {
            buckets(round).update(normalized, value)
          }
        }
      }
      buckets
    }
  }

  /**
   * BmcUtilities provides helper functions for bounded model checking operations.
   * 
   * This includes utilities for:
   * - Managing state variables
   * - Converting between data structures
   * - Extracting execution traces from SMT models
   */
  private object BmcUtilities {
    /**
     * Extract the pure name for a Z3 symbol.
     * Z3 may wrap names with vertical bars like |name|; this removes the wrappers.
     */
    def pureName(name: String): String = {
      if (name.contains("|")) {
        val first = name.indexOf("|")
        val second = name.indexOf("|", first + 1)
        if (first >= 0 && second > first) name.substring(first + 1, second)
        else name
      } else name
    }
    
    /**
     * Splits state variable pairs into separate arrays for current and next states.
     * Variables are sorted by name to ensure consistent ordering.
     * 
     * @param vars Sequence of (current, next) state variable pairs
     * @return Tuple of (current state array, next state array)
     */
    def splitStateVariables(vars: Seq[(Expr[_], Expr[_])]): (Array[Expr[_]], Array[Expr[_]]) = {
      val ordered = vars.sortBy { case (cur, _) => cur.toString }
      val current = ordered.map(_._1).toArray
      val next = ordered.map(_._2).toArray
      (current, next)
    }

    /**
     * Orders and deduplicates an iterable of Z3 expressions.
     * Expressions are sorted by their string representation.
     * 
     * @param exprs Iterable of expressions to order
     * @return Ordered array with duplicates removed
     */
    def orderExprArray(exprs: Iterable[Expr[_]]): Array[Expr[_]] = {
      toArray(exprs.toSeq.distinct.sortBy(_.toString))
    }

    /**
     * Converts an iterable of expressions to an array.
     * 
     * @param seq Iterable of expressions
     * @return Array of expressions
     */
    def toArray(seq: Iterable[Expr[_]]): Array[Expr[_]] = seq.toArray[Expr[_]]

    /**
     * Converts a BMC model (sequence of variable assignments) to an execution trace.
     * 
     * The trace consists of transactions extracted from the model by:
     * 1. Looking for "transaction" or "func" variables in each step
     * 2. Filtering out initialization steps ("init", "constructor")
     * 3. Mapping transaction names to their corresponding Datalog relations
     * 
     * @param model Array of variable assignments, one map per execution step
     * @param relations Map from relation name to Relation object
     * @return Some(Trace) if transactions found, None if trace is empty
     */
    /**
     * Convert a BMC model (bucketed by rounds) into a human-readable trace.
     *
     * Enhancements:
     *  - Print, per step, the transaction name (if any), its parameter types and concrete values,
     *    and all state variable values.
     *  - Parameter values are retrieved from explicit transaction parameter state variables created
     *    by the verifier, following naming convention: tx_<relName>_<paramName>.
     *
     * Note: The returned Trace structure still carries only relation names with placeholder params,
     *       while detailed per-step values are printed to stdout for inspection.
     */
    def modelToTrace(
      model: Array[mutable.Map[String, Expr[_]]],
      relations: Map[String, Relation],
      interfacesByName: Map[String, datalog.Interface],
      stateVarBaseNames: Set[String]
    ): Option[Trace] = {
      // Print model summary for debugging
      println(s"[BMC][Trace] Analyzing counterexample model with ${model.length} steps")
      
      // First, print state evolution to help user understand the violation
      println(s"[BMC][Trace] State evolution:")
      model.zipWithIndex.foreach { case (entry, idx) =>
        // Get transaction if any
        val txOpt = entry.get("transaction").orElse(entry.get("func"))
        val txStr = txOpt.map(tx => s"[tx: ${tx.toString.replace("\"", "")}]").getOrElse("")
        
        // Get ALL variables (including BMC internal ones for debugging)
        val allVars = entry.toSeq.sortBy(_._1)
        val stateVars = allVars.filter { case (key, _) => 
          !key.contains(":r") && !key.contains("_next") && 
          !key.startsWith("P:") && key != "transaction"
        }
        val bmcVars = allVars.filter { case (key, _) => 
          key.contains(":r") && (key.contains("once") || key.contains("flag"))
        }
        
        if (stateVars.nonEmpty || txStr.nonEmpty) {
          val stateStr = stateVars.map { case (k, v) => s"$k=$v" }.mkString(", ")
          println(s"[BMC][Trace]   Step $idx: $txStr ${if (stateStr.nonEmpty) stateStr else "(no state changes)"}")
          
          // Show BMC-generated variables that might explain the violation
          if (bmcVars.nonEmpty && idx == 0) {
            println(s"[BMC][Trace]            (BMC variables: ${bmcVars.map { case (k, v) => s"$k=$v" }.mkString(", ")})")
          }
        }
      }
      
      // Extract transaction sequence, while printing detailed arguments and state variables
      // Print init snapshot (full values) before diffs
      if (model.nonEmpty) {
        val initEntry = model.head
        println(s"[BMC][Diff] Init snapshot:")
        // state snapshot
        val initState = initEntry.filter { case (k, _) => stateVarBaseNames.contains(k) }
        if (initState.nonEmpty) {
          val stateStr = initState.toSeq.sortBy(_._1).map { case (k, v) => s"${k}=${v}" }.mkString(", ")
          println(s"[BMC][Diff]           state: ${stateStr}")
        }
        // tx params snapshot (heuristic: keys starting with tx_)
        val initParams = initEntry.filter { case (k, _) => k.startsWith("tx_") }
        if (initParams.nonEmpty) {
          val paramsStr = initParams.toSeq.sortBy(_._1).map { case (k, v) => s"${k}=${v}" }.mkString(", ")
          println(s"[BMC][Diff]           params: ${paramsStr}")
        }
        // internal snapshot
        val initInternal = initEntry.filter { case (k, _) =>
          !stateVarBaseNames.contains(k) && k != "transaction" && !k.startsWith("P:") && !k.startsWith("tx_")
        }
        if (initInternal.nonEmpty) {
          val internalStr = initInternal.toSeq.sortBy(_._1).map { case (k, v) => s"${k}=${v}" }.mkString(", ")
          println(s"[BMC][Diff]           internal: ${internalStr}")
        }
      }

      // Helper to compute changed pairs for a set of keys
      def changedPairs(keys: Set[String], prev: mutable.Map[String, Expr[_]], curr: mutable.Map[String, Expr[_]]): List[String] = {
        keys.toList.sorted.flatMap { k =>
          val oldStr = prev.get(k).map(_.toString).getOrElse("_")
          val newStr = curr.get(k).map(_.toString).getOrElse("_")
          if (oldStr != newStr) Some(s"${k}: ${oldStr} -> ${newStr}") else None
        }
      }

      // Extract transaction sequence, while printing detailed arguments and state variables
      val steps = model.zipWithIndex.flatMap { case (entry, stepIdx) =>
        // Look for transaction identifier (either "transaction" or "func" variable)
        val txValueOpt = entry.get("transaction").orElse(entry.get("func"))
        txValueOpt.flatMap { expr =>
          val raw = expr.toString.replace("\"", "")
          val name = raw.trim
          
          // Skip initialization and empty transactions
          if (name.isEmpty || name.equalsIgnoreCase("init") || name.equalsIgnoreCase("constructor")) {
            None
          } else {
            // Map transaction name to its Datalog relation
            relations.get(name).map { rel =>
              // Print transaction header
              println(s"[BMC][Trace]   Step ${stepIdx}: tx=${name}")

              // Gather interface and parameter typing info
              val memberNames: List[String] = rel.memberNames
              val types: List[datalog.Type] = rel.sig
              val inputIdxs: List[Int] = interfacesByName.get(name).map(_.inputIndices).getOrElse(Nil)

              // Extract parameter values using naming convention: tx_<rel>_<param>
              val argReprs: List[String] = inputIdxs.zipWithIndex.map { case (paramIdx, argPos) =>
                val pName = if (paramIdx >= 0 && paramIdx < memberNames.length && memberNames(paramIdx) != null && memberNames(paramIdx).nonEmpty) memberNames(paramIdx) else s"arg${paramIdx}"
                val tpe = if (paramIdx >= 0 && paramIdx < types.length) types(paramIdx) else datalog.AnyType()
                val key = s"tx_${name}_${pName}"
                val valueStr = entry.get(key).map(_.toString).getOrElse("_")
                s"(${tpe.toString} ${pName} = ${valueStr})"
              }
            

              // Print all state variables for this step
              val stateEntries = entry.filter { case (k, _) => stateVarBaseNames.contains(k) || k == "transaction" }
              if (stateEntries.nonEmpty) {
                val stateStr = stateEntries.toSeq.sortBy(_._1).map { case (k, v) => s"${k}=${v}" }.mkString(", ")
              }

              // Print internal variables (non-state, non-param, non-tx guard)
              val paramKeys: Set[String] = inputIdxs.map { idx =>
                val pName = if (idx >= 0 && idx < memberNames.length && memberNames(idx) != null && memberNames(idx).nonEmpty) memberNames(idx) else s"arg${idx}"
                s"tx_${name}_${pName}"
              }.toSet
              val internalEntries = entry.filter { case (k, _) =>
                !stateVarBaseNames.contains(k) && k != "transaction" && !paramKeys.contains(k) && !k.startsWith("P:")
              }
              if (internalEntries.nonEmpty) {
                val internalStr = internalEntries.toSeq.sortBy(_._1).map { case (k, v) => s"${k}=${v}" }.mkString(", ")
              }

              // Per-step diffs starting from step 1
              if (stepIdx > 0) {
                val prev = model(stepIdx - 1)
                // Keys for categories
                val stateKeys: Set[String] = stateVarBaseNames
                val paramKeys: Set[String] = inputIdxs.map { idx =>
                  val pName = if (idx >= 0 && idx < memberNames.length && memberNames(idx) != null && memberNames(idx).nonEmpty) memberNames(idx) else s"arg${idx}"
                  s"tx_${name}_${pName}"
                }.toSet
                val internalKeys: Set[String] = (entry.keySet ++ prev.keySet).filter { k =>
                  !stateVarBaseNames.contains(k) && k != "transaction" && !paramKeys.contains(k) && !k.startsWith("P:")
                }.toSet

                val stateDiffs = changedPairs(stateKeys, prev, entry)
                val paramDiffs = changedPairs(paramKeys, prev, entry)
                val internalDiffs = changedPairs(internalKeys, prev, entry)

                if (stateDiffs.nonEmpty) println(s"[BMC][Diff]           state: ${stateDiffs.mkString(", ")}")
                if (paramDiffs.nonEmpty) println(s"[BMC][Diff]           params: ${paramDiffs.mkString(", ")}")
                if (internalDiffs.nonEmpty) println(s"[BMC][Diff]           internal: ${internalDiffs.mkString(", ")}")
              }

              /**
               * Build concrete parameters to satisfy Trace API:
               * - length equals rel.arity
               * - for indices in inputIdxs, try to use concrete value from model via key tx_<rel>_<paramName>
               * - otherwise, use wildcard Constant(t, "_") to bypass type checks
               */
              val params = types.zipWithIndex.map { case (tpe, idx) =>
                if (inputIdxs.contains(idx)) {
                  val pName = if (idx >= 0 && idx < memberNames.length && memberNames(idx) != null && memberNames(idx).nonEmpty) memberNames(idx) else s"arg${idx}"
                  val key = s"tx_${name}_${pName}"
                  // Convert Z3 Expr to a readable string; drop quotes for strings
                  val valueStr = entry.get(key).map(_.toString.replace("\"", "")).getOrElse("_")
                  Constant(tpe, valueStr)
                } else {
                  Constant(tpe, "_")
                }
              }
              // Build implicit parameters from model if available
              def parseInt(str: String): Int = {
                val s = str.replace("\"", "").trim
                // handle patterns like -1 or ( - 1 ) already normalized by entry.toString earlier
                try s.toInt catch { case _: Throwable => 0 }
              }
              val implicitParams = {
                val sender = entry.get("msgSender").map(e => parseInt(e.toString)).getOrElse(0)
                val value = entry.get("msgValue").map(e => parseInt(e.toString))
                  .orElse(entry.get("value").map(e => parseInt(e.toString))).getOrElse(0)
                ImplicitParameters(sender, value)
              }
              Transaction(rel, params.toList, implicitParams)
            }
          }
        }
      }
      
      // If no transactions found, create a more informative message
      if (steps.isEmpty) {
        println(s"[BMC][Trace] Note: Violation found in initial state or through implicit transitions")
        println(s"[BMC][Trace] The property may be violated without executing any transactions,")
        println(s"[BMC][Trace] possibly due to incomplete constraints in the Datalog specification.")
      }

      if (steps.isEmpty) None else Some(Trace(steps.toSeq))
    }
  }

  /**
   * ReflectionUtils provides utilities for accessing private fields and methods.
   * 
   * This is used to extract internal components from the Verifier class
   * that are not exposed through its public API (e.g., Z3 context, transition system).
   */
  private object ReflectionUtils {
    
    /**
     * Retrieves the value of a private field from an object instance.
     * 
     * @tparam T Expected type of the field value
     * @param instance The object instance to access
     * @param name Name of the field to retrieve
     * @return The field value cast to type T
     * @throws NoSuchFieldException if field doesn't exist
     * @throws ClassCastException if field type doesn't match T
     */
    def getField[T](instance: AnyRef, name: String): T = {
      val field = instance.getClass.getDeclaredField(name)
      field.setAccessible(true)  // Bypass private access modifier
      field.get(instance).asInstanceOf[T]
    }

    /**
     * Invokes a private method with no arguments on an object instance.
     * 
     * @tparam T Expected return type of the method
     * @param instance The object instance to invoke the method on
     * @param name Name of the method to invoke
     * @return The method's return value cast to type T
     * @throws NoSuchMethodException if method doesn't exist
     * @throws ClassCastException if return type doesn't match T
     */
    def invokeNoArg[T](instance: AnyRef, name: String): T = {
      val method = instance.getClass.getDeclaredMethod(name)
      method.setAccessible(true)  // Bypass private access modifier
      method.invoke(instance).asInstanceOf[T]
    }
  }
}
