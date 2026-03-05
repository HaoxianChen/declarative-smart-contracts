package synthesis

import datalog.{ArithOperator, Arithmetic, Assign, BooleanType, Constant, Equal, Functor, Geq, Greater, Leq, Lesser, Literal, MsgSender, MsgValue, Param, Parameter, Program, Relation, ReservedRelation, Rule, SimpleRelation, SingletonRelation, Unequal, Variable, Type}
import imp.{ImperativeAbstractProgram, ImperativeTranslator}
import Arithmetic.extractParameters
import viewMaterializer.BaseViewMaterializer
import PredicateEnumerator.functorParams
import imp.SolidityTranslator.transactionRelationPrefix

import scala.collection.mutable


case class Context(tx: Literal, bindingLiterals: Set[Literal]) {
  override def toString: String = {
    val txStr = tx.toString
    val bindingsStr = bindingLiterals.map(_.toString).mkString(", ")
    s"tx: $txStr, bindings: [$bindingsStr]"
  }

  def rename(mapping: Map[Parameter, Parameter]): Context = {
    val newTx = tx.rename(mapping)
    val newBindings = bindingLiterals.map(_.rename(mapping))
    this.copy(tx = newTx, bindingLiterals=newBindings)
  }
}
object Context {
  val msgSender: Literal = Literal(MsgSender(), List(Variable(datalog.Type.addressType, "msgSender")))
  val msgValue: Literal = Literal(MsgValue(), List(Variable(datalog.Type.uintType, "msgValue")))

  val getContextParams: Set[Parameter] = {
    (msgValue.fields ++ msgValue.fields).toSet
  }
}
case class Predicate(context: Context, functor: Functor) {
  override def toString: String = {
    s"Predicate(context: $context, functor: $functor)"
  }

  def referredMsgValue(): Boolean = {
    val params = functorParams(functor) ++ context.bindingLiterals.flatMap(_.fields)
    Context.msgValue.fields.intersect(params).nonEmpty
  }

  def referredMsgSender(): Boolean = {
    val params = functorParams(functor) ++ context.bindingLiterals.flatMap(_.fields)
    Context.msgSender.fields.intersect(params).nonEmpty
  }

  def rename(mapping: Map[Parameter, Parameter]): Predicate = {
    val newContext = context.rename(mapping)
    val newFunctor = Functor.rename(this.functor,mapping)
    this.copy(context=newContext, functor = newFunctor)
  }
}

case class InterpreterContext(relationIndices: Map[SimpleRelation, List[Int]],
                              materializedRelations: Set[Relation],
                              udfs: Set[Relation] = Set())

object InterpreterContext {
  def makeContext(program: Program): InterpreterContext = {
    val relationIndices = program.relationIndices

    val impTranslator = new ImperativeTranslator(program, Set(),
      isInstrument=true, enableProjection=true,
      monitorViolations = false, arithmeticOptimization = true)
    val imperative = impTranslator.translate()

    val baseViewMaterializer = new BaseViewMaterializer()
    val materializedRelations = baseViewMaterializer.getMaterializedRelations(
      imperative, program.interfaces).toSet

    InterpreterContext(relationIndices, materializedRelations, program.udfs)
  }
}

/**
 * Enumerates candidate Datalog literals directly from the program schema.
 *
 * Produces:
 *  - Relation atoms with fresh variables per instance.
 *  - Simple guards for each bound variable (e.g., x >= 0).
 *  - Cross-relation equalities between variables at corresponding positions.
 */
case class PredicateEnumerator(interpreterContext: InterpreterContext) {
  /** keep only materialized relation in the map */
  private val relationIndices = interpreterContext.relationIndices.filter {
    case (rel, _) => interpreterContext.materializedRelations.contains(rel)
  }

  private val singletonRelations: Set[SingletonRelation] =
    interpreterContext.materializedRelations.collect{
      case sr: SingletonRelation => {
        sr
      }
    }

  def extractPredicateFromTxProperties(program: Program): Map[Rule,Predicate] = {
    val txViolationRules = program.violationRules.filter(
      _.body.exists(_.relation.name.startsWith(transactionRelationPrefix)))

    // Build a lookup: tx relation -> canonical tx literal from an actual transaction rule.
    // This is used to rename violation-rule variables (e.g. `a`) to the names used in
    // the sketch transaction rules (e.g. `amount`), so the extracted predicate functor
    // does not contain free variables when inserted into the transaction rule body.
    val canonicalTxLiteral: Map[Relation, Literal] = program.transactionRules()
      .flatMap { txRule =>
        txRule.body.collectFirst {
          case lit: Literal if lit.relation.name.startsWith(transactionRelationPrefix) =>
            lit.relation -> lit
        }
      }.toMap

    // for each txViolation rule: recv_tx(...), p1, p2,...
    // if it has only one functor, then negate that functor, and keep p1,p2,...
    // as the binding literal, and recv_tx... as the tx literal.
    // if it has more than one functor, skip it for now.
    val predicates = txViolationRules.flatMap  { rule =>
      val txLiteral = PredicateEnumerator.extractTxLiteral(rule)

      // collect binding literals (all body Literals except the tx literal)
      val bindingLiterals: Set[Literal] =
        rule.body.collect { case l: Literal if l != txLiteral => l }.toSet

      // Assign functors are temp-variable definitions (e.g. total := earnings + affiliateEarnings),
      // not real guard conditions; ignore them when checking for "single condition" rules.
      val condFunctors = rule.functors.filterNot(_.isInstanceOf[Assign])
      if (condFunctors.size != 1) {
        None
      }
      else if (rule.body.exists(_.relation.name.startsWith("once"))) {
        // skip those tracking relations
        None
      }
      else {
        val f = condFunctors.head
        val negated = Functor.negate(f)
        val predicate = Predicate(Context(txLiteral, bindingLiterals), negated)

        // Rename violation-rule tx literal variables to match the sketch transaction rule.
        // E.g. violation rule uses `recv_withdrawMoney(a)` with functor `a>0`, but the
        // transaction rule uses `recv_withdrawMoney(amount)`.  Without renaming, `a` would
        // be an unbound free variable in the synthesized rule body.
        val renamed: Predicate = canonicalTxLiteral.get(txLiteral.relation) match {
          case Some(canonLit) =>
            // Only rename when the canonical position has a proper named variable (not a wildcard
            // `_`).  Renaming a violation-rule variable to `_` would produce degenerate functors
            // like `_==s_1` which always evaluate as free-variable matches and mislead CEGIS.
            val renameMap: Map[Parameter, Parameter] = txLiteral.fields.zip(canonLit.fields)
              .collect {
                case (from: Variable, to: Variable)
                  if from.name != to.name
                    && !to.name.startsWith("_")
                    && !from.name.startsWith("_") =>
                  from -> to
              }
              .toMap
            if (renameMap.nonEmpty) predicate.rename(renameMap) else predicate
          case None => predicate
        }

        Some(rule -> renamed)
      }
    }.toMap
    predicates
  }

  def enumeratePredicates(program: Program): Map[Rule,Set[Predicate]] = {
    val txRules = program.transactionRules()

    // init a mutable mapping from rule to set of predicate objects
    val predicates: mutable.Map[Rule, Set[Predicate]] = mutable.Map.empty


    for (txRule <- txRules) {
      val txLiteral = PredicateEnumerator.extractTxLiteral(txRule)
      /** 1. Simply comparing parameter in the txRule  */
      val singles = singlePredicate(txLiteral)

      /** 2. Binding one indexed relation, then add one predicate. */
      val indexedRelations = program.relations.collect {
        case r: SimpleRelation if ( relationIndices.contains(r)) => r
      }
      val withBindings: Set[Predicate] = withOneBinding(txLiteral, indexedRelations)

      /** 3. Binding one singleton relation, and add one predicate. */
      val withOneSingleton = singletonRelations.flatMap(
        r => withOneBindingSingleton(txLiteral, r))

      /** 4. todo: Bind two singleton relation, and add a binary operator that compares the two. */
      val withTwoSingleton = withTwoSingletonBindings(txLiteral, singletonRelations)

      /** 5. Bind UDF relations: generate predicates checking the UDF return value.
       *  This allows CEGIS to synthesize guards like require(isValidSignature(...)). */
      val udfRelations = program.udfs.collect { case sr: SimpleRelation => sr }
      val withUdf: Set[Predicate] = udfRelations.flatMap(udfRel => makeUdfBinding(txLiteral, udfRel))

      predicates.update(txRule, singles ++ withBindings ++ withOneSingleton ++ withTwoSingleton ++ withUdf)
    }
    predicates.toMap
  }

  /** Generate predicates that bind one UDF literal and check its boolean return value.
   *
   *  For a UDF `udf(in1, in2, ..., retVal: bool)`:
   *  - Map each input field to a type-compatible parameter from txLiteral/msgSender/msgValue.
   *  - Produce two predicates per valid binding: retVal == true and retVal == false.
   *  - Only boolean return UDFs are handled (UDFs whose last field is BooleanType).
   *
   *  At evaluate-time the Interpreter mocks the UDF return as 0 (false), so CEGIS
   *  will select the `retVal == true` predicate to block counterexample traces.
   */
  private def makeUdfBinding(txLiteral: Literal, udfRel: SimpleRelation): Set[Predicate] = {
    // Only handle UDFs whose last signature field is BooleanType (return value)
    val returnType = udfRel.sig.lastOption.getOrElse(return Set.empty)
    if (!returnType.isInstanceOf[BooleanType]) return Set.empty

    val returnIdx    = udfRel.sig.size - 1
    val inputIndices = udfRel.sig.indices.dropRight(1).toList

    // Primary candidates: parameters from the tx literal itself.
    // Fallback candidates: msgSender and msgValue, used only when no primary candidate
    // of the required type exists.  Preferring tx-literal params prevents the enumerator
    // from binding UDF inputs to unrelated context values (e.g. isValidTokenId(msgValue,…)
    // instead of isValidTokenId(tid,…)).
    val primaryCandidates = txLiteral.fields.filter {
      case v: Variable => v.name != "_" && !v.name.startsWith("_")
      case _: Constant => true
    }
    val fallbackCandidates = (Context.msgSender.fields ++ Context.msgValue.fields).filter {
      case v: Variable => v.name != "_" && !v.name.startsWith("_")
      case _: Constant => true
    }

    // For each input position find type-consistent candidates, primary first.
    val allParams: List[List[Parameter]] = inputIndices.map { idx =>
      val relType = udfRel.sig(idx)
      val primary  = primaryCandidates.collect { case p if p._type == relType => p }.toList
      val fallback = fallbackCandidates.collect { case p if p._type == relType => p }.toList
      if (primary.nonEmpty) primary else fallback
    }

    // If any input position has no candidates, no binding can be produced
    if (allParams.exists(_.isEmpty)) return Set.empty

    // Cartesian product of all input candidates; keep only fully-distinct combos
    val combos = allParams.foldLeft(Seq(Seq.empty[Parameter])) { (acc, params) =>
      for (a <- acc; p <- params) yield a :+ p
    }
    val uniqueCombos = combos.filter(ps => ps.distinct.size == ps.size)

    uniqueCombos.flatMap { inputParams =>
      val retVar = Variable(returnType, s"${udfRel.name}_ret")
      val fields: List[Parameter] = inputIndices.map(i => inputParams(i)) :+ retVar
      val bindingLiteral = Literal(udfRel, fields)
      val context = Context(txLiteral, Set(bindingLiteral))

      // Two guards: the UDF must succeed (true) / must fail (false)
      Set(
        Predicate(context, Equal(retVar, Constant(returnType, "true"))),
        Predicate(context, Equal(retVar, Constant(returnType, "false")))
      )
    }.toSet
  }

  private def singlePredicate(txLiteral: Literal): Set[Predicate] = {
    val context = Context(txLiteral, Set.empty)
    val functors: Set[Functor] = singleAtomCandidates(txLiteral)
    functors.map(f => Predicate(context, f))
  }

  /** For each relation in bindingRels, lookup its index;
   invoke makeOneBinding. For each output Literal,
   make a Predicate. */
  private def withOneBinding(txLiteral: Literal, bindingRels: Set[SimpleRelation]): Set[Predicate] = {
    // Count explicit address-typed parameters in the tx literal (wildcards excluded).
    val txAddressCount = txLiteral.fields.count {
      case v: Variable => v._type == Type.addressType && v.name != "_" && !v.name.startsWith("_")
      case _           => false
    }

    bindingRels.flatMap { rel =>
      val indices = relationIndices.getOrElse(rel, Nil)

      // If a state relation requires 2+ address key columns, only generate binding
      // predicates when the tx literal has strictly MORE address parameters than the
      // relation's key arity.  This prevents spurious allowance(msgSender, s, ...) > 0
      // guards from being generated for direct-transfer transactions like
      // transfer(from, to, n) (2 addresses), which have no "spender" role and do not
      // semantically interact with allowance mappings.
      val keyAddressCount = indices.count(i => rel.sig(i) == Type.addressType)
      if (keyAddressCount >= 2 && txAddressCount <= keyAddressCount) Set.empty[Predicate]
      else {

      val bindingLiterals = makeOneBinding(txLiteral, rel, indices)
      bindingLiterals.flatMap { bindingLiteral =>
        val context = Context(txLiteral, Set(bindingLiteral))

        // val bindingVars = bindingLiteral.fields.collect { case v: Variable => v }
        val bindingVars = indices.map(i => bindingLiteral.fields(i))
        val valueVars = bindingLiteral.fields.diff(bindingVars)

        // Only keep functors that refer to at least one variable in bindingLiteral.
        // crossMsgValue is intentionally excluded: comparing an indexed-relation value
        // (e.g. a stored timestamp or lockPeriod) against the ETH msg.value of the current
        // transaction is almost never semantically correct for non-ETH state variables and
        // produces spurious guards like `villageTimestamp_x1 < msgValue` that would block
        // all calls in real execution when msgValue == 0.
        val singles = singleAtomCandidates(bindingLiteral)
        val crossTx = crossRelationComparison(txLiteral, bindingLiteral)
        val crossMsgSender = crossRelationComparison(bindingLiteral, Context.msgSender)
        val functors = (singles ++ crossTx ++ crossMsgSender)
          .filter { f => functorParams(f).exists {
            // case v: Variable => bindingVars.contains(v)
            case v: Variable => valueVars.contains(v)
            case _ => false
          }}
        functors.map(f => Predicate(context, f))
      }
      } // end else (keyAddressCount filter)
    }
  }

  private def withOneBindingSingleton(txLiteral: Literal, relation: SingletonRelation): Set[Predicate] = {
    def makeSingletonLiteral(relation: SingletonRelation): Literal = {
      val paramName: String = s"${relation.name}_${relation.memberNames.head}"
      val p = Variable(relation.sig.head, paramName)
      Literal(relation, List(p))
    }
    val bindingLiteral: Literal = makeSingletonLiteral(relation)

    val context = Context(txLiteral, Set(bindingLiteral))

    // Only keep functors that refer to at least one variable in bindingLiteral
    val singles = singleAtomCandidates(bindingLiteral)
    val crossTx = crossRelationComparison(txLiteral, bindingLiteral)
    val crossMsgSender = crossRelationComparison(bindingLiteral, Context.msgSender)
    val crossMsgValue = crossRelationComparison(bindingLiteral, Context.msgValue)
    val functors = (singles ++ crossTx ++ crossMsgSender ++ crossMsgValue)
    //   .filter { f => functorParams(f).exists {
    //     case v: Variable => bindingVars.contains(v)
    //     case _ => false
    //   }}
    functors.map(f => Predicate(context, f))
  }


  /** Refactored: Given a txLiteral with parameters a,b,c...,
   *  make a set of literals of the indexed relation
   *  where each literal has the indexed parameter bind to one of the
   *  type consistent parameter in txLiteral, msgSender, or msgValue. */
  private def makeOneBinding(txLiteral: Literal, indexedRelation: Relation,
                             indices: List[Int]): Set[Literal] = {
    // Step 1: Gather candidate parameters from txLiteral, msgSender, and msgValue.
    // Exclude anonymous/wildcard variables (name == "_" or starts with "_") so that
    // key positions in the binding literal are always concretely bound. Without this
    // filter, wildcard fields in the tx literal (e.g. the `_` placeholders in
    // `recv_withdraw(earnings, affiliateEarnings, _, _, _, _, _)`) would be used as
    // key parameters, producing literals like `stakeLockPeriod(msgSender, _, x)` that
    // the SolidityTranslator cannot compile ("all keys must be in search conditions").
    val candidates = (txLiteral.fields ++ Context.msgSender.fields ++ Context.msgValue.fields).filter {
      case v: Variable => v.name != "_" && !v.name.startsWith("_")
      case _: Constant => true
    }
    // Step 2: For each index, find all type-consistent candidates
    val bindings = indices.flatMap { idx =>
      val relType = indexedRelation.sig(idx)
      candidates.collect { case p if p._type == relType => (idx, p) }
    }
    if (bindings.isEmpty) Set.empty
    else {
      val allIdxs = indices
      // Step 3: For each index, collect all possible parameters
      val allParams = allIdxs.map(idx => bindings.filter(_._1 == idx).map(_._2)).filter(_.nonEmpty)
      // Step 4: Cartesian product of all possible parameter choices for each index
      val combos = allParams.foldLeft(Seq(Seq.empty[Parameter])) { (acc, params) =>
        for (a <- acc; p <- params) yield a :+ p
      }
      // Step 5: Build fields for the new literal
      val uniqueCombos = combos.filter(params => params.distinct.size == params.size)

      // combos.map { paramsForIndices =>
      uniqueCombos.map { paramsForIndices =>
        val fields = indexedRelation.sig.zipWithIndex.map { case (t, i) =>
          val idxInIndices = allIdxs.indexOf(i)
          if (idxInIndices >= 0) paramsForIndices(idxInIndices) else Variable(t, s"${indexedRelation.name}_x$i")
        }
        Literal(indexedRelation, fields)
      }.toSet
    }
  }

  /** For each relation, return a type consistent literal,
   * and each literal uses unique variable name */
  private def makeLiterals(rels: Set[Relation]): Set[Literal] = {
    rels.flatMap { r =>
      val arity = relationArity(r)
      val vars = List.tabulate(arity)(i => Variable(r.sig(i), s"${r.name}_x$i"))
      Set(Literal(r, vars))
    }
  }

  private def withTwoSingletonBindings(txLiteral: Literal, singletonRelations: Set[SingletonRelation]): Set[Predicate] = {
    val allRelations: Set[Relation] = singletonRelations ++ Set(MsgSender())
    val singletonPairs = allRelations.subsets(2).collect {
      case pair if pair.size == 2 => pair.toList
    }
    singletonPairs.flatMap {
      case List(r1, r2) =>
        val lit1 = Literal(r1, r1.sig.map(t => Variable(t, s"${r1.name}_x")))
        val lit2 = Literal(r2, r2.sig.map(t => Variable(t, s"${r2.name}_x")))
        val context = Context(txLiteral, Set(lit1, lit2))
        val functors = crossRelationComparison(lit1, lit2).map(f => Predicate(context, f))
        functors
      case _ => Set.empty[Predicate]
    }.toSet
  }


  // ----- helpers -----
  private def relationArity(r: Relation): Int = r.sig.length
  /**
   * Emit for each variable `x`:
   * - If `x` has type `int`, generate: `x == 0`, `x != 0`
   */
  private def singleAtomCandidates(literal: Literal): Set[Functor] = {
    literal.fields.collect {
      case v: Variable =>
        v._type match {
          case datalog.NumberType(name) =>
            // IMPORTANT:
            // For Solidity `uint`, predicates like `x < 0` are impossible and lead to guards that
            // permanently disable transactions (e.g., `released_n < 0`).
            // So we do NOT generate `< 0` candidates for uint.
            if (name == "uint") {
              Set(
                Unequal(Constant(v._type, "0"), v),
                Greater(Param(v), Param(Constant(v._type, "0"))),
                Geq(Param(v), Param(Constant(v._type, "0")))
                // For uint, omit both `< 0` and `<= 0` candidates.
                // (<=0 is redundant with ==0, and can be abused to disable transactions.)
              )
            } else {
              Set(
                Equal(Constant(v._type, "0"), v),
                Unequal(Constant(v._type, "0"), v),
                Greater(Param(v), Param(Constant(v._type, "0"))),
                Geq(Param(v), Param(Constant(v._type, "0"))),
                Lesser(Param(v), Param(Constant(v._type, "0"))),
                Leq(Param(v), Param(Constant(v._type, "0")))
              )
            }
          case datalog.BooleanType() => Set(
            Equal(v, Constant.CTrue),
            Equal(v, Constant.CFalse),
          )
          case _ => Set.empty[Functor]
        }
    }.flatten.toSet
  }

  private def isTxnRelation(literal: Literal): Boolean = {
    literal.relation.name.startsWith("transaction")
  }

  /** Pair-wise relation comparison, avoid two transaction
   * relation (with the transaction relation prefix in name)
   * directly comparing though.
   * */
  private def pairwise(insts: Vector[Literal]): Set[(Literal, Literal)] = {
    (for {
      i <- insts.indices
      j <- (i + 1) until insts.size
      if !(isTxnRelation(insts(i)) && isTxnRelation(insts(j)))
    } yield (insts(i), insts(j))).toSet
  }

  /** For each pair produce equalities x_i == y_j at matching positions,
   only when x_i and y_j has the same type
  */
  private def crossRelationComparison(p: (Literal, Literal)): Set[Functor] = {
    val (a, b) = p
    (for {
      i <- a.fields.indices
      j <- b.fields.indices
      val fieldA = a.fields(i)
      val fieldB = b.fields(j)
      if fieldA._type == fieldB._type && fieldA != fieldB // avoid reflexive comparison
    } yield {
      val eqSet = Set(Equal(fieldA, fieldB))
      fieldA._type match {
        case datalog.NumberType(_) =>
          eqSet ++ Set(
            Greater(Param(fieldA), Param(fieldB)),
            Geq(Param(fieldA), Param(fieldB)),
            Lesser(Param(fieldA), Param(fieldB)),
            Leq(Param(fieldA), Param(fieldB))
          )
        case _ => eqSet
      }
    }).flatten.toSet
  }
}

object PredicateEnumerator {
  def extractTxLiteral(txRule: Rule): Literal = {
    val txLits: Seq[Literal] =
      txRule.body.collect { case lit: Literal
        if imp.SolidityTranslator.isTransactionTriggerRelation(lit.relation) => lit
      }.toSeq

    txLits match {
      case Seq(lit) => lit
      case Seq() =>
        throw new IllegalArgumentException(s"No transaction literal found in rule: $txRule")
      case _ =>
        throw new IllegalArgumentException(s"Multiple transaction literals found in rule: $txRule")
    }
  }

  // Helper to extract all parameters from a Functor
  def functorParams(f: Functor): Seq[Parameter] = f match {
    case operator: ArithOperator =>
      extractParameters(operator.a) ++ extractParameters(operator.b)
    case Equal(lhs, rhs) => extractParameters(lhs) ++ extractParameters(rhs)
    case Unequal(lhs, rhs) => extractParameters(lhs) ++ extractParameters(rhs)
    case Assign(a, b) => throw new UnsupportedOperationException("Assign functor is not supported")
  }


}