package synthesis

import datalog.{ArithOperator, Arithmetic, Assign, BooleanType, Constant, Equal, FieldConstraint, FieldNonNegative, FieldNonZero, FieldPositive, Functor, Geq, Greater, Leq, Lesser, Literal, MsgSender, MsgValue, NumberType, Param, Parameter, Program, Relation, ReservedRelation, Rule, SimpleRelation, SingletonRelation, Unequal, Variable, Type}
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
case class Predicate(context: Context, functor: Functor, helperFunctors: Set[Assign] = Set()) {
  override def toString: String = {
    val helpers = if (helperFunctors.isEmpty) "" else s", helperFunctors: ${helperFunctors.mkString("{", ", ", "}")}"
    s"Predicate(context: $context, functor: $functor$helpers)"
  }

  def stableKey: PredicateKey = PredicateKey.fromPredicate(this)

  def canonicalString: String = stableKey.canonicalString

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
    val newHelpers = helperFunctors.map(f => Functor.rename(f, mapping).asInstanceOf[Assign])
    this.copy(context=newContext, functor = newFunctor, helperFunctors = newHelpers)
  }
}

case class InterpreterContext(relationIndices: Map[SimpleRelation, List[Int]],
                              materializedRelations: Set[Relation],
                              udfs: Set[Relation] = Set(),
                              fieldConstraints: Map[String, List[List[FieldConstraint]]] = Map.empty)

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

    InterpreterContext(relationIndices, materializedRelations, program.udfs,
      program.relationFieldConstraints)
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

  def extractPredicateFromTxProperties(program: Program): Map[Rule, Set[Predicate]] = {
    val txViolationRules = program.violationRules.filter(
      _.body.exists(_.relation.name.startsWith(transactionRelationPrefix)))
    val txRulesByRelation: Map[Relation, Set[Rule]] = program.transactionRules().groupBy { txRule =>
      PredicateEnumerator.extractTxLiteral(txRule).relation
    }

    def predicateParams(predicate: Predicate): Set[Parameter] = {
      val bindingParams = predicate.context.bindingLiterals.flatMap(_.fields)
      val functorParamsSet = functorParams(predicate.functor)
      val helperParams = predicate.helperFunctors.flatMap { assign =>
        Arithmetic.extractParameters(assign.b) ++ Set(assign.a.p)
      }
      (bindingParams ++ functorParamsSet ++ helperParams).toSet
    }

    def isCompatibleWithTxRule(predicate: Predicate,
                               sourceTxLiteral: Literal,
                               targetTxLiteral: Literal): Boolean = {
      val referencedParams = predicateParams(predicate)
      sourceTxLiteral.fields.zip(targetTxLiteral.fields).forall {
        case (sourceVar: Variable, targetVar: Variable) if sourceVar.name != "_" && referencedParams.contains(sourceVar) =>
          targetVar.name != "_"
        case _ => true
      }
    }

    def hasDanglingVariables(predicate: Predicate, targetTxLiteral: Literal): Boolean = {
      val available = (
        targetTxLiteral.fields.filterNot(_.name == "_") ++
          predicate.context.bindingLiterals.flatMap(_.fields).filterNot(_.name == "_")
        ).toSet
      predicateParams(predicate).exists {
        case v: Variable =>
          v.name == "_" || (!available.contains(v) && !targetTxLiteral.relation.paramList.contains(v))
        case _ => false
      }
    }

    def attachPredicateToTxRules(predicate: Predicate,
                                 sourceTxLiteral: Literal): Map[Rule, Set[Predicate]] = {
      txRulesByRelation.getOrElse(sourceTxLiteral.relation, Set.empty)
        .flatMap { txRule =>
          val targetTxLiteral = PredicateEnumerator.extractTxLiteral(txRule)
          if (!isCompatibleWithTxRule(predicate, sourceTxLiteral, targetTxLiteral)) None
          else {
            val renameMap: Map[Parameter, Parameter] = sourceTxLiteral.fields.zip(targetTxLiteral.fields)
              .collect {
                case (from: Variable, to: Variable)
                  if from.name != "_"
                    && to.name != "_"
                    && from != to =>
                  from -> to
              }
              .toMap
            val renamed = if (renameMap.nonEmpty) predicate.rename(renameMap) else predicate
            if (hasDanglingVariables(renamed, targetTxLiteral)) None
            else Some(txRule -> renamed)
          }
        }
        .groupMap(_._1)(_._2)
        .view
        .mapValues(_.toSet)
        .toMap
    }

    // For tx-violation rules we seed the negation of safety conditions into the
    // corresponding transaction rule. Historically we only handled rules with a
    // single functor. For UDF-based violations such as:
    //   recv_update(...), this(self), to == self, isValidTokenId(value, ok), ok == true
    // we still want to extract the UDF polarity condition (ok != true), because
    // the non-UDF precondition can already be captured by a separate tx-violation.
    val predicates = txViolationRules.foldLeft(Map.empty[Rule, Set[Predicate]]) { (acc, rule) =>
      val txLiteral = PredicateEnumerator.extractTxLiteral(rule)

      // collect binding literals (all body Literals except the tx literal)
      val bindingLiterals: Set[Literal] =
        rule.body.collect { case l: Literal if l != txLiteral => l }.toSet

      // Assign functors are temp-variable definitions (e.g. total := earnings + affiliateEarnings),
      // not real guard conditions; ignore them when checking for "single condition" rules.
      val assignFunctors = rule.functors.collect { case a: Assign => a }
      val condFunctors = rule.functors.filterNot(_.isInstanceOf[Assign])
      val udfBindingParams: Set[Parameter] = bindingLiterals
        .filter(lit => interpreterContext.udfs.contains(lit.relation))
        .flatMap(_.fields)

      val seedFunctors: Set[Functor] =
        if (condFunctors.size == 1) condFunctors
        else condFunctors.filter { f =>
          functorParams(f).exists(udfBindingParams.contains)
        }

      if (seedFunctors.isEmpty) {
        acc
      }
      else if (rule.body.exists(_.relation.name.startsWith("once"))) {
        // skip those tracking relations
        acc
      }
      else {
        val attachedPredicates: Map[Rule, Set[Predicate]] = seedFunctors.foldLeft(Map.empty[Rule, Set[Predicate]]) {
          case (predAcc, f) =>
            val negated = Functor.negate(f)
            val predicate = Predicate(Context(txLiteral, bindingLiterals), negated, helperFunctors = assignFunctors)
            val attached = attachPredicateToTxRules(predicate, txLiteral)
            predAcc ++ attached.map { case (txRule, preds) =>
              txRule -> (predAcc.getOrElse(txRule, Set.empty) ++ preds)
            }
        }
        acc ++ attachedPredicates.map { case (txRule, preds) =>
          txRule -> (acc.getOrElse(txRule, Set.empty) ++ preds)
        }
      }
    }
    predicates
  }

  def enumeratePredicates(program: Program): Map[Rule,Set[Predicate]] = {
    enumeratePredicatesRestricted(program, fallbackToAllIfEmpty = true)
  }

  def enumeratePredicatesRestricted(program: Program,
                                    fallbackToAllIfEmpty: Boolean = true): Map[Rule, Set[Predicate]] = {
    enumeratePredicatesInternal(program, useRelevantWhitelist = true, fallbackToAllIfEmpty = fallbackToAllIfEmpty)
  }

  def enumeratePredicatesFull(program: Program): Map[Rule, Set[Predicate]] = {
    enumeratePredicatesInternal(program, useRelevantWhitelist = false, fallbackToAllIfEmpty = true)
  }

  private def enumeratePredicatesInternal(program: Program,
                                          useRelevantWhitelist: Boolean,
                                          fallbackToAllIfEmpty: Boolean): Map[Rule,Set[Predicate]] = {
    val txRules = program.transactionRules()

    // init a mutable mapping from rule to set of predicate objects
    val predicates: mutable.Map[Rule, Set[Predicate]] = mutable.Map.empty


    for (txRule <- txRules) {
      val txLiteral = PredicateEnumerator.extractTxLiteral(txRule)
      /** 1. Simply comparing parameter in the txRule  */
      val singles = singlePredicate(txLiteral)

      /** 2. Binding one indexed relation, then add one predicate.
       *
       *  Restrict binding candidates to relations that are semantically relevant to this
       *  transaction: only relations appearing in the tx rule's own sketch body or in
       *  violation rules that constrain this tx relation are allowed.
       *
       *  This prevents spurious bindings such as balanceOf appearing in increaseAllowance
       *  guards or referralPoint appearing in setReferrer guards, which the disambiguation
       *  mechanism cannot reliably suppress when the candidate pool is too broad.
       */
      val txRelName = txLiteral.relation.name
      val sketchBodyRels: Set[SimpleRelation] = txRule.body
        .filterNot(_ == txLiteral)
        .flatMap(lit => lit.relation match {
          case sr: SimpleRelation if relationIndices.contains(sr) => Some(sr)
          case _ => None
        })
      val violationBodyRels: Set[SimpleRelation] = program.violationRules
        .filter(_.body.exists(_.relation.name == txRelName))
        .flatMap(_.body)
        .flatMap(lit => lit.relation match {
          case sr: SimpleRelation if relationIndices.contains(sr) && sr.name != txRelName => Some(sr)
          case _ => None
        })
      val baseAllowedRels: Set[SimpleRelation] =
        if (useRelevantWhitelist) sketchBodyRels ++ violationBodyRels else relationIndices.keySet
      val allowedRels: Set[SimpleRelation] =
        if (baseAllowedRels.nonEmpty || !fallbackToAllIfEmpty) baseAllowedRels else relationIndices.keySet

      // For multi-address-key relations (like allowance), extract the specific parameter
      // binding patterns that appear in violation rules.  When available, these patterns
      // restrict makeOneBinding to only the semantically correct combinations rather than
      // enumerating the full Cartesian product of tx address parameters.
      //
      // Example: for recv_transferFrom(o,r,s,n) with violation literal allowance(o,sp,m),
      // map violation-tx-position → synthesis-tx-field to get allowance(o,s,m) exclusively,
      // preventing spurious candidates like allowance(r,o,...), allowance(msgSender,o,...).
      val violationBindingPatterns: Map[SimpleRelation, Set[Literal]] = {
        val relevantViolRules = program.violationRules
          .filter(_.body.exists(_.relation.name == txRelName))
        relevantViolRules.flatMap { vRule =>
          val violTxLit = vRule.body.find(_.relation.name == txRelName).get
          // Build position-based mapping: violation tx field name → synthesis tx field
          val nameToSynthField: Map[String, Parameter] =
            violTxLit.fields.zipWithIndex.collect {
              case (v: Variable, i) if !v.name.startsWith("_") && i < txLiteral.fields.size =>
                v.name -> txLiteral.fields(i)
            }.toMap
          vRule.body.flatMap { lit =>
            lit.relation match {
              case sr: SimpleRelation
                if violationBodyRels.contains(sr) =>
                val indices = relationIndices.getOrElse(sr, Nil)
                // Map each key field using the violation-tx → synthesis-tx name mapping.
                // Fall back to the original field if no mapping is found.
                val mappedFields = sr.sig.zipWithIndex.map { case (t, i) =>
                  if (indices.contains(i)) {
                    lit.fields(i) match {
                      case v: Variable => nameToSynthField.getOrElse(v.name, v)
                      case c: Constant => c
                    }
                  } else {
                    Variable(t, s"${sr.name}_x$i")
                  }
                }.toList
                Some(sr -> Literal(sr, mappedFields))
              case _ => None
            }
          }
        }
        .groupBy(_._1)
        .map { case (k, pairs) => k -> pairs.map(_._2).toSet }
      }

      val withBindings: Set[Predicate] = withOneBinding(txLiteral, allowedRels, violationBindingPatterns)

      /** 3 & 4. Apply whitelist to singleton relations: only allow those that appear
       *  in the tx rule's own sketch body or in violation rules for this tx.
       *  Mirrors the same logic used for SimpleRelation (allowedRels above).
       */
      val fromSketchSingletons: Set[SingletonRelation] = txRule.body
        .filterNot(_ == txLiteral)
        .flatMap(lit => lit.relation match {
          case sr: SingletonRelation if singletonRelations.contains(sr) => Some(sr)
          case _ => None
        })
      val fromViolationSingletons: Set[SingletonRelation] = program.violationRules
        .filter(_.body.exists(_.relation.name == txRelName))
        .flatMap(_.body)
        .flatMap(lit => lit.relation match {
          case sr: SingletonRelation if singletonRelations.contains(sr) && sr.name != txRelName => Some(sr)
          case _ => None
        })
      val baseAllowedSingletons: Set[SingletonRelation] =
        if (useRelevantWhitelist) fromSketchSingletons ++ fromViolationSingletons else singletonRelations
      val allowedSingletons: Set[SingletonRelation] =
        if (baseAllowedSingletons.nonEmpty || !fallbackToAllIfEmpty) baseAllowedSingletons else singletonRelations

      val withOneSingleton = allowedSingletons.flatMap(r => withOneBindingSingleton(txLiteral, r))
      val withTwoSingleton = withTwoSingletonBindings(txLiteral, allowedSingletons)

      /** 5. Bind UDF relations: generate predicates checking the UDF return value.
       *  This allows CEGIS to synthesize guards like require(isValidSignature(...)).
       *
       *  Only include UDFs that appear in violation rules referencing this transaction.
       *  Computation UDFs (e.g. computeFee, getTokenAmount) that are not guard conditions
       *  would cause Z3 asymmetry: the synthesizer mocks UDF returns as 0 while BMC
       *  treats them as uninterpreted, leading to ineffective guards being selected. */
      val allUdfRelations = program.udfs.collect { case sr: SimpleRelation => sr }
      val violationUdfNames: Set[String] = program.violationRules
        .filter(_.body.exists(_.relation.name == txRelName))
        .flatMap(_.body)
        .collect { case lit if allUdfRelations.exists(_.name == lit.relation.name) => lit.relation.name }
        .toSet
      val relevantUdfs = allUdfRelations.filter(udf => violationUdfNames.contains(udf.name))
      val withUdf: Set[Predicate] = relevantUdfs.flatMap(udfRel => makeUdfBinding(txLiteral, udfRel))

      predicates.update(txRule, singles ++ withBindings ++ withOneSingleton ++ withTwoSingleton ++ withUdf)
    }
    predicates.toMap
  }

  /** Generate predicates that bind one UDF literal and compare its return value.
   *
   *  Supported return types:
   *  - Boolean: retVal == true / false
   *  - Numeric: retVal > 0 / retVal == 0
   *
   *  The numeric templates mirror paper examples such as `reward(..., r), r > 0`
   *  while keeping the candidate space small.
   *
   *  At evaluate-time the Interpreter mocks the UDF return as 0, so positive-return
   *  predicates evaluate to false on counterexample traces and can be selected as
   *  blocking guards.
   */
  private def makeUdfBinding(txLiteral: Literal, udfRel: SimpleRelation): Set[Predicate] = {
    val returnType = udfRel.sig.lastOption.getOrElse(return Set.empty)
    returnType match {
      case _: BooleanType =>
      case _: NumberType =>
      case _ => return Set.empty
    }

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

      returnType match {
        case _: BooleanType =>
          // Two guards: the UDF must succeed (true) / must fail (false)
          Set(
            Predicate(context, Equal(retVar, Constant(returnType, "true"))),
            Predicate(context, Equal(retVar, Constant(returnType, "false")))
          )
        case _: NumberType =>
          val zero = Constant(returnType, "0")
          Set(
            Predicate(context, Greater(Param(retVar), Param(zero))),
            Predicate(context, Equal(retVar, zero))
          )
        case _ =>
          Set.empty
      }
    }.toSet
  }

  private def singlePredicate(txLiteral: Literal): Set[Predicate] = {
    val context = Context(txLiteral, Set.empty)
    val constraints = interpreterContext.fieldConstraints
      .getOrElse(txLiteral.relation.name, List.fill(txLiteral.fields.size)(Nil))
    val functors: Set[Functor] = singleAtomCandidates(txLiteral, constraints)
    functors.map(f => Predicate(context, f))
  }

  /** For each relation in bindingRels, lookup its index;
   invoke makeOneBinding. For each output Literal,
   make a Predicate. */
  private def withOneBinding(txLiteral: Literal, bindingRels: Set[SimpleRelation],
                             violationPatterns: Map[SimpleRelation, Set[Literal]] = Map.empty): Set[Predicate] = {
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

      // When violation-rule patterns are available for this relation, restrict to those
      // patterns only.  This eliminates semantically wrong candidates like
      // allowance(r,o,...) or balanceOf(msgSender,...) while keeping the correct one.
      val bindingLiterals: Set[Literal] =
        if (violationPatterns.contains(rel))
          violationPatterns(rel)
        else
          makeOneBinding(txLiteral, rel, indices)
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
        val bindingRelConstraints = interpreterContext.fieldConstraints
          .getOrElse(bindingLiteral.relation.name, List.fill(bindingLiteral.fields.size)(Nil))
        val singles = singleAtomCandidates(bindingLiteral, bindingRelConstraints)
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
    val singletonRelConstraints = interpreterContext.fieldConstraints
      .getOrElse(bindingLiteral.relation.name, List.fill(bindingLiteral.fields.size)(Nil))
    val singles = singleAtomCandidates(bindingLiteral, singletonRelConstraints)
    val crossTx = crossRelationComparison(txLiteral, bindingLiteral)
    val crossMsgSender = crossRelationComparison(bindingLiteral, Context.msgSender)
    // crossMsgValue intentionally excluded: comparing a singleton state value (e.g. funds, start)
    // against ETH msg.value almost never makes semantic sense and produces spurious guards like
    // `funds_b < msgValue` that would block all calls in real execution when msgValue == 0.
    val functors = (singles ++ crossTx ++ crossMsgSender)
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
   * Emit for each variable `x` a set of zero-comparison candidates.
   *
   * fieldConstraintsByField: per-field list of schema FieldConstraints for this literal's
   * relation, in the same order as literal.fields.  When a field has a FieldPositive (> 0)
   * or FieldNonZero (<> 0) constraint we skip Equal(0, v) because it is vacuously false on
   * every valid trace and only pollutes the candidate space with degenerate guards.
   * Similarly, FieldPositive / FieldNonNegative (>= 0) suppress Lesser(v, 0) / Leq(v, 0).
   */
  private def singleAtomCandidates(literal: Literal,
                                   fieldConstraintsByField: List[List[FieldConstraint]]): Set[Functor] = {
    val paddedConstraints =
      if (fieldConstraintsByField.size >= literal.fields.size) fieldConstraintsByField
      else fieldConstraintsByField ++ List.fill(literal.fields.size - fieldConstraintsByField.size)(Nil)

    literal.fields.zip(paddedConstraints).collect {
      case (v: Variable, fieldCons) =>
        val isPositive    = fieldCons.contains(FieldPositive)
        val isNonNegative = fieldCons.contains(FieldNonNegative)
        val isNonZero     = fieldCons.contains(FieldNonZero)
        v._type match {
          case datalog.NumberType(name) =>
            // For Solidity `uint` and any field with a >= 0 or > 0 schema constraint,
            // predicates like `x < 0` are impossible and permanently disable transactions.
            val excludeNegative = name == "uint" || isPositive || isNonNegative
            // For fields with > 0 or <> 0 schema constraint, `x == 0` is also impossible.
            val excludeZeroEq   = isPositive || isNonZero

            // For uint or non-negative fields, v >= 0 is a tautology and must be excluded.
            val excludeTautologicalGeq = excludeNegative
            val all = Set(
              Some(Equal(Constant(v._type, "0"), v))        .filterNot(_ => excludeZeroEq),
              Some(Unequal(Constant(v._type, "0"), v)),
              Some(Greater(Param(v), Param(Constant(v._type, "0")))),
              Some(Geq(Param(v), Param(Constant(v._type, "0"))))   .filterNot(_ => excludeTautologicalGeq),
              Some(Lesser(Param(v), Param(Constant(v._type, "0")))).filterNot(_ => excludeNegative),
              Some(Leq(Param(v), Param(Constant(v._type, "0"))))   .filterNot(_ => excludeNegative),
            ).flatten
            all
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