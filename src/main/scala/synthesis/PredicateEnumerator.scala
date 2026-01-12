package synthesis

import datalog.{ArithOperator, Arithmetic, Assign, Constant, Equal, Functor, Geq, Greater, Leq, Lesser, Literal, MsgSender, MsgValue, Param, Parameter, Program, Relation, ReservedRelation, Rule, SimpleRelation, SingletonRelation, Unequal, Variable}
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
                              materializedRelations: Set[Relation])

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

    InterpreterContext(relationIndices, materializedRelations)
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

    // for each txViolation rule: recv_tx(...), p1, p2,...
    // if it has only one functor, then negate that functor, and keep p1,p2,...
    // as the binding literal, and recv_tx... as the tx literal.
    // if it has more than one functor, skip it for now.
    val predicates = txViolationRules.flatMap  { rule =>
      val txLiteral = PredicateEnumerator.extractTxLiteral(rule)

      // collect binding literals (all body Literals except the tx literal)
      val bindingLiterals: Set[Literal] =
        rule.body.collect { case l: Literal if l != txLiteral => l }.toSet

      if (rule.functors.size > 1) {
        None
      }
      else if (rule.body.exists(_.relation.name.startsWith("once"))) {
        // skip those tracking relations
        None
      }
      else {
        val f = rule.functors.head
        val negated = Functor.negate(f)
        Some(rule -> Predicate(Context(txLiteral, bindingLiterals), negated))
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

      predicates.update(txRule, singles ++ withBindings ++ withOneSingleton ++ withTwoSingleton)
    }
    predicates.toMap
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
    bindingRels.flatMap { rel =>
      val indices = relationIndices.getOrElse(rel, Nil)
      val bindingLiterals = makeOneBinding(txLiteral, rel, indices)
      bindingLiterals.flatMap { bindingLiteral =>
        val context = Context(txLiteral, Set(bindingLiteral))

        // val bindingVars = bindingLiteral.fields.collect { case v: Variable => v }
        val bindingVars = indices.map(i => bindingLiteral.fields(i))
        val valueVars = bindingLiteral.fields.diff(bindingVars)

        // Only keep functors that refer to at least one variable in bindingLiteral
        val singles = singleAtomCandidates(bindingLiteral)
        val crossTx = crossRelationComparison(txLiteral, bindingLiteral)
        val crossMsgSender = crossRelationComparison(bindingLiteral, Context.msgSender)
        val crossMsgValue = crossRelationComparison(bindingLiteral, Context.msgValue)
        val functors = (singles ++ crossTx ++ crossMsgSender ++ crossMsgValue)
          .filter { f => functorParams(f).exists {
            // case v: Variable => bindingVars.contains(v)
            case v: Variable => valueVars.contains(v)
            case _ => false
          }}
        functors.map(f => Predicate(context, f))
      }
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
    // Step 1: Gather candidate parameters from txLiteral, msgSender, and msgValue
    val candidates = txLiteral.fields ++ Context.msgSender.fields ++ Context.msgValue.fields
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