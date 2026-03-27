package synthesis

import datalog.{Relation, Rule}
import util.Misc

import scala.util.parsing.json.JSON

case class PredicateKey(
  txRelation: String,
  bindingLiterals: List[String],
  functor: String,
  helperFunctors: List[String] = Nil
) {
  def normalized: PredicateKey = PredicateKey(
    txRelation = txRelation.trim,
    bindingLiterals = bindingLiterals.map(_.trim).filter(_.nonEmpty).sorted,
    functor = functor.trim,
    helperFunctors = helperFunctors.map(_.trim).filter(_.nonEmpty).sorted
  )

  def canonicalString: String = {
    val n = normalized
    val bindings = n.bindingLiterals.mkString("[", ", ", "]")
    val helpers =
      if (n.helperFunctors.isEmpty) ""
      else s", helpers=${n.helperFunctors.mkString("[", ", ", "]")}"
    s"${n.txRelation}: bindings=$bindings, functor=${n.functor}$helpers"
  }
}

object PredicateKey {
  def fromPredicate(predicate: Predicate): PredicateKey = {
    PredicateKey(
      txRelation = predicate.context.tx.relation.name,
      bindingLiterals = predicate.context.bindingLiterals.toList.map(_.toString).sorted,
      functor = predicate.functor.toString,
      helperFunctors = predicate.helperFunctors.toList.map(_.toString).sorted
    ).normalized
  }
}

case class PredicatePairKey(left: PredicateKey, right: PredicateKey) {
  def normalized: PredicatePairKey = {
    val normalizedKeys = List(left.normalized, right.normalized).sortBy(_.canonicalString)
    PredicatePairKey(normalizedKeys.head, normalizedKeys(1))
  }

  def canonicalString: String = {
    val n = normalized
    s"${n.left.canonicalString} && ${n.right.canonicalString}"
  }
}

object PredicatePairKey {
  def fromPredicates(left: Predicate, right: Predicate): PredicatePairKey =
    PredicatePairKey(PredicateKey.fromPredicate(left), PredicateKey.fromPredicate(right)).normalized
}

case class RelationForbiddenSpec(
  txRelation: String,
  singlePredicates: Set[PredicateKey] = Set.empty,
  predicatePairs: Set[PredicatePairKey] = Set.empty
) {
  def normalized: RelationForbiddenSpec = RelationForbiddenSpec(
    txRelation = txRelation.trim,
    singlePredicates = singlePredicates.map(_.normalized),
    predicatePairs = predicatePairs.map(_.normalized)
  )
}

case class ForbiddenSpec(relations: Map[String, RelationForbiddenSpec]) {
  lazy val normalized: ForbiddenSpec = ForbiddenSpec(
    relations.map { case (txRelation, spec) =>
      txRelation.trim -> spec.normalized.copy(txRelation = txRelation.trim)
    }
  )

  def specFor(txRelation: String): Option[RelationForbiddenSpec] =
    normalized.relations.get(txRelation.trim)

  def singleKeysFor(txRelation: String): Set[PredicateKey] =
    specFor(txRelation).map(_.singlePredicates).getOrElse(Set.empty)

  def pairKeysFor(txRelation: String): Set[PredicatePairKey] =
    specFor(txRelation).map(_.predicatePairs).getOrElse(Set.empty)

  def isEmpty: Boolean = normalized.relations.isEmpty

  def filterCandidateMap(candidates: Map[Rule, Set[Predicate]]): Map[Rule, Set[Predicate]] = {
    candidates.map { case (rule, preds) =>
      val txRelation = PredicateEnumerator.extractTxLiteral(rule).relation.name
      val (filtered, logs) = filterSinglePredicates(txRelation, preds)
      logs.foreach(println)
      rule -> filtered
    }
  }

  def filterSeededPredicateMap(candidates: Map[Rule, Set[Predicate]]): Map[Rule, Set[Predicate]] = {
    candidates.map { case (rule, preds) =>
      val txRelation = PredicateEnumerator.extractTxLiteral(rule).relation.name
      val (afterSingles, singleLogs) = filterSinglePredicates(txRelation, preds)
      val (afterPairs, pairLogs) = breakForbiddenSeededPairs(txRelation, afterSingles)
      (singleLogs ++ pairLogs).foreach(println)
      rule -> afterPairs
    }
  }

  private def filterSinglePredicates(txRelation: String,
                                     predicates: Set[Predicate]): (Set[Predicate], List[String]) = {
    val forbidden = singleKeysFor(txRelation)
    if (forbidden.isEmpty) return (predicates, Nil)

    val kept = predicates.filterNot { predicate =>
      forbidden.contains(PredicateKey.fromPredicate(predicate))
    }
    val removed = predicates.diff(kept).toList.sortBy(_.canonicalString)
    val logs = removed.map(p =>
      s"[forbid] Filtered single predicate for $txRelation: ${p.canonicalString}"
    )
    (kept, logs)
  }

  private def breakForbiddenSeededPairs(txRelation: String,
                                        predicates: Set[Predicate]): (Set[Predicate], List[String]) = {
    val forbiddenPairs = pairKeysFor(txRelation).toList.sortBy(_.canonicalString)
    if (forbiddenPairs.isEmpty) return (predicates, Nil)

    var current = predicates
    var logs = List.empty[String]

    for (pair <- forbiddenPairs) {
      val currentByKey = current.groupBy(PredicateKey.fromPredicate).view.mapValues(_.head).toMap
      val normalizedPair = pair.normalized
      if (currentByKey.contains(normalizedPair.left) && currentByKey.contains(normalizedPair.right)) {
        val dropped = currentByKey(normalizedPair.right)
        current -= dropped
        logs :+= s"[forbid] Broke seeded forbidden pair for $txRelation by dropping: ${dropped.canonicalString}"
      }
    }

    (current, logs)
  }
}

object ForbiddenSpec {
  val empty: ForbiddenSpec = ForbiddenSpec(Map.empty)

  def fromJsonFile(path: String): ForbiddenSpec = {
    val raw = Misc.fileToString(path)
    fromJsonString(raw)
  }

  def fromJsonString(json: String): ForbiddenSpec = {
    val parsed = JSON.parseFull(json).getOrElse {
      throw new IllegalArgumentException("Failed to parse forbidden predicate JSON")
    }
    fromParsedJson(parsed)
  }

  private def fromParsedJson(value: Any): ForbiddenSpec = {
    val root = asMap(value)
    val relations = asList(root.getOrElse("relations", Nil)).map(parseRelationSpec)
    ForbiddenSpec(relations.map(spec => spec.txRelation -> spec.normalized).toMap)
  }

  private def parseRelationSpec(value: Any): RelationForbiddenSpec = {
    val obj = asMap(value)
    val txRelation = requireString(obj, "txRelation")
    val singles = asList(obj.getOrElse("singlePredicates", Nil)).map(v => parsePredicateKey(v, txRelation)).toSet
    val pairs = asList(obj.getOrElse("predicatePairs", Nil)).map(v => parsePredicatePair(v, txRelation)).toSet
    RelationForbiddenSpec(txRelation, singles, pairs).normalized
  }

  private def parsePredicatePair(value: Any, txRelation: String): PredicatePairKey = {
    val items = asList(value)
    require(items.size == 2, s"predicatePairs entry for $txRelation must contain exactly 2 predicate keys")
    PredicatePairKey(
      parsePredicateKey(items.head, txRelation),
      parsePredicateKey(items(1), txRelation)
    ).normalized
  }

  private def parsePredicateKey(value: Any, txRelation: String): PredicateKey = {
    val obj = asMap(value)
    PredicateKey(
      txRelation = txRelation,
      bindingLiterals = asStringList(obj.getOrElse("bindingLiterals", Nil)),
      functor = requireString(obj, "functor"),
      helperFunctors = asStringList(obj.getOrElse("helperFunctors", Nil))
    ).normalized
  }

  private def asMap(value: Any): Map[String, Any] = value match {
    case m: Map[_, _] => m.asInstanceOf[Map[String, Any]]
    case _ => throw new IllegalArgumentException(s"Expected JSON object, got: $value")
  }

  private def asList(value: Any): List[Any] = value match {
    case l: List[_] => l.asInstanceOf[List[Any]]
    case _ => throw new IllegalArgumentException(s"Expected JSON array, got: $value")
  }

  private def asString(value: Any): String = value match {
    case s: String => s
    case _ => throw new IllegalArgumentException(s"Expected JSON string, got: $value")
  }

  private def asStringList(value: Any): List[String] = {
    asList(value).map(asString)
  }

  private def requireString(obj: Map[String, Any], key: String): String = {
    obj.get(key).map(asString).getOrElse {
      throw new IllegalArgumentException(s"Missing required key '$key' in forbidden predicate config")
    }
  }
}
