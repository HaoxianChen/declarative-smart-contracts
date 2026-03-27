# Interactive Predicate Forbidding

## Purpose

This document describes the configuration format and evaluation protocol for the
lightweight interactive feedback loop that forbids known-bad synthesized guard
predicates.

The first version supports:

- forbidding a single predicate
- forbidding a predicate pair from appearing together in the same transaction guard

## Config Format

The synthesizer accepts a JSON file through `--forbid-config`.

Supported commands:

- `sbt "run synthesis <path> --forbid-config <json>"`
- `sbt "run cegis --forbid-config <json>"`
- `sbt "run synthesis-all --forbid-config <json>"`
- `sbt "run synthesis-all-split --forbid-config <json>"`

Top-level schema:

```json
{
  "relations": [
    {
      "txRelation": "increaseAllowance",
      "singlePredicates": [
        {
          "bindingLiterals": ["balanceOf(s,balanceOf_x1)"],
          "functor": "d<=balanceOf_x1"
        }
      ],
      "predicatePairs": [
        [
          {
            "bindingLiterals": [],
            "functor": "closed_b==true"
          },
          {
            "bindingLiterals": [],
            "functor": "closed_b==false"
          }
        ]
      ]
    }
  ]
}
```

Notes:

- `bindingLiterals` and `helperFunctors` are matched after sorting.
- `txRelation` is scoped per relation entry, so each predicate key inside that
  entry inherits the same transaction relation.
- pair forbids are normalized internally, so order inside `predicatePairs` does
  not matter.

## Matching Semantics

Each runtime candidate predicate is canonicalized as:

- transaction relation name
- sorted binding literal strings
- functor string
- sorted helper functor strings

Single forbids remove matching candidates before solver search.

Pair forbids are enforced as solver blocking clauses:

- single: `not p`
- pair: `not p1 or not p2`

For property-seeded predicates that bypass solver selection, the implementation
also:

- removes forbidden single predicates before augmentation
- breaks forbidden seeded pairs deterministically by dropping the lexicographically
  later predicate key

## Suggested Evaluation Protocol

Use the seven remaining failure cases as the evaluation set:

- `crowdFunding`
- `vestingWallet`
- `level`
- `linktoken`
- `crowdsale`
- `crowdsale2`
- `controllable`

For each benchmark:

1. Run synthesis with no forbid config and inspect the synthesized guard.
2. If the result contains a clearly wrong predicate, add it to `singlePredicates`.
3. If the result contains a wrong conjunction, add it to `predicatePairs`.
4. Re-run synthesis with the updated config.
5. Repeat until the benchmark succeeds or the interaction budget reaches 10 rounds.

Recommended success criterion:

- the synthesized guard no longer contains the known-bad predicate or pair
- the benchmark reaches the intended transaction rule semantics for that failure case

Report:

- number of repaired benchmarks within 10 rounds
- total rounds used per benchmark
- which failures are fixed by single forbids vs pair forbids
- residual failures, especially `controllable`, where forbidding may still be
  insufficient because the search can converge to a different wrong fixed point

## Initial Case Mapping

- `crowdsale`, `crowdsale2`: best first target for single forbids
- `crowdFunding`: best first target for pair forbids
- `level`, `linktoken`: likely single forbids, but may need multiple rounds
- `vestingWallet`: may require several single forbids because the issue is overfitting
- `controllable`: treat as a stress case; do not assume single/pair forbids alone are enough
