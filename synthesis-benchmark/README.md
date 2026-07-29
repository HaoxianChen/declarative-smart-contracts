# Synthesis benchmark

Each directory contains the following three parts:
- `schema.dl`: Defines the schema for the contract,
  including transaction records and contract states.
- `rules.dl`: Inference rules.
- `properties.dl`: Safety properties.

## Schema definition

The schema defines the structure of transaction records and contract states using relations. For example:
```
transfer(from: address, to: address, amount: int)
```
specifies a transaction record for a transfer operation, with fields for the sender's address, recipient's address, and the amount transferred.

```
.public recv_transfer
```
specifies that this relation is public and can be accessed externally.

```
.violation refundAndWithdraw
```
specifies that this relation represents a safety violation check.

## Inference rules
Inference rules define how contract states are derived from transaction records and other states. For example, the rule:
```
balance(p,n) :- totalIn(p, ni), totalOut(p, no), n = ni - no.
```
This rule specifies that the balance `n` of participant `p` is computed by subtracting the total outgoing amount `no` from the total incoming amount `ni`.



## Temporal Properties in Relational Logic

Temporal properties are expressed relationally. Relations prefixed with `once` represent the temporal operator `Once`, indicating that the condition has been true at least once in the past. All other relations are interpreted as `Always`, meaning the condition must hold at all times.

Each rule specifies a safety violation check. For example, the property:
```
refundAndWithdraw() :- onceWithdraw(true), onceRefund(true).
```
This rule ensures that `withdraw(true)` and `refund(true)` have not both been true simultaneously at any point in the past.

