# Spec (and tooling) fixes for failing synthesis — summary

Context: fresh `sol/synthesis/*.sol` from `declarative-smart-contract/synthesis-output` vs current **`synthesis-benchmark/`** (`schema.dl`, `rules.dl`, `properties.dl`). Reference path on disk: **`/Users/hxc/projects/declarative-smart-contract/synthesis-benchmark/`**.

Below: each **gas-report N/A / synth fail** mapped to **likely spec gaps** and **concrete edits** (mostly Datalog + one harness note). Several failures are **codegen** bugs triggered by spec shapes; tightening the spec still reduces bad search space.

---

## 1. `auction/` — undeclared `_` / `n` in highest-bid update

**Observed Solidity:** `updateHighestBidOnInsertBid_r12` uses `_` and `n` where neither is in scope (incremental max update is wrong).

**Spec today (`rules.dl`):**  
`highestBid(n) :- bid(_, _), n = max m: bid(_, m).`  
`highestBidder(p) :- bid(p, m), highestBid(n), m == n.`

**Suggestions**

- **Avoid opaque aggregates in the guarded fragment** the translator lowers to Solidity: either document that `max m : bid(_, m)` must lower to a **well-scoped** comparison `(newBid > oldHigh)` with explicit variables, or **replace** with an incremental rule pattern (e.g. derive `highAfterBid(amount)` from last `recv_bid` and prior high) so all variables are named in one rule head.
- **Tie-breaking:** multiple `(p, m)` with `m == highestBid` → non-deterministic `highestBidder`. Add a **property** (forbid ties, or pick lexicographic `p`) so the imperative sketch is unique.

**Owner:** translator + small **`properties.dl`** clarification.

---

## 2. `cappedCrowdSale/` — undeclared `n` in `buyToken` guard

**Observed Solidity:** Condition mixes `p`, `m`, and a stray **`n`** (e.g. tokens minted `m*r` vs raw `m`).

**Spec today:** `mint(p,n) :- buyToken(p,m), rate(r), n := m*r.` and `raised` sums `buyToken` amounts. **`properties.dl`** (updated) no longer mentions `start`/`end`/`now` for buys — good; cap/finalization violations remain.

**Suggestions**

- In **`rules.dl`**, introduce an explicit derived name used only in buy semantics, e.g. `tokensFromPurchase(p, m, k) :- buyToken(p,m), rate(r), k := m*r`, and reference **`k`** everywhere mint/balance depends on purchase (so guards never invent a free `n`).
- Ensure **one** canonical name for the receive amount in **`schema.dl`** placeholders and **`recv_buyToken`** docs (always `m` or always `amount` in placeholder + rules).

**Owner:** **`rules.dl`** + schema comments.

---

## 3. `crowFunding/` — `payable(p).send(int)` type error

**Observed Solidity:** `.send` expects **`uint256`**; spec uses **`int`** for `refund` / `withdraw`.

**Spec today (`rules.dl`):**  
`send(p,n) :- refund(p,n).`  
`send(p,r) :- withdraw(p,r).`

**Suggestions**

- **Do not overload `send` as “native ETH transfer”** with `int` if the target language is Solidity 0.8+. Options:
  - Add `withdrawWei(p: address, amount: uint)` / `refundWei(...)` in **`schema.dl`** with **`uint`** amounts and map those to `call`/`send` in translation, **or**
  - Keep `int` but add a **validated range** property `amount >= 0` and a lowering rule that casts to `uint256` (translator change).
- Align **`invest` / `refund` / `withdraw`** amount types with whatever the chain primitive uses in codegen.

**Owner:** **`schema.dl`** types + **`properties.dl`** (non-negative amounts) + translator lowering.

---

## 4. `erc20burnable/` — undeclared `sp` in `burnFrom`

**Observed Solidity:** Guard uses **`sp`** not defined (should match `recv_burnFrom(from, spender, amount)`).

**Spec today (`schema.dl`):** Args are **`from`**, **`spender`**, **`amount`** — names are clear.

**Suggestions**

- In **`properties.dl`**, add **`invalidTx`** clauses that **use the same argument names** as `recv_burnFrom(from, spender, amount)` and relate them to `msgSender(s)` (e.g. `spender == s`, `amount > 0`, allowance/balance bounds). That gives CEGIS / codegen a single spelling for the spender variable.
- Add explicit **allowance ≥ amount** and **balance ≥ amount** for `burnFrom` so guards are fully constrained (see also `SYNTHESIS_BENCHMARK_SPEC_SUGGESTIONS.md`).

**Owner:** **`properties.dl`** + optional **`rules.dl`** auxiliaries.

---

## 5. `finalizableCrowdSale/` — undeclared `n` in buy path

**Observed Solidity:** Similar to capped: free **`n`** in a conjunction with `m`.

**Spec today (`rules.dl`, updated):** `buyToken(p,m) :- recv_buyToken(p,m).`, `mint(p,n) :- buyToken(p,m), rate(r), n := m*r.`, `rate(1) :- constructor().`, `finalized(true) :- finalize().` — structurally improved vs older broken placeholder.

**Suggestions**

- Same as capped: **one derived relation** for “tokens minted from this buy” and **no** reuse of `n` in guards unless bound in that rule.
- **`properties.dl`** still lacks **time window** / **buy-after-finalize** parity with OpenZeppelin-style crowdsales if you need them; add **`start`/`end`** init in **`rules.dl`** if you reintroduce time **`invalidTx`** lines.

**Owner:** **`rules.dl`** hygiene + optional **`properties.dl`** / init.

---

## 6. `voting/` — `WinsTuple memory` / identifier not unique

**Observed Solidity:** Codegen emits a struct **`WinsTuple`** that does not exist or clashes with ABI layout.

**Spec today (`schema.dl`):** `.decl wins(proposal: uint, b: bool)[0]` — a **binary relation** keyed by proposal.

**Suggestions**

- **Rename** the relation in **`schema.dl` / `rules.dl`** to something that does not trigger a “tuple struct” template (e.g. `proposalWon(proposal)` as a unary derived view, or `hasWon(p)`), **or**
- Split into **`winningProposal(p)`** + **`hasWinner`** only, avoid storing `(p, bool)` pairs if the translator maps every pair-relation to `FooTuple`.

**Owner:** **`schema.dl` + `rules.dl`** naming + translator mapping for 2-col “flag” relations.

---

## 7. `weth/` — undeclared `n` in `increaseAllowance` guard

**Observed Solidity:** `if (n > 0 && d >= 0)` but **`n`** is not a parameter (third param is **`d`**).

**Spec today (`properties.dl`):** `invalidTx() :- recv_increaseAllowance(_, _, n), n <= 0.` — uses **`n`** for the delta.

**Suggestions**

- Use **one variable name end-to-end**: e.g. `recv_increaseAllowance(o, s, delta)` in **`schema.dl`** and **`delta`** in **`properties.dl`** / comments so the translator does not mix **`n`** from another scope.
- If properties require **strict `delta > 0`**, say so consistently (OpenZeppelin allows zero in some paths).

**Owner:** **`schema.dl` + `properties.dl`** naming alignment.

---

## 8. `multiSig/` — deploy “Got 0 expected 2”

**Not primarily spec:** `constructor(o1, o2)` is clear in **`multiSig/schema.dl`**.

**Suggestion:** Gas harness must pass **`CTOR`** args for **synthesis** (same as reference). Track in **smart-contracts** `scenarios.js` (`CTOR.multisig`), not in Datalog.

---

## 9. `dao/` — no synthesis output in tree

**Spec exists** under `synthesis-benchmark/dao/` but **no `dao.sol`** in `synthesis-output` for this snapshot.

**Suggestion:** Add **`dao`** to the synthesis batch / exporter, or **drop `dao`** from paired synth–ref gas tables when only reference is built.

---

## Priority order (impact × ease)

1. **`crowFunding`** — fix **ETH amount type / `send` story** (schema types + lowering).  
2. **`cappedCrowdSale` / `finalizableCrowdSale`** — **eliminate free variables** in buy/mint linking (`k := m*r` pattern).  
3. **`voting`** — **rename / simplify `wins`** to avoid bogus struct emission.  
4. **`auction`** — **incremental high-bid** spec or translator contract for `max`.  
5. **`erc20burnable` / `weth`** — **properties + schema** uniform parameter names and full `burnFrom` constraints.  
6. **`multiSig` ctor** — **harness** only.  
7. **`dao`** — **pipeline** inclusion.

---

*Companion doc: `SYNTHESIS_BENCHMARK_SPEC_SUGGESTIONS.md` (broader benchmark hygiene). Regenerate gas after spec + codegen updates and `npm run sync:synth`.*
