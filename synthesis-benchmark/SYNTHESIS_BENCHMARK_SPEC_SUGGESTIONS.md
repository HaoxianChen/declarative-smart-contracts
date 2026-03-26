# Synthesis benchmark spec suggestions

Local note: upstream Datalog lives under `declarative-smart-contract/synthesis-benchmark/` (per-subdir `schema.dl`, `rules.dl`, `properties.dl`). The issues below are spec gaps or inconsistencies that tend to produce odd or degenerate synthesized Solidity.

---

## Critical fixes (likely to corrupt generated semantics)

### `finalizableCrowdSale/` — `rules.dl`

- **Placeholder bug:** `buyToken(p,n) :- recv_buyToken(p,m).` — `m` is not bound from the head in a consistent way; align with the receive tuple (e.g. `buyToken(p,m) :- recv_buyToken(p,m).`) and with `mint(p,n) :- buyToken(p,m), rate(r), n := m*r.`
- **Wrong transition:** `finalized(false) :- finalize().` — finalize should set **finalized = true** (plus any `onceFinalize` / “buy after finalize” tracking you want), not flip false.
- **Missing init:** `start`, `end`, and `rate` appear in `schema.dl` / `properties.dl` but are **not** derived in `rules.dl` from `constructor()`. Add init rules or drop unused properties.

### `cappedCrowdSale/` — `rules.dl` vs `properties.dl` vs `schema.dl`

- **Missing init:** Properties use `start(s)` / `end(e)` / `now(t)` for buys, but `rules.dl` never defines `start` / `end` from construction. Constructor is only `(c, r)`; either extend the constructor schema to include the sale window (and init rules) or remove those property clauses.
- **Cap vs raised:** `raised` sums raw `buyToken` amounts while minted tokens use **`m * r`**. Make the economic model explicit: add properties and rules so **cap** applies either to **funds raised** or **tokens minted**, consistently.

### `brickBlockToken/` — `rules.dl` + `properties.dl`

- **Undefined supply base:** `totalSupply(n) :- initialSupply(i), burnTotal(b), n := i - b` uses `initialSupply`, but nothing initializes it from `constructor()`. Add an init rule (e.g. zero or a constructor parameter) or derive supply like other ERC20-style specs.
- **`evacuateAfterUpgrade`:** Rule `evacuateAfterUpgrade() :- recv_evacuate(_), onceUpgrade(b), b == false.` fires when upgrade has **not** occurred; the comment says “cannot evacuate **after** upgrade”. Reconcile name, comment, and intended temporal property.

---

## Strengthen `properties.dl` (reduce degenerate guards)

### `tokenPartition/`

- Add **invalidTx** (or relational guards) for: `msgSender == sender` on partition transfer, **sufficient balance** on transfer/redeem, and **positive amounts** where intended — so synthesis does not settle on trivial **n == 0**–only success paths.

### `erc20burnable/`

- Add explicit **invalidTx** when **allowance < amount** or **balance < amount** on `burnFrom`, so generated guards match usual OpenZeppelin-style preconditions.

### `ltcSwapAsset/`

- Properties cover owner for mint/burn/swap; consider **amount > 0**, balance checks, and alignment with **effectiveTime** / **oldOwner** / **newOwner** in `rules.dl` so delayed ownership is not left to ad hoc inequalities in codegen.

### `auction/`

- **Ties at `highestBid`:** `highestBidder(p) :- bid(p,m), highestBid(n), m == n` may be multi-valued. Specify tie-breaking or forbid ties so `max`-style/`highestBid` updates have a single clear implementation strategy.

### `weth/`

- Properties require **strictly positive** `increaseAllowance` deltas. If the benchmark should allow zero bumps (or mirror **approve**), relax or reshape the property.

### `stakingRewards/`

- `rewardPool(n) :- allRewards(s), n := s` only accumulates `addRewards`. If rewards should be consumed or distributed to stakers, extend rules + properties; otherwise document that the spec is intentionally a minimal pool counter.

---

## Consistency / hygiene

- Lint for: ungrounded `*` relations, constructor arity mismatched with `rules.dl`, and **properties that reference relations never defined in `rules.dl`** (notably **start/end/rate** on capped vs finalizable crowdsales).
- Keep **`recv_*`** violation style consistent across benchmarks (`brickBlockToken` mixes patterns in `rules.dl` / `properties.dl`).

---

## Optional: gas-report-driven parity (reference vs synth)

When reference is **more expensive** because it performs real updates and synthesis often **reverts** or uses **degenerate guards**, fixing **spec + scenarios** together is more reliable than only editing reference Solidity. High-value benchmark names called out earlier: **`tokenPartition`** (nonzero transfer/redeem), **`ltcSwapAsset`** (strict inequalities / scenario alignment), **`cappedCrowdSale`** (rate, cap, finalize, time window), **`brickBlockToken`** (supply and evacuate lifecycle).

---

*Generated for local use; paths above are relative to `declarative-smart-contract` unless stated otherwise.*
