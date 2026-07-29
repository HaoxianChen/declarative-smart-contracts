# Next iteration: `ltcSwapAsset` (spec fixes)

Actionable items for **`synthesis-benchmark/ltcSwapAsset/`** only—driven by gas-sim reverts and reference parity in `experiments/synthesis-gas/`.

**Paths**

- Spec: `declarative-smart-contract/synthesis-benchmark/ltcSwapAsset/` (`schema.dl`, `rules.dl`, `properties.dl`).
- Reference vs synthesis: `experiments/synthesis-gas/sol/reference/ltcSwapAsset.sol`, `experiments/synthesis-gas/sol/synthesis/ltcSwapAsset.sol`.

**Goal:** Same successful traces on both sides for mint / burn / transfer / `increaseAllowance` / `transferFrom` / `swapOwnerTx`, so `results/gas-report.md` averages are comparable (`avg()` ignores reverts).

---

## 1. Symptoms

- **Synth average misleading:** Either most mutators **revert** (avg dominated by cheap calls) or a few paths succeed but **`swapOwnerTx`** cost is an outlier vs reference.
- Likely spec / lowering issues: **no initial `owner` from `constructor()`** in `rules.dl` (only delayed owner via `oldOwner` / `newOwner` / `effectiveTime` after swap); **`transfer` merged with `transferFrom`**; **strict `<` vs `>=`** on burn; **`allowance(owner, spender)`** not consistent end-to-end; guards that read **`allowance[s][o]`** while writing **`allowance[o][s]`**.

---

## 2. Checklist (declarative repo)

- [ ] **`rules.dl`:** Initial owner, e.g. `owner(p) :- constructor(), msgSender(p).`, matching reference (`_owner = msg.sender`); keep delayed ownership only for post–`swapOwner` (or use `initialOwner` cleanly).
- [ ] **`properties.dl`:** **Positive amounts**; **sufficient balance** for burn / transfer / `transferFrom` using **`>=`** (match reference).
- [ ] **`properties.dl`:** Do **not** link **`recv_transfer`** to allowance; allowance / **`spentTotal`** only for **`recv_transferFrom`**.
- [ ] **`schema.dl` + `rules.dl`:** Document **`recv_transferFrom`** argument order (e.g. `(operator, from, to, amount)` or whichever matches reference). Align **`allowanceTotal` / `spentTotal` / `allowance`** to **`(owner-of-funds, spender)`** per `transferFrom(...) :- recv_transferFrom(...)`.
- [ ] **`rules.dl`:** Revisit `transfer(o,r,n) :- transferFrom(o,r,_,n).` — split ledger vs recv if `recv_transfer` picks up wrong guards.
- [ ] **`properties.dl`:** **`invalidTx`** for **`recv_swapOwnerTx`** when **`d == 0`** if not already (reference requires `d > 0`).

---

## 3. Post–spec-fix gas run (feedback)

After `npm run sync:synth` and `GAS_FULL=1 npm test`:

- **`increaseAllowance`** and **`transferFrom`** may still **revert** on synthesis while reference succeeds.
- **`swapOwnerTx`** gas on synth can be **far higher** than reference (e.g. ~116k vs ~28k in one run). A large positive **Diff %** in the Summary is **not** a clean comparison if reverts are dropped from `avg()` or one call dominates.

Harness: `lib/scenarios.js` → `ltcSwapAsset` — setup `mint` + `increaseAllowance(a[1], a[2], 500)`; calls include `increaseAllowance(a[1], a[2], 50)` and `transferFrom(a[1], a[3], a[2], 5)` from `a[2]`.

---

## 4. Why `increaseAllowance` / `transferFrom` revert (synthesis)

Generated **`sol/synthesis/ltcSwapAsset.sol`** (line numbers drift after regenerate):

**`increaseAllowance(o, s, d)`** — implementation updates **`allowance[o][s]`**, but the guard often reads **`allowance[s][o]`** with a condition like **`d > 0 && d <= allowance[s][o]`**. That is the **transpose** of the ERC20 cell. Allowance from setup lives at **`[a[1]][a[2]]`**; the guard inspects **`[a[2]][a[1]]`** (typically zero), so the call **reverts** though reference would succeed.

**`transferFrom(o, r, s, n)`** — with harness args **`o` = token holder**, **`s` = spender** (`msg.sender`), the guard must use **`allowance[o][s]`** and **`balanceOf[o]`**. If the lowered code uses **`allowance[s][o]`** and **`balanceOf[s]`**, it **reverts** (wrong allowance cell + wrong balance account). Reference uses **`_allowance[o][s]`** and **`_balance[o]`**.

**Root cause (spec / translator):** enforce a single convention **`allowance(owner, spender)`** from **`schema.dl`** through **`allowanceTotal` / `spentTotal`** into every guard and store. **`recv_transferFrom`** order must be fixed so **from** and **spender** are not swapped in Solidity.

---

## 5. Verification

1. Edit `synthesis-benchmark/ltcSwapAsset/`; regenerate synthesis.  
2. `npm run sync:synth` and `GAS_FULL=1 npm test` under `experiments/synthesis-gas/`.  
3. In `results/gas-report.md`, confirm **`ltcswapasset`**: **`increaseAllowance`** and **`transferFrom`** show numeric gas (no reverts) and **`swapOwnerTx`** is sane vs ref unless extra logic is intentional.

---

*Companion docs: `SPEC_FIX_SUMMARY.md`, `SYNTHESIS_BENCHMARK_SPEC_SUGGESTIONS.md`.*
