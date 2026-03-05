// SPDX-License-Identifier: MIT
pragma solidity ^0.8.15;

/**
 * UDF base contract for the LevelReferralControllerV2 benchmark.
 *
 * Implements computeClaimable(epoch, user) -> result, which encapsulates the
 * full reward formula from the original contract's claimable() view function:
 *
 *   reward = (tradingPoint * DISCOUNT + referralPoint * REBATE) / PRECISION / TWAP
 *   if vestingDuration > 0:
 *       elapsed = min(now - allocationTime, vestingDuration)
 *       reward  = reward * elapsed / vestingDuration
 *   result = reward > claimed ? reward - claimed : 0
 *
 * Simplification vs original:
 *   - Uses fixed tier-1 rates (DISCOUNT = REBATE = 5e4) instead of dynamic tiers.
 *     Dynamic tier computation requires referrer counts and large thresholds (e30)
 *     that are impractical to exercise in test traces.
 *
 * Struct and mapping names must match the synthesizer's naming convention:
 *   - Struct name:  <RelationName>Tuple  (e.g. TradingPointTuple)
 *   - Field name:   last column of the Datalog declaration (e.g. .amount, .twap)
 *   - Mapping type: mapping(K0 => mapping(K1 => Struct)) for 2-key relations
 */
contract LevelUDF {
    // ── Structs (matching synthesizer convention) ────────────────────────────

    struct TradingPointTuple          { uint amount; bool _valid; }
    struct ReferralPointTuple         { uint amount; bool _valid; }
    struct EpochTWAPTuple             { uint twap;   bool _valid; }
    struct EpochAllocationTimeTuple   { uint t;      bool _valid; }
    struct EpochVestingDurationTuple  { uint d;      bool _valid; }
    struct ClaimedTuple               { uint amount; bool _valid; }

    // ── State mirrors (populated by the synthesized contract) ────────────────

    mapping(uint => mapping(address => TradingPointTuple))  tradingPoint;
    mapping(uint => mapping(address => ReferralPointTuple)) referralPoint;
    mapping(uint => EpochTWAPTuple)                         epochTWAP;
    mapping(uint => EpochAllocationTimeTuple)               epochAllocationTime;
    mapping(uint => EpochVestingDurationTuple)              epochVestingDuration;
    mapping(uint => mapping(address => ClaimedTuple))       claimed;

    // ── Constants ────────────────────────────────────────────────────────────

    uint constant PRECISION = 1e6;
    // Fixed tier-1 rates: 5% discount for trader, 5% rebate for referrer
    uint constant DISCOUNT  = 5e4;
    uint constant REBATE    = 5e4;

    // ── UDF ──────────────────────────────────────────────────────────────────

    /**
     * @notice Compute the amount claimable by `user` for `epoch`.
     *
     * Reads tradingPoint, referralPoint, epochTWAP, epochAllocationTime,
     * epochVestingDuration and claimed from storage, then applies:
     *   1. Tier-1 reward formula (trading + referral contributions, divided by TWAP)
     *   2. Linear vesting reduction if vestingDuration > 0
     *   3. Subtract already-claimed amount
     */
    function computeClaimable(uint epoch, address user) internal view returns (uint) {
        uint twap = epochTWAP[epoch].twap;
        if (twap == 0) return 0;

        uint tp         = tradingPoint[epoch][user].amount;
        uint rp         = referralPoint[epoch][user].amount;
        uint allocTime  = epochAllocationTime[epoch].t;
        uint vestDur    = epochVestingDuration[epoch].d;
        uint claimedAmt = claimed[epoch][user].amount;

        uint reward = (tp * DISCOUNT + rp * REBATE) / PRECISION / twap;

        if (vestDur > 0 && block.timestamp > allocTime) {
            uint elapsed = block.timestamp >= allocTime + vestDur
                ? vestDur
                : block.timestamp - allocTime;
            reward = reward * elapsed / vestDur;
        }

        return reward > claimedAmt ? reward - claimedAmt : 0;
    }
}
