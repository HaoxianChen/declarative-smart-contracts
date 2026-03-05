// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

// UDF implementations for the SheepFarm benchmark.
//
// getYield:         pure lookup table (farmId, sheepIdx) -> yield per hour increment.
// getUpgradePrice:  pure lookup table (farmId, sheepIdx) -> gem cost (original / 10).
// syncYield:        state-reading — accrued wool = villageYield * elapsed_hours (capped 24h).
//
// State variable naming follows synthesizer convention:
//   villageYield     -> mapping(address => VillageYieldTuple)     (field: .y)
//   villageTimestamp -> mapping(address => VillageTimestampTuple) (field: .t)

contract SheepFarmUDF {
    struct VillageYieldTuple     { int y;  bool _valid; }
    struct VillageTimestampTuple { uint t; bool _valid; }

    mapping(address => VillageYieldTuple)     villageYield;
    mapping(address => VillageTimestampTuple) villageTimestamp;

    uint constant WAREHOUSE_CAP = 24; // base warehouse capacity in hours

    /// @notice Yield increment added to hourly rate when buying the nth sheep on farm f.
    ///         Data taken verbatim from the original SheepFarm contract.
    function getYield(uint farmId, uint sheepId) external pure returns (int) {
        if (sheepId == 1)  { uint[6] memory t = [uint(5),    56,   179,   382,    678,    762];   return int(t[farmId]); }
        if (sheepId == 2)  { uint[6] memory t = [uint(8),    85,   272,   581,    1030,   1142];  return int(t[farmId]); }
        if (sheepId == 3)  { uint[6] memory t = [uint(12),   128,  413,   882,    1564,   1714];  return int(t[farmId]); }
        if (sheepId == 4)  { uint[6] memory t = [uint(18),   195,  628,   1340,   2379,   2570];  return int(t[farmId]); }
        if (sheepId == 5)  { uint[6] memory t = [uint(28),   297,  954,   2035,   3620,   3856];  return int(t[farmId]); }
        if (sheepId == 6)  { uint[6] memory t = [uint(42),   450,  1439,  3076,   5506,   5783];  return int(t[farmId]); }
        if (sheepId == 7)  { uint[6] memory t = [uint(63),   675,  2159,  4614,   8259,   8675];  return int(t[farmId]); }
        if (sheepId == 8)  { uint[6] memory t = [uint(95),   1013, 3238,  6921,   12389,  13013]; return int(t[farmId]); }
        if (sheepId == 9)  { uint[6] memory t = [uint(142),  1519, 4857,  10382,  18583,  19519]; return int(t[farmId]); }
        if (sheepId == 10) { uint[6] memory t = [uint(213),  2278, 7285,  15572,  27874,  29278]; return int(t[farmId]); }
        return 0;
    }

    /// @notice Gem cost for buying the nth sheep on farm f (original price / denominator=10).
    function getUpgradePrice(uint farmId, uint sheepId) external pure returns (int) {
        if (sheepId == 1)  { uint[6] memory t = [uint(40),   400,  1200,  2400,   4000,   6000];  return int(t[farmId]); }
        if (sheepId == 2)  { uint[6] memory t = [uint(60),   600,  1800,  3600,   6000,   9000];  return int(t[farmId]); }
        if (sheepId == 3)  { uint[6] memory t = [uint(90),   900,  2700,  5400,   9000,   13500]; return int(t[farmId]); }
        if (sheepId == 4)  { uint[6] memory t = [uint(135),  1300, 4000,  8100,   13500,  20200]; return int(t[farmId]); }
        if (sheepId == 5)  { uint[6] memory t = [uint(200),  2000, 6000,  12100,  20200,  30300]; return int(t[farmId]); }
        if (sheepId == 6)  { uint[6] memory t = [uint(300),  3000, 9100,  18200,  30300,  45500]; return int(t[farmId]); }
        if (sheepId == 7)  { uint[6] memory t = [uint(450),  4500, 13600, 27300,  45500,  68300]; return int(t[farmId]); }
        if (sheepId == 8)  { uint[6] memory t = [uint(680),  6800, 20500, 41000,  68300,  102500];return int(t[farmId]); }
        if (sheepId == 9)  { uint[6] memory t = [uint(1000), 10200,30700, 61500,  102500, 153700];return int(t[farmId]); }
        if (sheepId == 10) { uint[6] memory t = [uint(1500), 15400,46100, 92200,  153700, 230000];return int(t[farmId]); }
        return 0;
    }

    /// @notice Accrued wool for `user` at current block.timestamp.
    ///         Simplified: wool = villageYield.y * min(elapsed_hours, WAREHOUSE_CAP).
    function syncYield(address user) external view returns (int) {
        VillageYieldTuple     memory vy = villageYield[user];
        VillageTimestampTuple memory vt = villageTimestamp[user];
        if (!vy._valid || vy.y <= 0) return 0;
        if (!vt._valid || vt.t == 0) return 0;
        uint elapsed = (block.timestamp - vt.t) / 3600;
        if (elapsed > WAREHOUSE_CAP) elapsed = WAREHOUSE_CAP;
        return vy.y * int(elapsed);
    }
}
