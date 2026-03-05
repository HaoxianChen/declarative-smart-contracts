// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

/**
 * UDF base contract for the tether benchmark.
 *
 * Implements the fee calculation from the original Tether (USDT) contract:
 *   fee = (value * basisPointsRate) / 10000
 *   if (fee > maximumFee) fee = maximumFee
 *
 * Function signatures must stay aligned with the .udf declarations in schema.dl:
 *   - computeFee(value: int, fee: int)  ->  last column is the return value
 */
contract TetherUDF {
    // Basis points rate (e.g. 10 = 0.1%). Set to 0 by default (no fee).
    // Corresponds to basisPointsRate in the original Tether contract.
    int256 public _basisPointsRate = 0;

    // Maximum fee cap in token units.
    // Corresponds to maximumFee in the original Tether contract.
    int256 public _maximumFee = 0;

    // computeFee(value: int, fee: int) -- last column is the return value
    function computeFee(int256 value) internal view returns (int256) {
        int256 fee = (value * _basisPointsRate) / 10000;
        if (fee > _maximumFee) {
            fee = _maximumFee;
        }
        return fee;
    }
}
