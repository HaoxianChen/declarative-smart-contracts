// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

/**
 * UDF base contract for the crowdsale benchmark.
 *
 * The synthesis pipeline will import this file and emit:
 *   contract <SynthName> is CrowdsaleUDF { ... }
 *
 * Function signatures must stay aligned with the .udf declarations in schema.dl:
 *   - getTokenAmount(weiAmount: int, tokens: int)  ->  last column is the return value
 */
contract CrowdsaleUDF {
    // Exchange rate: number of token units a buyer gets per wei.
    // Corresponds to the `rate` variable in the original CrowdsaleOriginal contract.
    int256 public _rate = 1;

    // getTokenAmount(weiAmount: int, tokens: int) -- last column is the return value
    function getTokenAmount(int256 weiAmount) internal view returns (int256) {
        return weiAmount * _rate;
    }
}
