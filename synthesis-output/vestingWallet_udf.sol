// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

/**
 * UDF base contract for the vestingWallet benchmark.
 *
 * Implements the three-segment linear vesting schedule from the original VestingWallet:
 *   - before vesting starts:  0
 *   - after full duration:    totalAllocation
 *   - during vesting:         (totalAllocation * elapsed) / duration
 *
 * Function signatures must stay aligned with the .udf declarations in schema.dl:
 *   - vestedAmount(totalAlloc: uint, elapsed: uint, amount: uint)  ->  last column is return value
 */
contract VestingWalletUDF {
    // Vesting duration in seconds. Corresponds to _duration in the original contract.
    uint64 public _duration = 365 days;

    // vestedAmount(totalAlloc: uint, elapsed: uint, amount: uint) -- last column is the return value
    function vestedAmount(uint256 totalAlloc, uint256 elapsed) internal view returns (uint256) {
        if (elapsed == 0) {
            return 0;
        }
        if (elapsed >= _duration) {
            return totalAlloc;
        }
        return (totalAlloc * elapsed) / _duration;
    }
}
