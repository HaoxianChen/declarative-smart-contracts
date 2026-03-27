// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

/**
 * UDF base contract for the bnb benchmark.
 *
 * Represents the native ETH transfer in the original BNB withdrawEther function:
 *   payable(owner).transfer(amount)
 *
 * Function signatures must stay aligned with the .udf declarations in schema.dl:
 *   - sendEther(recipient: address, amount: int, success: bool)
 *     -> last column is the return value
 */
contract BnbUDF {
    // sendEther(recipient, amount) -- last column is the return value
    function sendEther(
        address  /*recipient*/,
        int256   /*amount*/
    ) internal pure returns (bool) {
        // Stub: always succeeds.
        // A real implementation would be: payable(recipient).transfer(uint256(amount))
        return true;
    }
}
