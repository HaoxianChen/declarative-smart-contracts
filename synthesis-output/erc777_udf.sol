// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

/**
 * UDF base contract for the erc777 benchmark.
 *
 * Represents the _callTokensReceived hook from the original ERC777 OpenZeppelin implementation.
 * The hook notifies recipient contracts of incoming tokens (ERC-1820 registry lookup was removed
 * in the benchmark version; this stub always succeeds).
 *
 * Function signatures must stay aligned with the .udf declarations in schema.dl:
 *   - callTokensReceived(from: address, to: address, amount: int, success: bool)
 *     -> last column is the return value
 */
contract ERC777UDF {
    // callTokensReceived(from, to, amount) -- last column is the return value
    function callTokensReceived(
        address /*from*/,
        address /*to*/,
        int256  /*amount*/
    ) internal pure returns (bool) {
        // Stub: always succeeds. A real implementation would call IERC777Recipient.tokensReceived.
        return true;
    }
}
