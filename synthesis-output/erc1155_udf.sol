// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

/**
 * UDF base contract for the erc1155 benchmark.
 *
 * Represents the _doSafeTransferAcceptanceCheck call from the original ERC1155 safeTransferFrom.
 * In the original OpenZeppelin implementation this calls IERC1155Receiver.onERC1155Received
 * on the recipient contract and checks the return value. The benchmark version comments out
 * this check; this stub always succeeds.
 *
 * Function signatures must stay aligned with the .udf declarations in schema.dl:
 *   - onERC1155Received(to: address, tokenId: int, amount: int, success: bool)
 *     -> last column is the return value
 */
contract ERC1155UDF {
    // onERC1155Received(to, tokenId, amount) -- last column is the return value
    function onERC1155Received(
        address  /*to*/,
        int256   /*tokenId*/,
        int256   /*amount*/
    ) internal pure returns (bool) {
        // Stub: always succeeds. A real implementation would call IERC1155Receiver.onERC1155Received.
        return true;
    }
}
