// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

/**
 * UDF base contract for the nft benchmark.
 *
 * Represents the _checkOnERC721Received call from the original ERC721 safeTransferFrom.
 * In the original OpenZeppelin implementation this calls IERC721Receiver.onERC721Received
 * on the recipient contract and checks the return value. The benchmark version omits the
 * external call; this stub always succeeds.
 *
 * Function signatures must stay aligned with the .udf declarations in schema.dl:
 *   - onERC721Received(to: address, tokenId: uint, success: bool)
 *     -> last column is the return value
 */
contract NFTUDF {
    // onERC721Received(to, tokenId) -- last column is the return value
    function onERC721Received(
        address  /*to*/,
        uint256  /*tokenId*/
    ) internal pure returns (bool) {
        // Stub: always succeeds. A real implementation would call IERC721Receiver.onERC721Received.
        return true;
    }
}
