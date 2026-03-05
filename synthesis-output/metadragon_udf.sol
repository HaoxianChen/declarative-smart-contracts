// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

// UDF implementations for the MetaDragon (P404Token) benchmark.
//
// isValidTokenId: pure — returns true if value < 30001 (NFT tokenId range).
//                 Used to route transfers: tokenId values go through NFT path,
//                 ERC20 amounts (>= 30001) go through ERC20 path.
//
// nftsForAmount: pure — returns how many NFTs correspond to a given ERC20 amount.
//                TRANSFORM_PRICE = 10000 * 10^18; nfts = amount / TRANSFORM_PRICE.
//                Simplified here to integer arithmetic for synthesis compatibility.

contract MetaDragonUDF {
    uint256 constant TRANSFORM_PRICE = 10000; // simplified: ignore 10^18 scaling

    /// @notice Returns true if `value` is in the NFT tokenId range (< 30001).
    /// @param value The transfer value (tokenId or ERC20 amount).
    /// @return ok   True if value is a valid NFT tokenId.
    function isValidTokenId(uint value) external pure returns (bool ok) {
        return value < 30001;
    }

    /// @notice Returns the number of NFTs that can be minted for `amount` ERC20 tokens.
    ///         nfts = amount / TRANSFORM_PRICE  (integer division).
    ///         Requires amount >= TRANSFORM_PRICE and amount % TRANSFORM_PRICE == 0.
    /// @param amount ERC20 token amount (simplified: no 10^18 scaling).
    /// @return n     Number of NFTs to mint.
    function nftsForAmount(int amount) external pure returns (int n) {
        if (amount <= 0) return 0;
        return amount / int(TRANSFORM_PRICE);
    }
}
