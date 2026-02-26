// SPDX-License-Identifier: MIT
pragma solidity ^0.8.21;

interface IERC20 {
    function balanceOf(address account) external view returns (uint256);
}

/**
 * UDF base contract for the `jokintheboxstakin` benchmark.
 *
 * NOTE:
 * - The synthesis pipeline will `import` this file and generate `contract X is <Base> { ... }`.
 * - Keep function signatures aligned with `.udf` relations in `schema.dl`.
 */
contract JokintheboxstakinUDF {
    IERC20 public jokToken = IERC20(0xA728Aa2De568766E2Fa4544Ec7A77f79c0bf9F97);
    address public stakingSigner = 0x8aaBaf348B299E759D091F17100a95A0F9caD89C;
    mapping(address => uint256) public nonce;

    // isValidSignature(sender, totalEarnings, inETH, messageHash, v, r, s, valid)
    function isValidSignature(
        address beneficiary,
        uint256 amount,
        bool inETH,
        bytes32 messageHash,
        uint256 v,
        bytes32 r,
        bytes32 s
    ) internal view returns (bool) {
        bytes32 payloadHash = keccak256(
            abi.encodePacked(address(this), beneficiary, amount, inETH, messageHash, nonce[beneficiary])
        );
        bytes32 prefixedHash = keccak256(abi.encodePacked("\x19Ethereum Signed Message:\n32", payloadHash));
        address recoveredSigner = ecrecover(prefixedHash, uint8(v), r, s);
        return recoveredSigner == stakingSigner;
    }

    // jokTokenBalance(balance)
    function jokTokenBalance() internal view returns (uint256) {
        return jokToken.balanceOf(address(this));
    }

    function _setStakingSigner(address _stakingSigner) internal {
        stakingSigner = _stakingSigner;
    }

    function _setJokToken(address _jokToken) internal {
        jokToken = IERC20(_jokToken);
    }
}

