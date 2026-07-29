contract Nft {
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct OwnerOfTuple {
    address p;
    bool _valid;
  }
  struct ApprovedTuple {
    address p;
    bool _valid;
  }
  struct TotalMintedTuple {
    uint n;
    bool _valid;
  }
  mapping(int=>OwnerOfTuple) ownerOf;
  mapping(int=>ApprovedTuple) approved;
  TotalMintedTuple totalMinted;
  OwnerTuple owner;
  event Mint(address to,int tokenId);
  event TransferFrom(address from,address to,int tokenId);
  event InvalidTx();
  event Transfer(address from,address to,int tokenId);
  event Burn(int tokenId);
  event Approve(address owner,address spender,int tokenId);
  constructor() public {
    updateOwnerOnInsertConstructor_r9();
  }
  function transfer(address from,address to,int tokenId) public    {
      bool r8 = updateTransferOnInsertRecv_transfer_r8(from,to,tokenId);
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function burn(int tokenId) public    {
      bool r6 = updateBurnOnInsertRecv_burn_r6(tokenId);
      if(r6==false) {
        revert("Rule condition failed");
      }
  }
  function approve(address spender,int tokenId) public    {
      bool r12 = updateApproveOnInsertRecv_approve_r12(spender,tokenId);
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalMinted() public view  returns (uint) {
      uint n = totalMinted.n;
      return n;
  }
  function getOwner() public view  returns (address) {
      address p = owner.p;
      return p;
  }
  function transferFrom(address from,address to,int tokenId) public    {
      bool r2 = updateTransferFromOnInsertRecv_transferFrom_r2(from,to,tokenId);
      if(r2==false) {
        revert("Rule condition failed");
      }
  }
  function getOwnerOf(int tokenId) public view  returns (address) {
      address p = ownerOf[tokenId].p;
      return p;
  }
  function getApproved(int tokenId) public view  returns (address) {
      address p = approved[tokenId].p;
      return p;
  }
  function mint(address to,int tokenId) public    {
      bool r11 = updateMintOnInsertRecv_mint_r11(to,tokenId);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function updateTransferOnInsertRecv_transfer_r8(address from,address to,int tokenId) private   returns (bool) {
      address s = msg.sender;
      address o = ownerOf[tokenId].p;
      if(o==s) {
        updateOwnerOfOnInsertTransfer_r3(to,tokenId);
        emit Transfer(from,to,tokenId);
        return true;
      }
      return false;
  }
  function updateOwnerOfOnInsertTransfer_r3(address to,int tokenId) private    {
      ownerOf[tokenId] = OwnerOfTuple(to,true);
  }
  function updateApproveOnInsertRecv_approve_r12(address spender,int tokenId) private   returns (bool) {
      address s = msg.sender;
      address o = ownerOf[tokenId].p;
      if(o==s) {
        updateApprovedOnInsertApprove_r0(spender,tokenId);
        emit Approve(o,spender,tokenId);
        return true;
      }
      return false;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r2(address from,address to,int tokenId) private   returns (bool) {
      address s = msg.sender;
      address a = approved[tokenId].p;
      if(a==s) {
        updateOwnerOfOnInsertTransferFrom_r16(to,tokenId);
        emit TransferFrom(from,to,tokenId);
        return true;
      }
      return false;
  }
  function updateTotalMintedOnIncrementMintCount_r10(int n) private    {
      int _delta = int(n);
      uint x_totalMinted__n = totalMinted.n;
      uint newValue = updateuintByint(x_totalMinted__n,_delta);
      totalMinted.n = newValue;
  }
  function updateBurnOnInsertRecv_burn_r6(int tokenId) private   returns (bool) {
      address s = msg.sender;
      address o = ownerOf[tokenId].p;
      if(o==s) {
        emit Burn(tokenId);
        return true;
      }
      return false;
  }
  function updateOwnerOfOnInsertMint_r5(address to,int tokenId) private    {
      ownerOf[tokenId] = OwnerOfTuple(to,true);
  }
  function updateOwnerOfOnInsertTransferFrom_r16(address to,int tokenId) private    {
      ownerOf[tokenId] = OwnerOfTuple(to,true);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateMintCountOnInsertMint_r15(address _to0,int _tokenId1) private    {
      int delta0 = int(1);
      updateTotalMintedOnIncrementMintCount_r10(delta0);
  }
  function updateOwnerOnInsertConstructor_r9() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateMintOnInsertRecv_mint_r11(address to,int tokenId) private   returns (bool) {
      uint totalMinted_n = totalMinted.n;
      address s = msg.sender;
      address o = owner.p;
      if(o==s && totalMinted_n<0) {
        updateOwnerOfOnInsertMint_r5(to,tokenId);
        updateMintCountOnInsertMint_r15(to,tokenId);
        emit Mint(to,tokenId);
        return true;
      }
      return false;
  }
  function updateApprovedOnInsertApprove_r0(address spender,int tokenId) private    {
      approved[tokenId] = ApprovedTuple(spender,true);
  }
}