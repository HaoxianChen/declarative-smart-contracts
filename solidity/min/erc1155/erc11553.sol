contract Erc1155 {
  struct TotalBurnTuple {
    uint n;
    bool _valid;
  }
  struct TotalOutTuple {
    uint n;
    bool _valid;
  }
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct TotalInTuple {
    uint n;
    bool _valid;
  }
  struct TotalBalancesTuple {
    uint m;
    bool _valid;
  }
  struct AllowanceTuple {
    uint n;
    bool _valid;
  }
  struct TotalMintTuple {
    uint n;
    bool _valid;
  }
  struct IsApprovedForAllTuple {
    bool approval;
    bool _valid;
  }
  struct TotalSupplyTuple {
    uint n;
    bool _valid;
  }
  mapping(uint=>mapping(address=>TotalOutTuple)) totalOut;
  mapping(uint=>mapping(address=>TotalInTuple)) totalIn;
  mapping(uint=>TotalBalancesTuple) totalBalances;
  mapping(uint=>mapping(address=>TotalBurnTuple)) totalBurn;
  OwnerTuple owner;
  mapping(uint=>mapping(address=>mapping(address=>AllowanceTuple))) allowance;
  mapping(uint=>mapping(address=>TotalMintTuple)) totalMint;
  mapping(address=>mapping(address=>IsApprovedForAllTuple)) isApprovedForAll;
  mapping(uint=>TotalSupplyTuple) totalSupply;
  event Burn(uint tokenId,address p,uint amount);
  event SetApprovalForAll(address sender,address operator,bool approved);
  event Transfer(uint tokenId,address from,address to,uint amount);
  event TransferFrom(uint tokenId,address from,address to,address spender,uint amount);
  event Mint(uint tokenId,address p,uint amount);
  event IncreaseAllowance(uint tokenId,address p,address s,uint n);
  constructor() public {
    updateOwnerOnInsertConstructor_r8();
  }
  function approve(uint tokenId,address s,uint n) public    {
      bool r1 = updateIncreaseAllowanceOnInsertRecv_approve_r1(tokenId,s,n);
      if(r1==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(uint tokenId,address from,address to,uint amount) public    {
      bool r20 = updateTransferFromOnInsertRecv_transferFrom_r20(tokenId,from,to,amount);
      if(r20==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(uint tokenId,address p,address s) public view  returns (uint) {
      uint n = allowance[tokenId][p][s].n;
      return n;
  }
  function getTotalSupply(uint tokenId) public view  returns (uint) {
      uint n = totalSupply[tokenId].n;
      return n;
  }
  function getBalanceOf(uint tokenId,address p) public view  returns (uint) {
      uint n = balanceOf(tokenId,p);
      return n;
  }
  function getIsApprovedForAll(address owner,address operator) public view  returns (bool) {
      bool approval = isApprovedForAll[owner][operator].approval;
      return approval;
  }
  function burn(uint tokenId,address p,uint amount) public    {
      bool r6 = updateBurnOnInsertRecv_burn_r6(tokenId,p,amount);
      if(r6==false) {
        revert("Rule condition failed");
      }
  }
  function safeTransferFrom(address f,address t,uint id,uint amount,uint data) public    {
      revert("Rule condition failed");
  }
  function mint(uint tokenId,address p,uint amount) public    {
      bool r9 = updateMintOnInsertRecv_mint_r9(tokenId,p,amount);
      if(r9==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(uint tokenId,address to,uint amount) public    {
      bool r11 = updateTransferOnInsertRecv_transfer_r11(tokenId,to,amount);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function updateAllowanceOnIncrementAllowanceTotal_r0(uint t,address o,address s,int m) private    {
      int _delta = int(m);
      uint newValue = updateuintByint(allowance[t][o][s].n,_delta);
      allowance[t][o][s].n = newValue;
  }
  function updateAllMintOnInsertMint_r16(uint t,uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r12(t,delta0);
  }
  function updateTotalOutOnInsertTransfer_r14(uint t,address p,uint n) private    {
      totalOut[t][p].n += n;
  }
  function updateSpentTotalOnInsertTransferFrom_r19(uint t,address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r0(t,o,s,delta0);
  }
  function updateAllBurnOnInsertBurn_r5(uint t,uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r12(t,delta0);
  }
  function updateTotalInOnInsertTransfer_r17(uint t,address p,uint n) private    {
      totalIn[t][p].n += n;
  }
  function updateMintOnInsertRecv_mint_r9(uint t,address p,uint n) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        if(p!=address(0)) {
          updateAllMintOnInsertMint_r16(t,n);
          updateTotalMintOnInsertMint_r15(t,p,n);
          emit Mint(t,p,n);
          return true;
        }
      }
      return false;
  }
  function updateTransferOnInsertTransferFrom_r10(uint t,address o,address r,uint n) private    {
      updateTotalInOnInsertTransfer_r17(t,r,n);
      updateTotalOutOnInsertTransfer_r14(t,o,n);
      emit Transfer(t,o,r,n);
  }
  function updateTotalSupplyOnIncrementAllBurn_r12(uint t,int b) private    {
      int _delta = int(-b);
      uint newValue = updateuintByint(totalSupply[t].n,_delta);
      totalSupply[t].n = newValue;
  }
  function updateOwnerOnInsertConstructor_r8() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTotalMintOnInsertMint_r15(uint t,address p,uint n) private    {
      totalMint[t][p].n += n;
  }
  function updateTotalSupplyOnIncrementAllMint_r12(uint t,int m) private    {
      int _delta = int(m);
      uint newValue = updateuintByint(totalSupply[t].n,_delta);
      totalSupply[t].n = newValue;
  }
  function updateAllowanceOnIncrementSpentTotal_r0(uint t,address o,address s,int l) private    {
      int _delta = int(-l);
      uint newValue = updateuintByint(allowance[t][o][s].n,_delta);
      allowance[t][o][s].n = newValue;
  }
  function updateIncreaseAllowanceOnInsertRecv_approve_r1(uint t,address s,uint n) private   returns (bool) {
      address o = msg.sender;
      uint m = allowance[t][o][s].n;
      uint d = n-m;
      updateAllowanceTotalOnInsertIncreaseAllowance_r18(t,o,s,d);
      emit IncreaseAllowance(t,o,s,d);
      return true;
      return false;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r18(uint t,address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r0(t,o,s,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalBurnOnInsertBurn_r7(uint t,address p,uint n) private    {
      totalBurn[t][p].n += n;
  }
  function balanceOf(uint t,address p) private view  returns (uint) {
      uint i = totalIn[t][p].n;
      uint o = totalOut[t][p].n;
      uint m = totalBurn[t][p].n;
      uint n = totalMint[t][p].n;
      uint s = ((n+i)-m)-o;
      return s;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r20(uint t,address o,address r,uint n) private   returns (bool) {
      address s = msg.sender;
      uint k = allowance[t][o][s].n;
      uint m = balanceOf(t,o);
      if(m>=n && k>=n) {
        updateSpentTotalOnInsertTransferFrom_r19(t,o,s,n);
        updateTransferOnInsertTransferFrom_r10(t,o,r,n);
        emit TransferFrom(t,o,r,s,n);
        return true;
      }
      return false;
  }
  function updateBurnOnInsertRecv_burn_r6(uint t,address p,uint n) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        uint m = balanceOf(t,p);
        if(p!=address(0) && n<=m) {
          updateTotalBurnOnInsertBurn_r7(t,p,n);
          updateAllBurnOnInsertBurn_r5(t,n);
          emit Burn(t,p,n);
          return true;
        }
      }
      return false;
  }
  function updateTransferOnInsertRecv_transfer_r11(uint t,address r,uint n) private   returns (bool) {
      address s = msg.sender;
      uint m = balanceOf(t,s);
      if(n<=m) {
        updateTotalInOnInsertTransfer_r17(t,r,n);
        updateTotalOutOnInsertTransfer_r14(t,s,n);
        emit Transfer(t,s,r,n);
        return true;
      }
      return false;
  }
}