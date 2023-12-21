contract Erc1155 {
  struct SpentTotalTuple {
    uint m;
    bool _valid;
  }
  struct BalanceOfTuple {
    uint n;
    bool _valid;
  }
  struct AllMintTuple {
    uint n;
    bool _valid;
  }
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct AllBurnTuple {
    uint n;
    bool _valid;
  }
  struct AllowanceTotalTuple {
    uint m;
    bool _valid;
  }
  struct TotalBalancesTuple {
    uint m;
    bool _valid;
  }
  struct IsApprovedForAllTuple {
    bool approval;
    bool _valid;
  }
  mapping(uint=>mapping(address=>mapping(address=>SpentTotalTuple))) spentTotal;
  mapping(uint=>mapping(address=>BalanceOfTuple)) balanceOf;
  mapping(address=>mapping(address=>IsApprovedForAllTuple)) isApprovedForAll;
  mapping(uint=>AllMintTuple) allMint;
  OwnerTuple owner;
  mapping(uint=>AllBurnTuple) allBurn;
  mapping(uint=>mapping(address=>mapping(address=>AllowanceTotalTuple))) allowanceTotal;
  mapping(uint=>TotalBalancesTuple) totalBalances;
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
      uint n = allowance(tokenId,p,s);
      return n;
  }
  function getTotalSupply(uint tokenId) public view  returns (uint) {
      uint n = totalSupply(tokenId);
      return n;
  }
  function getBalanceOf(uint tokenId,address p) public view  returns (uint) {
      uint n = balanceOf[tokenId][p].n;
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
  function updateBalanceOfOnIncrementTotalBurn_r3(uint t,address p,int m) private    {
      int _delta = int(-m);
      uint newValue = updateuintByint(balanceOf[t][p].n,_delta);
      balanceOf[t][p].n = newValue;
  }
  function updateOwnerOnInsertConstructor_r8() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateBalanceOfOnIncrementTotalMint_r3(uint t,address p,int n) private    {
      int _delta = int(n);
      uint newValue = updateuintByint(balanceOf[t][p].n,_delta);
      balanceOf[t][p].n = newValue;
  }
  function totalSupply(uint t) private view  returns (uint) {
      uint b = allBurn[t].n;
      uint m = allMint[t].n;
      uint n = m-b;
      return n;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalMintOnInsertMint_r15(uint t,address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r3(t,p,delta0);
  }
  function updateIncreaseAllowanceOnInsertRecv_approve_r1(uint t,address s,uint n) private   returns (bool) {
      address o = msg.sender;
      uint m = allowance(t,o,s);
      uint d = n-m;
      updateAllowanceTotalOnInsertIncreaseAllowance_r18(t,o,s,d);
      emit IncreaseAllowance(t,o,s,d);
      return true;
      return false;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r18(uint t,address o,address s,uint n) private    {
      allowanceTotal[t][o][s].m += n;
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
  function updateTotalInOnInsertTransfer_r17(uint t,address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r3(t,p,delta0);
  }
  function updateSpentTotalOnInsertTransferFrom_r19(uint t,address o,address s,uint n) private    {
      spentTotal[t][o][s].m += n;
  }
  function updateTransferOnInsertRecv_transfer_r11(uint t,address r,uint n) private   returns (bool) {
      address s = msg.sender;
      uint m = balanceOf[t][s].n;
      if(n<=m) {
        updateTotalInOnInsertTransfer_r17(t,r,n);
        updateTotalOutOnInsertTransfer_r14(t,s,n);
        emit Transfer(t,s,r,n);
        return true;
      }
      return false;
  }
  function updateTotalBurnOnInsertBurn_r7(uint t,address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r3(t,p,delta0);
  }
  function updateAllMintOnInsertMint_r16(uint t,uint n) private    {
      allMint[t].n += n;
  }
  function updateBurnOnInsertRecv_burn_r6(uint t,address p,uint n) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        uint m = balanceOf[t][p].n;
        if(p!=address(0) && n<=m) {
          updateTotalBurnOnInsertBurn_r7(t,p,n);
          updateAllBurnOnInsertBurn_r5(t,n);
          emit Burn(t,p,n);
          return true;
        }
      }
      return false;
  }
  function updateAllBurnOnInsertBurn_r5(uint t,uint n) private    {
      allBurn[t].n += n;
  }
  function updateBalanceOfOnIncrementTotalIn_r3(uint t,address p,int i) private    {
      int _delta = int(i);
      uint newValue = updateuintByint(balanceOf[t][p].n,_delta);
      balanceOf[t][p].n = newValue;
  }
  function updateBalanceOfOnIncrementTotalOut_r3(uint t,address p,int o) private    {
      int _delta = int(-o);
      uint newValue = updateuintByint(balanceOf[t][p].n,_delta);
      balanceOf[t][p].n = newValue;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r20(uint t,address o,address r,uint n) private   returns (bool) {
      address s = msg.sender;
      uint m = balanceOf[t][o].n;
      uint k = allowance(t,o,s);
      if(m>=n && k>=n) {
        updateSpentTotalOnInsertTransferFrom_r19(t,o,s,n);
        updateTransferOnInsertTransferFrom_r10(t,o,r,n);
        emit TransferFrom(t,o,r,s,n);
        return true;
      }
      return false;
  }
  function updateTotalOutOnInsertTransfer_r14(uint t,address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r3(t,p,delta0);
  }
  function allowance(uint t,address o,address s) private view  returns (uint) {
      uint l = spentTotal[t][o][s].m;
      uint m = allowanceTotal[t][o][s].m;
      uint n = m-l;
      return n;
  }
}