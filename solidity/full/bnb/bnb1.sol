contract Bnb {
  struct TotalOutTuple {
    uint n;
    bool _valid;
  }
  struct TotalFreezeTuple {
    uint n;
    bool _valid;
  }
  struct TotalMintTuple {
    uint n;
    bool _valid;
  }
  struct TotalSupplyTuple {
    uint n;
    bool _valid;
  }
  struct AllMintTuple {
    uint n;
    bool _valid;
  }
  struct SpentTotalTuple {
    uint m;
    bool _valid;
  }
  struct AllowanceTuple {
    uint n;
    bool _valid;
  }
  struct TotalInTuple {
    uint n;
    bool _valid;
  }
  struct TotalUnfreezeTuple {
    uint n;
    bool _valid;
  }
  struct TotalBurnTuple {
    uint n;
    bool _valid;
  }
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct FreezeOfTuple {
    uint n;
    bool _valid;
  }
  struct AllowanceTotalTuple {
    uint m;
    bool _valid;
  }
  struct BalanceOfTuple {
    uint n;
    bool _valid;
  }
  struct AllBurnTuple {
    uint n;
    bool _valid;
  }
  mapping(address=>TotalInTuple) totalIn;
  mapping(address=>TotalOutTuple) totalOut;
  mapping(address=>TotalFreezeTuple) totalFreeze;
  OwnerTuple owner;
  mapping(address=>FreezeOfTuple) freezeOf;
  mapping(address=>TotalMintTuple) totalMint;
  TotalSupplyTuple totalSupply;
  AllMintTuple allMint;
  mapping(address=>mapping(address=>AllowanceTotalTuple)) allowanceTotal;
  mapping(address=>mapping(address=>SpentTotalTuple)) spentTotal;
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  mapping(address=>TotalUnfreezeTuple) totalUnfreeze;
  mapping(address=>TotalBurnTuple) totalBurn;
  mapping(address=>BalanceOfTuple) balanceOf;
  AllBurnTuple allBurn;
  event TransferFrom(address from,address to,address spender,uint amount);
  event Burn(address p,uint amount);
  event Mint(address p,uint amount);
  event WithdrawEther(address p,uint amount);
  event IncreaseAllowance(address p,address s,uint n);
  event Unfreeze(address p,uint n);
  event Freeze(address p,uint n);
  event Transfer(address from,address to,uint amount);
  constructor(uint initialSupply) public {
    updateAllMintOnInsertConstructor_r9(initialSupply);
    updateTotalSupplyOnInsertConstructor_r5(initialSupply);
    updateBalanceOfOnInsertConstructor_r4(initialSupply);
    updateTotalBalancesOnInsertConstructor_r25(initialSupply);
    updateTotalMintOnInsertConstructor_r32(initialSupply);
    updateOwnerOnInsertConstructor_r7();
    updateTotalInOnInsertConstructor_r30(initialSupply);
  }
  function approve(address s,uint n) public    {
      bool r29 = updateIncreaseAllowanceOnInsertRecv_approve_r29(s,n);
      if(r29==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address to,uint amount) public    {
      bool r8 = updateTransferOnInsertRecv_transfer_r8(to,amount);
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function freeze(uint n) public    {
      bool r24 = updateFreezeOnInsertRecv_freeze_r24(n);
      if(r24==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (uint) {
      uint n = allowance[p][s].n;
      return n;
  }
  function getTotalSupply() public view  returns (uint) {
      uint n = totalSupply.n;
      return n;
  }
  function transferFrom(address from,address to,uint amount) public    {
      bool r14 = updateTransferFromOnInsertRecv_transferFrom_r14(from,to,amount);
      if(r14==false) {
        revert("Rule condition failed");
      }
  }
  function unfreeze(uint n) public    {
      bool r6 = updateUnfreezeOnInsertRecv_unfreeze_r6(n);
      if(r6==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (uint) {
      uint n = balanceOf[p].n;
      return n;
  }
  function withdrawEther(uint amount) public    {
      bool r23 = updateWithdrawEtherOnInsertRecv_withdrawEther_r23(amount);
      if(r23==false) {
        revert("Rule condition failed");
      }
  }
  function burn(uint amount) public    {
      bool r16 = updateBurnOnInsertRecv_burn_r16(amount);
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function updateTransferFromOnInsertRecv_transferFrom_r14(address o,address r,uint n) private   returns (bool) {
      address s = msg.sender;
      uint k = allowance[o][s].n;
      uint m = balanceOf[o].n;
      if(m>=n && r!=address(0) && n+m>=m && n>0 && k>=n) {
        updateSpentTotalOnInsertTransferFrom_r22(o,s,n);
        updateTransferOnInsertTransferFrom_r1(o,r,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalIn_r15(address p,int i) private    {
      int _delta = int(i);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateFreezeOfOnIncrementTotalUnfreeze_r12(address p,int u) private    {
      int delta0 = int(-u);
      updateBalanceOfOnIncrementFreezeOf_r15(p,delta0);
      int _delta = int(-u);
      uint newValue = updateuintByint(freezeOf[p].n,_delta);
      freezeOf[p].n = newValue;
  }
  function updateTotalBurnOnInsertBurn_r17(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r15(p,delta0);
      totalBurn[p].n += n;
  }
  function updateOwnerOnInsertConstructor_r7() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTotalInOnInsertTransfer_r31(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r15(p,delta0);
      totalIn[p].n += n;
  }
  function updateTransferOnInsertRecv_transfer_r8(address r,uint n) private   returns (bool) {
      address s = msg.sender;
      uint m = balanceOf[s].n;
      if(n<=m && n>0 && r!=address(0) && n+m>=m) {
        updateTotalOutOnInsertTransfer_r20(s,n);
        updateTotalInOnInsertTransfer_r31(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateTotalOutOnInsertTransfer_r20(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r15(p,delta0);
      totalOut[p].n += n;
  }
  function updateAllMintOnInsertConstructor_r9(uint n) private    {
      allMint = AllMintTuple(n,true);
  }
  function updateFreezeOfOnIncrementTotalFreeze_r12(address p,int f) private    {
      int delta0 = int(f);
      updateBalanceOfOnIncrementFreezeOf_r15(p,delta0);
      int _delta = int(f);
      uint newValue = updateuintByint(freezeOf[p].n,_delta);
      freezeOf[p].n = newValue;
  }
  function updateBalanceOfOnIncrementFreezeOf_r15(address p,int f) private    {
      int _delta = int(-f);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateBalanceOfOnInsertConstructor_r4(uint n) private    {
      address s = msg.sender;
      balanceOf[s] = BalanceOfTuple(n,true);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r26(address o,address s,int m) private    {
      int _delta = int(m);
      uint newValue = updateuintByint(allowance[o][s].n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateBalanceOfOnIncrementTotalBurn_r15(address p,int m) private    {
      int _delta = int(-m);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateTotalInOnInsertConstructor_r30(uint n) private    {
      address s = msg.sender;
      totalIn[s] = TotalInTuple(n,true);
  }
  function updateTotalBalancesOnInsertConstructor_r25(uint n) private    {
      // Empty()
  }
  function updateTransferOnInsertTransferFrom_r1(address o,address r,uint n) private    {
      updateTotalInOnInsertTransfer_r31(r,n);
      updateTotalOutOnInsertTransfer_r20(o,n);
      emit Transfer(o,r,n);
  }
  function updateFreezeOnInsertRecv_freeze_r24(uint n) private   returns (bool) {
      address p = msg.sender;
      uint m = balanceOf[p].n;
      if(n<=m && n>0) {
        updateTotalFreezeOnInsertFreeze_r0(p,n);
        emit Freeze(p,n);
        return true;
      }
      return false;
  }
  function updateAllBurnOnInsertBurn_r28(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r19(delta0);
      allBurn.n += n;
  }
  function updateUnfreezeOnInsertRecv_unfreeze_r6(uint n) private   returns (bool) {
      address p = msg.sender;
      uint m = freezeOf[p].n;
      if(n<=m && n>0) {
        updateTotalUnfreezeOnInsertUnfreeze_r13(p,n);
        emit Unfreeze(p,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r15(address p,int o) private    {
      int _delta = int(-o);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateAllowanceOnIncrementSpentTotal_r26(address o,address s,int l) private    {
      int _delta = int(-l);
      uint newValue = updateuintByint(allowance[o][s].n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateIncreaseAllowanceOnInsertRecv_approve_r29(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      uint m = allowance[o][s].n;
      uint d = n-m;
      updateAllowanceTotalOnInsertIncreaseAllowance_r33(o,s,d);
      emit IncreaseAllowance(o,s,d);
      return true;
      return false;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalFreezeOnInsertFreeze_r0(address p,uint n) private    {
      int delta0 = int(n);
      updateFreezeOfOnIncrementTotalFreeze_r12(p,delta0);
      totalFreeze[p].n += n;
  }
  function updateTotalUnfreezeOnInsertUnfreeze_r13(address p,uint n) private    {
      int delta0 = int(n);
      updateFreezeOfOnIncrementTotalUnfreeze_r12(p,delta0);
      totalUnfreeze[p].n += n;
  }
  function updateBurnOnInsertRecv_burn_r16(uint n) private   returns (bool) {
      address p = msg.sender;
      uint m = balanceOf[p].n;
      if(p!=address(0) && n<=m) {
        updateAllBurnOnInsertBurn_r28(n);
        updateTotalBurnOnInsertBurn_r17(p,n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateSendOnInsertWithdrawEther_r3(address p,uint n) private    {
      payable(p).send(n);
  }
  function updateTotalSupplyOnInsertConstructor_r5(uint n) private    {
      totalSupply = TotalSupplyTuple(n,true);
  }
  function updateTotalSupplyOnIncrementAllBurn_r19(int b) private    {
      int _delta = int(-b);
      uint newValue = updateuintByint(totalSupply.n,_delta);
      totalSupply.n = newValue;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r33(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r26(o,s,delta0);
      allowanceTotal[o][s].m += n;
  }
  function updateSpentTotalOnInsertTransferFrom_r22(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r26(o,s,delta0);
      spentTotal[o][s].m += n;
  }
  function updateWithdrawEtherOnInsertRecv_withdrawEther_r23(uint n) private   returns (bool) {
      address p = owner.p;
      if(p==msg.sender) {
        updateSendOnInsertWithdrawEther_r3(p,n);
        emit WithdrawEther(p,n);
        return true;
      }
      return false;
  }
  function updateTotalMintOnInsertConstructor_r32(uint n) private    {
      address s = msg.sender;
      totalMint[s] = TotalMintTuple(n,true);
  }
}