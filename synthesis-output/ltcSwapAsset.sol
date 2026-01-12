contract LtcSwapAsset {
  struct AllowanceTuple {
    int n;
    bool _valid;
  }
  struct BalanceOfTuple {
    int n;
    bool _valid;
  }
  struct OldOwnerTuple {
    address p;
    bool _valid;
  }
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct EffectiveTimeTuple {
    uint t;
    bool _valid;
  }
  struct NewOwnerTuple {
    address p;
    bool _valid;
  }
  struct TotalSupplyTuple {
    int n;
    bool _valid;
  }
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  EffectiveTimeTuple effectiveTime;
  mapping(address=>BalanceOfTuple) balanceOf;
  OldOwnerTuple oldOwner;
  OwnerTuple owner;
  NewOwnerTuple newOwner;
  TotalSupplyTuple totalSupply;
  event InvalidTx();
  event Transfer(address s,address r,int n);
  event TransferFrom(address o,address r,address s,int n);
  event Mint(address p,int n);
  event Burn(address p,int n);
  event SwapOwnerTx(address p,address q,uint d);
  event IncreaseAllowance(address p,address s,int n);
  constructor() public {
    updateTotalBalancesOnInsertConstructor_r27();
    updateTotalSupplyOnInsertConstructor_r3();
  }
  function mint(address p,int n) public    {
      bool r13 = updateMintOnInsertRecv_mint_r13(p,n);
      if(r13==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r8 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r8(p,s,n);
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function swapOwnerTx(address p,address q,uint d) public    {
      bool r4 = updateSwapOwnerTxOnInsertRecv_swapOwnerTx_r4(p,q,d);
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int n) public    {
      bool r14 = updateBurnOnInsertRecv_burn_r14(p,n);
      if(r14==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function transfer(address s,address r,int n) public    {
      bool r31 = updateTransferOnInsertRecv_transfer_r31(s,r,n);
      if(r31==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address o,address r,address s,int n) public    {
      bool r7 = updateTransferFromOnInsertRecv_transferFrom_r7(o,r,s,n);
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function updateEffectiveTimeOnInsertSwapOwner_r6(uint t) private    {
      updateOwnerOnInsertEffectiveTime_r26(t);
      updateOwnerOnInsertEffectiveTime_r19(t);
      effectiveTime = EffectiveTimeTuple(t,true);
  }
  function updateNewOwnerOnInsertSwapOwner_r2(address q) private    {
      updateOwnerOnInsertNewOwner_r19(q);
      newOwner = NewOwnerTuple(q,true);
  }
  function updateOwnerOnInsertEffectiveTime_r26(uint t2) private    {
      address p = oldOwner.p;
      uint t = block.timestamp;
      if(t<t2) {
        owner = OwnerTuple(p,true);
      }
  }
  function updateBalanceOfOnIncrementTotalIn_r24(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateOldOwnerOnInsertSwapOwner_r16(address p) private    {
      updateOwnerOnInsertOldOwner_r26(p);
      oldOwner = OldOwnerTuple(p,true);
  }
  function updateAllBurnOnInsertBurn_r1(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r18(delta0);
  }
  function updateSwapOwnerOnInsertSwapOwnerTx_r22(address p,address q,uint d) private    {
      uint t0 = block.timestamp;
      uint t = t0+d;
      updateOldOwnerOnInsertSwapOwner_r16(p);
      updateNewOwnerOnInsertSwapOwner_r2(q);
      updateEffectiveTimeOnInsertSwapOwner_r6(t);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r29(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTotalSupplyOnInsertConstructor_r3() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateBalanceOfOnIncrementTotalMint_r24(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateAllMintOnInsertMint_r15(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r18(delta0);
  }
  function updateMintOnInsertRecv_mint_r13(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>0) {
        updateTotalMintOnInsertMint_r21(p,n);
        updateAllMintOnInsertMint_r15(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertRecv_transfer_r31(address s,address r,int n) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[s].n;
      if(n>=0 && n<balanceOf_x1_1) {
        updateTotalOutOnInsertTransfer_r30(s,n);
        updateTotalInOnInsertTransfer_r5(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateTotalOutOnInsertTransfer_r30(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r24(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalBurn_r24(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateSwapOwnerTxOnInsertRecv_swapOwnerTx_r4(address p,address q,uint d) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        updateSwapOwnerOnInsertSwapOwnerTx_r22(p,q,d);
        emit SwapOwnerTx(p,q,d);
        return true;
      }
      return false;
  }
  function updateSpentTotalOnInsertTransferFrom_r20(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r29(o,s,delta0);
  }
  function updateTotalMintOnInsertMint_r21(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r24(p,delta0);
  }
  function updateAllowanceOnIncrementSpentTotal_r29(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateOwnerOnInsertEffectiveTime_r19(uint t2) private    {
      address p = newOwner.p;
      uint t = block.timestamp;
      if(t>=t2) {
        owner = OwnerTuple(p,true);
      }
  }
  function updateBalanceOfOnIncrementTotalOut_r24(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r28(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r29(o,s,delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r7(address o,address r,address s,int n) private   returns (bool) {
      if(0==n) {
        updateSpentTotalOnInsertTransferFrom_r20(o,s,n);
        updateTransferOnInsertTransferFrom_r23(o,r,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r18(int m) private    {
      totalSupply.n += m;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalBurnOnInsertBurn_r12(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r24(p,delta0);
  }
  function updateTotalInOnInsertTransfer_r5(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r24(p,delta0);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r8(address p,address s,int n) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r28(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r18(int b) private    {
      totalSupply.n -= b;
  }
  function updateTransferOnInsertTransferFrom_r23(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r30(o,n);
      updateTotalInOnInsertTransfer_r5(r,n);
      emit Transfer(o,r,n);
  }
  function updateOwnerOnInsertOldOwner_r26(address p) private    {
      uint t2 = effectiveTime.t;
      uint t = block.timestamp;
      if(t<t2) {
        owner = OwnerTuple(p,true);
      }
  }
  function updateTotalBalancesOnInsertConstructor_r27() private    {
      // Empty()
  }
  function updateOwnerOnInsertNewOwner_r19(address p) private    {
      uint t2 = effectiveTime.t;
      uint t = block.timestamp;
      if(t>=t2) {
        owner = OwnerTuple(p,true);
      }
  }
  function updateBurnOnInsertRecv_burn_r14(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && n<balanceOf_x1) {
        updateAllBurnOnInsertBurn_r1(n);
        updateTotalBurnOnInsertBurn_r12(p,n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
}