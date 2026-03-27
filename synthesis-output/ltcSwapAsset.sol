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
  event Transfer(address s,address r,int n);
  event UnauthorizedBurn();
  event TransferFrom(address o,address r,address s,int n);
  event Mint(address p,int n);
  event Burn(address p,int n);
  event UnauthorizedSwapOwnerTx();
  event UnauthorizedMint();
  event SwapOwnerTx(address p,address q,uint d);
  event IncreaseAllowance(address p,address s,int n);
  constructor() public {
    updateTotalBalancesOnInsertConstructor_r27();
    updateTotalSupplyOnInsertConstructor_r3();
  }
  function mint(address p,int n) public    {
      bool r14 = updateMintOnInsertRecv_mint_r14(p,n);
      if(r14==false) {
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
  function transferFrom(address o,address r,address s,int n) public    {
      bool r5 = updateTransferFromOnInsertRecv_transferFrom_r5(o,r,s,n);
      if(r5==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int n) public    {
      bool r31 = updateBurnOnInsertRecv_burn_r31(p,n);
      if(r31==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r9 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r9(p,s,n);
      if(r9==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function transfer(address s,address r,int n) public    {
      bool r12 = updateTransferOnInsertRecv_transfer_r12(s,r,n);
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function updateSpentTotalOnInsertTransferFrom_r21(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r29(o,s,delta0);
  }
  function updateOwnerOnInsertEffectiveTime_r8(uint t2) private    {
      address p = oldOwner.p;
      uint t = block.timestamp;
      if(t<t2) {
        owner = OwnerTuple(p,true);
      }
  }
  function updateAllowanceOnIncrementSpentTotal_r29(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateSwapOwnerTxOnInsertRecv_swapOwnerTx_r4(address p,address q,uint d) private   returns (bool) {
      address current = owner.p;
      if(p==current) {
        updateSwapOwnerOnInsertSwapOwnerTx_r23(p,q,d);
        emit SwapOwnerTx(p,q,d);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r26(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateAllBurnOnInsertBurn_r1(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r19(delta0);
  }
  function updateOwnerOnInsertEffectiveTime_r20(uint t2) private    {
      address p = newOwner.p;
      uint t = block.timestamp;
      if(t>=t2) {
        owner = OwnerTuple(p,true);
      }
  }
  function updateBalanceOfOnIncrementTotalBurn_r26(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateSwapOwnerOnInsertSwapOwnerTx_r23(address p,address q,uint d) private    {
      uint t0 = block.timestamp;
      uint t = t0+d;
      updateEffectiveTimeOnInsertSwapOwner_r7(t);
      updateNewOwnerOnInsertSwapOwner_r2(q);
      updateOldOwnerOnInsertSwapOwner_r17(p);
  }
  function updateOwnerOnInsertOldOwner_r8(address p) private    {
      uint t2 = effectiveTime.t;
      uint t = block.timestamp;
      if(t<t2) {
        owner = OwnerTuple(p,true);
      }
  }
  function updateBalanceOfOnIncrementTotalMint_r26(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateOwnerOnInsertNewOwner_r20(address p) private    {
      uint t2 = effectiveTime.t;
      uint t = block.timestamp;
      if(t>=t2) {
        owner = OwnerTuple(p,true);
      }
  }
  function updateNewOwnerOnInsertSwapOwner_r2(address q) private    {
      updateOwnerOnInsertNewOwner_r20(q);
      newOwner = NewOwnerTuple(q,true);
  }
  function updateTotalOutOnInsertTransfer_r30(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r26(p,delta0);
  }
  function updateMintOnInsertRecv_mint_r14(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(s==o && n>0) {
        updateTotalMintOnInsertMint_r22(p,n);
        updateAllMintOnInsertMint_r15(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r28(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r29(o,s,delta0);
  }
  function updateTransferOnInsertTransferFrom_r25(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r30(o,n);
      updateTotalInOnInsertTransfer_r6(r,n);
      emit Transfer(o,r,n);
  }
  function updateTotalSupplyOnIncrementAllMint_r19(int m) private    {
      totalSupply.n += m;
  }
  function updateTotalMintOnInsertMint_r22(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r26(p,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalBurnOnInsertBurn_r32(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r26(p,delta0);
  }
  function updateTotalSupplyOnIncrementAllBurn_r19(int b) private    {
      totalSupply.n -= b;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r5(address o,address r,address s,int n) private   returns (bool) {
      int balanceOf_x1_2 = balanceOf[o].n;
      int allowance_x2_1 = allowance[o][s].n;
      if(n>0 && n<=allowance_x2_1 && n<balanceOf_x1_2) {
        updateTransferOnInsertTransferFrom_r25(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r21(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateOldOwnerOnInsertSwapOwner_r17(address p) private    {
      updateOwnerOnInsertOldOwner_r8(p);
      oldOwner = OldOwnerTuple(p,true);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r29(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateBurnOnInsertRecv_burn_r31(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(s==o && n<=balanceOf_x1) {
        updateTotalBurnOnInsertBurn_r32(p,n);
        updateAllBurnOnInsertBurn_r1(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalIn_r26(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r9(address p,address s,int n) private   returns (bool) {
      if(n>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r28(p,s,n);
        emit IncreaseAllowance(p,s,n);
        return true;
      }
      return false;
  }
  function updateAllMintOnInsertMint_r15(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r19(delta0);
  }
  function updateEffectiveTimeOnInsertSwapOwner_r7(uint t) private    {
      updateOwnerOnInsertEffectiveTime_r20(t);
      updateOwnerOnInsertEffectiveTime_r8(t);
      effectiveTime = EffectiveTimeTuple(t,true);
  }
  function updateTotalInOnInsertTransfer_r6(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r26(p,delta0);
  }
  function updateTotalBalancesOnInsertConstructor_r27() private    {
      // Empty()
  }
  function updateTotalSupplyOnInsertConstructor_r3() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateTransferOnInsertRecv_transfer_r12(address s,address r,int n) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[s].n;
      if(n>0 && n<balanceOf_x1_1) {
        updateTotalOutOnInsertTransfer_r30(s,n);
        updateTotalInOnInsertTransfer_r6(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
}