contract LtcSwapAsset_lean {
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
    updateOwnerOnInsertConstructor_r26();
    updateTotalSupplyOnInsertConstructor_r4();
    updateTotalBalancesOnInsertConstructor_r28();
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r8 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r8(p,s,n);
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address s,address r,int n) public    {
      bool r32 = updateTransferOnInsertRecv_transfer_r32(s,r,n);
      if(r32==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function mint(address p,int n) public    {
      bool r1 = updateMintOnInsertRecv_mint_r1(p,n);
      if(r1==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function transferFrom(address o,address r,address s,int n) public    {
      bool r14 = updateTransferFromOnInsertRecv_transferFrom_r14(o,r,s,n);
      if(r14==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function swapOwnerTx(address p,address q,uint d) public    {
      bool r5 = updateSwapOwnerTxOnInsertRecv_swapOwnerTx_r5(p,q,d);
      if(r5==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int n) public    {
      bool r13 = updateBurnOnInsertRecv_burn_r13(p,n);
      if(r13==false) {
        revert("Rule condition failed");
      }
  }
  function updateTotalInOnInsertTransfer_r6(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r24(p,delta0);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r8(address o,address s,int d) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r29(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateOwnerOnInsertOldOwner_r27(address p) private    {
      uint t2 = effectiveTime.t;
      uint t = block.timestamp;
      if(t<t2) {
        owner = OwnerTuple(p,true);
      }
  }
  function updateBalanceOfOnIncrementTotalIn_r24(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateBalanceOfOnIncrementTotalBurn_r24(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateTotalMintOnInsertMint_r21(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r24(p,delta0);
  }
  function updateTotalSupplyOnInsertConstructor_r4() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateOldOwnerOnInsertSwapOwner_r16(address p) private    {
      updateOwnerOnInsertOldOwner_r27(p);
      oldOwner = OldOwnerTuple(p,true);
  }
  function updateSpentTotalOnInsertTransferFrom_r20(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r30(o,s,delta0);
  }
  function updateTotalBurnOnInsertBurn_r12(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r24(p,delta0);
  }
  function updateNewOwnerOnInsertSwapOwner_r3(address q) private    {
      updateOwnerOnInsertNewOwner_r19(q);
      newOwner = NewOwnerTuple(q,true);
  }
  function updateTransferOnInsertTransferFrom_r23(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r31(o,n);
      updateTotalInOnInsertTransfer_r6(r,n);
      emit Transfer(o,r,n);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r29(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r30(o,s,delta0);
  }
  function updateSwapOwnerOnInsertSwapOwnerTx_r22(address p,address q,uint d) private    {
      uint t0 = block.timestamp;
      uint t = t0+d;
      updateOldOwnerOnInsertSwapOwner_r16(p);
      updateEffectiveTimeOnInsertSwapOwner_r7(t);
      updateNewOwnerOnInsertSwapOwner_r3(q);
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
  function updateTotalSupplyOnIncrementAllMint_r18(int m) private    {
      totalSupply.n += m;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateMintOnInsertRecv_mint_r1(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>=0) {
        updateTotalMintOnInsertMint_r21(p,n);
        updateAllMintOnInsertMint_r15(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateAllBurnOnInsertBurn_r2(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r18(delta0);
  }
  function updateTotalBalancesOnInsertConstructor_r28() private    {
      // Empty()
  }
  function updateEffectiveTimeOnInsertSwapOwner_r7(uint t) private    {
      updateOwnerOnInsertEffectiveTime_r27(t);
      updateOwnerOnInsertEffectiveTime_r19(t);
      effectiveTime = EffectiveTimeTuple(t,true);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r30(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTotalOutOnInsertTransfer_r31(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r24(p,delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r14(address o,address r,address s,int n) private   returns (bool) {
      int balanceOf_x1_2 = balanceOf[o].n;
      int allowance_x2_1 = allowance[o][s].n;
      if(n>0 && n<allowance_x2_1 && n<=balanceOf_x1_2) {
        updateSpentTotalOnInsertTransferFrom_r20(o,s,n);
        updateTransferOnInsertTransferFrom_r23(o,r,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateSwapOwnerTxOnInsertRecv_swapOwnerTx_r5(address p,address q,uint d) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        updateSwapOwnerOnInsertSwapOwnerTx_r22(p,q,d);
        emit SwapOwnerTx(p,q,d);
        return true;
      }
      return false;
  }
  function updateBurnOnInsertRecv_burn_r13(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && n<balanceOf_x1) {
        updateAllBurnOnInsertBurn_r2(n);
        updateTotalBurnOnInsertBurn_r12(p,n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateOwnerOnInsertConstructor_r26() private    {
      address p = msg.sender;
      owner = OwnerTuple(p,true);
  }
  function updateTotalSupplyOnIncrementAllBurn_r18(int b) private    {
      totalSupply.n -= b;
  }
  function updateAllowanceOnIncrementSpentTotal_r30(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateBalanceOfOnIncrementTotalMint_r24(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateAllMintOnInsertMint_r15(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r18(delta0);
  }
  function updateOwnerOnInsertEffectiveTime_r27(uint t2) private    {
      address p = oldOwner.p;
      uint t = block.timestamp;
      if(t<t2) {
        owner = OwnerTuple(p,true);
      }
  }
  function updateTransferOnInsertRecv_transfer_r32(address s,address r,int n) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[s].n;
      if(n>=0 && n<balanceOf_x1_1) {
        updateTotalInOnInsertTransfer_r6(r,n);
        updateTotalOutOnInsertTransfer_r31(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateOwnerOnInsertNewOwner_r19(address p) private    {
      uint t2 = effectiveTime.t;
      uint t = block.timestamp;
      if(t>=t2) {
        owner = OwnerTuple(p,true);
      }
  }
}