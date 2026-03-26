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
    updateTotalBalancesOnInsertConstructor_r34();
    updateTotalSupplyOnInsertConstructor_r18();
    updateOwnerOnInsertConstructor_r32();
  }
  function burn(address p,int n) public    {
      bool r15 = updateBurnOnInsertRecv_burn_r15(p,n);
      if(r15==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int n) public    {
      bool r11 = updateMintOnInsertRecv_mint_r11(p,n);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address o,address r,address s,int n) public    {
      bool r27 = updateTransferFromOnInsertRecv_transferFrom_r27(o,r,s,n);
      if(r27==false) {
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
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r7 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r7(p,s,n);
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function swapOwnerTx(address p,address q,uint d) public    {
      bool r17 = updateSwapOwnerTxOnInsertRecv_swapOwnerTx_r17(p,q,d);
      if(r17==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address s,address r,int n) public    {
      bool r41 = updateTransferOnInsertRecv_transfer_r41(s,r,n);
      if(r41==false) {
        revert("Rule condition failed");
      }
  }
  function updateEffectiveTimeOnInsertSwapOwner_r6(uint t) private    {
      updateOwnerOnInsertEffectiveTime_r33(t);
      updateOwnerOnInsertEffectiveTime_r4(t);
      effectiveTime = EffectiveTimeTuple(t,true);
  }
  function updateOwnerOnInsertConstructor_r32() private    {
      address p = msg.sender;
      owner = OwnerTuple(p,true);
  }
  function updateTotalOutOnInsertTransfer_r38(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r30(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalMint_r30(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r35(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r37(o,s,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r7(address o,address s,int d) private   returns (bool) {
      int allowance_x2 = allowance[s][o].n;
      if(d>0 && d<=allowance_x2) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r35(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateSpentTotalOnInsertTransferFrom_r24(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r37(s,o,delta0);
  }
  function updateOwnerOnInsertOldOwner_r33(address p) private    {
      uint t2 = effectiveTime.t;
      uint t = block.timestamp;
      if(t<t2) {
        owner = OwnerTuple(p,true);
      }
  }
  function updateAllBurnOnInsertBurn_r1(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r22(delta0);
  }
  function updateTotalSupplyOnInsertConstructor_r18() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateTotalSupplyOnIncrementAllBurn_r22(int b) private    {
      totalSupply.n -= b;
  }
  function updateAllMintOnInsertMint_r16(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r22(delta0);
  }
  function updateTotalInOnInsertTransfer_r5(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r30(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalOut_r30(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateSwapOwnerTxOnInsertRecv_swapOwnerTx_r17(address p,address q,uint d) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(d>0 && o_1==s_1) {
        updateSwapOwnerOnInsertSwapOwnerTx_r26(p,q,d);
        emit SwapOwnerTx(p,q,d);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r37(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateTotalSupplyOnIncrementAllMint_r22(int m) private    {
      totalSupply.n += m;
  }
  function updateOwnerOnInsertEffectiveTime_r4(uint t2) private    {
      address p = newOwner.p;
      uint t = block.timestamp;
      if(t>=t2) {
        owner = OwnerTuple(p,true);
      }
  }
  function updateTransferFromOnInsertRecv_transferFrom_r27(address o,address r,address s,int n) private   returns (bool) {
      int a_2 = allowance[s][o].n;
      int b_1 = balanceOf[s].n;
      if(n>0 && n<=b_1 && n<=a_2) {
        updateTransferOnInsertTransferFrom_r29(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r24(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateOwnerOnInsertNewOwner_r4(address p) private    {
      uint t2 = effectiveTime.t;
      uint t = block.timestamp;
      if(t>=t2) {
        owner = OwnerTuple(p,true);
      }
  }
  function updateTransferOnInsertTransferFrom_r29(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r38(o,n);
      updateTotalInOnInsertTransfer_r5(r,n);
      emit Transfer(o,r,n);
  }
  function updateTotalBalancesOnInsertConstructor_r34() private    {
      // Empty()
  }
  function updateOwnerOnInsertEffectiveTime_r33(uint t2) private    {
      address p = oldOwner.p;
      uint t = block.timestamp;
      if(t<t2) {
        owner = OwnerTuple(p,true);
      }
  }
  function updateTotalBurnOnInsertBurn_r42(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r30(p,delta0);
  }
  function updateMintOnInsertRecv_mint_r11(address p,int n) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(n>0 && o_1==s_1) {
        updateAllMintOnInsertMint_r16(n);
        updateTotalMintOnInsertMint_r25(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalIn_r30(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateOldOwnerOnInsertSwapOwner_r20(address p) private    {
      updateOwnerOnInsertOldOwner_r33(p);
      oldOwner = OldOwnerTuple(p,true);
  }
  function updateBurnOnInsertRecv_burn_r15(address p,int n) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      int b_2 = balanceOf[p].n;
      if(n>0 && o_1==s_1 && n<=b_2) {
        updateAllBurnOnInsertBurn_r1(n);
        updateTotalBurnOnInsertBurn_r42(p,n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTransferOnInsertRecv_transfer_r41(address s,address r,int n) private   returns (bool) {
      int b_1 = balanceOf[s].n;
      if(n>0 && n<=b_1) {
        updateTotalInOnInsertTransfer_r5(r,n);
        updateTotalOutOnInsertTransfer_r38(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateNewOwnerOnInsertSwapOwner_r2(address q) private    {
      updateOwnerOnInsertNewOwner_r4(q);
      newOwner = NewOwnerTuple(q,true);
  }
  function updateTotalMintOnInsertMint_r25(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r30(p,delta0);
  }
  function updateAllowanceOnIncrementSpentTotal_r37(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateSwapOwnerOnInsertSwapOwnerTx_r26(address p,address q,uint d) private    {
      uint t0 = block.timestamp;
      uint t = t0+d;
      updateOldOwnerOnInsertSwapOwner_r20(p);
      updateNewOwnerOnInsertSwapOwner_r2(q);
      updateEffectiveTimeOnInsertSwapOwner_r6(t);
  }
  function updateBalanceOfOnIncrementTotalBurn_r30(address p,int m) private    {
      balanceOf[p].n -= m;
  }
}