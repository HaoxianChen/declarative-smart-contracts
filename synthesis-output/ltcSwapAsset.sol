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
    updateTotalSupplyOnInsertConstructor_r6();
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function swapOwnerTx(address p,address q,uint d) public    {
      bool r8 = updateSwapOwnerTxOnInsertRecv_swapOwnerTx_r8(p,q,d);
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address o,address r,address s,int n) public    {
      bool r43 = updateTransferFromOnInsertRecv_transferFrom_r43(o,r,s,n);
      if(r43==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int n) public    {
      bool r20 = updateBurnOnInsertRecv_burn_r20(p,n);
      if(r20==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r10 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r10(p,s,n);
      if(r10==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function mint(address p,int n) public    {
      bool r27 = updateMintOnInsertRecv_mint_r27(p,n);
      if(r27==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function transfer(address s,address r,int n) public    {
      bool r14 = updateTransferOnInsertRecv_transfer_r14(s,r,n);
      if(r14==false) {
        revert("Rule condition failed");
      }
  }
  function updateTotalMintOnInsertMint_r23(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r30(p,delta0);
  }
  function updateTransferOnInsertRecv_transfer_r14(address s,address r,int n) private   returns (bool) {
      int m_1 = balanceOf[s].n;
      if(n>0 && r!=address(0) && s!=address(0) && n<=m_1) {
        updateTotalOutOnInsertTransfer_r40(s,n);
        updateTotalInOnInsertTransfer_r9(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateMintOnInsertRecv_mint_r27(address p,int n) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(p!=address(0) && n>0 && o_1==s_1) {
        updateAllMintOnInsertMint_r3(n);
        updateTotalMintOnInsertMint_r23(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateOwnerOnInsertEffectiveTime_r33(uint t2) private    {
      address p = oldOwner.p;
      uint t = block.timestamp;
      if(t<t2) {
        owner = OwnerTuple(p,true);
      }
  }
  function updateTransferFromOnInsertRecv_transferFrom_r43(address o,address r,address s,int n) private   returns (bool) {
      int m_1 = balanceOf[o].n;
      if(r!=address(0) && n<=m_1 && s!=address(0) && o!=address(0) && n>0) {
        updateSpentTotalOnInsertTransferFrom_r22(o,s,n);
        updateTransferOnInsertTransferFrom_r28(o,r,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalMint_r30(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateSwapOwnerOnInsertSwapOwnerTx_r24(address p,address q,uint d) private    {
      uint t0 = block.timestamp;
      uint t = t0+d;
      updateEffectiveTimeOnInsertSwapOwner_r26(t);
      updateOldOwnerOnInsertSwapOwner_r15(p);
      updateNewOwnerOnInsertSwapOwner_r5(q);
  }
  function updateEffectiveTimeOnInsertSwapOwner_r26(uint t) private    {
      updateOwnerOnInsertEffectiveTime_r33(t);
      updateOwnerOnInsertEffectiveTime_r21(t);
      effectiveTime = EffectiveTimeTuple(t,true);
  }
  function updateTotalSupplyOnIncrementAllBurn_r19(int b) private    {
      totalSupply.n -= b;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r41(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateOwnerOnInsertEffectiveTime_r21(uint t2) private    {
      address p = newOwner.p;
      uint t = block.timestamp;
      if(t>=t2) {
        owner = OwnerTuple(p,true);
      }
  }
  function updateAllMintOnInsertMint_r3(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r19(delta0);
  }
  function updateBalanceOfOnIncrementTotalBurn_r30(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateSwapOwnerTxOnInsertRecv_swapOwnerTx_r8(address p,address q,uint d) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        updateSwapOwnerOnInsertSwapOwnerTx_r24(p,q,d);
        emit SwapOwnerTx(p,q,d);
        return true;
      }
      return false;
  }
  function updateTotalBalancesOnInsertConstructor_r34() private    {
      // Empty()
  }
  function updateOldOwnerOnInsertSwapOwner_r15(address p) private    {
      updateOwnerOnInsertOldOwner_r33(p);
      oldOwner = OldOwnerTuple(p,true);
  }
  function updateBalanceOfOnIncrementTotalIn_r30(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateOwnerOnInsertNewOwner_r21(address p) private    {
      uint t2 = effectiveTime.t;
      uint t = block.timestamp;
      if(t>=t2) {
        owner = OwnerTuple(p,true);
      }
  }
  function updateTotalBurnOnInsertBurn_r13(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r30(p,delta0);
  }
  function updateTotalSupplyOnIncrementAllMint_r19(int m) private    {
      totalSupply.n += m;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateAllBurnOnInsertBurn_r4(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r19(delta0);
  }
  function updateOwnerOnInsertOldOwner_r33(address p) private    {
      uint t2 = effectiveTime.t;
      uint t = block.timestamp;
      if(t<t2) {
        owner = OwnerTuple(p,true);
      }
  }
  function updateTotalInOnInsertTransfer_r9(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r30(p,delta0);
  }
  function updateBurnOnInsertRecv_burn_r20(address p,int n) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      int m_2 = balanceOf[p].n;
      if(n>0 && p!=address(0) && o_1==s_1 && n<=m_2) {
        updateTotalBurnOnInsertBurn_r13(p,n);
        updateAllBurnOnInsertBurn_r4(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateTotalOutOnInsertTransfer_r40(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r30(p,delta0);
  }
  function updateSpentTotalOnInsertTransferFrom_r22(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r41(o,s,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateNewOwnerOnInsertSwapOwner_r5(address q) private    {
      updateOwnerOnInsertNewOwner_r21(q);
      newOwner = NewOwnerTuple(q,true);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r10(address p,address s,int n) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r36(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r6() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateAllowanceOnIncrementSpentTotal_r41(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTransferOnInsertTransferFrom_r28(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r40(o,n);
      updateTotalInOnInsertTransfer_r9(r,n);
      emit Transfer(o,r,n);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r36(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r41(o,s,delta0);
  }
  function updateBalanceOfOnIncrementTotalOut_r30(address p,int o) private    {
      balanceOf[p].n -= o;
  }
}