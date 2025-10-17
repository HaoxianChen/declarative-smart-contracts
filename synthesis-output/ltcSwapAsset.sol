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
    updateTotalSupplyOnInsertConstructor_r5();
    updateNewOwnerOnInsertConstructor_r27();
    updateTotalBalancesOnInsertConstructor_r29();
    updateEffectiveTimeOnInsertConstructor_r13();
  }
  function transferFrom(address o,address r,address s,int n) public    {
      bool r33 = updateTransferFromOnInsertRecv_transferFrom_r33(o,r,s,n);
      if(r33==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r28 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r28(p,s,n);
      if(r28==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function transfer(address s,address r,int n) public    {
      bool r21 = updateTransferOnInsertRecv_transfer_r21(s,r,n);
      if(r21==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int n) public    {
      bool r1 = updateMintOnInsertRecv_mint_r1(p,n);
      if(r1==false) {
        revert("Rule condition failed");
      }
  }
  function swapOwnerTx(address p,address q,uint d) public    {
      bool r14 = updateSwapOwnerTxOnInsertRecv_swapOwnerTx_r14(p,q,d);
      if(r14==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int n) public    {
      bool r34 = updateBurnOnInsertRecv_burn_r34(p,n);
      if(r34==false) {
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
  function updateTotalInOnInsertTransfer_r6(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r24(p,delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r30(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r31(o,s,delta0);
  }
  function updateBalanceOfOnIncrementTotalIn_r24(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateBalanceOfOnIncrementTotalBurn_r24(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function owner(address p) private view  returns (bool) {
      if(p==oldOwner.p) {
        uint t2 = effectiveTime.t;
        uint t = block.timestamp;
        if(t<t2) {
          return true;
        }
      }
      if(p==newOwner.p) {
        uint t2 = effectiveTime.t;
        uint t = block.timestamp;
        if(t>=t2) {
          return true;
        }
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r17(int m) private    {
      totalSupply.n += m;
  }
  function updateOldOwnerOnInsertSwapOwner_r15(address p) private    {
      oldOwner = OldOwnerTuple(p,true);
  }
  function updateEffectiveTimeOnInsertSwapOwner_r7(uint t) private    {
      effectiveTime = EffectiveTimeTuple(t,true);
  }
  function updateTotalOutOnInsertTransfer_r32(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r24(p,delta0);
  }
  function updateBurnOnInsertRecv_burn_r34(address p,int n) private   returns (bool) {
      address s = msg.sender;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && 0!=balanceOf_x1 && owner(o)) {
        updateAllBurnOnInsertBurn_r3(n);
        updateTotalBurnOnInsertBurn_r12(p,n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateTotalBurnOnInsertBurn_r12(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r24(p,delta0);
  }
  function updateTransferOnInsertRecv_transfer_r21(address s,address r,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[s].n;
      if(0!=balanceOf_x1) {
        updateTotalInOnInsertTransfer_r6(r,n);
        updateTotalOutOnInsertTransfer_r32(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r31(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateBalanceOfOnIncrementTotalMint_r24(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateEffectiveTimeOnInsertConstructor_r13() private    {
      uint t = block.timestamp;
      effectiveTime = EffectiveTimeTuple(t,true);
  }
  function updateTotalSupplyOnIncrementAllBurn_r17(int b) private    {
      totalSupply.n -= b;
  }
  function updateAllMintOnInsertMint_r2(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r17(delta0);
  }
  function updateTransferOnInsertTransferFrom_r23(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r6(r,n);
      updateTotalOutOnInsertTransfer_r32(o,n);
      emit Transfer(o,r,n);
  }
  function updateBalanceOfOnIncrementTotalOut_r24(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateSpentTotalOnInsertTransferFrom_r19(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r31(o,s,delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r33(address o,address r,address s,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[r].n;
      if(0!=balanceOf_x1) {
        updateTransferOnInsertTransferFrom_r23(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r19(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r5() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateNewOwnerOnInsertConstructor_r27() private    {
      address s = msg.sender;
      newOwner = NewOwnerTuple(s,true);
  }
  function updateMintOnInsertRecv_mint_r1(address p,int n) private   returns (bool) {
      address s = msg.sender;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && balanceOf_x1>0 && owner(o)) {
        updateAllMintOnInsertMint_r2(n);
        updateTotalMintOnInsertMint_r20(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateNewOwnerOnInsertSwapOwner_r4(address q) private    {
      newOwner = NewOwnerTuple(q,true);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateAllowanceOnIncrementSpentTotal_r31(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateSwapOwnerOnInsertSwapOwnerTx_r22(address p,address q,uint d) private    {
      uint t0 = block.timestamp;
      uint t = t0+d;
      updateEffectiveTimeOnInsertSwapOwner_r7(t);
      updateOldOwnerOnInsertSwapOwner_r15(p);
      updateNewOwnerOnInsertSwapOwner_r4(q);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r28(address o,address s,int d) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r30(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateTotalMintOnInsertMint_r20(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r24(p,delta0);
  }
  function updateAllBurnOnInsertBurn_r3(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r17(delta0);
  }
  function updateSwapOwnerTxOnInsertRecv_swapOwnerTx_r14(address p,address q,uint d) private   returns (bool) {
      address s = msg.sender;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && balanceOf_x1>0 && owner(o)) {
        updateSwapOwnerOnInsertSwapOwnerTx_r22(p,q,d);
        emit SwapOwnerTx(p,q,d);
        return true;
      }
      return false;
  }
  function updateTotalBalancesOnInsertConstructor_r29() private    {
      // Empty()
  }
}