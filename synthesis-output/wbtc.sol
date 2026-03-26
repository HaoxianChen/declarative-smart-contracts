contract Wbtc {
  struct AllowanceTuple {
    int n;
    bool _valid;
  }
  struct PendingOwnerTuple {
    address p;
    bool _valid;
  }
  struct BalanceOfTuple {
    int n;
    bool _valid;
  }
  struct TotalSupplyTuple {
    int n;
    bool _valid;
  }
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct PausedTuple {
    bool b;
    bool _valid;
  }
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  PendingOwnerTuple pendingOwner;
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalSupplyTuple totalSupply;
  PausedTuple paused;
  OwnerTuple owner;
  event TransferOwnership(address p);
  event Transfer(address s,address r,int n);
  event Mint(address p,int amount);
  event IncreaseApproval(address p,address s,int n);
  event ClaimOwnership(address p);
  event Burn(address p,int amount);
  event TransferFrom(address o,address r,address s,int n);
  event DecreaseApproval(address p,address s,int n);
  event Paused(bool b);
  event InvalidTx();
  constructor() public {
    updateTotalSupplyOnInsertConstructor_r3();
    updateOwnerOnInsertConstructor_r32();
  }
  function burn(address p,int amount) public    {
      bool r34 = updateBurnOnInsertRecv_burn_r34(p,amount);
      if(r34==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function transferFrom(address o,address r,address s,int n) public    {
      bool r2 = updateTransferFromOnInsertRecv_transferFrom_r2(o,r,s,n);
      if(r2==false) {
        revert("Rule condition failed");
      }
  }
  function increaseApproval(address p,address s,int n) public    {
      bool r9 = updateIncreaseApprovalOnInsertRecv_increaseApproval_r9(p,s,n);
      if(r9==false) {
        revert("Rule condition failed");
      }
  }
  function transferOwnership(address p) public    {
      bool r4 = updateTransferOwnershipOnInsertRecv_transferOwnership_r4(p);
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function decreaseApproval(address p,address s,int n) public    {
      bool r33 = updateDecreaseApprovalOnInsertRecv_decreaseApproval_r33(p,s,n);
      if(r33==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r15 = updateMintOnInsertRecv_mint_r15(p,amount);
      if(r15==false) {
        revert("Rule condition failed");
      }
  }
  function unpause() public    {
      bool r28 = updatePausedOnInsertRecv_unpause_r28();
      if(r28==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address s,address r,int n) public    {
      bool r38 = updateTransferOnInsertRecv_transfer_r38(s,r,n);
      if(r38==false) {
        revert("Rule condition failed");
      }
  }
  function pause() public    {
      bool r39 = updatePausedOnInsertRecv_pause_r39();
      if(r39==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function claimOwnership(address p) public    {
      bool r12 = updateClaimOwnershipOnInsertRecv_claimOwnership_r12(p);
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function updatePausedOnInsertRecv_unpause_r28() private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      bool b_0 = paused.b;
      if(b_0!=false && o_1==s_1) {
        paused = PausedTuple(false,true);
        emit Paused(false);
        return true;
      }
      return false;
  }
  function updateMintOnInsertRecv_mint_r15(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>=0) {
        updateAllMintOnInsertMint_r0(n);
        updateTotalMintOnInsertMint_r29(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateDecreaseAllowanceTotalOnInsertDecreaseApproval_r24(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementDecreaseAllowanceTotal_r20(o,s,delta0);
  }
  function updateTotalBurnOnInsertBurn_r11(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r26(p,delta0);
  }
  function updateIncreaseApprovalOnInsertRecv_increaseApproval_r9(address o,address s,int n) private   returns (bool) {
      if(n>=0) {
        updateAllowanceTotalOnInsertIncreaseApproval_r30(o,s,n);
        emit IncreaseApproval(o,s,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r3() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateBalanceOfOnIncrementTotalBurn_r26(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateTransferOwnershipOnInsertRecv_transferOwnership_r4(address p) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(p!=address(0) && o_1==s_1) {
        updatePendingOwnerOnInsertTransferOwnership_r36(p);
        emit TransferOwnership(p);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalMint_r26(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateBurnOnInsertRecv_burn_r34(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && n<=balanceOf_x1) {
        updateAllBurnOnInsertBurn_r17(n);
        updateTotalBurnOnInsertBurn_r11(p,n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateTotalMintOnInsertMint_r29(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r26(p,delta0);
  }
  function updateAllowanceOnIncrementDecreaseAllowanceTotal_r20(address o,address s,int d) private    {
      allowance[o][s].n -= d;
  }
  function updateSpentTotalOnInsertTransferFrom_r19(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r20(o,s,delta0);
  }
  function updateTransferOnInsertRecv_transfer_r38(address s,address r,int n) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[s].n;
      if(n>0 && n<balanceOf_x1_1) {
        updateTotalInOnInsertTransfer_r5(r,n);
        updateTotalOutOnInsertTransfer_r35(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updatePausedOnInsertRecv_pause_r39() private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      bool b_0 = paused.b;
      if(b_0!=true && o_1==s_1) {
        paused = PausedTuple(true,true);
        emit Paused(true);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementSpentTotal_r20(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r18(delta0);
  }
  function updateBalanceOfOnIncrementTotalOut_r26(address p,int o) private    {
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
  function updateAllBurnOnInsertBurn_r17(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r18(delta0);
  }
  function updatePendingOwnerOnInsertClaimOwnership_r10() private    {
      pendingOwner = PendingOwnerTuple(address(0),true);
  }
  function updateBalanceOfOnIncrementTotalIn_r26(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r20(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateTotalSupplyOnIncrementAllBurn_r18(int b) private    {
      totalSupply.n -= b;
  }
  function updateAllowanceTotalOnInsertIncreaseApproval_r30(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r20(o,s,delta0);
  }
  function updateTotalInOnInsertTransfer_r5(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r26(p,delta0);
  }
  function updateClaimOwnershipOnInsertRecv_claimOwnership_r12(address p) private   returns (bool) {
      address s_1 = pendingOwner.p;
      if(p!=address(0) && s_1!=p) {
        updateOwnerOnInsertClaimOwnership_r37(p);
        updatePendingOwnerOnInsertClaimOwnership_r10();
        emit ClaimOwnership(p);
        return true;
      }
      return false;
  }
  function updateTotalOutOnInsertTransfer_r35(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r26(p,delta0);
  }
  function updateDecreaseApprovalOnInsertRecv_decreaseApproval_r33(address o,address s,int n) private   returns (bool) {
      int allowance_x2 = allowance[o][s].n;
      if(n<allowance_x2) {
        updateDecreaseAllowanceTotalOnInsertDecreaseApproval_r24(o,s,n);
        emit DecreaseApproval(o,s,n);
        return true;
      }
      return false;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r2(address o,address r,address s,int n) private   returns (bool) {
      int balanceOf_x1_2 = balanceOf[o].n;
      int allowance_x2_1 = allowance[o][s].n;
      if(n>0 && n<=allowance_x2_1 && n<=balanceOf_x1_2) {
        updateTransferOnInsertTransferFrom_r25(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r19(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateOwnerOnInsertConstructor_r32() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTransferOnInsertTransferFrom_r25(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r5(r,n);
      updateTotalOutOnInsertTransfer_r35(o,n);
      emit Transfer(o,r,n);
  }
  function updateOwnerOnInsertClaimOwnership_r37(address s) private    {
      owner = OwnerTuple(s,true);
  }
  function updatePendingOwnerOnInsertTransferOwnership_r36(address p) private    {
      pendingOwner = PendingOwnerTuple(p,true);
  }
}