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
    updateTotalSupplyOnInsertConstructor_r2();
    updateOwnerOnInsertConstructor_r31();
  }
  function transferFrom(address o,address r,address s,int n) public    {
      bool r38 = updateTransferFromOnInsertRecv_transferFrom_r38(o,r,s,n);
      if(r38==false) {
        revert("Rule condition failed");
      }
  }
  function claimOwnership(address p) public    {
      bool r10 = updateClaimOwnershipOnInsertRecv_claimOwnership_r10(p);
      if(r10==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function burn(address p,int amount) public    {
      bool r32 = updateBurnOnInsertRecv_burn_r32(p,amount);
      if(r32==false) {
        revert("Rule condition failed");
      }
  }
  function transferOwnership(address p) public    {
      bool r3 = updateTransferOwnershipOnInsertRecv_transferOwnership_r3(p);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function increaseApproval(address p,address s,int n) public    {
      bool r39 = updateIncreaseApprovalOnInsertRecv_increaseApproval_r39(p,s,n);
      if(r39==false) {
        revert("Rule condition failed");
      }
  }
  function unpause() public    {
      bool r27 = updatePausedOnInsertRecv_unpause_r27();
      if(r27==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address s,address r,int n) public    {
      bool r36 = updateTransferOnInsertRecv_transfer_r36(s,r,n);
      if(r36==false) {
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
  function mint(address p,int amount) public    {
      bool r13 = updateMintOnInsertRecv_mint_r13(p,amount);
      if(r13==false) {
        revert("Rule condition failed");
      }
  }
  function pause() public    {
      bool r37 = updatePausedOnInsertRecv_pause_r37();
      if(r37==false) {
        revert("Rule condition failed");
      }
  }
  function decreaseApproval(address p,address s,int n) public    {
      bool r17 = updateDecreaseApprovalOnInsertRecv_decreaseApproval_r17(p,s,n);
      if(r17==false) {
        revert("Rule condition failed");
      }
  }
  function updateDecreaseAllowanceTotalOnInsertDecreaseApproval_r23(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementDecreaseAllowanceTotal_r19(o,s,delta0);
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r16(delta0);
  }
  function updateTotalSupplyOnIncrementAllMint_r16(int m) private    {
      totalSupply.n += m;
  }
  function updateBalanceOfOnIncrementTotalOut_r25(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateTransferOwnershipOnInsertRecv_transferOwnership_r3(address p) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(p!=address(0) && o_1==s_1) {
        updatePendingOwnerOnInsertTransferOwnership_r34(p);
        emit TransferOwnership(p);
        return true;
      }
      return false;
  }
  function updatePendingOwnerOnInsertClaimOwnership_r8() private    {
      pendingOwner = PendingOwnerTuple(address(0),true);
  }
  function updateTotalBurnOnInsertBurn_r9(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r25(p,delta0);
  }
  function updateIncreaseApprovalOnInsertRecv_increaseApproval_r39(address p,address s,int n) private   returns (bool) {
      if(n>=0) {
        updateAllowanceTotalOnInsertIncreaseApproval_r29(o,s,n);
        emit IncreaseApproval(o,s,n);
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
  function updateTotalSupplyOnInsertConstructor_r2() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateClaimOwnershipOnInsertRecv_claimOwnership_r10(address p) private   returns (bool) {
      address s_1 = pendingOwner.p;
      if(p!=address(0) && s_1!=p) {
        updatePendingOwnerOnInsertClaimOwnership_r8();
        updateOwnerOnInsertClaimOwnership_r35(p);
        emit ClaimOwnership(p);
        return true;
      }
      return false;
  }
  function updateDecreaseApprovalOnInsertRecv_decreaseApproval_r17(address p,address s,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[s].n;
      if(n<balanceOf_x1) {
        updateDecreaseAllowanceTotalOnInsertDecreaseApproval_r23(o,s,n);
        emit DecreaseApproval(o,s,n);
        return true;
      }
      return false;
  }
  function updateAllowanceTotalOnInsertIncreaseApproval_r29(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r19(o,s,delta0);
  }
  function updatePausedOnInsertRecv_unpause_r27() private   returns (bool) {
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
  function updateBalanceOfOnIncrementTotalBurn_r25(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateMintOnInsertRecv_mint_r13(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>=0) {
        updateAllMintOnInsertMint_r0(n);
        updateTotalMintOnInsertMint_r28(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalMint_r25(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateBurnOnInsertRecv_burn_r32(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && n<=balanceOf_x1) {
        updateAllBurnOnInsertBurn_r15(n);
        updateTotalBurnOnInsertBurn_r9(p,n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateAllBurnOnInsertBurn_r15(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r16(delta0);
  }
  function updatePausedOnInsertRecv_pause_r37() private   returns (bool) {
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
  function updateTransferOnInsertRecv_transfer_r36(address s,address r,int n) private   returns (bool) {
      updateTotalInOnInsertTransfer_r4(r,n);
      updateTotalOutOnInsertTransfer_r33(s,n);
      emit Transfer(s,r,n);
      return true;
      return false;
  }
  function updateTotalMintOnInsertMint_r28(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r25(p,delta0);
  }
  function updateTotalSupplyOnIncrementAllBurn_r16(int b) private    {
      totalSupply.n -= b;
  }
  function updateOwnerOnInsertClaimOwnership_r35(address s) private    {
      owner = OwnerTuple(s,true);
  }
  function updateAllowanceOnIncrementDecreaseAllowanceTotal_r19(address o,address s,int d) private    {
      allowance[o][s].n -= d;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r19(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateSpentTotalOnInsertTransferFrom_r18(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r19(o,s,delta0);
  }
  function updateBalanceOfOnIncrementTotalIn_r25(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updatePendingOwnerOnInsertTransferOwnership_r34(address p) private    {
      pendingOwner = PendingOwnerTuple(p,true);
  }
  function updateTotalOutOnInsertTransfer_r33(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r25(p,delta0);
  }
  function updateOwnerOnInsertConstructor_r31() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTotalInOnInsertTransfer_r4(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r25(p,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTransferOnInsertTransferFrom_r24(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r33(o,n);
      updateTotalInOnInsertTransfer_r4(r,n);
      emit Transfer(o,r,n);
  }
  function updateAllowanceOnIncrementSpentTotal_r19(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r38(address o,address r,address s,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[r].n;
      if(0!=balanceOf_x1) {
        updateTransferOnInsertTransferFrom_r24(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r18(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
}