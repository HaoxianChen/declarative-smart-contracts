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
    updateOwnerOnInsertConstructor_r35();
    updateTotalSupplyOnInsertConstructor_r2();
  }
  function transfer(address s,address r,int n) public    {
      bool r23 = updateTransferOnInsertRecv_transfer_r23(s,r,n);
      if(r23==false) {
        revert("Rule condition failed");
      }
  }
  function pause() public    {
      bool r6 = updatePausedOnInsertRecv_pause_r6();
      if(r6==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function transferFrom(address o,address r,address s,int n) public    {
      bool r8 = updateTransferFromOnInsertRecv_transferFrom_r8(o,r,s,n);
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function claimOwnership(address p) public    {
      bool r11 = updateClaimOwnershipOnInsertRecv_claimOwnership_r11(p);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function transferOwnership(address p) public    {
      bool r24 = updateTransferOwnershipOnInsertRecv_transferOwnership_r24(p);
      if(r24==false) {
        revert("Rule condition failed");
      }
  }
  function decreaseApproval(address p,address s,int n) public    {
      bool r19 = updateDecreaseApprovalOnInsertRecv_decreaseApproval_r19(p,s,n);
      if(r19==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r15 = updateMintOnInsertRecv_mint_r15(p,amount);
      if(r15==false) {
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
  function burn(address p,int amount) public    {
      bool r36 = updateBurnOnInsertRecv_burn_r36(p,amount);
      if(r36==false) {
        revert("Rule condition failed");
      }
  }
  function unpause() public    {
      bool r31 = updatePausedOnInsertRecv_unpause_r31();
      if(r31==false) {
        revert("Rule condition failed");
      }
  }
  function increaseApproval(address p,address s,int n) public    {
      bool r13 = updateIncreaseApprovalOnInsertRecv_increaseApproval_r13(p,s,n);
      if(r13==false) {
        revert("Rule condition failed");
      }
  }
  function updateTotalBurnOnInsertBurn_r10(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r29(p,delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r8(address o,address r,address s,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[r].n;
      if(0!=balanceOf_x1) {
        updateSpentTotalOnInsertTransferFrom_r20(o,s,n);
        updateTransferOnInsertTransferFrom_r28(o,r,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateDecreaseApprovalOnInsertRecv_decreaseApproval_r19(address p,address s,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[s].n;
      if(n<balanceOf_x1) {
        updateDecreaseAllowanceTotalOnInsertDecreaseApproval_r27(o,s,n);
        emit DecreaseApproval(o,s,n);
        return true;
      }
      return false;
  }
  function updateBurnOnInsertRecv_burn_r36(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && n<=balanceOf_x1) {
        updateAllBurnOnInsertBurn_r17(n);
        updateTotalBurnOnInsertBurn_r10(p,n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementDecreaseAllowanceTotal_r21(address o,address s,int d) private    {
      allowance[o][s].n -= d;
  }
  function updateAllowanceTotalOnInsertIncreaseApproval_r33(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r21(o,s,delta0);
  }
  function updatePendingOwnerOnInsertClaimOwnership_r9() private    {
      pendingOwner = PendingOwnerTuple(address(0),true);
  }
  function updateTotalOutOnInsertTransfer_r37(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r29(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalOut_r29(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateOwnerOnInsertConstructor_r35() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateOwnerOnInsertClaimOwnership_r39(address s) private    {
      owner = OwnerTuple(s,true);
  }
  function updateBalanceOfOnIncrementTotalBurn_r29(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateTotalMintOnInsertMint_r32(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r29(p,delta0);
  }
  function updatePausedOnInsertRecv_pause_r6() private   returns (bool) {
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
  function updatePendingOwnerOnInsertTransferOwnership_r38(address p) private    {
      pendingOwner = PendingOwnerTuple(p,true);
  }
  function updateDecreaseAllowanceTotalOnInsertDecreaseApproval_r27(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementDecreaseAllowanceTotal_r21(o,s,delta0);
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r18(delta0);
  }
  function updateTransferOnInsertRecv_transfer_r23(address s,address r,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[s].n;
      if(0!=balanceOf_x1) {
        updateTotalOutOnInsertTransfer_r37(s,n);
        updateTotalInOnInsertTransfer_r3(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateTransferOwnershipOnInsertRecv_transferOwnership_r24(address p) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(p!=address(0) && o_1==s_1) {
        updatePendingOwnerOnInsertTransferOwnership_r38(p);
        emit TransferOwnership(p);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalIn_r29(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateClaimOwnershipOnInsertRecv_claimOwnership_r11(address p) private   returns (bool) {
      address s_1 = pendingOwner.p;
      if(p!=address(0) && s_1!=p) {
        updateOwnerOnInsertClaimOwnership_r39(p);
        updatePendingOwnerOnInsertClaimOwnership_r9();
        emit ClaimOwnership(p);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r21(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateAllowanceOnIncrementSpentTotal_r21(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateMintOnInsertRecv_mint_r15(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>=0) {
        updateTotalMintOnInsertMint_r32(p,n);
        updateAllMintOnInsertMint_r0(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalMint_r29(address p,int n) private    {
      balanceOf[p].n += n;
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
  function updateTotalInOnInsertTransfer_r3(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r29(p,delta0);
  }
  function updateTotalSupplyOnInsertConstructor_r2() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateSpentTotalOnInsertTransferFrom_r20(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r21(o,s,delta0);
  }
  function updateAllBurnOnInsertBurn_r17(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r18(delta0);
  }
  function updatePausedOnInsertRecv_unpause_r31() private   returns (bool) {
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
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTransferOnInsertTransferFrom_r28(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r3(r,n);
      updateTotalOutOnInsertTransfer_r37(o,n);
      emit Transfer(o,r,n);
  }
  function updateIncreaseApprovalOnInsertRecv_increaseApproval_r13(address p,address s,int n) private   returns (bool) {
      if(n>0) {
        updateAllowanceTotalOnInsertIncreaseApproval_r33(o,s,n);
        emit IncreaseApproval(o,s,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r18(int b) private    {
      totalSupply.n -= b;
  }
}