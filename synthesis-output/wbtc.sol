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
    updateOwnerOnInsertConstructor_r29();
    updateTotalSupplyOnInsertConstructor_r4();
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function transferOwnership(address p) public    {
      bool r5 = updateTransferOwnershipOnInsertRecv_transferOwnership_r5(p);
      if(r5==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r32 = updateBurnOnInsertRecv_burn_r32(p,amount);
      if(r32==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address s,address r,int n) public    {
      bool r38 = updateTransferOnInsertRecv_transfer_r38(s,r,n);
      if(r38==false) {
        revert("Rule condition failed");
      }
  }
  function claimOwnership(address p) public    {
      bool r25 = updateClaimOwnershipOnInsertRecv_claimOwnership_r25(p);
      if(r25==false) {
        revert("Rule condition failed");
      }
  }
  function increaseApproval(address p,address s,int n) public    {
      bool r9 = updateIncreaseApprovalOnInsertRecv_increaseApproval_r9(p,s,n);
      if(r9==false) {
        revert("Rule condition failed");
      }
  }
  function unpause() public    {
      bool r27 = updatePausedOnInsertRecv_unpause_r27();
      if(r27==false) {
        revert("Rule condition failed");
      }
  }
  function decreaseApproval(address p,address s,int n) public    {
      bool r31 = updateDecreaseApprovalOnInsertRecv_decreaseApproval_r31(p,s,n);
      if(r31==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function transferFrom(address o,address r,address s,int n) public    {
      bool r35 = updateTransferFromOnInsertRecv_transferFrom_r35(o,r,s,n);
      if(r35==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r13 = updateMintOnInsertRecv_mint_r13(p,amount);
      if(r13==false) {
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
  function updateSpentTotalOnInsertTransferFrom_r17(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r18(o,s,delta0);
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r16(delta0);
  }
  function updateBalanceOfOnIncrementTotalIn_r24(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateClaimOwnershipOnInsertRecv_claimOwnership_r25(address p) private   returns (bool) {
      address p_1 = pendingOwner.p;
      if(p!=address(0) && s!=p_1) {
        updateOwnerOnInsertClaimOwnership_r37(p);
        updatePendingOwnerOnInsertClaimOwnership_r10();
        emit ClaimOwnership(p);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r16(int b) private    {
      totalSupply.n -= b;
  }
  function updateBalanceOfOnIncrementTotalOut_r24(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r18(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateTotalBurnOnInsertBurn_r11(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r24(p,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateMintOnInsertRecv_mint_r13(address p,int n) private   returns (bool) {
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
  function updateTransferOnInsertTransferFrom_r23(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r6(r,n);
      updateTotalOutOnInsertTransfer_r33(o,n);
      emit Transfer(o,r,n);
  }
  function updateTransferOnInsertRecv_transfer_r38(address s,address r,int n) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[s].n;
      if(n>0 && n<balanceOf_x1_1) {
        updateTotalInOnInsertTransfer_r6(r,n);
        updateTotalOutOnInsertTransfer_r33(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementSpentTotal_r18(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateBalanceOfOnIncrementTotalMint_r24(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateTransferOwnershipOnInsertRecv_transferOwnership_r5(address p) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(p!=address(0) && o_1==s_1) {
        updatePendingOwnerOnInsertTransferOwnership_r36(p);
        emit TransferOwnership(p);
        return true;
      }
      return false;
  }
  function updateTotalInOnInsertTransfer_r6(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r24(p,delta0);
  }
  function updateAllBurnOnInsertBurn_r15(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r16(delta0);
  }
  function updateTotalSupplyOnIncrementAllMint_r16(int m) private    {
      totalSupply.n += m;
  }
  function updateDecreaseApprovalOnInsertRecv_decreaseApproval_r31(address o,address s,int n) private   returns (bool) {
      int allowance_x2 = allowance[o][s].n;
      if(n<allowance_x2) {
        updateDecreaseAllowanceTotalOnInsertDecreaseApproval_r22(o,s,n);
        emit DecreaseApproval(o,s,n);
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
  function updateBalanceOfOnIncrementTotalBurn_r24(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateAllowanceTotalOnInsertIncreaseApproval_r30(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r18(o,s,delta0);
  }
  function updateTotalSupplyOnInsertConstructor_r4() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateAllowanceOnIncrementDecreaseAllowanceTotal_r18(address o,address s,int d) private    {
      allowance[o][s].n -= d;
  }
  function updateOwnerOnInsertConstructor_r29() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r35(address o,address r,address s,int n) private   returns (bool) {
      int balanceOf_x1_2 = balanceOf[o].n;
      int allowance_x2_1 = allowance[o][s].n;
      if(n>=0 && n<allowance_x2_1 && n<balanceOf_x1_2) {
        updateTransferOnInsertTransferFrom_r23(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r17(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateBurnOnInsertRecv_burn_r32(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && n<=balanceOf_x1) {
        updateAllBurnOnInsertBurn_r15(n);
        updateTotalBurnOnInsertBurn_r11(p,n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updatePendingOwnerOnInsertClaimOwnership_r10() private    {
      pendingOwner = PendingOwnerTuple(address(0),true);
  }
  function updateDecreaseAllowanceTotalOnInsertDecreaseApproval_r22(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementDecreaseAllowanceTotal_r18(o,s,delta0);
  }
  function updateIncreaseApprovalOnInsertRecv_increaseApproval_r9(address o,address s,int n) private   returns (bool) {
      if(n>=0) {
        updateAllowanceTotalOnInsertIncreaseApproval_r30(o,s,n);
        emit IncreaseApproval(o,s,n);
        return true;
      }
      return false;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTotalMintOnInsertMint_r28(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r24(p,delta0);
  }
  function updateTotalOutOnInsertTransfer_r33(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r24(p,delta0);
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
  function updateOwnerOnInsertClaimOwnership_r37(address s) private    {
      owner = OwnerTuple(s,true);
  }
  function updatePendingOwnerOnInsertTransferOwnership_r36(address p) private    {
      pendingOwner = PendingOwnerTuple(p,true);
  }
}