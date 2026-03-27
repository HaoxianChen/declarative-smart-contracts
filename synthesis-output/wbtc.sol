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
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  PendingOwnerTuple pendingOwner;
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalSupplyTuple totalSupply;
  OwnerTuple owner;
  event TransferOwnership(address p);
  event PauseNotByOwner();
  event Paused(bool b);
  event Transfer(address s,address r,int n);
  event Mint(address p,int amount);
  event IncreaseApproval(address p,address s,int n);
  event UnauthorizedMint();
  event Burn(address p,int amount);
  event TransferFrom(address o,address r,address s,int n);
  event ClaimOwnershipWrongCaller();
  event DecreaseApproval(address p,address s,int n);
  event TransferOwnershipNotByOwner();
  event ClaimOwnership(address p);
  constructor() public {
    updateTotalSupplyOnInsertConstructor_r4();
    updateOwnerOnInsertConstructor_r29();
  }
  function mint(address p,int amount) public    {
      bool r32 = updateMintOnInsertRecv_mint_r32(p,amount);
      if(r32==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address s,address r,int n) public    {
      bool r10 = updateTransferOnInsertRecv_transfer_r10(s,r,n);
      if(r10==false) {
        revert("Rule condition failed");
      }
  }
  function increaseApproval(address p,address s,int n) public    {
      bool r2 = updateIncreaseApprovalOnInsertRecv_increaseApproval_r2(p,s,n);
      if(r2==false) {
        revert("Rule condition failed");
      }
  }
  function pause() public    {
      bool r18 = updatePausedOnInsertRecv_pause_r18();
      if(r18==false) {
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
  function claimOwnership(address p) public    {
      bool r3 = updateClaimOwnershipOnInsertRecv_claimOwnership_r3(p);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function unpause() public    {
      bool r21 = updatePausedOnInsertRecv_unpause_r21();
      if(r21==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function decreaseApproval(address p,address s,int n) public    {
      bool r15 = updateDecreaseApprovalOnInsertRecv_decreaseApproval_r15(p,s,n);
      if(r15==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address o,address r,address s,int n) public    {
      bool r8 = updateTransferFromOnInsertRecv_transferFrom_r8(o,r,s,n);
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function transferOwnership(address p) public    {
      bool r23 = updateTransferOwnershipOnInsertRecv_transferOwnership_r23(p);
      if(r23==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r11 = updateBurnOnInsertRecv_burn_r11(p,amount);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function updatePendingOwnerOnInsertClaimOwnership_r12() private    {
      pendingOwner = PendingOwnerTuple(address(0),true);
  }
  function updateMintOnInsertRecv_mint_r32(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(s==o && amount>0) {
        updateTotalMintOnInsertMint_r28(p,amount);
        updateAllMintOnInsertMint_r0(amount);
        emit Mint(p,amount);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertTransferFrom_r26(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r6(r,n);
      updateTotalOutOnInsertTransfer_r33(o,n);
      emit Transfer(o,r,n);
  }
  function updateBalanceOfOnIncrementTotalMint_r27(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateAllowanceOnIncrementSpentTotal_r25(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateAllBurnOnInsertBurn_r19(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r20(delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r8(address o,address r,address s,int n) private   returns (bool) {
      int allowance_x2 = allowance[o][s].n;
      if(n<=allowance_x2) {
        updateTransferOnInsertTransferFrom_r26(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r24(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateTransferOwnershipOnInsertRecv_transferOwnership_r23(address p) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(s==o) {
        updatePendingOwnerOnInsertTransferOwnership_r34(p);
        emit TransferOwnership(p);
        return true;
      }
      return false;
  }
  function updateOwnerOnInsertClaimOwnership_r35(address s) private    {
      owner = OwnerTuple(s,true);
  }
  function updateIncreaseApprovalOnInsertRecv_increaseApproval_r2(address p,address s,int n) private   returns (bool) {
      if(n>0) {
        updateAllowanceTotalOnInsertIncreaseApproval_r31(p,s,n);
        emit IncreaseApproval(p,s,n);
        return true;
      }
      return false;
  }
  function updateAllowanceTotalOnInsertIncreaseApproval_r31(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r25(o,s,delta0);
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r20(delta0);
  }
  function updateAllowanceOnIncrementDecreaseAllowanceTotal_r25(address o,address s,int d) private    {
      allowance[o][s].n -= d;
  }
  function updateBalanceOfOnIncrementTotalBurn_r27(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updatePendingOwnerOnInsertTransferOwnership_r34(address p) private    {
      pendingOwner = PendingOwnerTuple(p,true);
  }
  function updatePausedOnInsertRecv_pause_r18() private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(s==o) {
        emit Paused(true);
        return true;
      }
      return false;
  }
  function updateOwnerOnInsertConstructor_r29() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r25(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateTotalOutOnInsertTransfer_r33(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r27(p,delta0);
  }
  function updateTransferOnInsertRecv_transfer_r10(address s,address r,int n) private   returns (bool) {
      updateTotalInOnInsertTransfer_r6(r,n);
      updateTotalOutOnInsertTransfer_r33(s,n);
      emit Transfer(s,r,n);
      return true;
      return false;
  }
  function updateTotalInOnInsertTransfer_r6(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r27(p,delta0);
  }
  function updateClaimOwnershipOnInsertRecv_claimOwnership_r3(address p) private   returns (bool) {
      address expected_1 = pendingOwner.p;
      address s_0 = msg.sender;
      if(s_0==expected_1 && p==expected_1) {
        updatePendingOwnerOnInsertClaimOwnership_r12();
        updateOwnerOnInsertClaimOwnership_r35(p);
        emit ClaimOwnership(p);
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
  function updateTotalBurnOnInsertBurn_r13(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r27(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalIn_r27(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTotalMintOnInsertMint_r28(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r27(p,delta0);
  }
  function updateDecreaseAllowanceTotalOnInsertDecreaseApproval_r7(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementDecreaseAllowanceTotal_r25(o,s,delta0);
  }
  function updateSpentTotalOnInsertTransferFrom_r24(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r25(o,s,delta0);
  }
  function updatePausedOnInsertRecv_unpause_r21() private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(s==o) {
        emit Paused(false);
        return true;
      }
      return false;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateDecreaseApprovalOnInsertRecv_decreaseApproval_r15(address p,address s,int n) private   returns (bool) {
      int totalSupply_n = totalSupply.n;
      if(0!=totalSupply_n) {
        updateDecreaseAllowanceTotalOnInsertDecreaseApproval_r7(p,s,n);
        emit DecreaseApproval(p,s,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r20(int b) private    {
      totalSupply.n -= b;
  }
  function updateBalanceOfOnIncrementTotalOut_r27(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateTotalSupplyOnInsertConstructor_r4() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateBurnOnInsertRecv_burn_r11(address p,int amount) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(amount<=balanceOf_x1) {
        updateAllBurnOnInsertBurn_r19(amount);
        updateTotalBurnOnInsertBurn_r13(p,amount);
        emit Burn(p,amount);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r20(int m) private    {
      totalSupply.n += m;
  }
}