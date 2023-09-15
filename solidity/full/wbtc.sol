contract Wbtc {
  struct TotalInTuple {
    uint n;
    bool _valid;
  }
  struct TotalOutTuple {
    uint n;
    bool _valid;
  }
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct PendingOwnerTuple {
    address p;
    bool _valid;
  }
  struct TotalMintTuple {
    uint n;
    bool _valid;
  }
  struct TotalSupplyTuple {
    uint n;
    bool _valid;
  }
  struct AllMintTuple {
    uint n;
    bool _valid;
  }
  struct AllowanceTotalTuple {
    uint m;
    bool _valid;
  }
  struct SpentTotalTuple {
    uint m;
    bool _valid;
  }
  struct TotalBurnTuple {
    uint n;
    bool _valid;
  }
  struct ClaimOwnershipTuple {
    address p;
    bool _valid;
  }
  struct BalanceOfTuple {
    uint n;
    bool _valid;
  }
  struct AllBurnTuple {
    uint n;
    bool _valid;
  }
  struct AllowanceTuple {
    uint n;
    bool _valid;
  }
  struct PausedTuple {
    bool b;
    bool _valid;
  }
  mapping(address=>TotalInTuple) totalIn;
  mapping(address=>TotalOutTuple) totalOut;
  mapping(address=>TotalBurnTuple) totalBurn;
  OwnerTuple owner;
  PendingOwnerTuple pendingOwner;
  ClaimOwnershipTuple claimOwnership;
  mapping(address=>TotalMintTuple) totalMint;
  TotalSupplyTuple totalSupply;
  AllMintTuple allMint;
  mapping(address=>mapping(address=>AllowanceTotalTuple)) allowanceTotal;
  mapping(address=>mapping(address=>SpentTotalTuple)) spentTotal;
  mapping(address=>BalanceOfTuple) balanceOf;
  AllBurnTuple allBurn;
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  PausedTuple paused;
  event Burn(address p,uint amount);
  event ReclaimToken(address t,address s,uint n);
  event Mint(address p,uint amount);
  event PendingOwner(address p);
  event ClaimOwnership(address p);
  event DecreaseAllowance(address p,address s,uint n);
  event IncreaseAllowance(address p,address s,uint n);
  event Transfer(address from,address to,uint amount);
  event Paused(bool b);
  event TransferFrom(address from,address to,address spender,uint amount);
  constructor() public {
    updateTotalSupplyOnInsertConstructor_r2();
    updateOwnerOnInsertConstructor_r8();
    updateTotalBalancesOnInsertConstructor_r33();
  }
  function reclaimToken() public    {
      bool r23 = updateReclaimTokenOnInsertRecv_reclaimToken_r23();
      if(r23==false) {
        revert("Rule condition failed");
      }
  }
  function transferOwnership(address p) public    {
      bool r22 = updatePendingOwnerOnInsertRecv_transferOwnership_r22(p);
      if(r22==false) {
        revert("Rule condition failed");
      }
  }
  function decreaseApproval(address p,uint n) public    {
      bool r20 = updateDecreaseAllowanceOnInsertRecv_decreaseApproval_r20(p,n);
      if(r20==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (uint) {
      uint n = totalSupply.n;
      return n;
  }
  function increaseApproval(address p,uint n) public    {
      bool r16 = updateIncreaseAllowanceOnInsertRecv_increaseApproval_r16(p,n);
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address to,uint amount) public    {
      bool r3 = updateTransferOnInsertRecv_transfer_r3(to,amount);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (uint) {
      uint n = allowance[p][s].n;
      return n;
  }
  function getBalanceOf(address p) public view  returns (uint) {
      uint n = balanceOf[p].n;
      return n;
  }
  function pause() public    {
      bool r9 = updatePausedOnInsertRecv_pause_r9();
      if(r9==false) {
        revert("Rule condition failed");
      }
  }
  function burn(uint amount) public    {
      bool r14 = updateBurnOnInsertRecv_burn_r14(amount);
      if(r14==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,uint amount) public    {
      bool r10 = updateMintOnInsertRecv_mint_r10(p,amount);
      if(r10==false) {
        revert("Rule condition failed");
      }
  }
  function approve(address s,uint n) public    {
      bool r31 = updateIncreaseAllowanceOnInsertRecv_approve_r31(s,n);
      if(r31==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,uint amount) public    {
      bool r13 = updateTransferFromOnInsertRecv_transferFrom_r13(from,to,amount);
      if(r13==false) {
        revert("Rule condition failed");
      }
  }
  function claimOwnership() public    {
      bool r12 = updateClaimOwnershipOnInsertRecv_claimOwnership_r12();
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function unpause() public    {
      bool r26 = updatePausedOnInsertRecv_unpause_r26();
      if(r26==false) {
        revert("Rule condition failed");
      }
  }
  function updateMintOnInsertRecv_mint_r10(address p,uint n) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        if(p!=address(0)) {
          updateAllMintOnInsertMint_r15(n);
          updateTotalMintOnInsertMint_r18(p,n);
          emit Mint(p,n);
          return true;
        }
      }
      return false;
  }
  function updateOwnerOnInsertConstructor_r8() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTotalSupplyOnInsertConstructor_r2() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateSendOnInsertReclaimToken_r21(address s,uint n) private    {
      payable(s).send(n);
  }
  function updatePausedOnInsertRecv_unpause_r26() private   returns (bool) {
      if(true==paused.b) {
        address s = owner.p;
        if(s==msg.sender) {
          paused = PausedTuple(false,true);
          emit Paused(false);
          return true;
        }
      }
      return false;
  }
  function updateTransferOnInsertReclaimToken_r4(address t,address s,uint n) private    {
      updateTotalInOnInsertTransfer_r11(s,n);
      updateTotalOutOnInsertTransfer_r24(t,n);
      emit Transfer(t,s,n);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r13(address o,address r,uint n) private   returns (bool) {
      address s = msg.sender;
      if(false==paused.b) {
        uint m = balanceOf[o].n;
        uint k = allowance[o][s].n;
        if(m>=n && k>=n) {
          updateSpentTotalOnInsertTransferFrom_r27(o,s,n);
          updateTransferOnInsertTransferFrom_r0(o,r,n);
          emit TransferFrom(o,r,s,n);
          return true;
        }
      }
      return false;
  }
  function updateTransferOnInsertRecv_transfer_r3(address r,uint n) private   returns (bool) {
      if(false==paused.b) {
        address s = msg.sender;
        uint m = balanceOf[s].n;
        if(n<=m) {
          updateTotalInOnInsertTransfer_r11(r,n);
          updateTotalOutOnInsertTransfer_r24(s,n);
          emit Transfer(s,r,n);
          return true;
        }
      }
      return false;
  }
  function updateTotalBurnOnInsertBurn_r17(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r6(p,delta0);
      totalBurn[p].n += n;
  }
  function updateDecreaseAllowanceOnInsertRecv_decreaseApproval_r20(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      uint m = allowance[o][s].n;
      if(m>=n) {
        updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r7(o,s,n);
        emit DecreaseAllowance(o,s,n);
        return true;
      }
      return false;
  }
  function updateTotalMintOnInsertMint_r18(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r6(p,delta0);
      totalMint[p].n += n;
  }
  function updateBalanceOfOnIncrementTotalOut_r6(address p,int o) private    {
      int _delta = int(-o);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateTotalSupplyOnIncrementAllBurn_r19(int b) private    {
      int _delta = int(-b);
      uint newValue = updateuintByint(totalSupply.n,_delta);
      totalSupply.n = newValue;
  }
  function updateIncreaseAllowanceOnInsertRecv_approve_r31(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      uint m = allowance[o][s].n;
      uint d = n-m;
      updateAllowanceTotalOnInsertIncreaseAllowance_r32(o,s,d);
      emit IncreaseAllowance(o,s,d);
      return true;
      return false;
  }
  function updatePendingOwnerOnInsertRecv_transferOwnership_r22(address p) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        if(p!=address(0)) {
          pendingOwner = PendingOwnerTuple(p,true);
          emit PendingOwner(p);
          return true;
        }
      }
      return false;
  }
  function updateOwnerOnInsertClaimOwnership_r30(address s) private    {
      owner = OwnerTuple(s,true);
  }
  function updatePausedOnInsertRecv_pause_r9() private   returns (bool) {
      if(false==paused.b) {
        address s = owner.p;
        if(s==msg.sender) {
          paused = PausedTuple(true,true);
          emit Paused(true);
          return true;
        }
      }
      return false;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r28(address o,address s,int m) private    {
      int _delta = int(m);
      uint newValue = updateuintByint(allowance[o][s].n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r32(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r28(o,s,delta0);
      allowanceTotal[o][s].m += n;
  }
  function updateAllowanceOnIncrementSpentTotal_r28(address o,address s,int l) private    {
      int _delta = int(-l);
      uint newValue = updateuintByint(allowance[o][s].n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateClaimOwnershipOnInsertRecv_claimOwnership_r12() private   returns (bool) {
      address s = pendingOwner.p;
      if(s==msg.sender) {
        updateOwnerOnInsertClaimOwnership_r30(s);
        updatePendingOwnerOnInsertClaimOwnership_r1();
        claimOwnership = ClaimOwnershipTuple(s,true);
        emit ClaimOwnership(s);
        return true;
      }
      return false;
  }
  function updateTotalOutOnInsertTransfer_r24(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r6(p,delta0);
      totalOut[p].n += n;
  }
  function updateBurnOnInsertRecv_burn_r14(uint n) private   returns (bool) {
      address p = msg.sender;
      uint m = balanceOf[p].n;
      if(n<=m) {
        updateTotalBurnOnInsertBurn_r17(p,n);
        updateAllBurnOnInsertBurn_r29(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateTotalBalancesOnInsertConstructor_r33() private    {
      // Empty()
  }
  function updateSpentTotalOnInsertTransferFrom_r27(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r28(o,s,delta0);
      spentTotal[o][s].m += n;
  }
  function updateAllowanceOnIncrementDecreaseAllowanceTotal_r28(address o,address s,int d) private    {
      int _delta = int(-d);
      uint newValue = updateuintByint(allowance[o][s].n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateTransferOnInsertTransferFrom_r0(address o,address r,uint n) private    {
      updateTotalInOnInsertTransfer_r11(r,n);
      updateTotalOutOnInsertTransfer_r24(o,n);
      emit Transfer(o,r,n);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateAllMintOnInsertMint_r15(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r19(delta0);
      allMint.n += n;
  }
  function updateBalanceOfOnIncrementTotalBurn_r6(address p,int m) private    {
      int _delta = int(-m);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateBalanceOfOnIncrementTotalMint_r6(address p,int n) private    {
      int _delta = int(n);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateReclaimTokenOnInsertRecv_reclaimToken_r23() private   returns (bool) {
      address s = msg.sender;
      if(s==owner.p) {
        address t = address(this);
        uint n = balanceOf[t].n;
        updateSendOnInsertReclaimToken_r21(s,n);
        updateTransferOnInsertReclaimToken_r4(t,s,n);
        emit ReclaimToken(t,s,n);
        return true;
      }
      return false;
  }
  function updateAllBurnOnInsertBurn_r29(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r19(delta0);
      allBurn.n += n;
  }
  function updatePendingOwnerOnInsertClaimOwnership_r1() private    {
      pendingOwner = PendingOwnerTuple(address(0),true);
      emit PendingOwner(address(0));
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseApproval_r16(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      updateAllowanceTotalOnInsertIncreaseAllowance_r32(o,s,n);
      emit IncreaseAllowance(o,s,n);
      return true;
      return false;
  }
  function updateTotalInOnInsertTransfer_r11(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r6(p,delta0);
      totalIn[p].n += n;
  }
  function updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r7(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementDecreaseAllowanceTotal_r28(o,s,delta0);
  }
  function updateTotalSupplyOnIncrementAllMint_r19(int m) private    {
      int _delta = int(m);
      uint newValue = updateuintByint(totalSupply.n,_delta);
      totalSupply.n = newValue;
  }
  function updateBalanceOfOnIncrementTotalIn_r6(address p,int i) private    {
      int _delta = int(i);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
}