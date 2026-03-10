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
    updateOwnerOnInsertConstructor_r34();
    updateTotalSupplyOnInsertConstructor_r3();
  }
  function transfer(address s,address r,int n) public    {
      bool r45 = updateTransferOnInsertRecv_transfer_r45(s,r,n);
      if(r45==false) {
        revert("Rule condition failed");
      }
  }
  function pause() public    {
      bool r44 = updatePausedOnInsertRecv_pause_r44();
      if(r44==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function unpause() public    {
      bool r31 = updatePausedOnInsertRecv_unpause_r31();
      if(r31==false) {
        revert("Rule condition failed");
      }
  }
  function claimOwnership(address p) public    {
      bool r7 = updateClaimOwnershipOnInsertRecv_claimOwnership_r7(p);
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address o,address r,address s,int n) public    {
      bool r41 = updateTransferFromOnInsertRecv_transferFrom_r41(o,r,s,n);
      if(r41==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r38 = updateBurnOnInsertRecv_burn_r38(p,amount);
      if(r38==false) {
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
  function increaseApproval(address p,address s,int n) public    {
      bool r46 = updateIncreaseApprovalOnInsertRecv_increaseApproval_r46(p,s,n);
      if(r46==false) {
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
      bool r13 = updateMintOnInsertRecv_mint_r13(p,amount);
      if(r13==false) {
        revert("Rule condition failed");
      }
  }
  function transferOwnership(address p) public    {
      bool r23 = updateTransferOwnershipOnInsertRecv_transferOwnership_r23(p);
      if(r23==false) {
        revert("Rule condition failed");
      }
  }
  function updateTotalBurnOnInsertBurn_r11(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r29(p,delta0);
  }
  function updateBurnOnInsertRecv_burn_r38(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && amount<=balanceOf_x1) {
        updateAllBurnOnInsertBurn_r17(amount);
        updateTotalBurnOnInsertBurn_r11(p,amount);
        emit Burn(p,amount);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementDecreaseAllowanceTotal_r20(address o,address s,int d) private    {
      allowance[o][s].n -= d;
  }
  function updateAllowanceOnIncrementSpentTotal_r20(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTransferOwnershipOnInsertRecv_transferOwnership_r23(address p) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(p!=address(0) && o_1==s_1) {
        updatePendingOwnerOnInsertTransferOwnership_r40(p);
        emit TransferOwnership(p);
        return true;
      }
      return false;
  }
  function updateIncreaseApprovalOnInsertRecv_increaseApproval_r46(address p,address s,int n) private   returns (bool) {
      if(n>=0) {
        updateAllowanceTotalOnInsertIncreaseApproval_r35(p,s,n);
        emit IncreaseApproval(p,s,n);
        return true;
      }
      return false;
  }
  function updateMintOnInsertRecv_mint_r13(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && amount>=0) {
        updateAllMintOnInsertMint_r0(amount);
        updateTotalMintOnInsertMint_r32(p,amount);
        emit Mint(p,amount);
        return true;
      }
      return false;
  }
  function updateAllBurnOnInsertBurn_r17(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r18(delta0);
  }
  function updateBalanceOfOnIncrementTotalOut_r29(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updatePendingOwnerOnInsertClaimOwnership_r10() private    {
      pendingOwner = PendingOwnerTuple(address(0),true);
  }
  function updateSpentTotalOnInsertTransferFrom_r19(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r20(o,s,delta0);
  }
  function updatePausedOnInsertRecv_pause_r44() private   returns (bool) {
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
  function updateBalanceOfOnIncrementTotalBurn_r29(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateTotalMintOnInsertMint_r32(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r29(p,delta0);
  }
  function updateOwnerOnInsertConstructor_r34() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateAllowanceTotalOnInsertIncreaseApproval_r35(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r20(o,s,delta0);
  }
  function updateTransferOnInsertTransferFrom_r27(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r5(r,n);
      updateTotalOutOnInsertTransfer_r39(o,n);
      emit Transfer(o,r,n);
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r18(delta0);
  }
  function updateDecreaseApprovalOnInsertRecv_decreaseApproval_r33(address p,address s,int n) private   returns (bool) {
      if(n>0) {
        updateDecreaseAllowanceTotalOnInsertDecreaseApproval_r26(p,s,n);
        emit DecreaseApproval(p,s,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalIn_r29(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTotalOutOnInsertTransfer_r39(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r29(p,delta0);
  }
  function updatePendingOwnerOnInsertTransferOwnership_r40(address p) private    {
      pendingOwner = PendingOwnerTuple(p,true);
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
  function updateDecreaseAllowanceTotalOnInsertDecreaseApproval_r26(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementDecreaseAllowanceTotal_r20(o,s,delta0);
  }
  function updateClaimOwnershipOnInsertRecv_claimOwnership_r7(address p) private   returns (bool) {
      address s_1 = pendingOwner.p;
      if(p!=address(0) && s_1==p) {
        updatePendingOwnerOnInsertClaimOwnership_r10();
        updateOwnerOnInsertClaimOwnership_r42(p);
        emit ClaimOwnership(p);
        return true;
      }
      return false;
  }
  function updateTotalInOnInsertTransfer_r5(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r29(p,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateOwnerOnInsertClaimOwnership_r42(address s) private    {
      owner = OwnerTuple(s,true);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r20(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateTotalSupplyOnIncrementAllBurn_r18(int b) private    {
      totalSupply.n -= b;
  }
  function updateTotalSupplyOnInsertConstructor_r3() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateTransferOnInsertRecv_transfer_r45(address s,address r,int n) private   returns (bool) {
      int m_1 = balanceOf[s].n;
      if(n>0 && s!=address(0) && r!=address(0) && n<=m_1) {
        updateTotalInOnInsertTransfer_r5(r,n);
        updateTotalOutOnInsertTransfer_r39(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r41(address o,address r,address s,int n) private   returns (bool) {
      int allowance_x2 = allowance[o][s].n;
      int m_1 = balanceOf[o].n;
      if(n>0 && n<=m_1 && n<=allowance_x2) {
        updateTransferOnInsertTransferFrom_r27(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r19(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
}