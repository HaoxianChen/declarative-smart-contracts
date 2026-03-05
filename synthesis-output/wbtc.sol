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
    updateTotalSupplyOnInsertConstructor_r13();
    updateOwnerOnInsertConstructor_r34();
  }
  function increaseApproval(address p,address s,int n) public    {
      bool r45 = updateIncreaseApprovalOnInsertRecv_increaseApproval_r45(p,s,n);
      if(r45==false) {
        revert("Rule condition failed");
      }
  }
  function claimOwnership(address p) public    {
      bool r6 = updateClaimOwnershipOnInsertRecv_claimOwnership_r6(p);
      if(r6==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function pause() public    {
      bool r43 = updatePausedOnInsertRecv_pause_r43();
      if(r43==false) {
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
  function transferFrom(address o,address r,address s,int n) public    {
      bool r14 = updateTransferFromOnInsertRecv_transferFrom_r14(o,r,s,n);
      if(r14==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function transfer(address s,address r,int n) public    {
      bool r44 = updateTransferOnInsertRecv_transfer_r44(s,r,n);
      if(r44==false) {
        revert("Rule condition failed");
      }
  }
  function unpause() public    {
      bool r32 = updatePausedOnInsertRecv_unpause_r32();
      if(r32==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r12 = updateMintOnInsertRecv_mint_r12(p,amount);
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function updateTotalOutOnInsertTransfer_r39(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r30(p,delta0);
  }
  function updateMintOnInsertRecv_mint_r12(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>=0) {
        updateAllMintOnInsertMint_r0(n);
        updateTotalMintOnInsertMint_r33(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updatePausedOnInsertRecv_unpause_r32() private   returns (bool) {
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
  function updateTotalBurnOnInsertBurn_r10(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r30(p,delta0);
  }
  function updateAllowanceOnIncrementDecreaseAllowanceTotal_r21(address o,address s,int d) private    {
      allowance[o][s].n -= d;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updatePendingOwnerOnInsertClaimOwnership_r9() private    {
      pendingOwner = PendingOwnerTuple(address(0),true);
  }
  function updateTotalSupplyOnIncrementAllBurn_r18(int b) private    {
      totalSupply.n -= b;
  }
  function updateTotalSupplyOnInsertConstructor_r13() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateIncreaseApprovalOnInsertRecv_increaseApproval_r45(address p,address s,int n) private   returns (bool) {
      if(n>=0) {
        updateAllowanceTotalOnInsertIncreaseApproval_r35(o,s,n);
        emit IncreaseApproval(o,s,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r30(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateBalanceOfOnIncrementTotalBurn_r30(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateTransferOnInsertRecv_transfer_r44(address s,address r,int n) private   returns (bool) {
      int m_1 = balanceOf[s].n;
      if(n>0 && r!=address(0) && s!=address(0) && n<=m_1) {
        updateTotalOutOnInsertTransfer_r39(s,n);
        updateTotalInOnInsertTransfer_r4(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateClaimOwnershipOnInsertRecv_claimOwnership_r6(address p) private   returns (bool) {
      address s_1 = pendingOwner.p;
      if(p!=address(0) && s_1==p) {
        updateOwnerOnInsertClaimOwnership_r41(p);
        updatePendingOwnerOnInsertClaimOwnership_r9();
        emit ClaimOwnership(p);
        return true;
      }
      return false;
  }
  function updatePausedOnInsertRecv_pause_r43() private   returns (bool) {
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
  function updateBalanceOfOnIncrementTotalIn_r30(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTotalMintOnInsertMint_r33(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r30(p,delta0);
  }
  function updateTransferOwnershipOnInsertRecv_transferOwnership_r24(address p) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(p!=address(0) && o_1==s_1) {
        updatePendingOwnerOnInsertTransferOwnership_r40(p);
        emit TransferOwnership(p);
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
  function updateBalanceOfOnIncrementTotalMint_r30(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateDecreaseAllowanceTotalOnInsertDecreaseApproval_r27(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementDecreaseAllowanceTotal_r21(o,s,delta0);
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r18(delta0);
  }
  function updateBurnOnInsertRecv_burn_r38(address p,int amount) private   returns (bool) {
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
  function updateTransferOnInsertTransferFrom_r28(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r39(o,n);
      updateTotalInOnInsertTransfer_r4(r,n);
      emit Transfer(o,r,n);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r14(address o,address r,address s,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[r].n;
      int m_1 = balanceOf[o].n;
      if(n>0 && n<=m_1 && 0!=balanceOf_x1) {
        updateSpentTotalOnInsertTransferFrom_r20(o,s,n);
        updateTransferOnInsertTransferFrom_r28(o,r,n);
        emit TransferFrom(o,r,s,n);
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
  function updateTotalSupplyOnIncrementAllMint_r18(int m) private    {
      totalSupply.n += m;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateOwnerOnInsertClaimOwnership_r41(address s) private    {
      owner = OwnerTuple(s,true);
  }
  function updateTotalInOnInsertTransfer_r4(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r30(p,delta0);
  }
  function updatePendingOwnerOnInsertTransferOwnership_r40(address p) private    {
      pendingOwner = PendingOwnerTuple(p,true);
  }
  function updateSpentTotalOnInsertTransferFrom_r20(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r21(o,s,delta0);
  }
  function updateAllBurnOnInsertBurn_r17(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r18(delta0);
  }
  function updateOwnerOnInsertConstructor_r34() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateAllowanceTotalOnInsertIncreaseApproval_r35(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r21(o,s,delta0);
  }
}