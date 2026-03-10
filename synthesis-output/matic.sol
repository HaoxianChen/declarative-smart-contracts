contract Matic {
  struct BalanceOfTuple {
    int n;
    bool _valid;
  }
  struct PausedTuple {
    bool b;
    bool _valid;
  }
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct AllowanceTuple {
    int n;
    bool _valid;
  }
  struct TotalSupplyTuple {
    int n;
    bool _valid;
  }
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalSupplyTuple totalSupply;
  PausedTuple paused;
  OwnerTuple owner;
  event InvalidTx();
  event Transfer(address s,address r,int n);
  event Mint(address p,int amount);
  event Pause();
  event Burn(address p,int amount);
  event Unpause();
  event IncreaseAllowance(address p,address s,int n);
  event TransferFrom(address o,address r,address s,int n);
  event DecreaseAllowance(address p,address s,int n);
  constructor(int n) public {
    updateOwnerOnInsertConstructor_r12();
    updatePausedOnInsertConstructor_r5();
    updateIsPauserOnInsertConstructor_r18();
  }
  function pause() public    {
      bool r4 = updatePauseOnInsertRecv_pause_r4();
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function getPaused() public view  returns (bool) {
      bool b = paused.b;
      return b;
  }
  function decreaseAllowance(address p,address s,int n) public    {
      bool r27 = updateDecreaseAllowanceOnInsertRecv_decreaseAllowance_r27(p,s,n);
      if(r27==false) {
        revert("Rule condition failed");
      }
  }
  function getOwner() public view  returns (address) {
      address p = owner.p;
      return p;
  }
  function mint(address p,int amount) public    {
      bool r43 = updateMintOnInsertRecv_mint_r43(p,amount);
      if(r43==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address o,address r,address s,int n) public    {
      bool r41 = updateTransferFromOnInsertRecv_transferFrom_r41(o,r,s,n);
      if(r41==false) {
        revert("Rule condition failed");
      }
  }
  function unpause() public    {
      bool r30 = updateUnpauseOnInsertRecv_unpause_r30();
      if(r30==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r19 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r19(p,s,n);
      if(r19==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address s,address r,int n) public    {
      bool r15 = updateTransferOnInsertRecv_transfer_r15(s,r,n);
      if(r15==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r20 = updateBurnOnInsertRecv_burn_r20(p,amount);
      if(r20==false) {
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
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateBurnOnInsertRecv_burn_r20(address p,int amount) private   returns (bool) {
      address s_0 = msg.sender;
      address o_0 = owner.p;
      int m_1 = balanceOf[p].n;
      if(o_0==s_0 && amount<=m_1) {
        updateAllBurnOnInsertBurn_r16(amount);
        updateTotalBurnOnInsertBurn_r1(p,amount);
        emit Burn(p,amount);
        return true;
      }
      return false;
  }
  function updatePauseOnInsertRecv_pause_r4() private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        updatePausedOnInsertPause_r25();
        emit Pause();
        return true;
      }
      return false;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r41(address o,address r,address s,int n) private   returns (bool) {
      bool b_1 = paused.b;
      int allowance_x2 = allowance[o][s].n;
      int m_2 = balanceOf[o].n;
      if(r!=address(0) && b_1!=true && n<=allowance_x2 && o!=address(0) && n<=m_2 && n>0) {
        updateSpentTotalOnInsertTransferFrom_r22(o,s,n);
        updateTransferOnInsertTransferFrom_r32(o,r,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r23(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateBalanceOfOnIncrementTotalIn_r34(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateSpentTotalOnInsertTransferFrom_r22(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r23(o,s,delta0);
  }
  function updatePausedOnInsertUnpause_r29() private    {
      paused = PausedTuple(false,true);
  }
  function updateAllBurnOnInsertBurn_r16(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r17(delta0);
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r17(delta0);
  }
  function updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r35(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementDecreaseAllowanceTotal_r23(o,s,delta0);
  }
  function updateBalanceOfOnIncrementTotalBurn_r34(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateAllowanceOnIncrementSpentTotal_r23(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTotalMintOnInsertMint_r39(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r34(p,delta0);
  }
  function updateMintOnInsertRecv_mint_r43(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && amount>0) {
        updateAllMintOnInsertMint_r0(amount);
        updateTotalMintOnInsertMint_r39(p,amount);
        emit Mint(p,amount);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r17(int m) private    {
      totalSupply.n += m;
  }
  function updateTotalBurnOnInsertBurn_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r34(p,delta0);
  }
  function updateTotalOutOnInsertTransfer_r44(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r34(p,delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r7(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r23(o,s,delta0);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r19(address p,address s,int n) private   returns (bool) {
      if(n>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r7(p,s,n);
        emit IncreaseAllowance(p,s,n);
        return true;
      }
      return false;
  }
  function updatePausedOnInsertPause_r25() private    {
      paused = PausedTuple(true,true);
  }
  function updatePausedOnInsertConstructor_r5() private    {
      paused = PausedTuple(false,true);
  }
  function updateTotalSupplyOnIncrementAllBurn_r17(int b) private    {
      totalSupply.n -= b;
  }
  function updateUnpauseOnInsertRecv_unpause_r30() private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        updatePausedOnInsertUnpause_r29();
        emit Unpause();
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementDecreaseAllowanceTotal_r23(address o,address s,int d) private    {
      allowance[o][s].n -= d;
  }
  function updateTransferOnInsertTransferFrom_r32(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r28(r,n);
      updateTotalOutOnInsertTransfer_r44(o,n);
      emit Transfer(o,r,n);
  }
  function updateOwnerOnInsertConstructor_r12() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTotalInOnInsertTransfer_r28(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r34(p,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateBalanceOfOnIncrementTotalOut_r34(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateDecreaseAllowanceOnInsertRecv_decreaseAllowance_r27(address p,address s,int n) private   returns (bool) {
      if(n<=0) {
        updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r35(p,s,n);
        emit DecreaseAllowance(p,s,n);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertRecv_transfer_r15(address s,address r,int n) private   returns (bool) {
      bool b_1 = paused.b;
      int m_2 = balanceOf[s].n;
      if(r!=address(0) && n<=m_2 && n>0 && s!=address(0) && b_1!=true) {
        updateTotalOutOnInsertTransfer_r44(s,n);
        updateTotalInOnInsertTransfer_r28(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalMint_r34(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateIsPauserOnInsertConstructor_r18() private    {
      address s = msg.sender;
      // Empty()
  }
}