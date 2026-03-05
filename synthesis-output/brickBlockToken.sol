contract BrickBlockToken {
  struct AllowedTuple {
    int a;
    bool _valid;
  }
  struct PausedTuple {
    bool p;
    bool _valid;
  }
  struct DeadTuple {
    bool b;
    bool _valid;
  }
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct BalancesTuple {
    int a;
    bool _valid;
  }
  mapping(address=>mapping(address=>AllowedTuple)) allowed;
  PausedTuple paused;
  DeadTuple dead;
  mapping(address=>BalancesTuple) balances;
  OwnerTuple owner;
  event DecreaseAllowance(address o,address s,int d);
  event InvalidTx();
  event Pause();
  event Upgrade(address p);
  event TransferFrom(address operator,address from,address to,int a);
  event Evacuate(address p);
  event Transfer(address from,address to,int a);
  event IncreaseAllowance(address o,address s,int d);
  event Mint(address p,int n);
  event TransferBeforeUnpause();
  event EvacuateAfterUpgrade();
  event Unpause();
  constructor() public {
    updateOnceUnpausedOnInsertConstructor_r33();
    updatePausedOnInsertConstructor_r40();
    updateOwnerOnInsertConstructor_r34();
    updateOnceUpgradeOnInsertConstructor_r17();
  }
  function getBalances(address p) public view  returns (int) {
      int a = balances[p].a;
      return a;
  }
  function mint(address p,int n) public    {
      bool r5 = updateMintOnInsertRecv_mint_r5(p,n);
      if(r5==false) {
        revert("Rule condition failed");
      }
  }
  function upgrade(address p) public    {
      bool r6 = updateUpgradeOnInsertRecv_upgrade_r6(p);
      if(r6==false) {
        revert("Rule condition failed");
      }
  }
  function getPaused() public view  returns (bool) {
      bool p = paused.p;
      return p;
  }
  function transferFrom(address operator,address from,address to,int a) public    {
      bool r10 = updateTransferFromOnInsertRecv_transferFrom_r10(operator,from,to,a);
      if(r10==false) {
        revert("Rule condition failed");
      }
  }
  function getDead() public view  returns (bool) {
      bool b = dead.b;
      return b;
  }
  function pause() public    {
      bool r15 = updatePauseOnInsertRecv_pause_r15();
      if(r15==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int a) public    {
      bool r38 = updateTransferOnInsertRecv_transfer_r38(from,to,a);
      if(r38==false) {
        revert("Rule condition failed");
      }
  }
  function unpause() public    {
      bool r25 = updateUnpauseOnInsertRecv_unpause_r25();
      if(r25==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowed(address p,address p2) public view  returns (int) {
      int a = allowed[p][p2].a;
      return a;
  }
  function evacuate(address p) public    {
      bool r30 = updateEvacuateOnInsertRecv_evacuate_r30(p);
      if(r30==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address o,address s,int d) public    {
      bool r4 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r4(o,s,d);
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function decreaseAllowance(address o,address s,int d) public    {
      bool r13 = updateDecreaseAllowanceOnInsertRecv_decreaseAllowance_r13(o,s,d);
      if(r13==false) {
        revert("Rule condition failed");
      }
  }
  function updateDeadOnInsertUpgrade_r36() private    {
      dead = DeadTuple(true,true);
  }
  function updateMintOnInsertRecv_mint_r5(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        updateTransferOnInsertMint_r32(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updatePausedOnInsertUpgrade_r26() private    {
      paused = PausedTuple(true,true);
  }
  function updateOnceUpgradeOnInsertConstructor_r17() private    {
      // Empty()
  }
  function updateBurnOnInsertEvacuate_r24(address p) private    {
      int n = balances[p].a;
      updateTransferOnInsertBurn_r19(p,n);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r18(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowedOnIncrementAllowanceTotal_r41(o,s,delta0);
  }
  function updatePausedOnInsertUnpause_r7() private    {
      paused = PausedTuple(false,true);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateBalancesOnIncrementTotalIn_r3(address p,int i) private    {
      balances[p].a += i;
  }
  function updateOnceUnpausedOnInsertConstructor_r33() private    {
      // Empty()
  }
  function updateTransferOnInsertBurn_r19(address s,int n) private    {
      updateTotalOutOnInsertTransfer_r28(s,n);
      updateTotalInOnInsertTransfer_r2(address(0),n);
      emit Transfer(s,address(0),n);
  }
  function updateTransferOnInsertTransferFrom_r1(address s,address r,int n) private    {
      updateTotalInOnInsertTransfer_r2(r,n);
      updateTotalOutOnInsertTransfer_r28(s,n);
      emit Transfer(s,r,n);
  }
  function updateAllowedOnIncrementSpentTotal_r41(address f,address s,int l) private    {
      allowed[f][s].a -= l;
  }
  function updateOnceUpgradeOnInsertUpgrade_r31() private    {
      // Empty()
  }
  function updatePausedOnInsertPause_r37() private    {
      bool a = true;
      paused = PausedTuple(a,true);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r10(address operator,address from,address to,int a) private   returns (bool) {
      int m = balances[f].a;
      if(n<=m) {
        updateTransferOnInsertTransferFrom_r1(f,r,n);
        updateSpentTotalOnInsertTransferFrom_r35(s,f,n);
        emit TransferFrom(s,f,r,n);
        return true;
      }
      return false;
  }
  function updateAllowedOnIncrementAllowanceTotal_r41(address f,address s,int n) private    {
      allowed[f][s].a += n;
  }
  function updateEvacuateOnInsertRecv_evacuate_r30(address p) private   returns (bool) {
      int balances_x1 = balances[p].a;
      if(balances_x1>0) {
        updateBurnOnInsertEvacuate_r24(p);
        emit Evacuate(p);
        return true;
      }
      return false;
  }
  function updateDecreaseAllowanceOnInsertRecv_decreaseAllowance_r13(address o,address s,int d) private   returns (bool) {
      int balances_x1 = balances[s].a;
      if(d<=balances_x1) {
        updateDecreaseTotalOnInsertDecreaseAllowance_r9(o,s,d);
        emit DecreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertRecv_transfer_r38(address from,address to,int a) private   returns (bool) {
      bool b_2 = paused.p;
      int m_1 = balances[s].a;
      if(n>0 && n<=m_1 && b_2!=true) {
        updateTotalInOnInsertTransfer_r2(r,n);
        updateTotalOutOnInsertTransfer_r28(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r4(address o,address s,int d) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r18(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateTotalOutOnInsertTransfer_r28(address p,int n) private    {
      int delta0 = int(n);
      updateBalancesOnIncrementTotalOut_r3(p,delta0);
  }
  function updateAllowedOnIncrementDecreaseTotal_r41(address f,address s,int d) private    {
      allowed[f][s].a -= d;
  }
  function updatePausedOnInsertConstructor_r40() private    {
      paused = PausedTuple(true,true);
  }
  function updateDecreaseTotalOnInsertDecreaseAllowance_r9(address f,address s,int n) private    {
      int delta0 = int(n);
      updateAllowedOnIncrementDecreaseTotal_r41(f,s,delta0);
  }
  function updatePauseOnInsertRecv_pause_r15() private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        updatePausedOnInsertPause_r37();
        emit Pause();
        return true;
      }
      return false;
  }
  function updateTransferOnInsertMint_r32(address p,int n) private    {
      updateTotalOutOnInsertTransfer_r28(address(0),n);
      updateTotalInOnInsertTransfer_r2(p,n);
      emit Transfer(address(0),p,n);
  }
  function updateSpentTotalOnInsertTransferFrom_r35(address f,address s,int n) private    {
      int delta0 = int(n);
      updateAllowedOnIncrementSpentTotal_r41(f,s,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateUnpauseOnInsertRecv_unpause_r25() private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        updatePausedOnInsertUnpause_r7();
        emit Unpause();
        return true;
      }
      return false;
  }
  function updateBalancesOnIncrementTotalOut_r3(address p,int o) private    {
      balances[p].a -= o;
  }
  function updateUpgradeOnInsertRecv_upgrade_r6(address p) private   returns (bool) {
      updateDeadOnInsertUpgrade_r36();
      updateOnceUpgradeOnInsertUpgrade_r31();
      updatePausedOnInsertUpgrade_r26();
      emit Upgrade(p);
      return true;
      return false;
  }
  function updateOwnerOnInsertConstructor_r34() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTotalInOnInsertTransfer_r2(address p,int n) private    {
      int delta0 = int(n);
      updateBalancesOnIncrementTotalIn_r3(p,delta0);
  }
}