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
    updateOnceUpgradeOnInsertConstructor_r17();
    updateOwnerOnInsertConstructor_r34();
    updatePausedOnInsertConstructor_r40();
  }
  function upgrade(address p) public    {
      bool r5 = updateUpgradeOnInsertRecv_upgrade_r5(p);
      if(r5==false) {
        revert("Rule condition failed");
      }
  }
  function getBalances(address p) public view  returns (int) {
      int a = balances[p].a;
      return a;
  }
  function increaseAllowance(address o,address s,int d) public    {
      bool r3 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r3(o,s,d);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address operator,address from,address to,int a) public    {
      bool r9 = updateTransferFromOnInsertRecv_transferFrom_r9(operator,from,to,a);
      if(r9==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int n) public    {
      bool r4 = updateMintOnInsertRecv_mint_r4(p,n);
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function unpause() public    {
      bool r27 = updateUnpauseOnInsertRecv_unpause_r27();
      if(r27==false) {
        revert("Rule condition failed");
      }
  }
  function decreaseAllowance(address o,address s,int d) public    {
      bool r28 = updateDecreaseAllowanceOnInsertRecv_decreaseAllowance_r28(o,s,d);
      if(r28==false) {
        revert("Rule condition failed");
      }
  }
  function getPaused() public view  returns (bool) {
      bool p = paused.p;
      return p;
  }
  function getAllowed(address p,address p2) public view  returns (int) {
      int a = allowed[p][p2].a;
      return a;
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
  function evacuate(address p) public    {
      bool r10 = updateEvacuateOnInsertRecv_evacuate_r10(p);
      if(r10==false) {
        revert("Rule condition failed");
      }
  }
  function updateBalancesOnIncrementTotalIn_r2(address p,int i) private    {
      balances[p].a += i;
  }
  function updateDecreaseAllowanceOnInsertRecv_decreaseAllowance_r28(address o,address s,int d) private   returns (bool) {
      int balances_x1 = balances[s].a;
      if(0!=balances_x1) {
        updateDecreaseTotalOnInsertDecreaseAllowance_r8(o,s,d);
        emit DecreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateOwnerOnInsertConstructor_r34() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updatePausedOnInsertUpgrade_r25() private    {
      paused = PausedTuple(true,true);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r18(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowedOnIncrementAllowanceTotal_r41(o,s,delta0);
  }
  function updateTotalOutOnInsertTransfer_r29(address p,int n) private    {
      int delta0 = int(n);
      updateBalancesOnIncrementTotalOut_r2(p,delta0);
  }
  function updateOnceUpgradeOnInsertConstructor_r17() private    {
      // Empty()
  }
  function updateTransferOnInsertRecv_transfer_r38(address from,address to,int a) private   returns (bool) {
      bool b_2 = paused.p;
      int m_1 = balances[from].a;
      if(a>0 && a<=m_1 && b_2!=true) {
        updateTotalOutOnInsertTransfer_r29(from,a);
        updateTotalInOnInsertTransfer_r1(to,a);
        emit Transfer(from,to,a);
        return true;
      }
      return false;
  }
  function updatePausedOnInsertPause_r37() private    {
      bool a = true;
      paused = PausedTuple(a,true);
  }
  function updateTransferOnInsertMint_r32(address p,int n) private    {
      updateTotalInOnInsertTransfer_r1(p,n);
      updateTotalOutOnInsertTransfer_r29(address(0),n);
      emit Transfer(address(0),p,n);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r9(address operator,address from,address to,int a) private   returns (bool) {
      int m = balances[from].a;
      if(a<=m) {
        updateTransferOnInsertTransferFrom_r11(from,to,a);
        updateSpentTotalOnInsertTransferFrom_r35(operator,from,a);
        emit TransferFrom(operator,from,to,a);
        return true;
      }
      return false;
  }
  function updatePausedOnInsertUnpause_r6() private    {
      paused = PausedTuple(false,true);
  }
  function updateMintOnInsertRecv_mint_r4(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        updateTransferOnInsertMint_r32(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateBurnOnInsertEvacuate_r24(address p) private    {
      int n = balances[p].a;
      updateTransferOnInsertBurn_r19(p,n);
  }
  function updateTransferOnInsertBurn_r19(address s,int n) private    {
      updateTotalOutOnInsertTransfer_r29(s,n);
      updateTotalInOnInsertTransfer_r1(address(0),n);
      emit Transfer(s,address(0),n);
  }
  function updateAllowedOnIncrementAllowanceTotal_r41(address f,address s,int n) private    {
      allowed[f][s].a += n;
  }
  function updateUnpauseOnInsertRecv_unpause_r27() private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        updatePausedOnInsertUnpause_r6();
        emit Unpause();
        return true;
      }
      return false;
  }
  function updateEvacuateOnInsertRecv_evacuate_r10(address p) private   returns (bool) {
      address msgSender = msg.sender;
      int balances_x1 = balances[msgSender].a;
      if(0!=balances_x1) {
        updateBurnOnInsertEvacuate_r24(p);
        emit Evacuate(p);
        return true;
      }
      return false;
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
  function updateTotalInOnInsertTransfer_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalancesOnIncrementTotalIn_r2(p,delta0);
  }
  function updateAllowedOnIncrementDecreaseTotal_r41(address f,address s,int d) private    {
      allowed[f][s].a -= d;
  }
  function updateDecreaseTotalOnInsertDecreaseAllowance_r8(address f,address s,int n) private    {
      int delta0 = int(n);
      updateAllowedOnIncrementDecreaseTotal_r41(f,s,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateBalancesOnIncrementTotalOut_r2(address p,int o) private    {
      balances[p].a -= o;
  }
  function updateUpgradeOnInsertRecv_upgrade_r5(address p) private   returns (bool) {
      updateDeadOnInsertUpgrade_r36();
      updatePausedOnInsertUpgrade_r25();
      updateOnceUpgradeOnInsertUpgrade_r31();
      emit Upgrade(p);
      return true;
      return false;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r3(address o,address s,int d) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r18(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateOnceUpgradeOnInsertUpgrade_r31() private    {
      // Empty()
  }
  function updateSpentTotalOnInsertTransferFrom_r35(address f,address s,int n) private    {
      int delta0 = int(n);
      updateAllowedOnIncrementSpentTotal_r41(f,s,delta0);
  }
  function updateOnceUnpausedOnInsertConstructor_r33() private    {
      // Empty()
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateDeadOnInsertUpgrade_r36() private    {
      dead = DeadTuple(true,true);
  }
  function updatePausedOnInsertConstructor_r40() private    {
      paused = PausedTuple(true,true);
  }
  function updateAllowedOnIncrementSpentTotal_r41(address f,address s,int l) private    {
      allowed[f][s].a -= l;
  }
  function updateTransferOnInsertTransferFrom_r11(address s,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r29(s,n);
      updateTotalInOnInsertTransfer_r1(r,n);
      emit Transfer(s,r,n);
  }
}