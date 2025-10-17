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
  struct BalancesTuple {
    int a;
    bool _valid;
  }
  mapping(address=>mapping(address=>AllowedTuple)) allowed;
  PausedTuple paused;
  DeadTuple dead;
  mapping(address=>BalancesTuple) balances;
  event DecreaseAllowance(address o,address s,int d);
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
    updateOnceUpgradeOnInsertConstructor_r19();
    updatePausedOnInsertConstructor_r34();
    updateOnceUnpausedOnInsertConstructor_r29();
    updateOwnerOnInsertConstructor_r30();
  }
  function transferFrom(address operator,address from,address to,int a) public    {
      bool r17 = updateTransferFromOnInsertRecv_transferFrom_r17(operator,from,to,a);
      if(r17==false) {
        revert("Rule condition failed");
      }
  }
  function getBalances(address p) public view  returns (int) {
      int a = balances[p].a;
      return a;
  }
  function increaseAllowance(address o,address s,int d) public    {
      bool r8 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r8(o,s,d);
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int n) public    {
      bool r10 = updateMintOnInsertRecv_mint_r10(p,n);
      if(r10==false) {
        revert("Rule condition failed");
      }
  }
  function decreaseAllowance(address o,address s,int d) public    {
      bool r25 = updateDecreaseAllowanceOnInsertRecv_decreaseAllowance_r25(o,s,d);
      if(r25==false) {
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
  function evacuate(address p) public    {
      bool r7 = updateEvacuateOnInsertRecv_evacuate_r7(p);
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int a) public    {
      bool r11 = updateTransferOnInsertRecv_transfer_r11(from,to,a);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function upgrade(address p) public    {
      bool r12 = updateUpgradeOnInsertRecv_upgrade_r12(p);
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function unpause() public    {
      bool r16 = updateUnpauseOnInsertRecv_unpause_r16();
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function updateUnpauseOnInsertRecv_unpause_r16() private   returns (bool) {
      updatePausedOnInsertUnpause_r13();
      emit Unpause();
      return true;
      return false;
  }
  function updateDecreaseAllowanceOnInsertRecv_decreaseAllowance_r25(address o,address s,int d) private   returns (bool) {
      int allowed_x2 = allowed[o][s].a;
      if(d<=allowed_x2) {
        updateDecreaseTotalOnInsertDecreaseAllowance_r15(o,s,d);
        emit DecreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateBalancesOnIncrementTotalOut_r5(address p,int o) private    {
      balances[p].a -= o;
  }
  function updateTotalOutOnInsertTransfer_r24(address p,int n) private    {
      int delta0 = int(n);
      updateBalancesOnIncrementTotalOut_r5(p,delta0);
  }
  function updateAllowedOnIncrementAllowanceTotal_r35(address f,address s,int n) private    {
      allowed[f][s].a += n;
  }
  function updateOnceUnpausedOnInsertConstructor_r29() private    {
      // Empty()
  }
  function updatePausedOnInsertUpgrade_r23() private    {
      paused = PausedTuple(true,true);
  }
  function updateOnceUpgradeOnInsertUpgrade_r27() private    {
      // Empty()
  }
  function updateOnceUpgradeOnInsertConstructor_r19() private    {
      // Empty()
  }
  function updateDecreaseTotalOnInsertDecreaseAllowance_r15(address f,address s,int n) private    {
      int delta0 = int(n);
      updateAllowedOnIncrementDecreaseTotal_r35(f,s,delta0);
  }
  function updateSpentTotalOnInsertTransferFrom_r31(address f,address s,int n) private    {
      int delta0 = int(n);
      updateAllowedOnIncrementSpentTotal_r35(f,s,delta0);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r8(address o,address s,int d) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r20(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateAllowedOnIncrementSpentTotal_r35(address f,address s,int l) private    {
      allowed[f][s].a -= l;
  }
  function updateTransferOnInsertTransferFrom_r1(address s,address r,int n) private    {
      updateTotalInOnInsertTransfer_r2(r,n);
      updateTotalOutOnInsertTransfer_r24(s,n);
      emit Transfer(s,r,n);
  }
  function updateTransferOnInsertRecv_transfer_r11(address s,address r,int n) private   returns (bool) {
      bool b = paused.p;
      int balances_x1_1 = balances[s].a;
      if(b!=true && n>0 && n<balances_x1_1) {
        updateTotalInOnInsertTransfer_r2(r,n);
        updateTotalOutOnInsertTransfer_r24(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertBurn_r21(address s,int n) private    {
      updateTotalOutOnInsertTransfer_r24(s,n);
      updateTotalInOnInsertTransfer_r2(address(0),n);
      emit Transfer(s,address(0),n);
  }
  function updateEvacuateOnInsertRecv_evacuate_r7(address p) private   returns (bool) {
      bool dead_b = dead.b;
      if(dead_b==true) {
        updateBurnOnInsertEvacuate_r22(p);
        emit Evacuate(p);
        return true;
      }
      return false;
  }
  function updateTotalInOnInsertTransfer_r2(address p,int n) private    {
      int delta0 = int(n);
      updateBalancesOnIncrementTotalIn_r5(p,delta0);
  }
  function updateMintOnInsertRecv_mint_r10(address p,int n) private   returns (bool) {
      if(n>=0) {
        updateTransferOnInsertMint_r28(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateBurnOnInsertEvacuate_r22(address p) private    {
      int n = balances[p].a;
      updateTransferOnInsertBurn_r21(p,n);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r20(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowedOnIncrementAllowanceTotal_r35(o,s,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateBalancesOnIncrementTotalIn_r5(address p,int i) private    {
      balances[p].a += i;
  }
  function updateDeadOnInsertUpgrade_r32() private    {
      dead = DeadTuple(true,true);
  }
  function updatePausedOnInsertConstructor_r34() private    {
      paused = PausedTuple(true,true);
  }
  function updateTransferOnInsertMint_r28(address p,int n) private    {
      updateTotalOutOnInsertTransfer_r24(address(0),n);
      updateTotalInOnInsertTransfer_r2(p,n);
      emit Transfer(address(0),p,n);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r17(address s,address f,address r,int n) private   returns (bool) {
      int allowed_x2_2 = allowed[s][f].a;
      int balances_x1_1 = balances[f].a;
      if(n>=0 && n<=balances_x1_1 && n<=allowed_x2_2) {
        updateTransferOnInsertTransferFrom_r1(f,r,n);
        updateSpentTotalOnInsertTransferFrom_r31(s,f,n);
        emit TransferFrom(s,f,r,n);
        return true;
      }
      return false;
  }
  function updateOwnerOnInsertConstructor_r30() private    {
      address s = msg.sender;
      // Empty()
  }
  function updatePausedOnInsertUnpause_r13() private    {
      paused = PausedTuple(false,true);
  }
  function updateAllowedOnIncrementDecreaseTotal_r35(address f,address s,int d) private    {
      allowed[f][s].a -= d;
  }
  function updateUpgradeOnInsertRecv_upgrade_r12(address p) private   returns (bool) {
      updateDeadOnInsertUpgrade_r32();
      updatePausedOnInsertUpgrade_r23();
      updateOnceUpgradeOnInsertUpgrade_r27();
      emit Upgrade(p);
      return true;
      return false;
  }
}