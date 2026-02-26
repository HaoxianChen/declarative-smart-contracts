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
    updatePausedOnInsertConstructor_r34();
    updateOnceUpgradeOnInsertConstructor_r16();
    updateOnceUnpausedOnInsertConstructor_r29();
    updateOwnerOnInsertConstructor_r30();
  }
  function transfer(address from,address to,int a) public    {
      bool r12 = updateTransferOnInsertRecv_transfer_r12(from,to,a);
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function decreaseAllowance(address o,address s,int d) public    {
      bool r24 = updateDecreaseAllowanceOnInsertRecv_decreaseAllowance_r24(o,s,d);
      if(r24==false) {
        revert("Rule condition failed");
      }
  }
  function evacuate(address p) public    {
      bool r26 = updateEvacuateOnInsertRecv_evacuate_r26(p);
      if(r26==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address operator,address from,address to,int a) public    {
      bool r20 = updateTransferFromOnInsertRecv_transferFrom_r20(operator,from,to,a);
      if(r20==false) {
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
  function getBalances(address p) public view  returns (int) {
      int a = balances[p].a;
      return a;
  }
  function mint(address p,int n) public    {
      bool r14 = updateMintOnInsertRecv_mint_r14(p,n);
      if(r14==false) {
        revert("Rule condition failed");
      }
  }
  function unpause() public    {
      bool r11 = updateUnpauseOnInsertRecv_unpause_r11();
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function upgrade(address p) public    {
      bool r7 = updateUpgradeOnInsertRecv_upgrade_r7(p);
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function getDead() public view  returns (bool) {
      bool b = dead.b;
      return b;
  }
  function increaseAllowance(address o,address s,int d) public    {
      bool r5 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r5(o,s,d);
      if(r5==false) {
        revert("Rule condition failed");
      }
  }
  function updateTransferOnInsertRecv_transfer_r12(address from,address to,int a) private   returns (bool) {
      bool b = paused.p;
      int balances_x1 = balances[r].a;
      if(b!=true && balances_x1>0) {
        updateTotalOutOnInsertTransfer_r23(s,n);
        updateTotalInOnInsertTransfer_r13(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateTotalInOnInsertTransfer_r13(address p,int n) private    {
      int delta0 = int(n);
      updateBalancesOnIncrementTotalIn_r19(p,delta0);
  }
  function updateTransferOnInsertMint_r28(address p,int n) private    {
      updateTotalOutOnInsertTransfer_r23(address(0),n);
      updateTotalInOnInsertTransfer_r13(p,n);
      emit Transfer(address(0),p,n);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r5(address o,address s,int d) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r17(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updatePausedOnInsertUnpause_r8() private    {
      paused = PausedTuple(false,true);
  }
  function updateOnceUpgradeOnInsertUpgrade_r27() private    {
      // Empty()
  }
  function updateBurnOnInsertEvacuate_r21(address p) private    {
      int n = balances[p].a;
      updateTransferOnInsertBurn_r18(p,n);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r20(address operator,address from,address to,int a) private   returns (bool) {
      int balances_x1 = balances[f].a;
      if(0!=balances_x1) {
        updateTransferOnInsertTransferFrom_r1(f,r,n);
        updateSpentTotalOnInsertTransferFrom_r31(s,f,n);
        emit TransferFrom(s,f,r,n);
        return true;
      }
      return false;
  }
  function updateSpentTotalOnInsertTransferFrom_r31(address f,address s,int n) private    {
      int delta0 = int(n);
      updateAllowedOnIncrementSpentTotal_r35(f,s,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateBalancesOnIncrementTotalOut_r19(address p,int o) private    {
      balances[p].a -= o;
  }
  function updateAllowedOnIncrementSpentTotal_r35(address f,address s,int l) private    {
      allowed[f][s].a -= l;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r17(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowedOnIncrementAllowanceTotal_r35(o,s,delta0);
  }
  function updateAllowedOnIncrementDecreaseTotal_r35(address f,address s,int d) private    {
      allowed[f][s].a -= d;
  }
  function updateTransferOnInsertBurn_r18(address s,int n) private    {
      updateTotalOutOnInsertTransfer_r23(s,n);
      updateTotalInOnInsertTransfer_r13(address(0),n);
      emit Transfer(s,address(0),n);
  }
  function updateAllowedOnIncrementAllowanceTotal_r35(address f,address s,int n) private    {
      allowed[f][s].a += n;
  }
  function updatePausedOnInsertUpgrade_r22() private    {
      paused = PausedTuple(true,true);
  }
  function updateOnceUnpausedOnInsertConstructor_r29() private    {
      // Empty()
  }
  function updateDecreaseAllowanceOnInsertRecv_decreaseAllowance_r24(address o,address s,int d) private   returns (bool) {
      int allowed_x2 = allowed[o][s].a;
      if(d<=allowed_x2) {
        updateDecreaseTotalOnInsertDecreaseAllowance_r10(o,s,d);
        emit DecreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateDeadOnInsertUpgrade_r32() private    {
      dead = DeadTuple(true,true);
  }
  function updatePausedOnInsertConstructor_r34() private    {
      paused = PausedTuple(true,true);
  }
  function updateTransferOnInsertTransferFrom_r1(address s,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r23(s,n);
      updateTotalInOnInsertTransfer_r13(r,n);
      emit Transfer(s,r,n);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateBalancesOnIncrementTotalIn_r19(address p,int i) private    {
      balances[p].a += i;
  }
  function updateDecreaseTotalOnInsertDecreaseAllowance_r10(address f,address s,int n) private    {
      int delta0 = int(n);
      updateAllowedOnIncrementDecreaseTotal_r35(f,s,delta0);
  }
  function updateOnceUpgradeOnInsertConstructor_r16() private    {
      // Empty()
  }
  function updateUpgradeOnInsertRecv_upgrade_r7(address p) private   returns (bool) {
      updateDeadOnInsertUpgrade_r32();
      updatePausedOnInsertUpgrade_r22();
      updateOnceUpgradeOnInsertUpgrade_r27();
      emit Upgrade(p);
      return true;
      return false;
  }
  function updateOwnerOnInsertConstructor_r30() private    {
      address s = msg.sender;
      // Empty()
  }
  function updateUnpauseOnInsertRecv_unpause_r11() private   returns (bool) {
      updatePausedOnInsertUnpause_r8();
      emit Unpause();
      return true;
      return false;
  }
  function updateMintOnInsertRecv_mint_r14(address p,int n) private   returns (bool) {
      if(n>0) {
        updateTransferOnInsertMint_r28(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateEvacuateOnInsertRecv_evacuate_r26(address p) private   returns (bool) {
      int balances_x1 = balances[p].a;
      if(balances_x1>0) {
        updateBurnOnInsertEvacuate_r21(p);
        emit Evacuate(p);
        return true;
      }
      return false;
  }
  function updateTotalOutOnInsertTransfer_r23(address p,int n) private    {
      int delta0 = int(n);
      updateBalancesOnIncrementTotalOut_r19(p,delta0);
  }
}