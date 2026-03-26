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
  event InvalidTx();
  event Pause();
  event Unpause();
  event Upgrade(address p);
  event TransferFrom(address operator,address from,address to,int a);
  event Evacuate(address p);
  event Transfer(address from,address to,int a);
  event IncreaseAllowance(address o,address s,int d);
  event Mint(address p,int n);
  event TransferBeforeUnpause();
  constructor() public {
    updatePausedOnInsertConstructor_r36();
    updateInitialSupplyOnInsertConstructor_r5();
    updateOnceUpgradeOnInsertConstructor_r18();
    updateOwnerOnInsertConstructor_r31();
    updateOnceUnpausedOnInsertConstructor_r30();
  }
  function upgrade(address p) public    {
      bool r10 = updateUpgradeOnInsertRecv_upgrade_r10(p);
      if(r10==false) {
        revert("Rule condition failed");
      }
  }
  function decreaseAllowance(address o,address s,int d) public    {
      bool r26 = updateDecreaseAllowanceOnInsertRecv_decreaseAllowance_r26(o,s,d);
      if(r26==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address operator,address from,address to,int a) public    {
      bool r32 = updateTransferFromOnInsertRecv_transferFrom_r32(operator,from,to,a);
      if(r32==false) {
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
  function transfer(address from,address to,int a) public    {
      bool r35 = updateTransferOnInsertRecv_transfer_r35(from,to,a);
      if(r35==false) {
        revert("Rule condition failed");
      }
  }
  function getDead() public view  returns (bool) {
      bool b = dead.b;
      return b;
  }
  function unpause() public    {
      bool r39 = updateUnpauseOnInsertRecv_unpause_r39();
      if(r39==false) {
        revert("Rule condition failed");
      }
  }
  function getBalances(address p) public view  returns (int) {
      int a = balances[p].a;
      return a;
  }
  function evacuate(address p) public    {
      bool r16 = updateEvacuateOnInsertRecv_evacuate_r16(p);
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address o,address s,int d) public    {
      bool r7 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r7(o,s,d);
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int n) public    {
      bool r9 = updateMintOnInsertRecv_mint_r9(p,n);
      if(r9==false) {
        revert("Rule condition failed");
      }
  }
  function updateSpentTotalOnInsertTransferFrom_r6(address s,address f,int n) private    {
      int delta0 = int(n);
      updateAllowedOnIncrementSpentTotal_r37(f,s,delta0);
  }
  function updateUnpauseOnInsertRecv_unpause_r39() private   returns (bool) {
      updatePausedOnInsertUnpause_r13();
      emit Unpause();
      return true;
      return false;
  }
  function updateAllowedOnIncrementAllowanceTotal_r37(address f,address s,int n) private    {
      allowed[f][s].a += n;
  }
  function updateTransferOnInsertRecv_transfer_r35(address s,address r,int n) private   returns (bool) {
      bool b_2 = paused.p;
      int b_1 = balances[s].a;
      if(n>0 && n<=b_1 && b_2!=true) {
        updateTotalOutOnInsertTransfer_r25(s,n);
        updateTotalInOnInsertTransfer_r1(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updatePausedOnInsertConstructor_r36() private    {
      paused = PausedTuple(true,true);
  }
  function updateTransferOnInsertBurn_r20(address s,int n) private    {
      updateTotalOutOnInsertTransfer_r25(s,n);
      updateTotalInOnInsertTransfer_r1(address(0),n);
      emit Transfer(s,address(0),n);
  }
  function updateTotalInOnInsertTransfer_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalancesOnIncrementTotalIn_r3(p,delta0);
  }
  function updateUpgradeOnInsertRecv_upgrade_r10(address p) private   returns (bool) {
      updatePausedOnInsertUpgrade_r24();
      emit Upgrade(p);
      return true;
      return false;
  }
  function updateBurnOnInsertEvacuate_r23(address p) private    {
      int n = balances[p].a;
      updateTransferOnInsertBurn_r20(p,n);
  }
  function updatePausedOnInsertUnpause_r13() private    {
      paused = PausedTuple(false,true);
  }
  function updateAllowedOnIncrementSpentTotal_r37(address f,address s,int l) private    {
      allowed[f][s].a -= l;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r32(address s,address f,address r,int n) private   returns (bool) {
      int b_2 = balances[f].a;
      int a_1 = allowed[f][s].a;
      if(n>0 && n<=a_1 && n<=b_2) {
        updateSpentTotalOnInsertTransferFrom_r6(s,f,n);
        updateTransferOnInsertTransferFrom_r0(f,r,n);
        emit TransferFrom(s,f,r,n);
        return true;
      }
      return false;
  }
  function updateOwnerOnInsertConstructor_r31() private    {
      address s = msg.sender;
      // Empty()
  }
  function updatePausedOnInsertUpgrade_r24() private    {
      paused = PausedTuple(true,true);
  }
  function updateEvacuateOnInsertRecv_evacuate_r16(address p) private   returns (bool) {
      updateBurnOnInsertEvacuate_r23(p);
      emit Evacuate(p);
      return true;
      return false;
  }
  function updateTransferOnInsertTransferFrom_r0(address s,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r25(s,n);
      updateTotalInOnInsertTransfer_r1(r,n);
      emit Transfer(s,r,n);
  }
  function updateDecreaseTotalOnInsertDecreaseAllowance_r14(address f,address s,int n) private    {
      int delta0 = int(n);
      updateAllowedOnIncrementDecreaseTotal_r37(f,s,delta0);
  }
  function updateOnceUpgradeOnInsertConstructor_r18() private    {
      // Empty()
  }
  function updateInitialSupplyOnInsertConstructor_r5() private    {
      // Empty()
  }
  function updateTransferOnInsertMint_r29(address p,int n) private    {
      updateTotalOutOnInsertTransfer_r25(address(0),n);
      updateTotalInOnInsertTransfer_r1(p,n);
      emit Transfer(address(0),p,n);
  }
  function updateTotalOutOnInsertTransfer_r25(address p,int n) private    {
      int delta0 = int(n);
      updateBalancesOnIncrementTotalOut_r3(p,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateOnceUnpausedOnInsertConstructor_r30() private    {
      // Empty()
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateMintOnInsertRecv_mint_r9(address p,int n) private   returns (bool) {
      if(n>=0) {
        updateTransferOnInsertMint_r29(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateBalancesOnIncrementTotalIn_r3(address p,int i) private    {
      balances[p].a += i;
  }
  function updateBalancesOnIncrementTotalOut_r3(address p,int o) private    {
      balances[p].a -= o;
  }
  function updateDecreaseAllowanceOnInsertRecv_decreaseAllowance_r26(address o,address s,int d) private   returns (bool) {
      int allowed_x2 = allowed[o][s].a;
      if(d<=allowed_x2) {
        updateDecreaseTotalOnInsertDecreaseAllowance_r14(o,s,d);
        emit DecreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r19(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowedOnIncrementAllowanceTotal_r37(o,s,delta0);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r7(address o,address s,int d) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r19(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateAllowedOnIncrementDecreaseTotal_r37(address f,address s,int d) private    {
      allowed[f][s].a -= d;
  }
}