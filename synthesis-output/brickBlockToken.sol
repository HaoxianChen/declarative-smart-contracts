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
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  mapping(address=>mapping(address=>AllowedTuple)) allowed;
  PausedTuple paused;
  DeadTuple dead;
  mapping(address=>BalancesTuple) balances;
  OwnerTuple owner;
  event EvacuateNotByOwner();
  event Mint(address p,int n);
  event Unpause();
  event Upgrade(address p);
  event TransferFrom(address operator,address from,address to,int a);
  event UnauthorizedUpgrade();
  event Evacuate(address p);
  event UnauthorizedUnpause();
  event DecreaseAllowance(address o,address s,int d);
  event Transfer(address from,address to,int a);
  event IncreaseAllowance(address o,address s,int d);
  event UnauthorizedPause();
  event UnauthorizedMint();
  event Pause();
  constructor() public {
    updatePausedOnInsertConstructor_r32();
    updateOwnerOnInsertConstructor_r28();
  }
  function getBalances(address p) public view  returns (int) {
      int a = balances[p].a;
      return a;
  }
  function transferFrom(address operator,address from,address to,int a) public    {
      bool r18 = updateTransferFromOnInsertRecv_transferFrom_r18(operator,from,to,a);
      if(r18==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowed(address p,address p2) public view  returns (int) {
      int a = allowed[p][p2].a;
      return a;
  }
  function getDead() public view  returns (bool) {
      bool b = dead.b;
      return b;
  }
  function mint(address p,int n) public    {
      bool r12 = updateMintOnInsertRecv_mint_r12(p,n);
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function unpause() public    {
      bool r20 = updateUnpauseOnInsertRecv_unpause_r20();
      if(r20==false) {
        revert("Rule condition failed");
      }
  }
  function evacuate(address p) public    {
      bool r23 = updateEvacuateOnInsertRecv_evacuate_r23(p);
      if(r23==false) {
        revert("Rule condition failed");
      }
  }
  function decreaseAllowance(address o,address s,int d) public    {
      bool r9 = updateDecreaseAllowanceOnInsertRecv_decreaseAllowance_r9(o,s,d);
      if(r9==false) {
        revert("Rule condition failed");
      }
  }
  function getPaused() public view  returns (bool) {
      bool p = paused.p;
      return p;
  }
  function upgrade(address p) public    {
      bool r13 = updateUpgradeOnInsertRecv_upgrade_r13(p);
      if(r13==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address o,address s,int d) public    {
      bool r22 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r22(o,s,d);
      if(r22==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int a) public    {
      bool r11 = updateTransferOnInsertRecv_transfer_r11(from,to,a);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function pause() public    {
      bool r21 = updatePauseOnInsertRecv_pause_r21();
      if(r21==false) {
        revert("Rule condition failed");
      }
  }
  function updateEvacuateOnInsertRecv_evacuate_r23(address p) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(s==o) {
        updateBurnOnInsertEvacuate_r25(p);
        emit Evacuate(p);
        return true;
      }
      return false;
  }
  function updatePauseOnInsertRecv_pause_r21() private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(s==o) {
        updatePausedOnInsertPause_r31();
        emit Pause();
        return true;
      }
      return false;
  }
  function updateAllowedOnIncrementSpentTotal_r33(address f,address s,int l) private    {
      allowed[f][s].a -= l;
  }
  function updateUnpauseOnInsertRecv_unpause_r20() private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(s==o) {
        updatePausedOnInsertUnpause_r6();
        emit Unpause();
        return true;
      }
      return false;
  }
  function updateTotalInOnInsertTransfer_r14(address p,int n) private    {
      int delta0 = int(n);
      updateBalancesOnIncrementTotalIn_r17(p,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateBalancesOnIncrementTotalOut_r17(address p,int o) private    {
      balances[p].a -= o;
  }
  function updateAllowedOnIncrementDecreaseTotal_r33(address f,address s,int d) private    {
      allowed[f][s].a -= d;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r18(address operator,address from,address to,int a) private   returns (bool) {
      int balances_x1 = balances[from].a;
      if(0!=balances_x1) {
        updateSpentTotalOnInsertTransferFrom_r29(operator,from,a);
        updateTransferOnInsertTransferFrom_r0(from,to,a);
        emit TransferFrom(operator,from,to,a);
        return true;
      }
      return false;
  }
  function updateOwnerOnInsertConstructor_r28() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTotalOutOnInsertTransfer_r4(address p,int n) private    {
      int delta0 = int(n);
      updateBalancesOnIncrementTotalOut_r17(p,delta0);
  }
  function updateUpgradeOnInsertRecv_upgrade_r13(address p) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(s==o) {
        updatePausedOnInsertUpgrade_r26();
        updateDeadOnInsertUpgrade_r30();
        emit Upgrade(p);
        return true;
      }
      return false;
  }
  function updateAllowedOnIncrementAllowanceTotal_r33(address f,address s,int n) private    {
      allowed[f][s].a += n;
  }
  function updateBalancesOnIncrementTotalIn_r17(address p,int i) private    {
      balances[p].a += i;
  }
  function updatePausedOnInsertConstructor_r32() private    {
      paused = PausedTuple(true,true);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r15(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowedOnIncrementAllowanceTotal_r33(o,s,delta0);
  }
  function updateTransferOnInsertBurn_r16(address s,int n) private    {
      updateTotalOutOnInsertTransfer_r4(s,n);
      updateTotalInOnInsertTransfer_r14(address(0),n);
      emit Transfer(s,address(0),n);
  }
  function updateTransferOnInsertTransferFrom_r0(address s,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r4(s,n);
      updateTotalInOnInsertTransfer_r14(r,n);
      emit Transfer(s,r,n);
  }
  function updatePausedOnInsertPause_r31() private    {
      bool a = true;
      paused = PausedTuple(a,true);
  }
  function updateDecreaseTotalOnInsertDecreaseAllowance_r8(address f,address s,int n) private    {
      int delta0 = int(n);
      updateAllowedOnIncrementDecreaseTotal_r33(f,s,delta0);
  }
  function updateSpentTotalOnInsertTransferFrom_r29(address f,address s,int n) private    {
      int delta0 = int(n);
      updateAllowedOnIncrementSpentTotal_r33(f,s,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTransferOnInsertRecv_transfer_r11(address from,address to,int a) private   returns (bool) {
      address msgSender = msg.sender;
      int balances_x1 = balances[msgSender].a;
      if(0!=balances_x1) {
        updateTotalInOnInsertTransfer_r14(to,a);
        updateTotalOutOnInsertTransfer_r4(from,a);
        emit Transfer(from,to,a);
        return true;
      }
      return false;
  }
  function updateMintOnInsertRecv_mint_r12(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(s==o && n>0) {
        updateTransferOnInsertMint_r5(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertMint_r5(address p,int n) private    {
      updateTotalInOnInsertTransfer_r14(p,n);
      updateTotalOutOnInsertTransfer_r4(address(0),n);
      emit Transfer(address(0),p,n);
  }
  function updateBurnOnInsertEvacuate_r25(address p) private    {
      int n = balances[p].a;
      updateTransferOnInsertBurn_r16(p,n);
  }
  function updatePausedOnInsertUpgrade_r26() private    {
      paused = PausedTuple(true,true);
  }
  function updatePausedOnInsertUnpause_r6() private    {
      paused = PausedTuple(false,true);
  }
  function updateDecreaseAllowanceOnInsertRecv_decreaseAllowance_r9(address o,address s,int d) private   returns (bool) {
      address msgSender = msg.sender;
      int balances_x1 = balances[msgSender].a;
      if(0!=balances_x1) {
        updateDecreaseTotalOnInsertDecreaseAllowance_r8(o,s,d);
        emit DecreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r22(address o,address s,int d) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r15(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateDeadOnInsertUpgrade_r30() private    {
      dead = DeadTuple(true,true);
  }
}