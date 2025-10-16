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
    updateIsPauserOnInsertConstructor_r19();
    updatePausedOnInsertConstructor_r5();
    updateOwnerOnInsertConstructor_r15();
  }
  function unpause() public    {
      bool r12 = updateUnpauseOnInsertRecv_unpause_r12();
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r32 = updateBurnOnInsertRecv_burn_r32(p,amount);
      if(r32==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address o,address r,address s,int n) public    {
      bool r3 = updateTransferFromOnInsertRecv_transferFrom_r3(o,r,s,n);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function getPaused() public view  returns (bool) {
      bool b = paused.b;
      return b;
  }
  function decreaseAllowance(address p,address s,int n) public    {
      bool r2 = updateDecreaseAllowanceOnInsertRecv_decreaseAllowance_r2(p,s,n);
      if(r2==false) {
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
  function transfer(address s,address r,int n) public    {
      bool r8 = updateTransferOnInsertRecv_transfer_r8(s,r,n);
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function pause() public    {
      bool r6 = updatePauseOnInsertRecv_pause_r6();
      if(r6==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function mint(address p,int amount) public    {
      bool r4 = updateMintOnInsertRecv_mint_r4(p,amount);
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r20 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r20(p,s,n);
      if(r20==false) {
        revert("Rule condition failed");
      }
  }
  function getOwner() public view  returns (address) {
      address p = owner.p;
      return p;
  }
  function updateTotalOutOnInsertTransfer_r33(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r27(p,delta0);
  }
  function updateTransferOnInsertTransferFrom_r26(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r24(r,n);
      updateTotalOutOnInsertTransfer_r33(o,n);
      emit Transfer(o,r,n);
  }
  function updateAllowanceOnIncrementSpentTotal_r22(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r18(delta0);
  }
  function updatePausedOnInsertUnpause_r25() private    {
      paused = PausedTuple(false,true);
  }
  function updateDecreaseAllowanceOnInsertRecv_decreaseAllowance_r2(address o,address s,int n) private   returns (bool) {
      int allowance_x2 = allowance[o][s].n;
      if(n<allowance_x2) {
        updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r28(o,s,n);
        emit DecreaseAllowance(o,s,n);
        return true;
      }
      return false;
  }
  function updateUnpauseOnInsertRecv_unpause_r12() private   returns (bool) {
      updatePausedOnInsertUnpause_r25();
      emit Unpause();
      return true;
      return false;
  }
  function updateAllBurnOnInsertBurn_r17(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r18(delta0);
  }
  function updatePausedOnInsertConstructor_r5() private    {
      paused = PausedTuple(false,true);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTotalSupplyOnIncrementAllBurn_r18(int b) private    {
      totalSupply.n -= b;
  }
  function updateBalanceOfOnIncrementTotalOut_r27(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateBalanceOfOnIncrementTotalBurn_r27(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateSpentTotalOnInsertTransferFrom_r21(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r22(o,s,delta0);
  }
  function updatePauseOnInsertRecv_pause_r6() private   returns (bool) {
      updatePausedOnInsertPause_r7();
      emit Pause();
      return true;
      return false;
  }
  function updatePausedOnInsertPause_r7() private    {
      paused = PausedTuple(true,true);
  }
  function updateAllowanceOnIncrementDecreaseAllowanceTotal_r22(address o,address s,int d) private    {
      allowance[o][s].n -= d;
  }
  function updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r28(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementDecreaseAllowanceTotal_r22(o,s,delta0);
  }
  function updateBalanceOfOnIncrementTotalMint_r27(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateBurnOnInsertRecv_burn_r32(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && n<=balanceOf_x1) {
        updateTotalBurnOnInsertBurn_r1(p,n);
        updateAllBurnOnInsertBurn_r17(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateTotalMintOnInsertMint_r31(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r27(p,delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r9(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r22(o,s,delta0);
  }
  function updateTotalInOnInsertTransfer_r24(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r27(p,delta0);
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
  function updateMintOnInsertRecv_mint_r4(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>=0) {
        updateAllMintOnInsertMint_r0(n);
        updateTotalMintOnInsertMint_r31(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateOwnerOnInsertConstructor_r15() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateBalanceOfOnIncrementTotalIn_r27(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateIsPauserOnInsertConstructor_r19() private    {
      address s = msg.sender;
      // Empty()
  }
  function updateTransferFromOnInsertRecv_transferFrom_r3(address o,address r,address s,int n) private   returns (bool) {
      bool b = paused.b;
      int balanceOf_x1_2 = balanceOf[o].n;
      int allowance_x2_1 = allowance[o][s].n;
      if(b!=true && n>=0 && n<allowance_x2_1 && n<balanceOf_x1_2) {
        updateTransferOnInsertTransferFrom_r26(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r21(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r22(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r20(address o,address s,int n) private   returns (bool) {
      if(n>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r9(o,s,n);
        emit IncreaseAllowance(o,s,n);
        return true;
      }
      return false;
  }
  function updateTotalBurnOnInsertBurn_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r27(p,delta0);
  }
  function updateTransferOnInsertRecv_transfer_r8(address s,address r,int n) private   returns (bool) {
      bool b = paused.b;
      int balanceOf_x1_1 = balanceOf[s].n;
      if(b!=true && n>0 && n<=balanceOf_x1_1) {
        updateTotalInOnInsertTransfer_r24(r,n);
        updateTotalOutOnInsertTransfer_r33(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
}