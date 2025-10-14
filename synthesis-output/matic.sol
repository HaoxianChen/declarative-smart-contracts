contract Matic {
  struct BalanceOfTuple {
    int n;
    bool _valid;
  }
  struct PausedTuple {
    bool b;
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
    updatePausedOnInsertConstructor_r4();
    updateOwnerOnInsertConstructor_r16();
    updateIsPauserOnInsertConstructor_r20();
  }
  function pause() public    {
      bool r5 = updatePauseOnInsertRecv_pause_r5();
      if(r5==false) {
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
  function transferFrom(address o,address r,address s,int n) public    {
      bool r28 = updateTransferFromOnInsertRecv_transferFrom_r28(o,r,s,n);
      if(r28==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r21 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r21(p,s,n);
      if(r21==false) {
        revert("Rule condition failed");
      }
  }
  function decreaseAllowance(address p,address s,int n) public    {
      bool r2 = updateDecreaseAllowanceOnInsertRecv_decreaseAllowance_r2(p,s,n);
      if(r2==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r13 = updateBurnOnInsertRecv_burn_r13(p,amount);
      if(r13==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function unpause() public    {
      bool r12 = updateUnpauseOnInsertRecv_unpause_r12();
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address s,address r,int n) public    {
      bool r3 = updateTransferOnInsertRecv_transfer_r3(s,r,n);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r31 = updateMintOnInsertRecv_mint_r31(p,amount);
      if(r31==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function updatePausedOnInsertUnpause_r25() private    {
      paused = PausedTuple(false,true);
  }
  function updateTotalMintOnInsertMint_r10(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r27(p,delta0);
  }
  function updateUnpauseOnInsertRecv_unpause_r12() private   returns (bool) {
      updatePausedOnInsertUnpause_r25();
      emit Unpause();
      return true;
      return false;
  }
  function updateBalanceOfOnIncrementTotalIn_r27(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateAllMintOnInsertMint_r14(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r19(delta0);
  }
  function updateTransferOnInsertRecv_transfer_r3(address s,address r,int n) private   returns (bool) {
      bool paused_b_2 = paused.b;
      int balanceOf_x1_1 = balanceOf[s].n;
      if(n>0 && n<balanceOf_x1_1 && paused_b_2==false) {
        updateTotalInOnInsertTransfer_r24(r,n);
        updateTotalOutOnInsertTransfer_r29(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateMintOnInsertRecv_mint_r31(address p,int n) private   returns (bool) {
      if(n>=0) {
        updateAllMintOnInsertMint_r14(n);
        updateTotalMintOnInsertMint_r10(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r27(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateTotalBurnOnInsertBurn_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r27(p,delta0);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r21(address o,address s,int n) private   returns (bool) {
      if(n>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r9(o,s,n);
        emit IncreaseAllowance(o,s,n);
        return true;
      }
      return false;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r9(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r23(o,s,delta0);
  }
  function updateSpentTotalOnInsertTransferFrom_r22(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r23(o,s,delta0);
  }
  function updateBalanceOfOnIncrementTotalMint_r27(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updatePausedOnInsertConstructor_r4() private    {
      paused = PausedTuple(false,true);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r28(address o,address r,address s,int n) private   returns (bool) {
      bool paused_b_3 = paused.b;
      int balanceOf_x1_2 = balanceOf[o].n;
      int allowance_x2_1 = allowance[o][s].n;
      if(n>0 && n<=allowance_x2_1 && n<=balanceOf_x1_2 && paused_b_3==false) {
        updateTransferOnInsertTransferFrom_r26(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r22(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertTransferFrom_r26(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r24(r,n);
      updateTotalOutOnInsertTransfer_r29(o,n);
      emit Transfer(o,r,n);
  }
  function updateBurnOnInsertRecv_burn_r13(address p,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(n<=balanceOf_x1) {
        updateTotalBurnOnInsertBurn_r1(p,n);
        updateAllBurnOnInsertBurn_r18(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementSpentTotal_r23(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r8(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementDecreaseAllowanceTotal_r23(o,s,delta0);
  }
  function updateTotalInOnInsertTransfer_r24(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r27(p,delta0);
  }
  function updateAllowanceOnIncrementDecreaseAllowanceTotal_r23(address o,address s,int d) private    {
      allowance[o][s].n -= d;
  }
  function updatePauseOnInsertRecv_pause_r5() private   returns (bool) {
      updatePausedOnInsertPause_r6();
      emit Pause();
      return true;
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r19(int m) private    {
      totalSupply.n += m;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalOutOnInsertTransfer_r29(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r27(p,delta0);
  }
  function updateTotalSupplyOnIncrementAllBurn_r19(int b) private    {
      totalSupply.n -= b;
  }
  function updateIsPauserOnInsertConstructor_r20() private    {
      address s = msg.sender;
      // Empty()
  }
  function updateDecreaseAllowanceOnInsertRecv_decreaseAllowance_r2(address o,address s,int n) private   returns (bool) {
      int allowance_x2 = allowance[o][s].n;
      if(n<allowance_x2) {
        updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r8(o,s,n);
        emit DecreaseAllowance(o,s,n);
        return true;
      }
      return false;
  }
  function updateAllBurnOnInsertBurn_r18(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r19(delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateOwnerOnInsertConstructor_r16() private    {
      address s = msg.sender;
      // Empty()
  }
  function updatePausedOnInsertPause_r6() private    {
      paused = PausedTuple(true,true);
  }
  function updateBalanceOfOnIncrementTotalBurn_r27(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r23(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
}