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
    updatePausedOnInsertConstructor_r4();
    updateIsPauserOnInsertConstructor_r17();
    updateOwnerOnInsertConstructor_r13();
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r18 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r18(p,s,n);
      if(r18==false) {
        revert("Rule condition failed");
      }
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
  function burn(address p,int amount) public    {
      bool r32 = updateBurnOnInsertRecv_burn_r32(p,amount);
      if(r32==false) {
        revert("Rule condition failed");
      }
  }
  function getPaused() public view  returns (bool) {
      bool b = paused.b;
      return b;
  }
  function getOwner() public view  returns (address) {
      address p = owner.p;
      return p;
  }
  function unpause() public    {
      bool r10 = updateUnpauseOnInsertRecv_unpause_r10();
      if(r10==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address o,address r,address s,int n) public    {
      bool r27 = updateTransferFromOnInsertRecv_transferFrom_r27(o,r,s,n);
      if(r27==false) {
        revert("Rule condition failed");
      }
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
  function mint(address p,int amount) public    {
      bool r3 = updateMintOnInsertRecv_mint_r3(p,amount);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address s,address r,int n) public    {
      bool r31 = updateTransferOnInsertRecv_transfer_r31(s,r,n);
      if(r31==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r7(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r20(o,s,delta0);
  }
  function updateDecreaseAllowanceOnInsertRecv_decreaseAllowance_r2(address o,address s,int n) private   returns (bool) {
      int allowance_x2 = allowance[o][s].n;
      if(n<allowance_x2) {
        updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r26(o,s,n);
        emit DecreaseAllowance(o,s,n);
        return true;
      }
      return false;
  }
  function updateSpentTotalOnInsertTransferFrom_r19(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r20(o,s,delta0);
  }
  function updateTotalSupplyOnIncrementAllBurn_r16(int b) private    {
      totalSupply.n -= b;
  }
  function updatePauseOnInsertRecv_pause_r5() private   returns (bool) {
      updatePausedOnInsertPause_r6();
      emit Pause();
      return true;
      return false;
  }
  function updateTotalMintOnInsertMint_r30(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r25(p,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r20(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updatePausedOnInsertPause_r6() private    {
      paused = PausedTuple(true,true);
  }
  function updateTransferOnInsertRecv_transfer_r31(address s,address r,int n) private   returns (bool) {
      bool b = paused.b;
      int balanceOf_x1_1 = balanceOf[s].n;
      if(b!=true && n>=0 && n<balanceOf_x1_1) {
        updateTotalInOnInsertTransfer_r22(r,n);
        updateTotalOutOnInsertTransfer_r33(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalMint_r25(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateAllBurnOnInsertBurn_r15(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r16(delta0);
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r16(delta0);
  }
  function updateAllowanceOnIncrementDecreaseAllowanceTotal_r20(address o,address s,int d) private    {
      allowance[o][s].n -= d;
  }
  function updateTotalSupplyOnIncrementAllMint_r16(int m) private    {
      totalSupply.n += m;
  }
  function updatePausedOnInsertConstructor_r4() private    {
      paused = PausedTuple(false,true);
  }
  function updateBalanceOfOnIncrementTotalOut_r25(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateAllowanceOnIncrementSpentTotal_r20(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTotalBurnOnInsertBurn_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r25(p,delta0);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r18(address o,address s,int n) private   returns (bool) {
      if(n>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r7(o,s,n);
        emit IncreaseAllowance(o,s,n);
        return true;
      }
      return false;
  }
  function updateOwnerOnInsertConstructor_r13() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateUnpauseOnInsertRecv_unpause_r10() private   returns (bool) {
      updatePausedOnInsertUnpause_r23();
      emit Unpause();
      return true;
      return false;
  }
  function updateBalanceOfOnIncrementTotalIn_r25(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTotalOutOnInsertTransfer_r33(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r25(p,delta0);
  }
  function updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r26(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementDecreaseAllowanceTotal_r20(o,s,delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r27(address o,address r,address s,int n) private   returns (bool) {
      bool b = paused.b;
      int balanceOf_x1_2 = balanceOf[o].n;
      int allowance_x2_1 = allowance[o][s].n;
      if(b!=true && n>0 && n<=allowance_x2_1 && n<balanceOf_x1_2) {
        updateTransferOnInsertTransferFrom_r24(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r19(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateBurnOnInsertRecv_burn_r32(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && n<=balanceOf_x1) {
        updateTotalBurnOnInsertBurn_r1(p,n);
        updateAllBurnOnInsertBurn_r15(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateMintOnInsertRecv_mint_r3(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>=0) {
        updateAllMintOnInsertMint_r0(n);
        updateTotalMintOnInsertMint_r30(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateTotalInOnInsertTransfer_r22(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r25(p,delta0);
  }
  function updateTransferOnInsertTransferFrom_r24(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r22(r,n);
      updateTotalOutOnInsertTransfer_r33(o,n);
      emit Transfer(o,r,n);
  }
  function updatePausedOnInsertUnpause_r23() private    {
      paused = PausedTuple(false,true);
  }
  function updateBalanceOfOnIncrementTotalBurn_r25(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateIsPauserOnInsertConstructor_r17() private    {
      address s = msg.sender;
      // Empty()
  }
}