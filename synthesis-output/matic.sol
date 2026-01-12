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
    updateOwnerOnInsertConstructor_r16();
    updateIsPauserOnInsertConstructor_r20();
  }
  function mint(address p,int amount) public    {
      bool r2 = updateMintOnInsertRecv_mint_r2(p,amount);
      if(r2==false) {
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
  function getPaused() public view  returns (bool) {
      bool b = paused.b;
      return b;
  }
  function getOwner() public view  returns (address) {
      address p = owner.p;
      return p;
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r21 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r21(p,s,n);
      if(r21==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r13 = updateBurnOnInsertRecv_burn_r13(p,amount);
      if(r13==false) {
        revert("Rule condition failed");
      }
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
  function decreaseAllowance(address p,address s,int n) public    {
      bool r29 = updateDecreaseAllowanceOnInsertRecv_decreaseAllowance_r29(p,s,n);
      if(r29==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function transferFrom(address o,address r,address s,int n) public    {
      bool r7 = updateTransferFromOnInsertRecv_transferFrom_r7(o,r,s,n);
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function updateSpentTotalOnInsertTransferFrom_r22(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r23(o,s,delta0);
  }
  function updateOwnerOnInsertConstructor_r16() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updatePausedOnInsertConstructor_r4() private    {
      paused = PausedTuple(false,true);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r8(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r23(o,s,delta0);
  }
  function updateMintOnInsertRecv_mint_r2(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>=0) {
        updateAllMintOnInsertMint_r0(n);
        updateTotalMintOnInsertMint_r33(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateTotalOutOnInsertTransfer_r10(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r28(p,delta0);
  }
  function updateTotalBurnOnInsertBurn_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r28(p,delta0);
  }
  function updatePausedOnInsertUnpause_r26() private    {
      paused = PausedTuple(false,true);
  }
  function updateTransferOnInsertRecv_transfer_r3(address s,address r,int n) private   returns (bool) {
      bool b = paused.b;
      int balanceOf_x1 = balanceOf[s].n;
      if(b!=true && 0!=balanceOf_x1) {
        updateTotalOutOnInsertTransfer_r10(s,n);
        updateTotalInOnInsertTransfer_r25(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updatePausedOnInsertPause_r6() private    {
      paused = PausedTuple(true,true);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r23(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateTotalInOnInsertTransfer_r25(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r28(p,delta0);
  }
  function updateTransferOnInsertTransferFrom_r27(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r10(o,n);
      updateTotalInOnInsertTransfer_r25(r,n);
      emit Transfer(o,r,n);
  }
  function updateAllowanceOnIncrementSpentTotal_r23(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateBalanceOfOnIncrementTotalIn_r28(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateAllowanceOnIncrementDecreaseAllowanceTotal_r23(address o,address s,int d) private    {
      allowance[o][s].n -= d;
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r19(delta0);
  }
  function updateBalanceOfOnIncrementTotalMint_r28(address p,int n) private    {
      balanceOf[p].n += n;
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
  function updateDecreaseAllowanceOnInsertRecv_decreaseAllowance_r29(address p,address s,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[o].n;
      if(balanceOf_x1>0) {
        updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r30(o,s,n);
        emit DecreaseAllowance(o,s,n);
        return true;
      }
      return false;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateBalanceOfOnIncrementTotalOut_r28(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateIsPauserOnInsertConstructor_r20() private    {
      address s = msg.sender;
      // Empty()
  }
  function updateTotalMintOnInsertMint_r33(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r28(p,delta0);
  }
  function updateTotalSupplyOnIncrementAllBurn_r19(int b) private    {
      totalSupply.n -= b;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r21(address p,address s,int n) private   returns (bool) {
      if(n>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r8(o,s,n);
        emit IncreaseAllowance(o,s,n);
        return true;
      }
      return false;
  }
  function updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r30(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementDecreaseAllowanceTotal_r23(o,s,delta0);
  }
  function updateAllBurnOnInsertBurn_r18(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r19(delta0);
  }
  function updateBalanceOfOnIncrementTotalBurn_r28(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r7(address o,address r,address s,int n) private   returns (bool) {
      bool b = paused.b;
      int balanceOf_x1 = balanceOf[r].n;
      if(b!=true && 0!=balanceOf_x1) {
        updateSpentTotalOnInsertTransferFrom_r22(o,s,n);
        updateTransferOnInsertTransferFrom_r27(o,r,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateBurnOnInsertRecv_burn_r13(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && 0!=balanceOf_x1) {
        updateTotalBurnOnInsertBurn_r1(p,n);
        updateAllBurnOnInsertBurn_r18(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateUnpauseOnInsertRecv_unpause_r12() private   returns (bool) {
      updatePausedOnInsertUnpause_r26();
      emit Unpause();
      return true;
      return false;
  }
}