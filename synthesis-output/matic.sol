contract Matic {
  struct BalanceOfTuple {
    int n;
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
  struct PausedTuple {
    bool b;
    bool _valid;
  }
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalSupplyTuple totalSupply;
  PausedTuple paused;
  OwnerTuple owner;
  event Transfer(address s,address r,int n);
  event Mint(address p,int amount);
  event UnauthorizedBurn();
  event Burn(address p,int amount);
  event IncreaseAllowance(address p,address s,int n);
  event DecreaseAllowance(address p,address s,int n);
  event UnauthorizedMint();
  event Pause();
  event UnpauseNotByPauser();
  event Unpause();
  event PauseNotByPauser();
  event TransferFrom(address o,address r,address s,int n);
  constructor(int n) public {
    updateOwnerOnInsertConstructor_r12();
    updateIsPauserOnInsertConstructor_r17();
    updatePausedOnInsertConstructor_r4();
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r18 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r18(p,s,n);
      if(r18==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r32 = updateMintOnInsertRecv_mint_r32(p,amount);
      if(r32==false) {
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
  function unpause() public    {
      bool r21 = updateUnpauseOnInsertRecv_unpause_r21();
      if(r21==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address o,address r,address s,int n) public    {
      bool r27 = updateTransferFromOnInsertRecv_transferFrom_r27(o,r,s,n);
      if(r27==false) {
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
  function burn(address p,int amount) public    {
      bool r3 = updateBurnOnInsertRecv_burn_r3(p,amount);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function pause() public    {
      bool r13 = updatePauseOnInsertRecv_pause_r13();
      if(r13==false) {
        revert("Rule condition failed");
      }
  }
  function decreaseAllowance(address p,address s,int n) public    {
      bool r28 = updateDecreaseAllowanceOnInsertRecv_decreaseAllowance_r28(p,s,n);
      if(r28==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address s,address r,int n) public    {
      bool r9 = updateTransferOnInsertRecv_transfer_r9(s,r,n);
      if(r9==false) {
        revert("Rule condition failed");
      }
  }
  function updateTransferOnInsertRecv_transfer_r9(address s,address r,int n) private   returns (bool) {
      updateTotalInOnInsertTransfer_r22(r,n);
      updateTotalOutOnInsertTransfer_r33(s,n);
      emit Transfer(s,r,n);
      return true;
      return false;
  }
  function updateBurnOnInsertRecv_burn_r3(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(s==o && amount<=balanceOf_x1) {
        updateTotalBurnOnInsertBurn_r10(p,amount);
        updateAllBurnOnInsertBurn_r15(amount);
        emit Burn(p,amount);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r16(int b) private    {
      totalSupply.n -= b;
  }
  function updateTotalMintOnInsertMint_r30(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r25(p,delta0);
  }
  function updateUnpauseOnInsertRecv_unpause_r21() private   returns (bool) {
      address s = msg.sender;
      address p = owner.p;
      if(s==p) {
        updatePausedOnInsertUnpause_r23();
        emit Unpause();
        return true;
      }
      return false;
  }
  function updateIsPauserOnInsertConstructor_r17() private    {
      address s = msg.sender;
      // Empty()
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r6(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementDecreaseAllowanceTotal_r20(o,s,delta0);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r20(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updatePausedOnInsertPause_r5() private    {
      paused = PausedTuple(true,true);
  }
  function updateTotalBurnOnInsertBurn_r10(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r25(p,delta0);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r18(address p,address s,int n) private   returns (bool) {
      if(n>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r29(p,s,n);
        emit IncreaseAllowance(p,s,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalBurn_r25(address p,int m) private    {
      balanceOf[p].n -= m;
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
  function updateAllowanceTotalOnInsertIncreaseAllowance_r29(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r20(o,s,delta0);
  }
  function updateSpentTotalOnInsertTransferFrom_r19(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r20(o,s,delta0);
  }
  function updateTransferOnInsertTransferFrom_r24(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r22(r,n);
      updateTotalOutOnInsertTransfer_r33(o,n);
      emit Transfer(o,r,n);
  }
  function updateBalanceOfOnIncrementTotalOut_r25(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r27(address o,address r,address s,int n) private   returns (bool) {
      int allowance_x2 = allowance[o][s].n;
      if(n<=allowance_x2) {
        updateTransferOnInsertTransferFrom_r24(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r19(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementSpentTotal_r20(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateOwnerOnInsertConstructor_r12() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updatePauseOnInsertRecv_pause_r13() private   returns (bool) {
      address s = msg.sender;
      address p = owner.p;
      if(s==p) {
        updatePausedOnInsertPause_r5();
        emit Pause();
        return true;
      }
      return false;
  }
  function updatePausedOnInsertUnpause_r23() private    {
      paused = PausedTuple(false,true);
  }
  function updateMintOnInsertRecv_mint_r32(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(s==o && amount>0) {
        updateAllMintOnInsertMint_r0(amount);
        updateTotalMintOnInsertMint_r30(p,amount);
        emit Mint(p,amount);
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
  function updateBalanceOfOnIncrementTotalIn_r25(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updatePausedOnInsertConstructor_r4() private    {
      paused = PausedTuple(false,true);
  }
  function updateTotalOutOnInsertTransfer_r33(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r25(p,delta0);
  }
  function updateDecreaseAllowanceOnInsertRecv_decreaseAllowance_r28(address p,address s,int n) private   returns (bool) {
      address msgSender_x_0 = msg.sender;
      address owner_x_0 = owner.p;
      int balanceOf_x1_1 = balanceOf[p].n;
      if(owner_x_0==msgSender_x_0 && n<=balanceOf_x1_1) {
        updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r6(p,s,n);
        emit DecreaseAllowance(p,s,n);
        return true;
      }
      return false;
  }
  function updateTotalInOnInsertTransfer_r22(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r25(p,delta0);
  }
}