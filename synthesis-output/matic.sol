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
    updatePausedOnInsertConstructor_r5();
    updateIsPauserOnInsertConstructor_r17();
    updateOwnerOnInsertConstructor_r11();
  }
  function unpause() public    {
      bool r28 = updateUnpauseOnInsertRecv_unpause_r28();
      if(r28==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r18 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r18(p,s,n);
      if(r18==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r19 = updateBurnOnInsertRecv_burn_r19(p,amount);
      if(r19==false) {
        revert("Rule condition failed");
      }
  }
  function pause() public    {
      bool r4 = updatePauseOnInsertRecv_pause_r4();
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function decreaseAllowance(address p,address s,int n) public    {
      bool r33 = updateDecreaseAllowanceOnInsertRecv_decreaseAllowance_r33(p,s,n);
      if(r33==false) {
        revert("Rule condition failed");
      }
  }
  function getOwner() public view  returns (address) {
      address p = owner.p;
      return p;
  }
  function mint(address p,int amount) public    {
      bool r43 = updateMintOnInsertRecv_mint_r43(p,amount);
      if(r43==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function transferFrom(address o,address r,address s,int n) public    {
      bool r36 = updateTransferFromOnInsertRecv_transferFrom_r36(o,r,s,n);
      if(r36==false) {
        revert("Rule condition failed");
      }
  }
  function getPaused() public view  returns (bool) {
      bool b = paused.b;
      return b;
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function transfer(address s,address r,int n) public    {
      bool r14 = updateTransferOnInsertRecv_transfer_r14(s,r,n);
      if(r14==false) {
        revert("Rule condition failed");
      }
  }
  function updateBurnOnInsertRecv_burn_r19(address p,int amount) private   returns (bool) {
      address s_0 = msg.sender;
      address o_0 = owner.p;
      int m_1 = balanceOf[p].n;
      if(o_0==s_0 && n<=m_1) {
        updateTotalBurnOnInsertBurn_r1(p,n);
        updateAllBurnOnInsertBurn_r15(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementSpentTotal_r22(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updatePausedOnInsertUnpause_r27() private    {
      paused = PausedTuple(false,true);
  }
  function updateTransferOnInsertRecv_transfer_r14(address s,address r,int n) private   returns (bool) {
      bool b_1 = paused.b;
      int m_2 = balanceOf[s].n;
      if(r!=address(0) && n<=m_2 && n>0 && s!=address(0) && b_1!=true) {
        updateTotalOutOnInsertTransfer_r44(s,n);
        updateTotalInOnInsertTransfer_r26(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r22(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateBalanceOfOnIncrementTotalIn_r32(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateUnpauseOnInsertRecv_unpause_r28() private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        updatePausedOnInsertUnpause_r27();
        emit Unpause();
        return true;
      }
      return false;
  }
  function updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r34(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementDecreaseAllowanceTotal_r22(o,s,delta0);
  }
  function updateIsPauserOnInsertConstructor_r17() private    {
      address s = msg.sender;
      // Empty()
  }
  function updateSpentTotalOnInsertTransferFrom_r21(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r22(o,s,delta0);
  }
  function updateAllBurnOnInsertBurn_r15(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r16(delta0);
  }
  function updateBalanceOfOnIncrementTotalOut_r32(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateAllowanceOnIncrementDecreaseAllowanceTotal_r22(address o,address s,int d) private    {
      allowance[o][s].n -= d;
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r16(delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r37(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r22(o,s,delta0);
  }
  function updateTransferOnInsertTransferFrom_r30(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r26(r,n);
      updateTotalOutOnInsertTransfer_r44(o,n);
      emit Transfer(o,r,n);
  }
  function updateOwnerOnInsertConstructor_r11() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTotalSupplyOnIncrementAllMint_r16(int m) private    {
      totalSupply.n += m;
  }
  function updateTotalSupplyOnIncrementAllBurn_r16(int b) private    {
      totalSupply.n -= b;
  }
  function updatePauseOnInsertRecv_pause_r4() private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        updatePausedOnInsertPause_r24();
        emit Pause();
        return true;
      }
      return false;
  }
  function updateTotalMintOnInsertMint_r40(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r32(p,delta0);
  }
  function updateMintOnInsertRecv_mint_r43(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>0) {
        updateTotalMintOnInsertMint_r40(p,n);
        updateAllMintOnInsertMint_r0(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalMint_r32(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateTotalBurnOnInsertBurn_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r32(p,delta0);
  }
  function updatePausedOnInsertPause_r24() private    {
      paused = PausedTuple(true,true);
  }
  function updateBalanceOfOnIncrementTotalBurn_r32(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateTotalOutOnInsertTransfer_r44(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r32(p,delta0);
  }
  function updateDecreaseAllowanceOnInsertRecv_decreaseAllowance_r33(address p,address s,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[o].n;
      if(balanceOf_x1>0) {
        updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r34(o,s,n);
        emit DecreaseAllowance(o,s,n);
        return true;
      }
      return false;
  }
  function updatePausedOnInsertConstructor_r5() private    {
      paused = PausedTuple(false,true);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r36(address o,address r,address s,int n) private   returns (bool) {
      bool b_1 = paused.b;
      int balanceOf_x1 = balanceOf[r].n;
      int m_2 = balanceOf[o].n;
      if(r!=address(0) && 0!=balanceOf_x1 && b_1!=true && o!=address(0) && n<=m_2 && n>0) {
        updateSpentTotalOnInsertTransferFrom_r21(o,s,n);
        updateTransferOnInsertTransferFrom_r30(o,r,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateTotalInOnInsertTransfer_r26(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r32(p,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r18(address p,address s,int n) private   returns (bool) {
      if(n>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r37(o,s,n);
        emit IncreaseAllowance(o,s,n);
        return true;
      }
      return false;
  }
}