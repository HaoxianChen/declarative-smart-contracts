contract Matic {
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct TotalSupplyTuple {
    uint n;
    bool _valid;
  }
  struct IsPauserTuple {
    bool b;
    bool _valid;
  }
  struct TotalBalancesTuple {
    uint m;
    bool _valid;
  }
  struct BalanceOfTuple {
    uint n;
    bool _valid;
  }
  struct AllowanceTuple {
    uint n;
    bool _valid;
  }
  struct PausedTuple {
    bool b;
    bool _valid;
  }
  TotalSupplyTuple totalSupply;
  mapping(address=>IsPauserTuple) isPauser;
  TotalBalancesTuple totalBalances;
  mapping(address=>BalanceOfTuple) balanceOf;
  OwnerTuple owner;
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  PausedTuple paused;
  event TransferFrom(address from,address to,address spender,uint amount);
  event Burn(address p,uint amount);
  event RenouncePauser(address p,bool b);
  event Mint(address p,uint amount);
  event Unpause(bool b);
  event DecreaseAllowance(address p,address s,uint n);
  event AddPauser(address p,bool b);
  event Transfer(address from,address to,uint amount);
  event IncreaseAllowance(address p,address s,uint n);
  event Pause(bool b);
  constructor(uint n) public {
    updateIsPauserOnInsertConstructor_r7();
    updatePausedOnInsertConstructor_r28();
    updateAllMintOnInsertConstructor_r16(n);
    updateTotalBalancesOnInsertConstructor_r36(n);
    updateOwnerOnInsertConstructor_r9();
    updateTotalMintOnInsertConstructor_r23(n);
    updateTotalSupplyOnInsertConstructor_r31(n);
    updateBalanceOfOnInsertConstructor_r6(n);
  }
  function burn(address p,uint amount) public    {
      bool r12 = updateBurnOnInsertRecv_burn_r12(p,amount);
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address to,uint amount) public    {
      bool r15 = updateTransferOnInsertRecv_transfer_r15(to,amount);
      if(r15==false) {
        revert("Rule condition failed");
      }
  }
  function approve(address s,uint n) public    {
      bool r35 = updateIncreaseAllowanceOnInsertRecv_approve_r35(s,n);
      if(r35==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (uint) {
      uint n = totalSupply.n;
      return n;
  }
  function pause() public    {
      bool r20 = updatePauseOnInsertRecv_pause_r20();
      if(r20==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (uint) {
      uint n = allowance[p][s].n;
      return n;
  }
  function getBalanceOf(address p) public view  returns (uint) {
      uint n = balanceOf[p].n;
      return n;
  }
  function transferFrom(address from,address to,uint amount) public    {
      bool r2 = updateTransferFromOnInsertRecv_transferFrom_r2(from,to,amount);
      if(r2==false) {
        revert("Rule condition failed");
      }
  }
  function addPauser(address p) public    {
      bool r25 = updateAddPauserOnInsertRecv_addPauser_r25(p);
      if(r25==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,uint n) public    {
      bool r17 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r17(p,n);
      if(r17==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,uint amount) public    {
      bool r4 = updateMintOnInsertRecv_mint_r4(p,amount);
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function unpause() public    {
      bool r5 = updateUnpauseOnInsertRecv_unpause_r5();
      if(r5==false) {
        revert("Rule condition failed");
      }
  }
  function renouncePauser() public    {
      bool r3 = updateRenouncePauserOnInsertRecv_renouncePauser_r3();
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function decreaseAllowance(address p,uint n) public    {
      bool r14 = updateDecreaseAllowanceOnInsertRecv_decreaseAllowance_r14(p,n);
      if(r14==false) {
        revert("Rule condition failed");
      }
  }
  function updateRenouncePauserOnInsertRecv_renouncePauser_r3() private   returns (bool) {
      address s = msg.sender;
      if(true==isPauser[s].b) {
        updateIsPauserOnInsertRenouncePauser_r34(s,bool(false));
        emit RenouncePauser(s,false);
        return true;
      }
      return false;
  }
  function updateMintOnInsertRecv_mint_r4(address p,uint n) private   returns (bool) {
      if(false==paused.b) {
        address s = owner.p;
        if(s==msg.sender) {
          if(p!=address(0)) {
            updateTotalMintOnInsertMint_r19(p,n);
            updateAllMintOnInsertMint_r13(n);
            emit Mint(p,n);
            return true;
          }
        }
      }
      return false;
  }
  function updateTransferOnInsertRecv_transfer_r15(address r,uint n) private   returns (bool) {
      if(false==paused.b) {
        address s = msg.sender;
        uint m = balanceOf[s].n;
        if(n<=m) {
          updateTotalInOnInsertTransfer_r11(r,n);
          updateTotalOutOnInsertTransfer_r26(s,n);
          emit Transfer(s,r,n);
          return true;
        }
      }
      return false;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateOwnerOnInsertConstructor_r9() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateAddPauserOnInsertRecv_addPauser_r25(address p) private   returns (bool) {
      address s = msg.sender;
      if(true==isPauser[s].b) {
        updateIsPauserOnInsertAddPauser_r24(p,bool(true));
        emit AddPauser(p,true);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertTransferFrom_r0(address o,address r,uint n) private    {
      updateTotalOutOnInsertTransfer_r26(o,n);
      updateTotalInOnInsertTransfer_r11(r,n);
      emit Transfer(o,r,n);
  }
  function updateBalanceOfOnIncrementTotalMint_r29(address p,int n) private    {
      int _delta = int(n);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateAllMintOnInsertConstructor_r16(uint n) private    {
      // Empty()
  }
  function updatePausedOnInsertConstructor_r28() private    {
      paused = PausedTuple(false,true);
  }
  function updateBurnOnInsertRecv_burn_r12(address p,uint n) private   returns (bool) {
      address s = msg.sender;
      if(false==paused.b) {
        if(s==owner.p) {
          uint m = balanceOf[p].n;
          if(p!=address(0) && n<=m) {
            updateAllBurnOnInsertBurn_r33(n);
            updateTotalBurnOnInsertBurn_r18(p,n);
            emit Burn(p,n);
            return true;
          }
        }
      }
      return false;
  }
  function updateUnpauseOnInsertRecv_unpause_r5() private   returns (bool) {
      if(true==paused.b) {
        address s = msg.sender;
        if(true==isPauser[s].b) {
          updatePausedOnInsertUnpause_r10(bool(false));
          emit Unpause(false);
          return true;
        }
      }
      return false;
  }
  function updatePausedOnInsertUnpause_r10(bool b) private    {
      paused = PausedTuple(b,true);
  }
  function updateAllMintOnInsertMint_r13(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r21(delta0);
  }
  function updateIsPauserOnInsertConstructor_r7() private    {
      address s = msg.sender;
      isPauser[s] = IsPauserTuple(true,true);
  }
  function updateIncreaseAllowanceOnInsertRecv_approve_r35(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      uint m = allowance[o][s].n;
      uint d = n-m;
      updateAllowanceTotalOnInsertIncreaseAllowance_r37(o,s,d);
      emit IncreaseAllowance(o,s,d);
      return true;
      return false;
  }
  function updateBalanceOfOnIncrementTotalBurn_r29(address p,int m) private    {
      int _delta = int(-m);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateBalanceOfOnInsertConstructor_r6(uint n) private    {
      address s = msg.sender;
      balanceOf[s] = BalanceOfTuple(n,true);
  }
  function updateTotalSupplyOnIncrementAllMint_r21(int m) private    {
      int _delta = int(m);
      uint newValue = updateuintByint(totalSupply.n,_delta);
      totalSupply.n = newValue;
  }
  function updatePausedOnInsertPause_r1(bool b) private    {
      paused = PausedTuple(b,true);
  }
  function updateSpentTotalOnInsertTransferFrom_r30(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r32(o,s,delta0);
  }
  function updateAllowanceOnIncrementDecreaseAllowanceTotal_r32(address o,address s,int d) private    {
      int _delta = int(-d);
      uint newValue = updateuintByint(allowance[o][s].n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r32(address o,address s,int m) private    {
      int _delta = int(m);
      uint newValue = updateuintByint(allowance[o][s].n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateIsPauserOnInsertAddPauser_r24(address p,bool b) private    {
      isPauser[p] = IsPauserTuple(b,true);
  }
  function updateBalanceOfOnIncrementTotalOut_r29(address p,int o) private    {
      int _delta = int(-o);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateTotalSupplyOnInsertConstructor_r31(uint n) private    {
      totalSupply = TotalSupplyTuple(n,true);
  }
  function updateBalanceOfOnIncrementTotalIn_r29(address p,int i) private    {
      int _delta = int(i);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateIsPauserOnInsertRenouncePauser_r34(address p,bool b) private    {
      isPauser[p] = IsPauserTuple(b,true);
  }
  function updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r8(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementDecreaseAllowanceTotal_r32(o,s,delta0);
  }
  function updateTotalInOnInsertTransfer_r11(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r29(p,delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r37(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r32(o,s,delta0);
  }
  function updateTotalMintOnInsertMint_r19(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r29(p,delta0);
  }
  function updateAllowanceOnIncrementSpentTotal_r32(address o,address s,int l) private    {
      int _delta = int(-l);
      uint newValue = updateuintByint(allowance[o][s].n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateAllBurnOnInsertBurn_r33(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r21(delta0);
  }
  function updateTotalSupplyOnIncrementAllBurn_r21(int b) private    {
      int _delta = int(-b);
      uint newValue = updateuintByint(totalSupply.n,_delta);
      totalSupply.n = newValue;
  }
  function updateDecreaseAllowanceOnInsertRecv_decreaseAllowance_r14(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      uint m = allowance[o][s].n;
      if(m>=n) {
        updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r8(o,s,n);
        emit DecreaseAllowance(o,s,n);
        return true;
      }
      return false;
  }
  function updateTotalBalancesOnInsertConstructor_r36(uint n) private    {
      totalBalances = TotalBalancesTuple(n,true);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r17(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      updateAllowanceTotalOnInsertIncreaseAllowance_r37(o,s,n);
      emit IncreaseAllowance(o,s,n);
      return true;
      return false;
  }
  function updateTotalOutOnInsertTransfer_r26(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r29(p,delta0);
  }
  function updateTotalMintOnInsertConstructor_r23(uint n) private    {
      address s = msg.sender;
      // Empty()
  }
  function updateTransferFromOnInsertRecv_transferFrom_r2(address o,address r,uint n) private   returns (bool) {
      address s = msg.sender;
      if(false==paused.b) {
        uint m = balanceOf[o].n;
        uint k = allowance[o][s].n;
        if(m>=n && k>=n) {
          updateSpentTotalOnInsertTransferFrom_r30(o,s,n);
          updateTransferOnInsertTransferFrom_r0(o,r,n);
          emit TransferFrom(o,r,s,n);
          return true;
        }
      }
      return false;
  }
  function updateTotalBurnOnInsertBurn_r18(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r29(p,delta0);
  }
  function updatePauseOnInsertRecv_pause_r20() private   returns (bool) {
      if(false==paused.b) {
        address s = msg.sender;
        if(true==isPauser[s].b) {
          updatePausedOnInsertPause_r1(bool(true));
          emit Pause(true);
          return true;
        }
      }
      return false;
  }
}