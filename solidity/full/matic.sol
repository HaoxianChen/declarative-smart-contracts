contract Matic {
  struct TotalInTuple {
    uint n;
    bool _valid;
  }
  struct TotalOutTuple {
    uint n;
    bool _valid;
  }
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct TotalMintTuple {
    uint n;
    bool _valid;
  }
  struct TotalSupplyTuple {
    uint n;
    bool _valid;
  }
  struct AllMintTuple {
    uint n;
    bool _valid;
  }
  struct IsPauserTuple {
    bool b;
    bool _valid;
  }
  struct AllowanceTotalTuple {
    uint m;
    bool _valid;
  }
  struct SpentTotalTuple {
    uint m;
    bool _valid;
  }
  struct TotalBurnTuple {
    uint n;
    bool _valid;
  }
  struct BalanceOfTuple {
    uint n;
    bool _valid;
  }
  struct AllBurnTuple {
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
  mapping(address=>TotalInTuple) totalIn;
  mapping(address=>TotalOutTuple) totalOut;
  mapping(address=>TotalBurnTuple) totalBurn;
  OwnerTuple owner;
  mapping(address=>TotalMintTuple) totalMint;
  TotalSupplyTuple totalSupply;
  AllMintTuple allMint;
  mapping(address=>IsPauserTuple) isPauser;
  mapping(address=>mapping(address=>AllowanceTotalTuple)) allowanceTotal;
  mapping(address=>mapping(address=>SpentTotalTuple)) spentTotal;
  mapping(address=>BalanceOfTuple) balanceOf;
  AllBurnTuple allBurn;
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  PausedTuple paused;
  event Burn(address p,uint amount);
  event Mint(address p,uint amount);
  event DecreaseAllowance(address p,address s,uint n);
  event IncreaseAllowance(address p,address s,uint n);
  event IsPauser(address p,bool b);
  event Transfer(address from,address to,uint amount);
  event Paused(bool b);
  event TransferFrom(address from,address to,address spender,uint amount);
  constructor(uint n) public {
    updateIsPauserOnInsertConstructor_r22();
    updatePausedOnInsertConstructor_r25();
    updateBalanceOfOnInsertConstructor_r18(n);
    updateTotalBalancesOnInsertConstructor_r32(n);
    updateTotalMintOnInsertConstructor_r20(n);
    updateAllMintOnInsertConstructor_r12(n);
    updateOwnerOnInsertConstructor_r6();
    updateTotalSupplyOnInsertConstructor_r28(n);
  }
  function addPauser(address p) public    {
      bool r5 = updateIsPauserOnInsertRecv_addPauser_r5(p);
      if(r5==false) {
        revert("Rule condition failed");
      }
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
  function transfer(address to,uint amount) public    {
      bool r11 = updateTransferOnInsertRecv_transfer_r11(to,amount);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function approve(address s,uint n) public    {
      bool r31 = updateIncreaseAllowanceOnInsertRecv_approve_r31(s,n);
      if(r31==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,uint amount) public    {
      bool r3 = updateMintOnInsertRecv_mint_r3(p,amount);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function unpause() public    {
      bool r13 = updatePausedOnInsertRecv_unpause_r13();
      if(r13==false) {
        revert("Rule condition failed");
      }
  }
  function increaseApproval(address p,uint n) public    {
      bool r10 = updateIncreaseAllowanceOnInsertRecv_increaseApproval_r10(p,n);
      if(r10==false) {
        revert("Rule condition failed");
      }
  }
  function decreaseApproval(address p,uint n) public    {
      bool r17 = updateDecreaseAllowanceOnInsertRecv_decreaseApproval_r17(p,n);
      if(r17==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (uint) {
      uint n = totalSupply.n;
      return n;
  }
  function getAllowance(address p,address s) public view  returns (uint) {
      uint n = allowance[p][s].n;
      return n;
  }
  function renouncePauser() public    {
      bool r0 = updateIsPauserOnInsertRecv_renouncePauser_r0();
      if(r0==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,uint amount) public    {
      bool r8 = updateBurnOnInsertRecv_burn_r8(p,amount);
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function pause() public    {
      bool r21 = updatePausedOnInsertRecv_pause_r21();
      if(r21==false) {
        revert("Rule condition failed");
      }
  }
  function updateBalanceOfOnIncrementTotalBurn_r26(address p,int m) private    {
      int _delta = int(-m);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updatePausedOnInsertConstructor_r25() private    {
      paused = PausedTuple(false,true);
      emit Paused(false);
  }
  function updateBalanceOfOnIncrementTotalIn_r26(address p,int i) private    {
      int _delta = int(i);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateTotalSupplyOnIncrementAllMint_r16(int m) private    {
      int _delta = int(m);
      uint newValue = updateuintByint(totalSupply.n,_delta);
      totalSupply.n = newValue;
  }
  function updateTotalBalancesOnInsertConstructor_r32(uint n) private    {
      // Empty()
  }
  function updateTotalMintOnInsertConstructor_r20(uint n) private    {
      address s = msg.sender;
      totalMint[s] = TotalMintTuple(n,true);
  }
  function updateBurnOnInsertRecv_burn_r8(address p,uint n) private   returns (bool) {
      address s = msg.sender;
      if(false==paused.b) {
        if(s==owner.p) {
          uint m = balanceOf[p].n;
          if(p!=address(0) && n<=m) {
            updateTotalBurnOnInsertBurn_r14(p,n);
            updateAllBurnOnInsertBurn_r30(n);
            emit Burn(p,n);
            return true;
          }
        }
      }
      return false;
  }
  function updateAllBurnOnInsertBurn_r30(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r16(delta0);
      allBurn.n += n;
  }
  function updateTotalOutOnInsertTransfer_r23(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r26(p,delta0);
      totalOut[p].n += n;
  }
  function updateIsPauserOnInsertRecv_renouncePauser_r0() private   returns (bool) {
      address s = msg.sender;
      if(true==isPauser[s].b) {
        isPauser[s] = IsPauserTuple(false,true);
        emit IsPauser(s,false);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r28(uint n) private    {
      totalSupply = TotalSupplyTuple(n,true);
  }
  function updateIsPauserOnInsertRecv_addPauser_r5(address p) private   returns (bool) {
      address s = msg.sender;
      if(true==isPauser[s].b) {
        isPauser[p] = IsPauserTuple(true,true);
        emit IsPauser(p,true);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r16(int b) private    {
      int _delta = int(-b);
      uint newValue = updateuintByint(totalSupply.n,_delta);
      totalSupply.n = newValue;
  }
  function updateTransferOnInsertTransferFrom_r1(address o,address r,uint n) private    {
      updateTotalInOnInsertTransfer_r7(r,n);
      updateTotalOutOnInsertTransfer_r23(o,n);
      emit Transfer(o,r,n);
  }
  function updatePausedOnInsertRecv_unpause_r13() private   returns (bool) {
      if(true==paused.b) {
        address s = msg.sender;
        if(true==isPauser[s].b) {
          paused = PausedTuple(false,true);
          emit Paused(false);
          return true;
        }
      }
      return false;
  }
  function updateMintOnInsertRecv_mint_r3(address p,uint n) private   returns (bool) {
      if(false==paused.b) {
        address s = owner.p;
        if(s==msg.sender) {
          if(p!=address(0)) {
            updateTotalMintOnInsertMint_r15(p,n);
            updateAllMintOnInsertMint_r9(n);
            emit Mint(p,n);
            return true;
          }
        }
      }
      return false;
  }
  function updateBalanceOfOnInsertConstructor_r18(uint n) private    {
      address s = msg.sender;
      balanceOf[s] = BalanceOfTuple(n,true);
  }
  function updateOwnerOnInsertConstructor_r6() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateBalanceOfOnIncrementTotalOut_r26(address p,int o) private    {
      int _delta = int(-o);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateTransferOnInsertRecv_transfer_r11(address r,uint n) private   returns (bool) {
      if(false==paused.b) {
        address s = msg.sender;
        uint m = balanceOf[s].n;
        if(n<=m) {
          updateTotalOutOnInsertTransfer_r23(s,n);
          updateTotalInOnInsertTransfer_r7(r,n);
          emit Transfer(s,r,n);
          return true;
        }
      }
      return false;
  }
  function updateDecreaseAllowanceOnInsertRecv_decreaseApproval_r17(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      uint m = allowance[o][s].n;
      if(m>=n) {
        updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r4(o,s,n);
        emit DecreaseAllowance(o,s,n);
        return true;
      }
      return false;
  }
  function updateTotalInOnInsertTransfer_r7(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r26(p,delta0);
      totalIn[p].n += n;
  }
  function updateIncreaseAllowanceOnInsertRecv_approve_r31(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      uint m = allowance[o][s].n;
      uint d = n-m;
      updateAllowanceTotalOnInsertIncreaseAllowance_r33(o,s,d);
      emit IncreaseAllowance(o,s,d);
      return true;
      return false;
  }
  function updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r4(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementDecreaseAllowanceTotal_r29(o,s,delta0);
  }
  function updateAllMintOnInsertConstructor_r12(uint n) private    {
      allMint = AllMintTuple(n,true);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r29(address o,address s,int m) private    {
      int _delta = int(m);
      uint newValue = updateuintByint(allowance[o][s].n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r33(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r29(o,s,delta0);
      allowanceTotal[o][s].m += n;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateSpentTotalOnInsertTransferFrom_r27(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r29(o,s,delta0);
      spentTotal[o][s].m += n;
  }
  function updateTotalBurnOnInsertBurn_r14(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r26(p,delta0);
      totalBurn[p].n += n;
  }
  function updateBalanceOfOnIncrementTotalMint_r26(address p,int n) private    {
      int _delta = int(n);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateAllowanceOnIncrementDecreaseAllowanceTotal_r29(address o,address s,int d) private    {
      int _delta = int(-d);
      uint newValue = updateuintByint(allowance[o][s].n,_delta);
      allowance[o][s].n = newValue;
  }
  function updatePausedOnInsertRecv_pause_r21() private   returns (bool) {
      if(false==paused.b) {
        address s = msg.sender;
        if(true==isPauser[s].b) {
          paused = PausedTuple(true,true);
          emit Paused(true);
          return true;
        }
      }
      return false;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r2(address o,address r,uint n) private   returns (bool) {
      address s = msg.sender;
      if(false==paused.b) {
        uint m = balanceOf[o].n;
        uint k = allowance[o][s].n;
        if(m>=n && k>=n) {
          updateSpentTotalOnInsertTransferFrom_r27(o,s,n);
          updateTransferOnInsertTransferFrom_r1(o,r,n);
          emit TransferFrom(o,r,s,n);
          return true;
        }
      }
      return false;
  }
  function updateAllowanceOnIncrementSpentTotal_r29(address o,address s,int l) private    {
      int _delta = int(-l);
      uint newValue = updateuintByint(allowance[o][s].n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateTotalMintOnInsertMint_r15(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r26(p,delta0);
      totalMint[p].n += n;
  }
  function updateAllMintOnInsertMint_r9(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r16(delta0);
      allMint.n += n;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseApproval_r10(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      updateAllowanceTotalOnInsertIncreaseAllowance_r33(o,s,n);
      emit IncreaseAllowance(o,s,n);
      return true;
      return false;
  }
  function updateIsPauserOnInsertConstructor_r22() private    {
      address s = msg.sender;
      isPauser[s] = IsPauserTuple(true,true);
      emit IsPauser(s,true);
  }
}