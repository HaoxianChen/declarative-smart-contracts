contract Matic {
  struct TotalSupplyTuple {
    uint n;
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
  TotalSupplyTuple totalSupply;
  mapping(address=>BalanceOfTuple) balanceOf;
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  event Burn(address p,uint amount);
  event Mint(address p,uint amount);
  event DecreaseAllowance(address p,address s,uint n);
  event IncreaseAllowance(address p,address s,uint n);
  event IsPauser(address p,bool b);
  event Transfer(address from,address to,uint amount);
  event Paused(bool b);
  event TransferFrom(address from,address to,address spender,uint amount);
  constructor(uint n) public {
    updateAllMintOnInsertConstructor_r5(n);
    updateBalanceOfOnInsertConstructor_r6(n);
    updateOwnerOnInsertConstructor_r10();
    updatePausedOnInsertConstructor_r24();
    updateIsPauserOnInsertConstructor_r7();
    updateTotalBalancesOnInsertConstructor_r32(n);
    updateTotalMintOnInsertConstructor_r20(n);
    updateTotalSupplyOnInsertConstructor_r28(n);
  }
  function transferFrom(address from,address to,uint amount) public    {
      bool r12 = updateTransferFromOnInsertRecv_transferFrom_r12(from,to,amount);
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function pause() public    {
      bool r14 = updatePausedOnInsertRecv_pause_r14();
      if(r14==false) {
        revert("Rule condition failed");
      }
  }
  function increaseApproval(address p,uint n) public    {
      bool r3 = updateIncreaseAllowanceOnInsertRecv_increaseApproval_r3(p,n);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function unpause() public    {
      bool r30 = updatePausedOnInsertRecv_unpause_r30();
      if(r30==false) {
        revert("Rule condition failed");
      }
  }
  function renouncePauser() public    {
      bool r8 = updateIsPauserOnInsertRecv_renouncePauser_r8();
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function decreaseApproval(address p,uint n) public    {
      bool r22 = updateDecreaseAllowanceOnInsertRecv_decreaseApproval_r22(p,n);
      if(r22==false) {
        revert("Rule condition failed");
      }
  }
  function addPauser(address p) public    {
      bool r11 = updateIsPauserOnInsertRecv_addPauser_r11(p);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function approve(address s,uint n) public    {
      bool r15 = updateIncreaseAllowanceOnInsertRecv_approve_r15(s,n);
      if(r15==false) {
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
  function transfer(address to,uint amount) public    {
      bool r1 = updateTransferOnInsertRecv_transfer_r1(to,amount);
      if(r1==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,uint amount) public    {
      bool r27 = updateBurnOnInsertRecv_burn_r27(p,amount);
      if(r27==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,uint amount) public    {
      bool r2 = updateMintOnInsertRecv_mint_r2(p,amount);
      if(r2==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (uint) {
      uint n = balanceOf[p].n;
      return n;
  }
  function updateTotalInOnInsertTransfer_r31(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r25(p,delta0);
  }
  function updateMintOnInsertRecv_mint_r2(address p,uint n) private   returns (bool) {
      updateTotalMintOnInsertMint_r17(p,n);
      updateAllMintOnInsertMint_r4(n);
      emit Mint(p,n);
      return true;
      return false;
  }
  function updateBalanceOfOnInsertConstructor_r6(uint n) private    {
      address s = msg.sender;
      balanceOf[s] = BalanceOfTuple(n,true);
  }
  function updateDecreaseAllowanceOnInsertRecv_decreaseApproval_r22(address s,uint n) private   returns (bool) {
      updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r9(o,s,n);
      emit DecreaseAllowance(o,s,n);
      return true;
      return false;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseApproval_r3(address s,uint n) private   returns (bool) {
      updateAllowanceTotalOnInsertIncreaseAllowance_r33(o,s,n);
      emit IncreaseAllowance(o,s,n);
      return true;
      return false;
  }
  function updateTotalBalancesOnInsertConstructor_r32(uint n) private    {
      // Empty()
  }
  function updateIncreaseAllowanceOnInsertRecv_approve_r15(address s,uint n) private   returns (bool) {
      updateAllowanceTotalOnInsertIncreaseAllowance_r33(o,s,d);
      emit IncreaseAllowance(o,s,d);
      return true;
      return false;
  }
  function updateBalanceOfOnIncrementTotalBurn_r25(address p,int m) private    {
      int _delta = int(-m);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateTotalBurnOnInsertBurn_r16(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r25(p,delta0);
  }
  function updateIsPauserOnInsertRecv_renouncePauser_r8() private   returns (bool) {
      emit IsPauser(s,false);
      return true;
      return false;
  }
  function updateBurnOnInsertRecv_burn_r27(address p,uint n) private   returns (bool) {
      updateTotalBurnOnInsertBurn_r16(p,n);
      updateAllBurnOnInsertBurn_r13(n);
      emit Burn(p,n);
      return true;
      return false;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r29(address o,address s,int m) private    {
      int _delta = int(m);
      uint x_allowance_o_s_n = allowance[o][s].n;
      uint newValue = updateuintByint(x_allowance_o_s_n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateTotalSupplyOnInsertConstructor_r28(uint n) private    {
      totalSupply = TotalSupplyTuple(n,true);
  }
  function updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r9(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementDecreaseAllowanceTotal_r29(o,s,delta0);
  }
  function updatePausedOnInsertRecv_pause_r14() private   returns (bool) {
      emit Paused(true);
      return true;
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r18(int b) private    {
      int _delta = int(-b);
      uint x_totalSupply__n = totalSupply.n;
      uint newValue = updateuintByint(x_totalSupply__n,_delta);
      totalSupply.n = newValue;
  }
  function updateAllowanceOnIncrementSpentTotal_r29(address o,address s,int l) private    {
      int _delta = int(-l);
      uint x_allowance_o_s_n = allowance[o][s].n;
      uint newValue = updateuintByint(x_allowance_o_s_n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateIsPauserOnInsertRecv_addPauser_r11(address p) private   returns (bool) {
      emit IsPauser(p,true);
      return true;
      return false;
  }
  function updateTotalMintOnInsertMint_r17(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r25(p,delta0);
  }
  function updateAllowanceOnIncrementDecreaseAllowanceTotal_r29(address o,address s,int d) private    {
      int _delta = int(-d);
      uint x_allowance_o_s_n = allowance[o][s].n;
      uint newValue = updateuintByint(x_allowance_o_s_n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateBalanceOfOnIncrementTotalMint_r25(address p,int n) private    {
      int _delta = int(n);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateTotalMintOnInsertConstructor_r20(uint n) private    {
      address s = msg.sender;
      // Empty()
  }
  function updatePausedOnInsertConstructor_r24() private    {
      emit Paused(false);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateSpentTotalOnInsertTransferFrom_r26(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r29(o,s,delta0);
  }
  function updateTotalSupplyOnIncrementAllMint_r18(int m) private    {
      int _delta = int(m);
      uint x_totalSupply__n = totalSupply.n;
      uint newValue = updateuintByint(x_totalSupply__n,_delta);
      totalSupply.n = newValue;
  }
  function updateTransferOnInsertTransferFrom_r0(address o,address r,uint n) private    {
      updateTotalInOnInsertTransfer_r31(r,n);
      updateTotalOutOnInsertTransfer_r21(o,n);
      emit Transfer(o,r,n);
  }
  function updateOwnerOnInsertConstructor_r10() private    {
      address s = msg.sender;
      // Empty()
  }
  function updateAllMintOnInsertMint_r4(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r18(delta0);
  }
  function updateAllMintOnInsertConstructor_r5(uint n) private    {
      // Empty()
  }
  function updatePausedOnInsertRecv_unpause_r30() private   returns (bool) {
      emit Paused(false);
      return true;
      return false;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r12(address o,address r,uint n) private   returns (bool) {
      updateSpentTotalOnInsertTransferFrom_r26(o,s,n);
      updateTransferOnInsertTransferFrom_r0(o,r,n);
      emit TransferFrom(o,r,s,n);
      return true;
      return false;
  }
  function updateIsPauserOnInsertConstructor_r7() private    {
      address s = msg.sender;
      emit IsPauser(s,true);
  }
  function updateBalanceOfOnIncrementTotalOut_r25(address p,int o) private    {
      int _delta = int(-o);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r33(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r29(o,s,delta0);
  }
  function updateTotalOutOnInsertTransfer_r21(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r25(p,delta0);
  }
  function updateAllBurnOnInsertBurn_r13(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r18(delta0);
  }
  function updateTransferOnInsertRecv_transfer_r1(address r,uint n) private   returns (bool) {
      updateTotalInOnInsertTransfer_r31(r,n);
      updateTotalOutOnInsertTransfer_r21(s,n);
      emit Transfer(s,r,n);
      return true;
      return false;
  }
  function updateBalanceOfOnIncrementTotalIn_r25(address p,int i) private    {
      int _delta = int(i);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
}