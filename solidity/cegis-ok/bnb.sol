contract Bnb {
  struct TotalSupplyTuple {
    uint n;
    bool _valid;
  }
  struct AllowanceTuple {
    uint n;
    bool _valid;
  }
  struct BalanceOfTuple {
    uint n;
    bool _valid;
  }
  TotalSupplyTuple totalSupply;
  mapping(address=>BalanceOfTuple) balanceOf;
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  event TransferFrom(address from,address to,address spender,uint amount);
  event Burn(address p,uint amount);
  event Mint(address p,uint amount);
  event WithdrawEther(address p,uint amount);
  event IncreaseAllowance(address p,address s,uint n);
  event Unfreeze(address p,uint n);
  event Freeze(address p,uint n);
  event Transfer(address from,address to,uint amount);
  constructor(uint initialSupply) public {
    updateAllMintOnInsertConstructor_r10(initialSupply);
    updateBalanceOfOnInsertConstructor_r21(initialSupply);
    updateOwnerOnInsertConstructor_r8();
    updateTotalSupplyOnInsertConstructor_r26(initialSupply);
    updateTotalMintOnInsertConstructor_r32(initialSupply);
    updateTotalInOnInsertConstructor_r29(initialSupply);
    updateTotalBalancesOnInsertConstructor_r5(initialSupply);
  }
  function transfer(address to,uint amount) public    {
      bool r30 = updateTransferOnInsertRecv_transfer_r30(to,amount);
      if(r30==false) {
        revert("Rule condition failed");
      }
  }
  function freeze(uint n) public    {
      bool r22 = updateFreezeOnInsertRecv_freeze_r22(n);
      if(r22==false) {
        revert("Rule condition failed");
      }
  }
  function unfreeze(uint n) public    {
      bool r25 = updateUnfreezeOnInsertRecv_unfreeze_r25(n);
      if(r25==false) {
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
  function burn(uint amount) public    {
      bool r19 = updateBurnOnInsertRecv_burn_r19(amount);
      if(r19==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,uint amount) public    {
      bool r27 = updateTransferFromOnInsertRecv_transferFrom_r27(from,to,amount);
      if(r27==false) {
        revert("Rule condition failed");
      }
  }
  function withdrawEther(uint amount) public    {
      bool r6 = updateWithdrawEtherOnInsertRecv_withdrawEther_r6(amount);
      if(r6==false) {
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
  function updateAllBurnOnInsertBurn_r7(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r18(delta0);
  }
  function updateTotalSupplyOnIncrementAllBurn_r18(int b) private    {
      int _delta = int(-b);
      uint x_totalSupply__n = totalSupply.n;
      uint newValue = updateuintByint(x_totalSupply__n,_delta);
      totalSupply.n = newValue;
  }
  function updateTransferOnInsertRecv_transfer_r30(address r,uint n) private   returns (bool) {
      address s = msg.sender;
      updateTotalOutOnInsertTransfer_r23(s,n);
      updateTotalInOnInsertTransfer_r31(r,n);
      emit Transfer(s,r,n);
      return true;
      return false;
  }
  function updateTotalMintOnInsertConstructor_r32(uint n) private    {
      address s = msg.sender;
      // Empty()
  }
  function updateAllowanceOnIncrementSpentTotal_r28(address o,address s,int l) private    {
      int _delta = int(-l);
      uint x_allowance_o_s_n = allowance[o][s].n;
      uint newValue = updateuintByint(x_allowance_o_s_n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateTotalInOnInsertConstructor_r29(uint n) private    {
      address s = msg.sender;
      // Empty()
  }
  function updateBalanceOfOnIncrementTotalIn_r2(address p,int i) private    {
      int _delta = int(i);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r33(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r28(o,s,delta0);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r28(address o,address s,int m) private    {
      int _delta = int(m);
      uint x_allowance_o_s_n = allowance[o][s].n;
      uint newValue = updateuintByint(x_allowance_o_s_n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateTotalOutOnInsertTransfer_r23(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r2(p,delta0);
  }
  function updateIncreaseAllowanceOnInsertRecv_approve_r15(address s,uint n) private   returns (bool) {
      updateAllowanceTotalOnInsertIncreaseAllowance_r33(o,s,d);
      emit IncreaseAllowance(o,s,d);
      return true;
      return false;
  }
  function updateSendOnInsertWithdrawEther_r3(address p,uint n) private    {
      payable(p).send(n);
  }
  function updateBalanceOfOnIncrementTotalOut_r2(address p,int o) private    {
      int _delta = int(-o);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateSpentTotalOnInsertTransferFrom_r4(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r28(o,s,delta0);
  }
  function updateAllMintOnInsertConstructor_r10(uint n) private    {
      // Empty()
  }
  function updateTransferFromOnInsertRecv_transferFrom_r27(address o,address r,uint n) private   returns (bool) {
      updateSpentTotalOnInsertTransferFrom_r4(o,s,n);
      updateTransferOnInsertTransferFrom_r1(o,r,n);
      emit TransferFrom(o,r,s,n);
      return true;
      return false;
  }
  function updateFreezeOfOnIncrementTotalFreeze_r13(address p,int f) private    {
      int delta0 = int(f);
      updateBalanceOfOnIncrementFreezeOf_r2(p,delta0);
  }
  function updateTotalUnfreezeOnInsertUnfreeze_r14(address p,uint n) private    {
      int delta0 = int(n);
      updateFreezeOfOnIncrementTotalUnfreeze_r13(p,delta0);
  }
  function updateWithdrawEtherOnInsertRecv_withdrawEther_r6(uint n) private   returns (bool) {
      updateSendOnInsertWithdrawEther_r3(p,n);
      emit WithdrawEther(p,n);
      return true;
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r26(uint n) private    {
      totalSupply = TotalSupplyTuple(n,true);
  }
  function updateUnfreezeOnInsertRecv_unfreeze_r25(uint n) private   returns (bool) {
      updateTotalUnfreezeOnInsertUnfreeze_r14(p,n);
      emit Unfreeze(p,n);
      return true;
      return false;
  }
  function updateFreezeOfOnIncrementTotalUnfreeze_r13(address p,int u) private    {
      int delta0 = int(-u);
      updateBalanceOfOnIncrementFreezeOf_r2(p,delta0);
  }
  function updateBalanceOfOnInsertConstructor_r21(uint n) private    {
      address s = msg.sender;
      balanceOf[s] = BalanceOfTuple(n,true);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTransferOnInsertTransferFrom_r1(address o,address r,uint n) private    {
      updateTotalInOnInsertTransfer_r31(r,n);
      updateTotalOutOnInsertTransfer_r23(o,n);
      emit Transfer(o,r,n);
  }
  function updateTotalBalancesOnInsertConstructor_r5(uint n) private    {
      // Empty()
  }
  function updateFreezeOnInsertRecv_freeze_r22(uint n) private   returns (bool) {
      updateTotalFreezeOnInsertFreeze_r0(p,n);
      emit Freeze(p,n);
      return true;
      return false;
  }
  function updateBurnOnInsertRecv_burn_r19(uint n) private   returns (bool) {
      address p = msg.sender;
      updateTotalBurnOnInsertBurn_r16(p,n);
      updateAllBurnOnInsertBurn_r7(n);
      emit Burn(p,n);
      return true;
      return false;
  }
  function updateTotalBurnOnInsertBurn_r16(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r2(p,delta0);
  }
  function updateTotalFreezeOnInsertFreeze_r0(address p,uint n) private    {
      int delta0 = int(n);
      updateFreezeOfOnIncrementTotalFreeze_r13(p,delta0);
  }
  function updateOwnerOnInsertConstructor_r8() private    {
      address s = msg.sender;
      // Empty()
  }
  function updateBalanceOfOnIncrementTotalBurn_r2(address p,int m) private    {
      int _delta = int(-m);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateBalanceOfOnIncrementFreezeOf_r2(address p,int f) private    {
      int _delta = int(-f);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateTotalInOnInsertTransfer_r31(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r2(p,delta0);
  }
}