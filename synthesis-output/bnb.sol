contract Bnb {
  struct BalanceOfTuple {
    int n;
    bool _valid;
  }
  struct TotalSupplyTuple {
    int n;
    bool _valid;
  }
  struct AllowanceTuple {
    int n;
    bool _valid;
  }
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalSupplyTuple totalSupply;
  event Unfreeze(address p,int n);
  event Transfer(address from,address to,int amount);
  event Mint(address p,int amount);
  event TransferFrom(address from,address to,address spender,int amount);
  event WithdrawEther(address p,int amount);
  event Freeze(address p,int n);
  event IncreaseAllowance(address o,address s,int n);
  event Burn(address p,int amount);
  constructor(int n) public {
    updateOwnerOnInsertConstructor_r17();
  }
  function unfreeze(address p,int n) public    {
      bool r23 = updateUnfreezeOnInsertRecv_unfreeze_r23(p,n);
      if(r23==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address o,address s,int n) public    {
      bool r2 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r2(o,s,n);
      if(r2==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r5 = updateTransferFromOnInsertRecv_transferFrom_r5(from,to,spender,amount);
      if(r5==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r27 = updateMintOnInsertRecv_mint_r27(p,amount);
      if(r27==false) {
        revert("Rule condition failed");
      }
  }
  function freeze(address p,int n) public    {
      bool r12 = updateFreezeOnInsertRecv_freeze_r12(p,n);
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function burn(address p,int amount) public    {
      bool r10 = updateBurnOnInsertRecv_burn_r10(p,amount);
      if(r10==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r9 = updateTransferOnInsertRecv_transfer_r9(from,to,amount);
      if(r9==false) {
        revert("Rule condition failed");
      }
  }
  function withdrawEther(address p,int amount) public    {
      bool r21 = updateWithdrawEtherOnInsertRecv_withdrawEther_r21(p,amount);
      if(r21==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function updateAllMintOnInsertMint_r11(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r20(delta0);
  }
  function updateSpentTotalOnInsertTransferFrom_r26(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r8(o,s,delta0);
  }
  function updateMintOnInsertRecv_mint_r27(address p,int n) private   returns (bool) {
      if(n>=0) {
        updateTotalMintOnInsertMint_r7(p,n);
        updateAllMintOnInsertMint_r11(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r2(address o,address s,int d) private   returns (bool) {
      if(d>=0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r6(o,s,d);
        emit IncreaseAllowance(o,s,d);
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
  function updateTransferOnInsertTransferFrom_r4(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r14(r,n);
      updateTotalOutOnInsertTransfer_r28(o,n);
      emit Transfer(o,r,n);
  }
  function updateTotalSupplyOnIncrementAllMint_r20(int m) private    {
      totalSupply.n += m;
  }
  function updateTotalMintOnInsertMint_r7(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r1(p,delta0);
  }
  function updateTotalInOnInsertTransfer_r14(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r1(p,delta0);
  }
  function updateTotalBurnOnInsertBurn_r15(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r1(p,delta0);
  }
  function updateBalanceOfOnIncrementFreezeOf_r1(address p,int f) private    {
      balanceOf[p].n -= f;
  }
  function updateTotalFreezeOnInsertFreeze_r22(address p,int n) private    {
      int delta0 = int(n);
      updateFreezeOfOnIncrementTotalFreeze_r0(p,delta0);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r8(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateWithdrawEtherOnInsertRecv_withdrawEther_r21(address p,int n) private   returns (bool) {
      emit WithdrawEther(p,n);
      return true;
      return false;
  }
  function updateAllowanceOnIncrementSpentTotal_r8(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateAllBurnOnInsertBurn_r19(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r20(delta0);
  }
  function updateBalanceOfOnIncrementTotalIn_r1(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateBalanceOfOnIncrementTotalMint_r1(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r5(address o,address r,address s,int n) private   returns (bool) {
      int balanceOf_x1_2 = balanceOf[o].n;
      int allowance_x2_1 = allowance[o][s].n;
      if(n>0 && n<allowance_x2_1 && n<balanceOf_x1_2) {
        updateTransferOnInsertTransferFrom_r4(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r26(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateFreezeOnInsertRecv_freeze_r12(address p,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(n<balanceOf_x1) {
        updateTotalFreezeOnInsertFreeze_r22(p,n);
        emit Freeze(p,n);
        return true;
      }
      return false;
  }
  function updateUnfreezeOnInsertRecv_unfreeze_r23(address p,int n) private   returns (bool) {
      if(n>=0) {
        updateTotalUnfreezeOnInsertUnfreeze_r16(p,n);
        emit Unfreeze(p,n);
        return true;
      }
      return false;
  }
  function updateFreezeOfOnIncrementTotalFreeze_r0(address p,int f) private    {
      int delta0 = int(f);
      updateBalanceOfOnIncrementFreezeOf_r1(p,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateBalanceOfOnIncrementTotalBurn_r1(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateOwnerOnInsertConstructor_r17() private    {
      address s = msg.sender;
      // Empty()
  }
  function updateBurnOnInsertRecv_burn_r10(address p,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(n<=balanceOf_x1) {
        updateTotalBurnOnInsertBurn_r15(p,n);
        updateAllBurnOnInsertBurn_r19(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateTotalUnfreezeOnInsertUnfreeze_r16(address p,int n) private    {
      int delta0 = int(n);
      updateFreezeOfOnIncrementTotalUnfreeze_r0(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalOut_r1(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateTransferOnInsertRecv_transfer_r9(address s,address r,int n) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[s].n;
      if(n>0 && n<=balanceOf_x1_1) {
        updateTotalInOnInsertTransfer_r14(r,n);
        updateTotalOutOnInsertTransfer_r28(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r20(int b) private    {
      totalSupply.n -= b;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r6(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r8(o,s,delta0);
  }
  function updateFreezeOfOnIncrementTotalUnfreeze_r0(address p,int u) private    {
      int delta0 = int(-u);
      updateBalanceOfOnIncrementFreezeOf_r1(p,delta0);
  }
  function updateTotalOutOnInsertTransfer_r28(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r1(p,delta0);
  }
}