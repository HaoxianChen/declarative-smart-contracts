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
    updateOwnerOnInsertConstructor_r16();
  }
  function increaseAllowance(address o,address s,int n) public    {
      bool r23 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r23(o,s,n);
      if(r23==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r7 = updateTransferFromOnInsertRecv_transferFrom_r7(from,to,spender,amount);
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function freeze(address p,int n) public    {
      bool r19 = updateFreezeOnInsertRecv_freeze_r19(p,n);
      if(r19==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function unfreeze(address p,int n) public    {
      bool r6 = updateUnfreezeOnInsertRecv_unfreeze_r6(p,n);
      if(r6==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r5 = updateMintOnInsertRecv_mint_r5(p,amount);
      if(r5==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function burn(address p,int amount) public    {
      bool r12 = updateBurnOnInsertRecv_burn_r12(p,amount);
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r4 = updateTransferOnInsertRecv_transfer_r4(from,to,amount);
      if(r4==false) {
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
  function updateBalanceOfOnIncrementTotalOut_r3(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateBalanceOfOnIncrementTotalBurn_r3(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateTotalUnfreezeOnInsertUnfreeze_r15(address p,int n) private    {
      int delta0 = int(n);
      updateFreezeOfOnIncrementTotalUnfreeze_r2(p,delta0);
  }
  function updateTotalBurnOnInsertBurn_r14(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r3(p,delta0);
  }
  function updateFreezeOfOnIncrementTotalFreeze_r2(address p,int f) private    {
      int delta0 = int(f);
      updateBalanceOfOnIncrementFreezeOf_r3(p,delta0);
  }
  function updateTransferOnInsertRecv_transfer_r4(address s,address r,int n) private   returns (bool) {
      updateTotalInOnInsertTransfer_r1(r,n);
      updateTotalOutOnInsertTransfer_r13(s,n);
      emit Transfer(s,r,n);
      return true;
      return false;
  }
  function updateWithdrawEtherOnInsertRecv_withdrawEther_r21(address p,int n) private   returns (bool) {
      emit WithdrawEther(p,n);
      return true;
      return false;
  }
  function updateMintOnInsertRecv_mint_r5(address p,int n) private   returns (bool) {
      updateAllMintOnInsertMint_r0(n);
      updateTotalMintOnInsertMint_r10(p,n);
      emit Mint(p,n);
      return true;
      return false;
  }
  function updateBalanceOfOnIncrementFreezeOf_r3(address p,int f) private    {
      balanceOf[p].n -= f;
  }
  function updateUnfreezeOnInsertRecv_unfreeze_r6(address p,int n) private   returns (bool) {
      updateTotalUnfreezeOnInsertUnfreeze_r15(p,n);
      emit Unfreeze(p,n);
      return true;
      return false;
  }
  function updateOwnerOnInsertConstructor_r16() private    {
      address s = msg.sender;
      // Empty()
  }
  function updateSpentTotalOnInsertTransferFrom_r11(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r24(o,s,delta0);
  }
  function updateFreezeOfOnIncrementTotalUnfreeze_r2(address p,int u) private    {
      int delta0 = int(-u);
      updateBalanceOfOnIncrementFreezeOf_r3(p,delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r9(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r24(o,s,delta0);
  }
  function updateTotalFreezeOnInsertFreeze_r22(address p,int n) private    {
      int delta0 = int(n);
      updateFreezeOfOnIncrementTotalFreeze_r2(p,delta0);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r24(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateTotalOutOnInsertTransfer_r13(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r3(p,delta0);
  }
  function updateTotalInOnInsertTransfer_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r3(p,delta0);
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r18(delta0);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r23(address o,address s,int d) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r9(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r18(int m) private    {
      totalSupply.n += m;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateAllBurnOnInsertBurn_r17(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r18(delta0);
  }
  function updateAllowanceOnIncrementSpentTotal_r24(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateBalanceOfOnIncrementTotalIn_r3(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r7(address o,address r,address s,int n) private   returns (bool) {
      int balanceOf_x1_0 = balanceOf[r].n;
      int allowance_x2_3 = allowance[r][o].n;
      int allowance_x2_1 = allowance[o][r].n;
      int balanceOf_x1_2 = balanceOf[s].n;
      if(n<balanceOf_x1_0 && n<=allowance_x2_1 && 0!=balanceOf_x1_2 && allowance_x2_3>0) {
        updateSpentTotalOnInsertTransferFrom_r11(o,s,n);
        updateTransferOnInsertTransferFrom_r8(o,r,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalMint_r3(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateTransferOnInsertTransferFrom_r8(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r13(o,n);
      updateTotalInOnInsertTransfer_r1(r,n);
      emit Transfer(o,r,n);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateFreezeOnInsertRecv_freeze_r19(address p,int n) private   returns (bool) {
      updateTotalFreezeOnInsertFreeze_r22(p,n);
      emit Freeze(p,n);
      return true;
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r18(int b) private    {
      totalSupply.n -= b;
  }
  function updateTotalMintOnInsertMint_r10(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r3(p,delta0);
  }
  function updateBurnOnInsertRecv_burn_r12(address p,int n) private   returns (bool) {
      updateAllBurnOnInsertBurn_r17(n);
      updateTotalBurnOnInsertBurn_r14(p,n);
      emit Burn(p,n);
      return true;
      return false;
  }
}