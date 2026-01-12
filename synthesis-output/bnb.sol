contract Bnb {
  struct BalanceOfTuple {
    int n;
    bool _valid;
  }
  struct TotalSupplyTuple {
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
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalSupplyTuple totalSupply;
  OwnerTuple owner;
  event Unfreeze(address p,int n);
  event Mint(address p,int amount);
  event TransferFrom(address from,address to,address spender,int amount);
  event WithdrawEther(address p,int amount);
  event Freeze(address p,int n);
  event Transfer(address from,address to,int amount);
  event InvalidTx();
  event IncreaseAllowance(address o,address s,int n);
  event Burn(address p,int amount);
  constructor(int n) public {
    updateOwnerOnInsertConstructor_r17();
  }
  function burn(address p,int amount) public    {
      bool r6 = updateBurnOnInsertRecv_burn_r6(p,amount);
      if(r6==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r8 = updateTransferFromOnInsertRecv_transferFrom_r8(from,to,spender,amount);
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function transfer(address from,address to,int amount) public    {
      bool r21 = updateTransferOnInsertRecv_transfer_r21(from,to,amount);
      if(r21==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r5 = updateMintOnInsertRecv_mint_r5(p,amount);
      if(r5==false) {
        revert("Rule condition failed");
      }
  }
  function unfreeze(address p,int n) public    {
      bool r27 = updateUnfreezeOnInsertRecv_unfreeze_r27(p,n);
      if(r27==false) {
        revert("Rule condition failed");
      }
  }
  function freeze(address p,int n) public    {
      bool r7 = updateFreezeOnInsertRecv_freeze_r7(p,n);
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function increaseAllowance(address o,address s,int n) public    {
      bool r3 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r3(o,s,n);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function withdrawEther(address p,int amount) public    {
      bool r23 = updateWithdrawEtherOnInsertRecv_withdrawEther_r23(p,amount);
      if(r23==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r11(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateBalanceOfOnIncrementTotalOut_r2(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateBalanceOfOnIncrementFreezeOf_r2(address p,int f) private    {
      balanceOf[p].n -= f;
  }
  function updateFreezeOfOnIncrementTotalFreeze_r1(address p,int f) private    {
      int delta0 = int(f);
      updateBalanceOfOnIncrementFreezeOf_r2(p,delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r8(address from,address to,address spender,int amount) private   returns (bool) {
      int balanceOf_x1 = balanceOf[o].n;
      if(balanceOf_x1>0) {
        updateSpentTotalOnInsertTransferFrom_r30(o,s,n);
        updateTransferOnInsertTransferFrom_r25(o,r,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateAllBurnOnInsertBurn_r19(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r20(delta0);
  }
  function updateUnfreezeOnInsertRecv_unfreeze_r27(address p,int n) private   returns (bool) {
      if(n>=0) {
        updateTotalUnfreezeOnInsertUnfreeze_r16(p,n);
        emit Unfreeze(p,n);
        return true;
      }
      return false;
  }
  function updateMintOnInsertRecv_mint_r5(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>=0) {
        updateAllMintOnInsertMint_r0(n);
        updateTotalMintOnInsertMint_r10(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r20(delta0);
  }
  function updateFreezeOnInsertRecv_freeze_r7(address p,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(0!=balanceOf_x1) {
        updateTotalFreezeOnInsertFreeze_r24(p,n);
        emit Freeze(p,n);
        return true;
      }
      return false;
  }
  function updateOwnerOnInsertConstructor_r17() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateBalanceOfOnIncrementTotalMint_r2(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r3(address o,address s,int n) private   returns (bool) {
      if(d>=0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r9(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalIn_r2(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTotalFreezeOnInsertFreeze_r24(address p,int n) private    {
      int delta0 = int(n);
      updateFreezeOfOnIncrementTotalFreeze_r1(p,delta0);
  }
  function updateBurnOnInsertRecv_burn_r6(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && balanceOf_x1>0) {
        updateTotalBurnOnInsertBurn_r15(p,n);
        updateAllBurnOnInsertBurn_r19(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementSpentTotal_r11(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTransferOnInsertTransferFrom_r25(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r14(r,n);
      updateTotalOutOnInsertTransfer_r12(o,n);
      emit Transfer(o,r,n);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalOutOnInsertTransfer_r12(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r2(p,delta0);
  }
  function updateTotalMintOnInsertMint_r10(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r2(p,delta0);
  }
  function updateTransferOnInsertRecv_transfer_r21(address from,address to,int amount) private   returns (bool) {
      int balanceOf_x1 = balanceOf[r].n;
      if(0!=balanceOf_x1) {
        updateTotalOutOnInsertTransfer_r12(s,n);
        updateTotalInOnInsertTransfer_r14(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateTotalInOnInsertTransfer_r14(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r2(p,delta0);
  }
  function updateTotalBurnOnInsertBurn_r15(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r2(p,delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r9(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r11(o,s,delta0);
  }
  function updateTotalUnfreezeOnInsertUnfreeze_r16(address p,int n) private    {
      int delta0 = int(n);
      updateFreezeOfOnIncrementTotalUnfreeze_r1(p,delta0);
  }
  function updateTotalSupplyOnIncrementAllBurn_r20(int b) private    {
      totalSupply.n -= b;
  }
  function updateBalanceOfOnIncrementTotalBurn_r2(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateWithdrawEtherOnInsertRecv_withdrawEther_r23(address p,int amount) private   returns (bool) {
      emit WithdrawEther(p,n);
      return true;
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r20(int m) private    {
      totalSupply.n += m;
  }
  function updateFreezeOfOnIncrementTotalUnfreeze_r1(address p,int u) private    {
      int delta0 = int(-u);
      updateBalanceOfOnIncrementFreezeOf_r2(p,delta0);
  }
  function updateSpentTotalOnInsertTransferFrom_r30(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r11(o,s,delta0);
  }
}