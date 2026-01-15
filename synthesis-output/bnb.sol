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
    updateOwnerOnInsertConstructor_r15();
  }
  function freeze(address p,int n) public    {
      bool r13 = updateFreezeOnInsertRecv_freeze_r13(p,n);
      if(r13==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r29 = updateBurnOnInsertRecv_burn_r29(p,amount);
      if(r29==false) {
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
  function transfer(address from,address to,int amount) public    {
      bool r19 = updateTransferOnInsertRecv_transfer_r19(from,to,amount);
      if(r19==false) {
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
  function mint(address p,int amount) public    {
      bool r4 = updateMintOnInsertRecv_mint_r4(p,amount);
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function unfreeze(address p,int n) public    {
      bool r25 = updateUnfreezeOnInsertRecv_unfreeze_r25(p,n);
      if(r25==false) {
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
  function updateTransferFromOnInsertRecv_transferFrom_r5(address from,address to,address spender,int amount) private   returns (bool) {
      int balanceOf_x1 = balanceOf[o].n;
      if(balanceOf_x1>0) {
        updateSpentTotalOnInsertTransferFrom_r28(o,s,n);
        updateTransferOnInsertTransferFrom_r23(o,r,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateTotalBurnOnInsertBurn_r11(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r1(p,delta0);
  }
  function updateFreezeOfOnIncrementTotalFreeze_r14(address p,int f) private    {
      int delta0 = int(f);
      updateBalanceOfOnIncrementFreezeOf_r1(p,delta0);
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r18(delta0);
  }
  function updateTotalFreezeOnInsertFreeze_r22(address p,int n) private    {
      int delta0 = int(n);
      updateFreezeOfOnIncrementTotalFreeze_r14(p,delta0);
  }
  function updateOwnerOnInsertConstructor_r15() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTotalOutOnInsertTransfer_r8(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r1(p,delta0);
  }
  function updateFreezeOfOnIncrementTotalUnfreeze_r14(address p,int u) private    {
      int delta0 = int(-u);
      updateBalanceOfOnIncrementFreezeOf_r1(p,delta0);
  }
  function updateTotalSupplyOnIncrementAllBurn_r18(int b) private    {
      totalSupply.n -= b;
  }
  function updateBalanceOfOnIncrementTotalOut_r1(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateAllowanceOnIncrementSpentTotal_r30(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateSpentTotalOnInsertTransferFrom_r28(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r30(o,s,delta0);
  }
  function updateTransferOnInsertTransferFrom_r23(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r8(o,n);
      updateTotalInOnInsertTransfer_r10(r,n);
      emit Transfer(o,r,n);
  }
  function updateTotalMintOnInsertMint_r7(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r1(p,delta0);
  }
  function updateBurnOnInsertRecv_burn_r29(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && n<=balanceOf_x1) {
        updateAllBurnOnInsertBurn_r17(n);
        updateTotalBurnOnInsertBurn_r11(p,n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementFreezeOf_r1(address p,int f) private    {
      balanceOf[p].n -= f;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r2(address o,address s,int n) private   returns (bool) {
      if(d>=0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r6(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateFreezeOnInsertRecv_freeze_r13(address p,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(n<=balanceOf_x1) {
        updateTotalFreezeOnInsertFreeze_r22(p,n);
        emit Freeze(p,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalIn_r1(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateBalanceOfOnIncrementTotalMint_r1(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateWithdrawEtherOnInsertRecv_withdrawEther_r21(address p,int amount) private   returns (bool) {
      emit WithdrawEther(p,n);
      return true;
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r18(int m) private    {
      totalSupply.n += m;
  }
  function updateAllBurnOnInsertBurn_r17(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r18(delta0);
  }
  function updateUnfreezeOnInsertRecv_unfreeze_r25(address p,int n) private   returns (bool) {
      if(n>=0) {
        updateTotalUnfreezeOnInsertUnfreeze_r12(p,n);
        emit Unfreeze(p,n);
        return true;
      }
      return false;
  }
  function updateMintOnInsertRecv_mint_r4(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>=0) {
        updateTotalMintOnInsertMint_r7(p,n);
        updateAllMintOnInsertMint_r0(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateTotalUnfreezeOnInsertUnfreeze_r12(address p,int n) private    {
      int delta0 = int(n);
      updateFreezeOfOnIncrementTotalUnfreeze_r14(p,delta0);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r30(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateBalanceOfOnIncrementTotalBurn_r1(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateTransferOnInsertRecv_transfer_r19(address from,address to,int amount) private   returns (bool) {
      int balanceOf_x1 = balanceOf[r].n;
      if(0!=balanceOf_x1) {
        updateTotalInOnInsertTransfer_r10(r,n);
        updateTotalOutOnInsertTransfer_r8(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r6(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r30(o,s,delta0);
  }
  function updateTotalInOnInsertTransfer_r10(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r1(p,delta0);
  }
}