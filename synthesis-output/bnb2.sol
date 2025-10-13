contract Bnb2 {
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
    updateOwnerOnInsertConstructor_r14();
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function mint(address p,int amount) public    {
      bool r23 = updateMintOnInsertRecv_mint_r23(p,amount);
      if(r23==false) {
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
  function increaseAllowance(address o,address s,int n) public    {
      bool r20 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r20(o,s,n);
      if(r20==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r8 = updateTransferOnInsertRecv_transfer_r8(from,to,amount);
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r22 = updateTransferFromOnInsertRecv_transferFrom_r22(from,to,spender,amount);
      if(r22==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r9 = updateBurnOnInsertRecv_burn_r9(p,amount);
      if(r9==false) {
        revert("Rule condition failed");
      }
  }
  function updateAllowanceOnIncrementAllowanceTotal_r7(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateMintOnInsertRecv_mint_r23(address p,int n) private   returns (bool) {
      if(n>=0) {
        updateTotalMintOnInsertMint_r21(p,n);
        updateAllMintOnInsertMint_r0(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateAllBurnOnInsertBurn_r16(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r17(delta0);
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r17(delta0);
  }
  function updateTransferOnInsertTransferFrom_r4(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r11(r,n);
      updateTotalOutOnInsertTransfer_r24(o,n);
      emit Transfer(o,r,n);
  }
  function updateTotalSupplyOnIncrementAllMint_r17(int m) private    {
      totalSupply.n += m;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r22(address o,address r,address s,int n) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[o].n;
      if(n>0 && n<balanceOf_x1_1) {
        updateSpentTotalOnInsertTransferFrom_r6(o,s,n);
        updateTransferOnInsertTransferFrom_r4(o,r,n);
        emit TransferFrom(o,r,s,n);
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
  function updateTotalOutOnInsertTransfer_r24(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r2(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalBurn_r2(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateOwnerOnInsertConstructor_r14() private    {
      address s = msg.sender;
      // Empty()
  }
  function updateTotalBurnOnInsertBurn_r12(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r2(p,delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r5(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r7(o,s,delta0);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r20(address o,address s,int d) private   returns (bool) {
      updateAllowanceTotalOnInsertIncreaseAllowance_r5(o,s,d);
      emit IncreaseAllowance(o,s,d);
      return true;
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r2(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateSpentTotalOnInsertTransferFrom_r6(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r7(o,s,delta0);
  }
  function updateBalanceOfOnIncrementTotalIn_r2(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTotalSupplyOnIncrementAllBurn_r17(int b) private    {
      totalSupply.n -= b;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateAllowanceOnIncrementSpentTotal_r7(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTotalInOnInsertTransfer_r11(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r2(p,delta0);
  }
  function updateTotalMintOnInsertMint_r21(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r2(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalMint_r2(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateBurnOnInsertRecv_burn_r9(address p,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(n<=balanceOf_x1) {
        updateAllBurnOnInsertBurn_r16(n);
        updateTotalBurnOnInsertBurn_r12(p,n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertRecv_transfer_r8(address s,address r,int n) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[s].n;
      if(n>0 && n<=balanceOf_x1_1) {
        updateTotalInOnInsertTransfer_r11(r,n);
        updateTotalOutOnInsertTransfer_r24(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
}