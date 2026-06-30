contract Weth {
  struct AllowanceTuple {
    int n;
    bool _valid;
  }
  struct BalanceOfTuple {
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
  event Withdraw(address p,int n);
  event TransferFrom(address from,address to,address spender,int amount);
  event Transfer(address from,address to,int amount);
  event InvalidTx();
  event Deposit(address p,int n);
  event IncreaseAllowance(address o,address s,int n);
  constructor() public {
    updateTotalSupplyOnInsertConstructor_r15();
  }
  function increaseAllowance(address o,address s,int n) public    {
      bool r10 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r10(o,s,n);
      if(r10==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function withdraw(address p,int n) public    {
      bool r3 = updateWithdrawOnInsertRecv_withdraw_r3(p,n);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function deposit(address p,int n) public    {
      bool r2 = updateDepositOnInsertRecv_deposit_r2(p,n);
      if(r2==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r16 = updateTransferFromOnInsertRecv_transferFrom_r16(from,to,spender,amount);
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function transfer(address from,address to,int amount) public    {
      bool r13 = updateTransferOnInsertRecv_transfer_r13(from,to,amount);
      if(r13==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address o,address s) public view  returns (int) {
      int n = allowance[o][s].n;
      return n;
  }
  function updateTotalSupplyOnIncrementAllWithdraw_r14(int w) private    {
      totalSupply.n -= w;
  }
  function updateAllDepositOnInsertDeposit_r18(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllDeposit_r14(delta0);
  }
  function updateBalanceOfOnIncrementTotalIn_r6(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r16(address o,address r,address s,int n) private   returns (bool) {
      int allowance_x2 = allowance[o][s].n;
      if(n<=allowance_x2) {
        updateSpentTotalOnInsertTransferFrom_r22(o,s,n);
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
  function updateAllWithdrawOnInsertWithdraw_r24(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllWithdraw_r14(delta0);
  }
  function updateBalanceOfOnIncrementTotalWithdraw_r6(address p,int w) private    {
      balanceOf[p].n -= w;
  }
  function updateBalanceOfOnIncrementTotalDeposit_r6(address p,int d) private    {
      balanceOf[p].n += d;
  }
  function updateTotalWithdrawOnInsertWithdraw_r8(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalWithdraw_r6(p,delta0);
  }
  function updateTransferOnInsertRecv_transfer_r13(address s,address r,int n) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[s].n;
      if(n>0 && n<=balanceOf_x1_1) {
        updateTotalOutOnInsertTransfer_r12(s,n);
        updateTotalInOnInsertTransfer_r1(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateDepositOnInsertRecv_deposit_r2(address p,int n) private   returns (bool) {
      if(n>0) {
        updateAllDepositOnInsertDeposit_r18(n);
        updateTotalDepositOnInsertDeposit_r5(p,n);
        emit Deposit(p,n);
        return true;
      }
      return false;
  }
  function updateSpentTotalOnInsertTransferFrom_r22(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r0(o,s,delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r9(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r0(o,s,delta0);
  }
  function updateTotalSupplyOnInsertConstructor_r15() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateWithdrawOnInsertRecv_withdraw_r3(address p,int n) private   returns (bool) {
      address s_1 = msg.sender;
      int balanceOf_x1 = balanceOf[p].n;
      if(n>0 && p==s_1 && n<=balanceOf_x1) {
        updateAllWithdrawOnInsertWithdraw_r24(n);
        updateTotalWithdrawOnInsertWithdraw_r8(p,n);
        emit Withdraw(p,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementSpentTotal_r0(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTotalDepositOnInsertDeposit_r5(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalDeposit_r6(p,delta0);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r10(address o,address s,int d) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r9(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r6(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateTotalSupplyOnIncrementAllDeposit_r14(int d) private    {
      totalSupply.n += d;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r0(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateTotalOutOnInsertTransfer_r12(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r6(p,delta0);
  }
  function updateTotalInOnInsertTransfer_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r6(p,delta0);
  }
}