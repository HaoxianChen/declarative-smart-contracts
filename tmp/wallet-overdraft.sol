contract Wallet-overdraft {
  struct BalanceTuple {
    uint n;
    bool _valid;
  }
  BalanceTuple balance;
  event Withdraw(address p,uint amount);
  event Deposit(address p,uint amount);
  constructor() public {
    updateBalanceOnInsertConstructor_r4();
  }
  function deposit() public  payable  {
      bool r2 = updateDepositOnInsertRecv_deposit_r2();
      if(r2==false) {
        revert("Rule condition failed");
      }
  }
  function withdraw() public  payable  {
      bool r7 = updateWithdrawOnInsertRecv_withdraw_r7();
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function getBalance() public view  returns (uint) {
      uint n = balance.n;
      return n;
  }
  function updateBalanceOnInsertConstructor_r4() private    {
      balance = BalanceTuple(0,true);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateSendOnInsertWithdraw_r8(address p,uint amount) private    {
      payable(p).send(amount);
  }
  function updateWithdrawOnInsertRecv_withdraw_r7() private   returns (bool) {
      uint amount = msg.value;
      address p = msg.sender;
      updateSendOnInsertWithdraw_r8(p,amount);
      emit Withdraw(p,amount);
      return true;
      return false;
  }
  function updateDepositOnInsertRecv_deposit_r2() private   returns (bool) {
      uint amount = msg.value;
      address p = msg.sender;
      updateBalanceOnInsertDeposit_r0(amount);
      emit Deposit(p,amount);
      return true;
      return false;
  }
  function updateBalanceOnInsertDeposit_r0(uint amount) private    {
      balance.n += amount;
  }
}