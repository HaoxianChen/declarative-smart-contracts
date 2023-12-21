contract Shib {
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct DecreaseAllowanceTotalTuple {
    uint m;
    bool _valid;
  }
  struct TotalSupplyTuple {
    uint n;
    bool _valid;
  }
  struct SpentTotalTuple {
    uint m;
    bool _valid;
  }
  struct TotalBalancesTuple {
    uint m;
    bool _valid;
  }
  struct BalanceOfTuple {
    uint n;
    bool _valid;
  }
  struct IncreaseAllowanceTotalTuple {
    uint m;
    bool _valid;
  }
  mapping(address=>mapping(address=>DecreaseAllowanceTotalTuple)) decreaseAllowanceTotal;
  TotalSupplyTuple totalSupply;
  TotalBalancesTuple totalBalances;
  mapping(address=>mapping(address=>SpentTotalTuple)) spentTotal;
  OwnerTuple owner;
  mapping(address=>BalanceOfTuple) balanceOf;
  mapping(address=>mapping(address=>IncreaseAllowanceTotalTuple)) increaseAllowanceTotal;
  event TransferFrom(address from,address to,address spender,uint amount);
  event Burn(address p,uint amount);
  event Mint(address p,uint amount);
  event DecreaseAllowance(address p,address s,uint n);
  event IncreaseAllowance(address p,address s,uint n);
  event BurnFrom(address p,address from,uint n);
  event Transfer(address from,address to,uint amount);
  constructor(uint name,uint symbol,uint decimals,uint totalSupply,address feeReceiver,address tokenOwnerAddress) public {
    updateBalanceOfOnInsertConstructor_r0(totalSupply,tokenOwnerAddress);
    updateSendOnInsertConstructor_r2(feeReceiver);
    updateTotalSupplyOnInsertConstructor_r4(totalSupply);
    updateOwnerOnInsertConstructor_r15();
    updateTotalBalancesOnInsertConstructor_r28(totalSupply);
  }
  function getTotalSupply() public view  returns (uint) {
      uint n = totalSupply.n;
      return n;
  }
  function getBalanceOf(address p) public view  returns (uint) {
      uint n = balanceOf[p].n;
      return n;
  }
  function burn(uint amount) public    {
      bool r5 = updateBurnOnInsertRecv_burn_r5(amount);
      if(r5==false) {
        revert("Rule condition failed");
      }
  }
  function approve(address s,uint n) public    {
      bool r18 = updateIncreaseAllowanceOnInsertRecv_approve_r18(s,n);
      bool r13 = updateDecreaseAllowanceOnInsertRecv_approve_r13(s,n);
      if(r18==false && r13==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address to,uint amount) public    {
      bool r20 = updateTransferOnInsertRecv_transfer_r20(to,amount);
      if(r20==false) {
        revert("Rule condition failed");
      }
  }
  function decreaseAllowance(address s,uint n) public    {
      bool r24 = updateDecreaseAllowanceOnInsertRecv_decreaseAllowance_r24(s,n);
      if(r24==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (uint) {
      uint n = allowance(p,s);
      return n;
  }
  function increaseAllowance(address s,uint n) public    {
      bool r11 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r11(s,n);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,uint amount) public    {
      bool r26 = updateTransferFromOnInsertRecv_transferFrom_r26(from,to,amount);
      if(r26==false) {
        revert("Rule condition failed");
      }
  }
  function updateSendOnInsertConstructor_r2(address p) private    {
      uint n = msg.value;
      payable(p).send(n);
  }
  function updateTransferOnInsertRecv_transfer_r20(address r,uint n) private   returns (bool) {
      address s = msg.sender;
      uint m = balanceOf[s].n;
      if(n<=m) {
        updateTotalInOnInsertTransfer_r30(r,n);
        updateTotalOutOnInsertTransfer_r22(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r17(int b) private    {
      int _delta = int(-b);
      uint newValue = updateuintByint(totalSupply.n,_delta);
      totalSupply.n = newValue;
  }
  function updateTotalSupplyOnInsertConstructor_r4(uint n) private    {
      totalSupply = TotalSupplyTuple(n,true);
  }
  function allowance(address o,address s) private view  returns (uint) {
      uint l = spentTotal[o][s].m;
      uint d = decreaseAllowanceTotal[o][s].m;
      uint m = increaseAllowanceTotal[o][s].m;
      uint n = (m-l)-d;
      return n;
  }
  function updateIncreaseAllowanceOnInsertRecv_approve_r18(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      uint m = allowance(o,s);
      if(n>=m) {
        uint d = n-m;
        updateIncreaseAllowanceTotalOnInsertIncreaseAllowance_r29(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateTotalBalancesOnInsertConstructor_r28(uint n) private    {
      totalBalances = TotalBalancesTuple(n,true);
  }
  function updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r8(address o,address s,uint n) private    {
      decreaseAllowanceTotal[o][s].m += n;
  }
  function updateTotalInOnInsertTransfer_r30(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r7(p,delta0);
  }
  function updateDecreaseAllowanceOnInsertRecv_decreaseAllowance_r24(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r8(o,s,n);
      emit DecreaseAllowance(o,s,n);
      return true;
      return false;
  }
  function updateTotalBurnOnInsertBurn_r16(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r7(p,delta0);
  }
  function updateSpentTotalOnInsertTransferFrom_r9(address o,address s,uint n) private    {
      spentTotal[o][s].m += n;
  }
  function updateTransferOnInsertTransferFrom_r12(address o,address r,uint n) private    {
      updateTotalInOnInsertTransfer_r30(r,n);
      updateTotalOutOnInsertTransfer_r22(o,n);
      emit Transfer(o,r,n);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r26(address o,address r,uint n) private   returns (bool) {
      address s = msg.sender;
      uint m = balanceOf[o].n;
      uint k = allowance(o,s);
      if(m>=n && k>=n) {
        updateSpentTotalOnInsertTransferFrom_r9(o,s,n);
        updateTransferOnInsertTransferFrom_r12(o,r,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateBurnOnInsertRecv_burn_r5(uint n) private   returns (bool) {
      address s = msg.sender;
      uint m = balanceOf[s].n;
      if(s!=address(0) && n<=m) {
        updateTotalBurnOnInsertBurn_r16(s,n);
        updateAllBurnOnInsertBurn_r27(n);
        emit Burn(s,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r7(address p,int o) private    {
      int _delta = int(-o);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateTotalOutOnInsertTransfer_r22(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r7(p,delta0);
  }
  function updateIncreaseAllowanceTotalOnInsertIncreaseAllowance_r29(address o,address s,uint n) private    {
      increaseAllowanceTotal[o][s].m += n;
  }
  function updateBalanceOfOnInsertConstructor_r0(uint n,address p) private    {
      balanceOf[p] = BalanceOfTuple(n,true);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r11(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      updateIncreaseAllowanceTotalOnInsertIncreaseAllowance_r29(o,s,n);
      emit IncreaseAllowance(o,s,n);
      return true;
      return false;
  }
  function updateOwnerOnInsertConstructor_r15() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateBalanceOfOnIncrementTotalBurn_r7(address p,int m) private    {
      int _delta = int(-m);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateDecreaseAllowanceOnInsertRecv_approve_r13(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      uint m = allowance(o,s);
      if(n<m) {
        uint d = m-n;
        updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r8(o,s,d);
        emit DecreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalIn_r7(address p,int i) private    {
      int _delta = int(i);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateAllBurnOnInsertBurn_r27(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r17(delta0);
  }
}