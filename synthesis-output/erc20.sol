contract Erc20 {
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
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalSupplyTuple totalSupply;
  OwnerTuple owner;
  event Transfer(address from,address to,int amount);
  event Mint(address p,int amount);
  event TransferFrom(address from,address to,address spender,int amount);
  event UnauthorizedMint();
  event UnauthorizedBurn();
  event Burn(address p,int amount);
  event IncreaseAllowance(address p,address s,int n);
  constructor() public {
    updateOwnerOnInsertConstructor_r19();
    updateTotalBalancesOnInsertConstructor_r16();
    updateTotalSupplyOnInsertConstructor_r1();
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r8 = updateTransferFromOnInsertRecv_transferFrom_r8(from,to,spender,amount);
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r2 = updateBurnOnInsertRecv_burn_r2(p,amount);
      if(r2==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function mint(address p,int amount) public    {
      bool r22 = updateMintOnInsertRecv_mint_r22(p,amount);
      if(r22==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r6 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r6(p,s,n);
      if(r6==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function transfer(address from,address to,int amount) public    {
      bool r7 = updateTransferOnInsertRecv_transfer_r7(from,to,amount);
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function updateTotalSupplyOnIncrementAllBurn_r15(int b) private    {
      totalSupply.n -= b;
  }
  function updateMintOnInsertRecv_mint_r22(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(s==o && amount>0) {
        updateAllMintOnInsertMint_r0(amount);
        updateTotalMintOnInsertMint_r18(p,amount);
        emit Mint(p,amount);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementSpentTotal_r23(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateBurnOnInsertRecv_burn_r2(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(s==o && amount<=balanceOf_x1) {
        updateTotalBurnOnInsertBurn_r11(p,amount);
        updateAllBurnOnInsertBurn_r14(amount);
        emit Burn(p,amount);
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
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r6(address p,address s,int n) private   returns (bool) {
      if(n>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r17(p,s,n);
        emit IncreaseAllowance(p,s,n);
        return true;
      }
      return false;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r8(address from,address to,address spender,int amount) private   returns (bool) {
      int balanceOf_x1_2 = balanceOf[from].n;
      int allowance_x2_1 = allowance[from][spender].n;
      if(amount>0 && amount<=allowance_x2_1 && amount<=balanceOf_x1_2) {
        updateSpentTotalOnInsertTransferFrom_r21(from,spender,amount);
        updateTransferOnInsertTransferFrom_r4(from,to,amount);
        emit TransferFrom(from,to,spender,amount);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalIn_r3(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTotalSupplyOnInsertConstructor_r1() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateTransferOnInsertTransferFrom_r4(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r10(r,n);
      updateTotalOutOnInsertTransfer_r25(o,n);
      emit Transfer(o,r,n);
  }
  function updateSpentTotalOnInsertTransferFrom_r21(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r23(o,s,delta0);
  }
  function updateTotalBalancesOnInsertConstructor_r16() private    {
      // Empty()
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r15(delta0);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r23(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateAllBurnOnInsertBurn_r14(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r15(delta0);
  }
  function updateBalanceOfOnIncrementTotalOut_r3(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateOwnerOnInsertConstructor_r19() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTotalMintOnInsertMint_r18(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r3(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalBurn_r3(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r17(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r23(o,s,delta0);
  }
  function updateTotalInOnInsertTransfer_r10(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r3(p,delta0);
  }
  function updateTotalOutOnInsertTransfer_r25(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r3(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalMint_r3(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateTransferOnInsertRecv_transfer_r7(address from,address to,int amount) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[from].n;
      if(amount>0 && amount<=balanceOf_x1_1) {
        updateTotalOutOnInsertTransfer_r25(from,amount);
        updateTotalInOnInsertTransfer_r10(to,amount);
        emit Transfer(from,to,amount);
        return true;
      }
      return false;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTotalBurnOnInsertBurn_r11(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r3(p,delta0);
  }
  function updateTotalSupplyOnIncrementAllMint_r15(int m) private    {
      totalSupply.n += m;
  }
}