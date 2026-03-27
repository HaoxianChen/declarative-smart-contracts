contract Shib {
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
  event Transfer(address s,address r,int n);
  event Mint(address p,int amount);
  event UnauthorizedMint();
  event UnauthorizedBurn();
  event Burn(address p,int amount);
  event TransferFrom(address o,address r,address s,int n);
  event BurnFrom(address p,address from,int n);
  event IncreaseAllowance(address p,address s,int d);
  constructor() public {
    updateOwnerOnInsertConstructor_r21();
    updateTotalSupplyOnInsertConstructor_r3();
  }
  function transfer(address s,address r,int n) public    {
      bool r13 = updateTransferOnInsertRecv_transfer_r13(s,r,n);
      if(r13==false) {
        revert("Rule condition failed");
      }
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
  function increaseAllowance(address p,address s,int d) public    {
      bool r15 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r15(p,s,d);
      if(r15==false) {
        revert("Rule condition failed");
      }
  }
  function burnFrom(address p,address from,int n) public    {
      bool r16 = updateBurnFromOnInsertRecv_burnFrom_r16(p,from,n);
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r4 = updateBurnOnInsertRecv_burn_r4(p,amount);
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address o,address r,address s,int n) public    {
      bool r2 = updateTransferFromOnInsertRecv_transferFrom_r2(o,r,s,n);
      if(r2==false) {
        revert("Rule condition failed");
      }
  }
  function updateOwnerOnInsertConstructor_r21() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTotalInOnInsertTransfer_r17(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r19(p,delta0);
  }
  function updateBurnFromOnInsertRecv_burnFrom_r16(address p,address from,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(n<=balanceOf_x1) {
        updateTransferFromOnInsertBurnFrom_r7(p,from,n);
        updateBurnOnInsertBurnFrom_r5(from,n);
        emit BurnFrom(p,from,n);
        return true;
      }
      return false;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r2(address o,address r,address s,int n) private   returns (bool) {
      int balanceOf_x1_2 = balanceOf[o].n;
      int allowance_x2_1 = allowance[o][s].n;
      if(n>0 && n<=allowance_x2_1 && n<=balanceOf_x1_2) {
        updateTransferOnInsertTransferFrom_r18(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r14(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalBurn_r19(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r24(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateBalanceOfOnIncrementTotalMint_r19(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateAllBurnOnInsertBurn_r11(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r12(delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateMintOnInsertRecv_mint_r23(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(s==o && amount>0) {
        updateTotalMintOnInsertMint_r20(p,amount);
        updateAllMintOnInsertMint_r0(amount);
        emit Mint(p,amount);
        return true;
      }
      return false;
  }
  function updateBurnOnInsertRecv_burn_r4(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(s==o && amount<=balanceOf_x1) {
        updateAllBurnOnInsertBurn_r11(amount);
        updateTotalBurnOnInsertBurn_r8(p,amount);
        emit Burn(p,amount);
        return true;
      }
      return false;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r15(address p,address s,int d) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r22(p,s,d);
        emit IncreaseAllowance(p,s,d);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertTransferFrom_r18(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r17(r,n);
      updateTotalOutOnInsertTransfer_r25(o,n);
      emit Transfer(o,r,n);
  }
  function updateBalanceOfOnIncrementTotalOut_r19(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateTotalOutOnInsertTransfer_r25(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r19(p,delta0);
  }
  function updateSpentTotalOnInsertTransferFrom_r14(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r24(o,s,delta0);
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r12(delta0);
  }
  function updateTotalBurnOnInsertBurn_r8(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r19(p,delta0);
  }
  function updateTotalSupplyOnIncrementAllMint_r12(int m) private    {
      totalSupply.n += m;
  }
  function updateAllowanceOnIncrementSpentTotal_r24(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTotalSupplyOnIncrementAllBurn_r12(int b) private    {
      totalSupply.n -= b;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTotalMintOnInsertMint_r20(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r19(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalIn_r19(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTransferFromOnInsertBurnFrom_r7(address s,address p,int n) private    {
      updateSpentTotalOnInsertTransferFrom_r14(s,address(0),n);
      updateTransferOnInsertTransferFrom_r18(s,p,n);
      emit TransferFrom(s,p,address(0),n);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r22(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r24(o,s,delta0);
  }
  function updateTotalSupplyOnInsertConstructor_r3() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateTransferOnInsertRecv_transfer_r13(address s,address r,int n) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[s].n;
      if(n>0 && n<=balanceOf_x1_1) {
        updateTotalInOnInsertTransfer_r17(r,n);
        updateTotalOutOnInsertTransfer_r25(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateBurnOnInsertBurnFrom_r5(address p,int n) private    {
      updateTotalBurnOnInsertBurn_r8(p,n);
      updateAllBurnOnInsertBurn_r11(n);
      emit Burn(p,n);
  }
}