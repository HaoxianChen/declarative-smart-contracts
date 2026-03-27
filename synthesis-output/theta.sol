contract Theta {
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
  event UnauthorizedAllowPrecirculation();
  event Mint(address p,int amount);
  event UnauthorizedDisallowPrecirculation();
  event Burn(address p,int amount);
  event AllowPrecirculation(address p);
  event IncreaseAllowance(address p,address s,int d);
  event DisallowPrecirculation(address p);
  event TransferFrom(address from,address to,address spender,int amount);
  constructor(uint t) public {
    updateTotalBalancesOnInsertConstructor_r3();
    updateUnlockTimeOnInsertConstructor_r8(t);
    updateOwnerOnInsertConstructor_r1();
    updateTotalSupplyOnInsertConstructor_r15();
  }
  function disallowPrecirculation(address p) public    {
      bool r2 = updateDisallowPrecirculationOnInsertRecv_disallowPrecirculation_r2(p);
      if(r2==false) {
        revert("Rule condition failed");
      }
  }
  function allowPrecirculation(address p) public    {
      bool r4 = updateAllowPrecirculationOnInsertRecv_allowPrecirculation_r4(p);
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r30 = updateTransferOnInsertRecv_transfer_r30(from,to,amount);
      if(r30==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int d) public    {
      bool r6 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r6(p,s,d);
      if(r6==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r13 = updateBurnOnInsertRecv_burn_r13(p,amount);
      if(r13==false) {
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
  function mint(address p,int amount) public    {
      bool r12 = updateMintOnInsertRecv_mint_r12(p,amount);
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r19 = updateTransferFromOnInsertRecv_transferFrom_r19(from,to,spender,amount);
      if(r19==false) {
        revert("Rule condition failed");
      }
  }
  function updateTotalBalancesOnInsertConstructor_r3() private    {
      // Empty()
  }
  function updateAllowanceOnIncrementAllowanceTotal_r11(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateOwnerOnInsertConstructor_r1() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateMintOnInsertRecv_mint_r12(address p,int amount) private   returns (bool) {
      if(amount>0) {
        updateAllMintOnInsertMint_r14(amount);
        updateTotalMintOnInsertMint_r23(p,amount);
        emit Mint(p,amount);
        return true;
      }
      return false;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r6(address p,address s,int d) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r24(p,s,d);
        emit IncreaseAllowance(p,s,d);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalBurn_r9(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r19(address from,address to,address spender,int amount) private   returns (bool) {
      int balanceOf_x1_2 = balanceOf[from].n;
      int allowance_x2_1 = allowance[from][spender].n;
      if(amount>0 && amount<=allowance_x2_1 && amount<balanceOf_x1_2) {
        updateTransferOnInsertTransferFrom_r10(from,to,amount);
        updateSpentTotalOnInsertTransferFrom_r26(from,spender,amount);
        emit TransferFrom(from,to,spender,amount);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementSpentTotal_r11(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateUnlockTimeOnInsertConstructor_r8(uint t) private    {
      // Empty()
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTotalInOnInsertTransfer_r17(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r9(p,delta0);
  }
  function updateTransferOnInsertTransferFrom_r10(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r17(r,n);
      updateTotalOutOnInsertTransfer_r28(o,n);
      emit Transfer(o,r,n);
  }
  function updateDisallowPrecirculationOnInsertRecv_disallowPrecirculation_r2(address p) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(s==o) {
        emit DisallowPrecirculation(p);
        return true;
      }
      return false;
  }
  function updateTotalMintOnInsertMint_r23(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r9(p,delta0);
  }
  function updateTotalSupplyOnIncrementAllMint_r22(int m) private    {
      totalSupply.n += m;
  }
  function updateTotalOutOnInsertTransfer_r28(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r9(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalMint_r9(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateAllBurnOnInsertBurn_r21(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r22(delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r24(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r11(o,s,delta0);
  }
  function updateAllowPrecirculationOnInsertRecv_allowPrecirculation_r4(address p) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(s==o) {
        emit AllowPrecirculation(p);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r15() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateAllMintOnInsertMint_r14(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r22(delta0);
  }
  function updateBalanceOfOnIncrementTotalIn_r9(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTransferOnInsertRecv_transfer_r30(address from,address to,int amount) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[from].n;
      if(amount>0 && amount<=balanceOf_x1_1) {
        updateTotalInOnInsertTransfer_r17(to,amount);
        updateTotalOutOnInsertTransfer_r28(from,amount);
        emit Transfer(from,to,amount);
        return true;
      }
      return false;
  }
  function updateSpentTotalOnInsertTransferFrom_r26(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r11(o,s,delta0);
  }
  function updateTotalSupplyOnIncrementAllBurn_r22(int b) private    {
      totalSupply.n -= b;
  }
  function updateBalanceOfOnIncrementTotalOut_r9(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateTotalBurnOnInsertBurn_r0(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r9(p,delta0);
  }
  function updateBurnOnInsertRecv_burn_r13(address p,int amount) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(amount<=balanceOf_x1) {
        updateAllBurnOnInsertBurn_r21(amount);
        updateTotalBurnOnInsertBurn_r0(p,amount);
        emit Burn(p,amount);
        return true;
      }
      return false;
  }
}