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
  event Mint(address p,int amount);
  event TransferFrom(address from,address to,address spender,int amount);
  event Burn(address p,int amount);
  event IncreaseAllowance(address p,address s,int n);
  event Transfer(address from,address to,int amount);
  event InvalidTx();
  constructor() public {
    updateTotalBalancesOnInsertConstructor_r17();
    updateOwnerOnInsertConstructor_r20();
    updateTotalSupplyOnInsertConstructor_r2();
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r5 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r5(p,s,n);
      if(r5==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r10 = updateTransferFromOnInsertRecv_transferFrom_r10(from,to,spender,amount);
      if(r10==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r11 = updateMintOnInsertRecv_mint_r11(p,amount);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function transfer(address from,address to,int amount) public    {
      bool r23 = updateTransferOnInsertRecv_transfer_r23(from,to,amount);
      if(r23==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r7 = updateBurnOnInsertRecv_burn_r7(p,amount);
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r14(delta0);
  }
  function updateBalanceOfOnIncrementTotalMint_r4(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateTotalBalancesOnInsertConstructor_r17() private    {
      // Empty()
  }
  function updateTotalBurnOnInsertBurn_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r4(p,delta0);
  }
  function updateTotalSupplyOnInsertConstructor_r2() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateAllowanceOnIncrementSpentTotal_r6(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateOwnerOnInsertConstructor_r20() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTotalOutOnInsertTransfer_r25(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r4(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalIn_r4(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateBalanceOfOnIncrementTotalBurn_r4(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r18(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r6(o,s,delta0);
  }
  function updateTotalInOnInsertTransfer_r9(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r4(p,delta0);
  }
  function updateTransferOnInsertTransferFrom_r15(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r9(r,n);
      updateTotalOutOnInsertTransfer_r25(o,n);
      emit Transfer(o,r,n);
  }
  function updateTotalSupplyOnIncrementAllMint_r14(int m) private    {
      totalSupply.n += m;
  }
  function updateBalanceOfOnIncrementTotalOut_r4(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateTotalMintOnInsertMint_r19(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r4(p,delta0);
  }
  function updateTransferOnInsertRecv_transfer_r23(address from,address to,int amount) private   returns (bool) {
      if(0==n) {
        updateTotalOutOnInsertTransfer_r25(s,n);
        updateTotalInOnInsertTransfer_r9(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateAllBurnOnInsertBurn_r13(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r14(delta0);
  }
  function updateMintOnInsertRecv_mint_r11(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>=0) {
        updateTotalMintOnInsertMint_r19(p,n);
        updateAllMintOnInsertMint_r0(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r10(address from,address to,address spender,int amount) private   returns (bool) {
      if(0==n) {
        updateTransferOnInsertTransferFrom_r15(o,r,n);
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
  function updateAllowanceOnIncrementAllowanceTotal_r6(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateBurnOnInsertRecv_burn_r7(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && 0==n) {
        updateTotalBurnOnInsertBurn_r1(p,n);
        updateAllBurnOnInsertBurn_r13(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r5(address p,address s,int n) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r18(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r14(int b) private    {
      totalSupply.n -= b;
  }
  function updateSpentTotalOnInsertTransferFrom_r22(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r6(o,s,delta0);
  }
}