contract Sushi {
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
  event IncreaseAllowance(address o,address s,int n);
  event Transfer(address from,address to,int amount);
  event InvalidTx();
  constructor() public {
    updateTotalSupplyOnInsertConstructor_r15();
    updateOwnerOnInsertConstructor_r17();
  }
  function mint(address p,int amount) public    {
      bool r2 = updateMintOnInsertRecv_mint_r2(p,amount);
      if(r2==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r18 = updateTransferOnInsertRecv_transfer_r18(from,to,amount);
      if(r18==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r14 = updateTransferFromOnInsertRecv_transferFrom_r14(from,to,spender,amount);
      if(r14==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function increaseAllowance(address o,address s,int n) public    {
      bool r1 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r1(o,s,n);
      if(r1==false) {
        revert("Rule condition failed");
      }
  }
  function updateAllowanceOnIncrementAllowanceTotal_r7(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateBalanceOfOnIncrementTotalMint_r10(address p,int m) private    {
      balanceOf[p].n += m;
  }
  function updateTotalOutOnInsertTransfer_r8(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r10(p,delta0);
  }
  function updateMintOnInsertRecv_mint_r2(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>=0) {
        updateAllMintOnInsertMint_r9(n);
        updateTotalMintOnInsertMint_r16(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r15() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateBalanceOfOnIncrementTotalOut_r10(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateBalanceOfOnIncrementTotalIn_r10(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r14(address o,address r,address s,int n) private   returns (bool) {
      int allowance_x2 = allowance[o][s].n;
      if(n<allowance_x2) {
        updateSpentTotalOnInsertTransferFrom_r6(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateAllMintOnInsertMint_r9(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r12(delta0);
  }
  function updateOwnerOnInsertConstructor_r17() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTotalInOnInsertTransfer_r0(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r10(p,delta0);
  }
  function updateTransferOnInsertRecv_transfer_r18(address s,address r,int n) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[s].n;
      if(n>=0 && n<=balanceOf_x1_1) {
        updateTotalInOnInsertTransfer_r0(r,n);
        updateTotalOutOnInsertTransfer_r8(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r5(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r7(o,s,delta0);
  }
  function updateSpentTotalOnInsertTransferFrom_r6(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r7(o,s,delta0);
  }
  function updateTotalSupplyOnIncrementAllMint_r12(int m) private    {
      totalSupply.n += m;
  }
  function updateTotalMintOnInsertMint_r16(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r10(p,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r1(address o,address s,int d) private   returns (bool) {
      if(d>=0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r5(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementSpentTotal_r7(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
}