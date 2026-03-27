contract Wallet {
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
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalSupplyTuple totalSupply;
  OwnerTuple owner;
  event Transfer(address from,address to,int amount);
  event Mint(address p,int amount);
  event UnauthorizedMint();
  event UnauthorizedBurn();
  event Burn(address p,int amount);
  constructor() public {
    updateOwnerOnInsertConstructor_r8();
  }
  function mint(address p,int amount) public    {
      bool r7 = updateMintOnInsertRecv_mint_r7(p,amount);
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r5 = updateTransferOnInsertRecv_transfer_r5(from,to,amount);
      if(r5==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function burn(address p,int amount) public    {
      bool r4 = updateBurnOnInsertRecv_burn_r4(p,amount);
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function updateBurnOnInsertRecv_burn_r4(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(s==o && amount<=balanceOf_x1) {
        updateAllBurnOnInsertBurn_r15(amount);
        updateTransferOnInsertBurn_r10(p,amount);
        emit Burn(p,amount);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r16(int m) private    {
      totalSupply.n += m;
  }
  function updateTotalInOnInsertTransfer_r12(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r3(p,delta0);
  }
  function updateTransferOnInsertBurn_r10(address p,int n) private    {
      updateTotalInOnInsertTransfer_r12(address(0),n);
      updateTotalOutOnInsertTransfer_r9(p,n);
      emit Transfer(p,address(0),n);
  }
  function updateTotalSupplyOnIncrementAllBurn_r16(int b) private    {
      totalSupply.n -= b;
  }
  function updateTransferOnInsertRecv_transfer_r5(address from,address to,int amount) private   returns (bool) {
      updateTotalInOnInsertTransfer_r12(to,amount);
      updateTotalOutOnInsertTransfer_r9(from,amount);
      emit Transfer(from,to,amount);
      return true;
      return false;
  }
  function updateBalanceOfOnIncrementTotalIn_r3(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateOwnerOnInsertConstructor_r8() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateMintOnInsertRecv_mint_r7(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(s==o) {
        updateTransferOnInsertMint_r14(p,amount);
        updateAllMintOnInsertMint_r0(amount);
        emit Mint(p,amount);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r3(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateAllBurnOnInsertBurn_r15(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r16(delta0);
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r16(delta0);
  }
  function updateTotalOutOnInsertTransfer_r9(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r3(p,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTransferOnInsertMint_r14(address p,int n) private    {
      updateTotalOutOnInsertTransfer_r9(address(0),n);
      updateTotalInOnInsertTransfer_r12(p,n);
      emit Transfer(address(0),p,n);
  }
}