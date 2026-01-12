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
  event Mint(address p,int amount);
  event Burn(address p,int amount);
  event Transfer(address from,address to,int amount);
  event InvalidTx();
  constructor() public {
    updateOwnerOnInsertConstructor_r5();
  }
  function burn(address p,int amount) public    {
      bool r6 = updateBurnOnInsertRecv_burn_r6(p,amount);
      if(r6==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function mint(address p,int amount) public    {
      bool r1 = updateMintOnInsertRecv_mint_r1(p,amount);
      if(r1==false) {
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
  function updateAllBurnOnInsertBurn_r11(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r12(delta0);
  }
  function updateOwnerOnInsertConstructor_r5() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTransferOnInsertRecv_transfer_r13(address from,address to,int amount) private   returns (bool) {
      if(0==n) {
        updateTotalOutOnInsertTransfer_r14(s,n);
        updateTotalInOnInsertTransfer_r9(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r12(int m) private    {
      totalSupply.n += m;
  }
  function updateMintOnInsertRecv_mint_r1(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>=0) {
        updateTransferOnInsertMint_r10(p,n);
        updateAllMintOnInsertMint_r0(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertMint_r10(address p,int n) private    {
      updateTotalInOnInsertTransfer_r9(p,n);
      updateTotalOutOnInsertTransfer_r14(address(0),n);
      emit Transfer(address(0),p,n);
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r12(delta0);
  }
  function updateBalanceOfOnIncrementTotalOut_r2(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateBurnOnInsertRecv_burn_r6(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && 0==n) {
        updateTransferOnInsertBurn_r7(p,n);
        updateAllBurnOnInsertBurn_r11(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateTotalOutOnInsertTransfer_r14(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r2(p,delta0);
  }
  function updateTransferOnInsertBurn_r7(address p,int n) private    {
      updateTotalOutOnInsertTransfer_r14(p,n);
      updateTotalInOnInsertTransfer_r9(address(0),n);
      emit Transfer(p,address(0),n);
  }
  function updateBalanceOfOnIncrementTotalIn_r2(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTotalInOnInsertTransfer_r9(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r2(p,delta0);
  }
  function updateTotalSupplyOnIncrementAllBurn_r12(int b) private    {
      totalSupply.n -= b;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
}