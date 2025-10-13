contract Wallet {
  struct BalanceOfTuple {
    int n;
    bool _valid;
  }
  struct TotalSupplyTuple {
    int n;
    bool _valid;
  }
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalSupplyTuple totalSupply;
  event Transfer(address from,address to,int amount);
  event Mint(address p,int amount);
  event Burn(address p,int amount);
  constructor() public {
    updateOwnerOnInsertConstructor_r2();
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
  function transfer(address from,address to,int amount) public    {
      bool r6 = updateTransferOnInsertRecv_transfer_r6(from,to,amount);
      if(r6==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r11 = updateMintOnInsertRecv_mint_r11(p,amount);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function updateOwnerOnInsertConstructor_r2() private    {
      address s = msg.sender;
      // Empty()
  }
  function updateMintOnInsertRecv_mint_r11(address p,int n) private   returns (bool) {
      if(n>=0) {
        updateTransferOnInsertMint_r8(p,n);
        updateAllMintOnInsertMint_r0(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertBurn_r3(address p,int n) private    {
      updateTotalOutOnInsertTransfer_r12(p,n);
      updateTotalInOnInsertTransfer_r7(address(0),n);
      emit Transfer(p,address(0),n);
  }
  function updateBurnOnInsertRecv_burn_r4(address p,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(n<=balanceOf_x1) {
        updateTransferOnInsertBurn_r3(p,n);
        updateAllBurnOnInsertBurn_r9(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateAllBurnOnInsertBurn_r9(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r10(delta0);
  }
  function updateTransferOnInsertMint_r8(address p,int n) private    {
      updateTotalOutOnInsertTransfer_r12(address(0),n);
      updateTotalInOnInsertTransfer_r7(p,n);
      emit Transfer(address(0),p,n);
  }
  function updateBalanceOfOnIncrementTotalIn_r1(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTotalInOnInsertTransfer_r7(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r1(p,delta0);
  }
  function updateTotalOutOnInsertTransfer_r12(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r1(p,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTotalSupplyOnIncrementAllMint_r10(int m) private    {
      totalSupply.n += m;
  }
  function updateBalanceOfOnIncrementTotalOut_r1(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateTransferOnInsertRecv_transfer_r6(address s,address r,int n) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[s].n;
      if(n>0 && n<balanceOf_x1_1) {
        updateTotalInOnInsertTransfer_r7(r,n);
        updateTotalOutOnInsertTransfer_r12(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r10(delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalSupplyOnIncrementAllBurn_r10(int b) private    {
      totalSupply.n -= b;
  }
}