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
    updateOwnerOnInsertConstructor_r4();
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
  function transfer(address from,address to,int amount) public    {
      bool r11 = updateTransferOnInsertRecv_transfer_r11(from,to,amount);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r14 = updateMintOnInsertRecv_mint_r14(p,amount);
      if(r14==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function updateTransferOnInsertRecv_transfer_r11(address s,address r,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[r].n;
      if(0!=balanceOf_x1) {
        updateTotalInOnInsertTransfer_r7(r,n);
        updateTotalOutOnInsertTransfer_r16(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateAllBurnOnInsertBurn_r9(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r10(delta0);
  }
  function updateBalanceOfOnIncrementTotalIn_r1(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTransferOnInsertMint_r8(address p,int n) private    {
      updateTotalOutOnInsertTransfer_r16(address(0),n);
      updateTotalInOnInsertTransfer_r7(p,n);
      emit Transfer(address(0),p,n);
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
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r10(delta0);
  }
  function updateOwnerOnInsertConstructor_r4() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTotalOutOnInsertTransfer_r16(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r1(p,delta0);
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
  function updateTotalInOnInsertTransfer_r7(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r1(p,delta0);
  }
  function updateBurnOnInsertRecv_burn_r2(address p,int n) private   returns (bool) {
      address msgSender = msg.sender;
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[msgSender].n;
      if(o==s && 0!=balanceOf_x1) {
        updateTransferOnInsertBurn_r5(p,n);
        updateAllBurnOnInsertBurn_r9(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateMintOnInsertRecv_mint_r14(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && 0!=balanceOf_x1) {
        updateTransferOnInsertMint_r8(p,n);
        updateAllMintOnInsertMint_r0(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertBurn_r5(address p,int n) private    {
      updateTotalOutOnInsertTransfer_r16(p,n);
      updateTotalInOnInsertTransfer_r7(address(0),n);
      emit Transfer(p,address(0),n);
  }
}