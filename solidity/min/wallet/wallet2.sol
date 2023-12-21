contract Wallet {
  struct AllBurnTuple {
    int n;
    bool _valid;
  }
  struct BalanceOfTuple {
    int n;
    bool _valid;
  }
  struct AllMintTuple {
    int n;
    bool _valid;
  }
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  AllBurnTuple allBurn;
  mapping(address=>BalanceOfTuple) balanceOf;
  AllMintTuple allMint;
  OwnerTuple owner;
  event Transfer(address from,address to,int amount);
  event Mint(address p,int amount);
  event Burn(address p,int amount);
  constructor() public {
    updateOwnerOnInsertConstructor_r2();
  }
  function transfer(address from,address to,int amount) public    {
      bool r12 = updateTransferOnInsertRecv_transfer_r12(from,to,amount);
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply();
      return n;
  }
  function burn(address p,int amount) public    {
      bool r3 = updateBurnOnInsertRecv_burn_r3(p,amount);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function mint(address p,int amount) public    {
      bool r9 = updateMintOnInsertRecv_mint_r9(p,amount);
      if(r9==false) {
        revert("Rule condition failed");
      }
  }
  function updateTotalOutOnInsertTransfer_r4(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r1(p,delta0);
  }
  function updateTransferOnInsertRecv_transfer_r12(address s,address r,int n) private   returns (bool) {
      int m = balanceOf[s].n;
      if(m>=n && n>0) {
        updateTotalInOnInsertTransfer_r6(r,n);
        updateTotalOutOnInsertTransfer_r4(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateMintOnInsertRecv_mint_r9(address p,int n) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        if(n>0 && p!=address(0)) {
          updateTransferOnInsertMint_r10(p,n);
          updateAllMintOnInsertMint_r0(n);
          emit Mint(p,n);
          return true;
        }
      }
      return false;
  }
  function updateTransferOnInsertBurn_r11(address p,int n) private    {
      updateTotalOutOnInsertTransfer_r4(p,n);
      updateTotalInOnInsertTransfer_r6(address(0),n);
      emit Transfer(p,address(0),n);
  }
  function updateBalanceOfOnIncrementTotalIn_r1(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateOwnerOnInsertConstructor_r2() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function totalSupply() private view  returns (int) {
      int b = allBurn.n;
      int m = allMint.n;
      int n = m-b;
      return n;
  }
  function updateAllBurnOnInsertBurn_r7(int n) private    {
      allBurn.n += n;
  }
  function updateBalanceOfOnIncrementTotalOut_r1(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateTotalInOnInsertTransfer_r6(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r1(p,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      allMint.n += n;
  }
  function updateBurnOnInsertRecv_burn_r3(address p,int n) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        int m = balanceOf[p].n;
        if(p!=address(0) && n<=m) {
          updateAllBurnOnInsertBurn_r7(n);
          updateTransferOnInsertBurn_r11(p,n);
          emit Burn(p,n);
          return true;
        }
      }
      return false;
  }
  function updateTransferOnInsertMint_r10(address p,int n) private    {
      updateTotalOutOnInsertTransfer_r4(address(0),n);
      updateTotalInOnInsertTransfer_r6(p,n);
      emit Transfer(address(0),p,n);
  }
}