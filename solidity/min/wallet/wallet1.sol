contract Wallet {
  struct AllBurnTuple {
    int n;
    bool _valid;
  }
  struct AllMintTuple {
    int n;
    bool _valid;
  }
  struct TotalOutTuple {
    int n;
    bool _valid;
  }
  struct TotalInTuple {
    int n;
    bool _valid;
  }
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  AllBurnTuple allBurn;
  AllMintTuple allMint;
  mapping(address=>TotalOutTuple) totalOut;
  mapping(address=>TotalInTuple) totalIn;
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
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf(p);
      return n;
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply();
      return n;
  }
  function mint(address p,int amount) public    {
      bool r9 = updateMintOnInsertRecv_mint_r9(p,amount);
      if(r9==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r3 = updateBurnOnInsertRecv_burn_r3(p,amount);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function updateTotalInOnInsertTransfer_r6(address p,int n) private    {
      totalIn[p].n += n;
  }
  function updateTotalOutOnInsertTransfer_r4(address p,int n) private    {
      totalOut[p].n += n;
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
  function updateBurnOnInsertRecv_burn_r3(address p,int n) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        int m = balanceOf(p);
        if(p!=address(0) && n<=m) {
          updateAllBurnOnInsertBurn_r7(n);
          updateTransferOnInsertBurn_r11(p,n);
          emit Burn(p,n);
          return true;
        }
      }
      return false;
  }
  function totalSupply() private view  returns (int) {
      int b = allBurn.n;
      int m = allMint.n;
      int n = m-b;
      return n;
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      allMint.n += n;
  }
  function updateAllBurnOnInsertBurn_r7(int n) private    {
      allBurn.n += n;
  }
  function updateTransferOnInsertMint_r10(address p,int n) private    {
      updateTotalOutOnInsertTransfer_r4(address(0),n);
      updateTotalInOnInsertTransfer_r6(p,n);
      emit Transfer(address(0),p,n);
  }
  function updateTransferOnInsertRecv_transfer_r12(address s,address r,int n) private   returns (bool) {
      int m = balanceOf(s);
      if(m>=n && n>0) {
        updateTotalInOnInsertTransfer_r6(r,n);
        updateTotalOutOnInsertTransfer_r4(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function balanceOf(address p) private view  returns (int) {
      int i = totalIn[p].n;
      int o = totalOut[p].n;
      int s = i-o;
      return s;
  }
}