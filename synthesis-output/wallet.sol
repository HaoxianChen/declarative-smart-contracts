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
    updateOwnerOnInsertConstructor_r23();
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
  function mint(address p,int amount) public    {
      bool r21 = updateMintOnInsertRecv_mint_r21(p,amount);
      if(r21==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r19 = updateTransferOnInsertRecv_transfer_r19(from,to,amount);
      if(r19==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function updateMintOnInsertRecv_mint_r21(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(amount>0 && p!=address(0) && o_1==s_1) {
        updateTransferOnInsertMint_r15(p,amount);
        updateAllMintOnInsertMint_r10(amount);
        emit Mint(p,amount);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r17(int m) private    {
      totalSupply.n += m;
  }
  function updateTotalSupplyOnIncrementAllBurn_r17(int b) private    {
      totalSupply.n -= b;
  }
  function updateBalanceOfOnIncrementTotalIn_r1(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTransferOnInsertRecv_transfer_r19(address from,address to,int amount) private   returns (bool) {
      int m_1 = balanceOf[from].n;
      if(to!=address(0) && from!=address(0) && amount>0 && amount<=m_1) {
        updateTotalOutOnInsertTransfer_r6(from,amount);
        updateTotalInOnInsertTransfer_r12(to,amount);
        emit Transfer(from,to,amount);
        return true;
      }
      return false;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateAllMintOnInsertMint_r10(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r17(delta0);
  }
  function updateBalanceOfOnIncrementTotalOut_r1(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateTotalInOnInsertTransfer_r12(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r1(p,delta0);
  }
  function updateAllBurnOnInsertBurn_r16(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r17(delta0);
  }
  function updateOwnerOnInsertConstructor_r23() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTransferOnInsertMint_r15(address p,int n) private    {
      updateTotalOutOnInsertTransfer_r6(address(0),n);
      updateTotalInOnInsertTransfer_r12(p,n);
      emit Transfer(address(0),p,n);
  }
  function updateBurnOnInsertRecv_burn_r4(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      int m_2 = balanceOf[p].n;
      if(p!=address(0) && amount>0 && o_1==s_1 && amount<=m_2) {
        updateAllBurnOnInsertBurn_r16(amount);
        updateTransferOnInsertBurn_r7(p,amount);
        emit Burn(p,amount);
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
  function updateTransferOnInsertBurn_r7(address p,int n) private    {
      updateTotalInOnInsertTransfer_r12(address(0),n);
      updateTotalOutOnInsertTransfer_r6(p,n);
      emit Transfer(p,address(0),n);
  }
  function updateTotalOutOnInsertTransfer_r6(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r1(p,delta0);
  }
}