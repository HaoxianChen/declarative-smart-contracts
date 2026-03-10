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
    updateOwnerOnInsertConstructor_r35();
    updateTotalSupplyOnInsertConstructor_r15();
    updateTotalBalancesOnInsertConstructor_r28();
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r16 = updateTransferFromOnInsertRecv_transferFrom_r16(from,to,spender,amount);
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r27 = updateTransferOnInsertRecv_transfer_r27(from,to,amount);
      if(r27==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function burn(address p,int amount) public    {
      bool r22 = updateBurnOnInsertRecv_burn_r22(p,amount);
      if(r22==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r20 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r20(p,s,n);
      if(r20==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r31 = updateMintOnInsertRecv_mint_r31(p,amount);
      if(r31==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function updateAllMintOnInsertMint_r9(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r19(delta0);
  }
  function updateTotalMintOnInsertMint_r33(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r23(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalOut_r23(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateMintOnInsertRecv_mint_r31(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(amount>0 && p!=address(0) && o_1==s_1) {
        updateTotalMintOnInsertMint_r33(p,amount);
        updateAllMintOnInsertMint_r9(amount);
        emit Mint(p,amount);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r15() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateSpentTotalOnInsertTransferFrom_r37(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r5(o,s,delta0);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r20(address p,address s,int n) private   returns (bool) {
      if(n>=0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r32(p,s,n);
        emit IncreaseAllowance(p,s,n);
        return true;
      }
      return false;
  }
  function updateBurnOnInsertRecv_burn_r22(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      int m_2 = balanceOf[p].n;
      if(amount>0 && p!=address(0) && o_1==s_1 && amount<=m_2) {
        updateTotalBurnOnInsertBurn_r0(p,amount);
        updateAllBurnOnInsertBurn_r18(amount);
        emit Burn(p,amount);
        return true;
      }
      return false;
  }
  function updateTotalBurnOnInsertBurn_r0(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r23(p,delta0);
  }
  function updateTotalBalancesOnInsertConstructor_r28() private    {
      // Empty()
  }
  function updateTransferOnInsertRecv_transfer_r27(address from,address to,int amount) private   returns (bool) {
      int m_1 = balanceOf[from].n;
      if(to!=address(0) && from!=address(0) && amount>0 && amount<=m_1) {
        updateTotalOutOnInsertTransfer_r39(from,amount);
        updateTotalInOnInsertTransfer_r12(to,amount);
        emit Transfer(from,to,amount);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r19(int m) private    {
      totalSupply.n += m;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r16(address from,address to,address spender,int amount) private   returns (bool) {
      int m_1 = balanceOf[from].n;
      if(to!=address(0) && amount<=m_1 && spender!=address(0) && from!=address(0) && amount>0) {
        updateTransferOnInsertTransferFrom_r24(from,to,amount);
        updateSpentTotalOnInsertTransferFrom_r37(from,spender,amount);
        emit TransferFrom(from,to,spender,amount);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r19(int b) private    {
      totalSupply.n -= b;
  }
  function updateBalanceOfOnIncrementTotalIn_r23(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTotalOutOnInsertTransfer_r39(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r23(p,delta0);
  }
  function updateTotalInOnInsertTransfer_r12(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r23(p,delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r32(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r5(o,s,delta0);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r5(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateBalanceOfOnIncrementTotalBurn_r23(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateAllBurnOnInsertBurn_r18(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r19(delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTransferOnInsertTransferFrom_r24(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r39(o,n);
      updateTotalInOnInsertTransfer_r12(r,n);
      emit Transfer(o,r,n);
  }
  function updateBalanceOfOnIncrementTotalMint_r23(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateOwnerOnInsertConstructor_r35() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateAllowanceOnIncrementSpentTotal_r5(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
}