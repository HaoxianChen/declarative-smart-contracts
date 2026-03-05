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
    updateTotalSupplyOnInsertConstructor_r17();
    updateTotalBalancesOnInsertConstructor_r28();
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r18 = updateTransferFromOnInsertRecv_transferFrom_r18(from,to,spender,amount);
      if(r18==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function burn(address p,int amount) public    {
      bool r22 = updateBurnOnInsertRecv_burn_r22(p,amount);
      if(r22==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r27 = updateTransferOnInsertRecv_transfer_r27(from,to,amount);
      if(r27==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r6 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r6(p,s,n);
      if(r6==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
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
  function updateTotalInOnInsertTransfer_r14(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r23(p,delta0);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r7(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateOwnerOnInsertConstructor_r35() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTotalMintOnInsertMint_r33(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r23(p,delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r18(address from,address to,address spender,int amount) private   returns (bool) {
      int m_1 = balanceOf[o].n;
      if(r!=address(0) && n<=m_1 && s!=address(0) && o!=address(0) && n>0) {
        updateTransferOnInsertTransferFrom_r24(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r37(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalIn_r23(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r32(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r7(o,s,delta0);
  }
  function updateTotalBurnOnInsertBurn_r0(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r23(p,delta0);
  }
  function updateMintOnInsertRecv_mint_r31(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(n>0 && p!=address(0) && o_1==s_1) {
        updateTotalMintOnInsertMint_r33(p,n);
        updateAllMintOnInsertMint_r11(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r21(int m) private    {
      totalSupply.n += m;
  }
  function updateTotalSupplyOnIncrementAllBurn_r21(int b) private    {
      totalSupply.n -= b;
  }
  function updateTotalSupplyOnInsertConstructor_r17() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateBalanceOfOnIncrementTotalOut_r23(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r6(address p,address s,int n) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r32(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateAllMintOnInsertMint_r11(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r21(delta0);
  }
  function updateSpentTotalOnInsertTransferFrom_r37(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r7(o,s,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalOutOnInsertTransfer_r39(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r23(p,delta0);
  }
  function updateAllBurnOnInsertBurn_r20(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r21(delta0);
  }
  function updateTotalBalancesOnInsertConstructor_r28() private    {
      // Empty()
  }
  function updateTransferOnInsertTransferFrom_r24(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r39(o,n);
      updateTotalInOnInsertTransfer_r14(r,n);
      emit Transfer(o,r,n);
  }
  function updateBalanceOfOnIncrementTotalBurn_r23(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateBurnOnInsertRecv_burn_r22(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      int m_2 = balanceOf[p].n;
      if(n>0 && p!=address(0) && o_1==s_1 && n<=m_2) {
        updateAllBurnOnInsertBurn_r20(n);
        updateTotalBurnOnInsertBurn_r0(p,n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertRecv_transfer_r27(address from,address to,int amount) private   returns (bool) {
      int m_1 = balanceOf[s].n;
      if(r!=address(0) && n>0 && s!=address(0) && n<=m_1) {
        updateTotalOutOnInsertTransfer_r39(s,n);
        updateTotalInOnInsertTransfer_r14(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateAllowanceOnIncrementSpentTotal_r7(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateBalanceOfOnIncrementTotalMint_r23(address p,int n) private    {
      balanceOf[p].n += n;
  }
}