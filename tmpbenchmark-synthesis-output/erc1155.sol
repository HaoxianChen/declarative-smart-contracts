contract Erc1155 {
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct TotalSupplyTuple {
    int n;
    bool _valid;
  }
  struct AllowanceTuple {
    int n;
    bool _valid;
  }
  struct BalanceOfTuple {
    int n;
    bool _valid;
  }
  mapping(int=>mapping(address=>mapping(address=>AllowanceTuple))) allowance;
  mapping(int=>TotalSupplyTuple) totalSupply;
  mapping(int=>mapping(address=>BalanceOfTuple)) balanceOf;
  OwnerTuple owner;
  event InvalidTx();
  event TransferFrom(int tokenId,address from,address to,address spender,int amount);
  event IncreaseAllowance(int tokenId,address o,address s,int d);
  event Transfer(int tokenId,address from,address to,int amount);
  event Burn(int tokenId,address p,int amount);
  event Mint(int tokenId,address p,int amount);
  constructor() public {
    updateOwnerOnInsertConstructor_r24();
  }
  function mint(int tokenId,address p,int amount) public    {
      bool r12 = updateMintOnInsertRecv_mint_r12(tokenId,p,amount);
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function burn(int tokenId,address p,int amount) public    {
      bool r26 = updateBurnOnInsertRecv_burn_r26(tokenId,p,amount);
      if(r26==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(int tokenId,address o,address s,int d) public    {
      bool r23 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r23(tokenId,o,s,d);
      if(r23==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply(int tokenId) public view  returns (int) {
      int n = totalSupply[tokenId].n;
      return n;
  }
  function transfer(int tokenId,address from,address to,int amount) public    {
      bool r4 = updateTransferOnInsertRecv_transfer_r4(tokenId,from,to,amount);
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(int tokenId,address p) public view  returns (int) {
      int n = balanceOf[tokenId][p].n;
      return n;
  }
  function getAllowance(int tokenId,address o,address s) public view  returns (int) {
      int n = allowance[tokenId][o][s].n;
      return n;
  }
  function transferFrom(int tokenId,address from,address to,address spender,int amount) public    {
      bool r7 = updateTransferFromOnInsertRecv_transferFrom_r7(tokenId,from,to,spender,amount);
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function updateSpentTotalOnInsertTransferFrom_r29(int t,address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r5(t,o,s,delta0);
  }
  function updateBurnOnInsertRecv_burn_r26(int t,address p,int n) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      int m_2 = balanceOf[t][p].n;
      if(p!=address(0) && o_1==s_1 && n<=m_2) {
        updateTotalBurnOnInsertBurn_r30(t,p,n);
        updateAllBurnOnInsertBurn_r0(t,n);
        emit Burn(t,p,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r27(int t,int b) private    {
      totalSupply[t].n -= b;
  }
  function updateTransferOnInsertTransferFrom_r25(int t,address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r13(t,o,n);
      updateTotalInOnInsertTransfer_r17(t,r,n);
      emit Transfer(t,o,r,n);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r7(int t,address o,address r,address sp,int n) private   returns (bool) {
      int m_1 = balanceOf[t][o].n;
      int k_0 = allowance[t][o][sp].n;
      if(n<=k_0 && n<=m_1 && o!=address(0) && r!=address(0)) {
        updateSpentTotalOnInsertTransferFrom_r29(t,o,sp,n);
        updateTransferOnInsertTransferFrom_r25(t,o,r,n);
        emit TransferFrom(t,o,r,sp,n);
        return true;
      }
      return false;
  }
  function updateTotalInOnInsertTransfer_r17(int t,address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r6(t,p,delta0);
  }
  function updateBalanceOfOnIncrementTotalMint_r6(int t,address p,int n) private    {
      balanceOf[t][p].n += n;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r9(int t,address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r5(t,o,s,delta0);
  }
  function updateAllBurnOnInsertBurn_r0(int t,int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r27(t,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r5(int t,address o,address s,int m) private    {
      allowance[t][o][s].n += m;
  }
  function updateAllMintOnInsertMint_r28(int t,int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r27(t,delta0);
  }
  function updateAllowanceOnIncrementSpentTotal_r5(int t,address o,address s,int l) private    {
      allowance[t][o][s].n -= l;
  }
  function updateTransferOnInsertRecv_transfer_r4(int t,address s,address r,int n) private   returns (bool) {
      int m_1 = balanceOf[t][s].n;
      if(r!=address(0) && s!=address(0) && n<=m_1) {
        updateTotalOutOnInsertTransfer_r13(t,s,n);
        updateTotalInOnInsertTransfer_r17(t,r,n);
        emit Transfer(t,s,r,n);
        return true;
      }
      return false;
  }
  function updateTotalBurnOnInsertBurn_r30(int t,address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r6(t,p,delta0);
  }
  function updateBalanceOfOnIncrementTotalOut_r6(int t,address p,int o) private    {
      balanceOf[t][p].n -= o;
  }
  function updateBalanceOfOnIncrementTotalIn_r6(int t,address p,int i) private    {
      balanceOf[t][p].n += i;
  }
  function updateBalanceOfOnIncrementTotalBurn_r6(int t,address p,int m) private    {
      balanceOf[t][p].n -= m;
  }
  function updateTotalOutOnInsertTransfer_r13(int t,address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r6(t,p,delta0);
  }
  function updateOwnerOnInsertConstructor_r24() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalSupplyOnIncrementAllMint_r27(int t,int m) private    {
      totalSupply[t].n += m;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r23(int t,address o,address s,int d) private   returns (bool) {
      updateAllowanceTotalOnInsertIncreaseAllowance_r9(t,o,s,d);
      emit IncreaseAllowance(t,o,s,d);
      return true;
      return false;
  }
  function updateTotalMintOnInsertMint_r16(int t,address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r6(t,p,delta0);
  }
  function updateMintOnInsertRecv_mint_r12(int t,address p,int n) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(p!=address(0) && o_1==s_1) {
        updateTotalMintOnInsertMint_r16(t,p,n);
        updateAllMintOnInsertMint_r28(t,n);
        emit Mint(t,p,n);
        return true;
      }
      return false;
  }
}