contract Linktoken {
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
  struct AllowanceTuple {
    int n;
    bool _valid;
  }
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalSupplyTuple totalSupply;
  OwnerTuple owner;
  event InvalidTx();
  event Transfer(address s,address r,int n);
  event Mint(address p,int amount);
  event Burn(address p,int amount);
  event DecreaseApproval(address o,address s,int n);
  event IncreaseApproval(address o,address s,int n);
  event TransferFrom(address o,address r,address s,int n);
  constructor(int n) public {
    updateTotalBalancesOnInsertConstructor_r34(n);
    updateOwnerOnInsertConstructor_r26(n);
    updateTotalSupplyOnInsertConstructor_r10(n);
    updateBalanceOfOnInsertConstructor_r13(n);
    updateTotalMintOnInsertConstructor_r24(n);
    updateTotalInOnInsertConstructor_r5(n);
  }
  function burn(address p,int amount) public    {
      bool r35 = updateBurnOnInsertRecv_burn_r35(p,amount);
      if(r35==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function decreaseApproval(address o,address s,int n) public    {
      bool r38 = updateDecreaseApprovalOnInsertRecv_decreaseApproval_r38(o,s,n);
      if(r38==false) {
        revert("Rule condition failed");
      }
  }
  function increaseApproval(address o,address s,int n) public    {
      bool r8 = updateIncreaseApprovalOnInsertRecv_increaseApproval_r8(o,s,n);
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address o,address r,address s,int n) public    {
      bool r14 = updateTransferFromOnInsertRecv_transferFrom_r14(o,r,s,n);
      if(r14==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function transfer(address s,address r,int n) public    {
      bool r30 = updateTransferOnInsertRecv_transfer_r30(s,r,n);
      if(r30==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function mint(address p,int amount) public    {
      bool r3 = updateMintOnInsertRecv_mint_r3(p,amount);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function updateTotalOutOnInsertTransfer_r36(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r29(p,delta0);
  }
  function updateTotalMintOnInsertConstructor_r24(int n) private    {
      address s = msg.sender;
      // Empty()
  }
  function updateAllowanceTotalOnInsertIncreaseApproval_r2(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r21(o,s,delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r14(address o,address r,address s,int n) private   returns (bool) {
      int m_2 = balanceOf[o].n;
      int m_1 = allowance[o][s].n;
      if(n>0 && n<=m_1 && n<=m_2) {
        updateSpentTotalOnInsertTransferFrom_r20(o,s,n);
        updateTransferOnInsertTransferFrom_r27(o,r,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateTotalMintOnInsertMint_r9(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r29(p,delta0);
  }
  function updateTotalSupplyOnIncrementAllBurn_r19(int b) private    {
      totalSupply.n -= b;
  }
  function updateAllMintOnInsertMint_r12(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r19(delta0);
  }
  function updateAllowanceOnIncrementDecreaseAllowanceTotal_r21(address o,address s,int d) private    {
      allowance[o][s].n -= d;
  }
  function updateTransferOnInsertTransferFrom_r27(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r7(r,n);
      updateTotalOutOnInsertTransfer_r36(o,n);
      emit Transfer(o,r,n);
  }
  function updateDecreaseAllowanceTotalOnInsertDecreaseApproval_r1(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementDecreaseAllowanceTotal_r21(o,s,delta0);
  }
  function updateOwnerOnInsertConstructor_r26(int n) private    {
      address s = msg.sender;
      if(s>address(0) && n>=0) {
        owner = OwnerTuple(s,true);
      }
  }
  function updateIncreaseApprovalOnInsertRecv_increaseApproval_r8(address o,address s,int n) private   returns (bool) {
      if(n>=0) {
        updateAllowanceTotalOnInsertIncreaseApproval_r2(o,s,n);
        emit IncreaseApproval(o,s,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalBurn_r29(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateBurnOnInsertRecv_burn_r35(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && n<=balanceOf_x1) {
        updateTotalBurnOnInsertBurn_r15(p,n);
        updateAllBurnOnInsertBurn_r18(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateTotalInOnInsertConstructor_r5(int n) private    {
      address s = msg.sender;
      // Empty()
  }
  function updateBalanceOfOnIncrementTotalIn_r29(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r21(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateAllowanceOnIncrementSpentTotal_r21(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateBalanceOfOnIncrementTotalMint_r29(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateTotalBurnOnInsertBurn_r15(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r29(p,delta0);
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
  function updateSpentTotalOnInsertTransferFrom_r20(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r21(o,s,delta0);
  }
  function updateMintOnInsertRecv_mint_r3(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>=0) {
        updateTotalMintOnInsertMint_r9(p,n);
        updateAllMintOnInsertMint_r12(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateAllBurnOnInsertBurn_r18(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r19(delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTotalSupplyOnInsertConstructor_r10(int n) private    {
      totalSupply = TotalSupplyTuple(n,true);
  }
  function updateTotalInOnInsertTransfer_r7(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r29(p,delta0);
  }
  function updateTransferOnInsertRecv_transfer_r30(address s,address r,int n) private   returns (bool) {
      address t_1 = address(this);
      int m_2 = balanceOf[s].n;
      if(n>0 && r!=address(0) && r!=t_1 && n<=m_2) {
        updateTotalInOnInsertTransfer_r7(r,n);
        updateTotalOutOnInsertTransfer_r36(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r29(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateBalanceOfOnInsertConstructor_r13(int n) private    {
      address p = msg.sender;
      balanceOf[p] = BalanceOfTuple(n,true);
  }
  function updateTotalBalancesOnInsertConstructor_r34(int n) private    {
      // Empty()
  }
  function updateDecreaseApprovalOnInsertRecv_decreaseApproval_r38(address o,address s,int n) private   returns (bool) {
      int totalSupply_n_0 = totalSupply.n;
      int balanceOf_x1_1 = balanceOf[o].n;
      if(n>0 && n==totalSupply_n_0 && 0!=balanceOf_x1_1) {
        updateDecreaseAllowanceTotalOnInsertDecreaseApproval_r1(o,s,n);
        emit DecreaseApproval(o,s,n);
        return true;
      }
      return false;
  }
}