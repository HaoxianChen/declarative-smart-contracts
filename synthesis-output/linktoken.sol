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
    updateTotalMintOnInsertConstructor_r23(n);
    updateTotalInOnInsertConstructor_r4(n);
    updateBalanceOfOnInsertConstructor_r12(n);
    updateTotalBalancesOnInsertConstructor_r33(n);
    updateOwnerOnInsertConstructor_r25(n);
    updateTotalSupplyOnInsertConstructor_r9(n);
  }
  function burn(address p,int amount) public    {
      bool r34 = updateBurnOnInsertRecv_burn_r34(p,amount);
      if(r34==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r35 = updateMintOnInsertRecv_mint_r35(p,amount);
      if(r35==false) {
        revert("Rule condition failed");
      }
  }
  function increaseApproval(address o,address s,int n) public    {
      bool r7 = updateIncreaseApprovalOnInsertRecv_increaseApproval_r7(o,s,n);
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function decreaseApproval(address o,address s,int n) public    {
      bool r38 = updateDecreaseApprovalOnInsertRecv_decreaseApproval_r38(o,s,n);
      if(r38==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function transfer(address s,address r,int n) public    {
      bool r29 = updateTransferOnInsertRecv_transfer_r29(s,r,n);
      if(r29==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address o,address r,address s,int n) public    {
      bool r13 = updateTransferFromOnInsertRecv_transferFrom_r13(o,r,s,n);
      if(r13==false) {
        revert("Rule condition failed");
      }
  }
  function updateAllowanceTotalOnInsertIncreaseApproval_r2(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r20(o,s,delta0);
  }
  function updateAllowanceOnIncrementDecreaseAllowanceTotal_r20(address o,address s,int d) private    {
      allowance[o][s].n -= d;
  }
  function updateTotalBurnOnInsertBurn_r14(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r28(p,delta0);
  }
  function updateAllowanceOnIncrementSpentTotal_r20(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTotalBalancesOnInsertConstructor_r33(int n) private    {
      // Empty()
  }
  function updateTotalMintOnInsertMint_r8(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r28(p,delta0);
  }
  function updateAllBurnOnInsertBurn_r17(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r18(delta0);
  }
  function updateOwnerOnInsertConstructor_r25(int n) private    {
      address s = msg.sender;
      if(s>address(0) && n>=0) {
        owner = OwnerTuple(s,true);
      }
  }
  function updateTotalOutOnInsertTransfer_r36(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r28(p,delta0);
  }
  function updateTotalSupplyOnInsertConstructor_r9(int n) private    {
      totalSupply = TotalSupplyTuple(n,true);
  }
  function updateTransferOnInsertTransferFrom_r26(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r6(r,n);
      updateTotalOutOnInsertTransfer_r36(o,n);
      emit Transfer(o,r,n);
  }
  function updateSpentTotalOnInsertTransferFrom_r19(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r20(o,s,delta0);
  }
  function updateTotalMintOnInsertConstructor_r23(int n) private    {
      address s = msg.sender;
      // Empty()
  }
  function updateMintOnInsertRecv_mint_r35(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && amount>0) {
        updateAllMintOnInsertMint_r11(amount);
        updateTotalMintOnInsertMint_r8(p,amount);
        emit Mint(p,amount);
        return true;
      }
      return false;
  }
  function updateDecreaseApprovalOnInsertRecv_decreaseApproval_r38(address o,address s,int n) private   returns (bool) {
      int totalSupply_n_0 = totalSupply.n;
      int balanceOf_x1_1 = balanceOf[o].n;
      if(n>0 && n>=totalSupply_n_0 && n<balanceOf_x1_1) {
        updateDecreaseAllowanceTotalOnInsertDecreaseApproval_r1(o,s,n);
        emit DecreaseApproval(o,s,n);
        return true;
      }
      return false;
  }
  function updateTotalInOnInsertConstructor_r4(int n) private    {
      address s = msg.sender;
      // Empty()
  }
  function updateBalanceOfOnIncrementTotalIn_r28(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r13(address o,address r,address s,int n) private   returns (bool) {
      int m_2 = balanceOf[o].n;
      int m_1 = allowance[o][s].n;
      if(n>0 && n<=m_1 && n<=m_2) {
        updateTransferOnInsertTransferFrom_r26(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r19(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalMint_r28(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateTotalSupplyOnIncrementAllMint_r18(int m) private    {
      totalSupply.n += m;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateBurnOnInsertRecv_burn_r34(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && amount<=balanceOf_x1) {
        updateAllBurnOnInsertBurn_r17(amount);
        updateTotalBurnOnInsertBurn_r14(p,amount);
        emit Burn(p,amount);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r28(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateTransferOnInsertRecv_transfer_r29(address s,address r,int n) private   returns (bool) {
      address t_1 = address(this);
      int m_2 = balanceOf[s].n;
      if(n>0 && r!=address(0) && r!=t_1 && n<=m_2) {
        updateTotalInOnInsertTransfer_r6(r,n);
        updateTotalOutOnInsertTransfer_r36(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalBurn_r28(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateTotalInOnInsertTransfer_r6(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r28(p,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateBalanceOfOnInsertConstructor_r12(int n) private    {
      address p = msg.sender;
      balanceOf[p] = BalanceOfTuple(n,true);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r20(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateTotalSupplyOnIncrementAllBurn_r18(int b) private    {
      totalSupply.n -= b;
  }
  function updateIncreaseApprovalOnInsertRecv_increaseApproval_r7(address o,address s,int n) private   returns (bool) {
      if(n>=0) {
        updateAllowanceTotalOnInsertIncreaseApproval_r2(o,s,n);
        emit IncreaseApproval(o,s,n);
        return true;
      }
      return false;
  }
  function updateAllMintOnInsertMint_r11(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r18(delta0);
  }
  function updateDecreaseAllowanceTotalOnInsertDecreaseApproval_r1(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementDecreaseAllowanceTotal_r20(o,s,delta0);
  }
}