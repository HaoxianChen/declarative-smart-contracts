contract Linktoken {
  struct BalanceOfTuple {
    int n;
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
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalSupplyTuple totalSupply;
  event InvalidTx();
  event Transfer(address s,address r,int n);
  event Mint(address p,int amount);
  event Burn(address p,int amount);
  event DecreaseApproval(address o,address s,int n);
  event IncreaseApproval(address o,address s,int n);
  event TransferFrom(address o,address r,address s,int n);
  constructor(int n) public {
    updateOwnerOnInsertConstructor_r12();
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function transfer(address s,address r,int n) public    {
      bool r6 = updateTransferOnInsertRecv_transfer_r6(s,r,n);
      if(r6==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r23 = updateMintOnInsertRecv_mint_r23(p,amount);
      if(r23==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address o,address r,address s,int n) public    {
      bool r11 = updateTransferFromOnInsertRecv_transferFrom_r11(o,r,s,n);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function increaseApproval(address o,address s,int n) public    {
      bool r8 = updateIncreaseApprovalOnInsertRecv_increaseApproval_r8(o,s,n);
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function decreaseApproval(address o,address s,int n) public    {
      bool r5 = updateDecreaseApprovalOnInsertRecv_decreaseApproval_r5(o,s,n);
      if(r5==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function burn(address p,int amount) public    {
      bool r24 = updateBurnOnInsertRecv_burn_r24(p,amount);
      if(r24==false) {
        revert("Rule condition failed");
      }
  }
  function updateBurnOnInsertRecv_burn_r24(address p,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(n<=balanceOf_x1) {
        updateTotalBurnOnInsertBurn_r2(p,n);
        updateAllBurnOnInsertBurn_r14(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r15(int b) private    {
      totalSupply.n -= b;
  }
  function updateTotalInOnInsertTransfer_r7(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r19(p,delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseApproval_r4(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r17(o,s,delta0);
  }
  function updateBalanceOfOnIncrementTotalBurn_r19(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateBalanceOfOnIncrementTotalMint_r19(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateDecreaseAllowanceTotalOnInsertDecreaseApproval_r3(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementDecreaseAllowanceTotal_r17(o,s,delta0);
  }
  function updateTotalMintOnInsertMint_r9(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r19(p,delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r11(address o,address r,address s,int n) private   returns (bool) {
      int balanceOf_x1_2 = balanceOf[o].n;
      int allowance_x2_1 = allowance[o][s].n;
      if(n>=0 && n<allowance_x2_1 && n<=balanceOf_x1_2) {
        updateSpentTotalOnInsertTransferFrom_r16(o,s,n);
        updateTransferOnInsertTransferFrom_r18(o,r,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertRecv_transfer_r6(address s,address r,int n) private   returns (bool) {
      address t_1 = address(this);
      int balanceOf_x1_1 = balanceOf[s].n;
      if(r!=address(0) && r!=t_1 && n>0 && n<balanceOf_x1_1) {
        updateTotalOutOnInsertTransfer_r10(s,n);
        updateTotalInOnInsertTransfer_r7(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTransferOnInsertTransferFrom_r18(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r7(r,n);
      updateTotalOutOnInsertTransfer_r10(o,n);
      emit Transfer(o,r,n);
  }
  function updateBalanceOfOnIncrementTotalIn_r19(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTotalSupplyOnIncrementAllMint_r15(int m) private    {
      totalSupply.n += m;
  }
  function updateIncreaseApprovalOnInsertRecv_increaseApproval_r8(address o,address s,int n) private   returns (bool) {
      if(n>=0) {
        updateAllowanceTotalOnInsertIncreaseApproval_r4(o,s,n);
        emit IncreaseApproval(o,s,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r19(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateAllBurnOnInsertBurn_r14(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r15(delta0);
  }
  function updateDecreaseApprovalOnInsertRecv_decreaseApproval_r5(address o,address s,int n) private   returns (bool) {
      int allowance_x2 = allowance[o][s].n;
      if(n<allowance_x2) {
        updateDecreaseAllowanceTotalOnInsertDecreaseApproval_r3(o,s,n);
        emit DecreaseApproval(o,s,n);
        return true;
      }
      return false;
  }
  function updateTotalBurnOnInsertBurn_r2(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r19(p,delta0);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r17(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateOwnerOnInsertConstructor_r12() private    {
      address s = msg.sender;
      // Empty()
  }
  function updateSpentTotalOnInsertTransferFrom_r16(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r17(o,s,delta0);
  }
  function updateAllowanceOnIncrementSpentTotal_r17(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTotalOutOnInsertTransfer_r10(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r19(p,delta0);
  }
  function updateMintOnInsertRecv_mint_r23(address p,int n) private   returns (bool) {
      if(n>=0) {
        updateAllMintOnInsertMint_r0(n);
        updateTotalMintOnInsertMint_r9(p,n);
        emit Mint(p,n);
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
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r15(delta0);
  }
  function updateAllowanceOnIncrementDecreaseAllowanceTotal_r17(address o,address s,int d) private    {
      allowance[o][s].n -= d;
  }
}