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
  event Transfer(address s,address r,int n);
  event Mint(address p,int amount);
  event UnauthorizedMint();
  event UnauthorizedBurn();
  event Burn(address p,int amount);
  event DecreaseApproval(address o,address s,int n);
  event IncreaseApproval(address o,address s,int n);
  event TransferFrom(address o,address r,address s,int n);
  constructor(int n) public {
    updateTotalBalancesOnInsertConstructor_r28(n);
    updateOwnerOnInsertConstructor_r9(n);
    updateBalanceOfOnInsertConstructor_r15(n);
    updateTotalSupplyOnInsertConstructor_r12(n);
    updateTotalInOnInsertConstructor_r5(n);
    updateTotalMintOnInsertConstructor_r8(n);
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
  function burn(address p,int amount) public    {
      bool r4 = updateBurnOnInsertRecv_burn_r4(p,amount);
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r29 = updateMintOnInsertRecv_mint_r29(p,amount);
      if(r29==false) {
        revert("Rule condition failed");
      }
  }
  function increaseApproval(address o,address s,int n) public    {
      bool r10 = updateIncreaseApprovalOnInsertRecv_increaseApproval_r10(o,s,n);
      if(r10==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address o,address r,address s,int n) public    {
      bool r26 = updateTransferFromOnInsertRecv_transferFrom_r26(o,r,s,n);
      if(r26==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function decreaseApproval(address o,address s,int n) public    {
      bool r3 = updateDecreaseApprovalOnInsertRecv_decreaseApproval_r3(o,s,n);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function updateAllowanceOnIncrementDecreaseAllowanceTotal_r22(address o,address s,int d) private    {
      allowance[o][s].n -= d;
  }
  function updateTotalOutOnInsertTransfer_r30(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r24(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalIn_r24(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateBalanceOfOnIncrementTotalBurn_r24(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateOwnerOnInsertConstructor_r9(int n) private    {
      address s = msg.sender;
      if(s>address(0) && n>=0) {
        owner = OwnerTuple(s,true);
      }
  }
  function updateTotalSupplyOnInsertConstructor_r12(int n) private    {
      totalSupply = TotalSupplyTuple(n,true);
  }
  function updateAllowanceOnIncrementSpentTotal_r22(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r26(address o,address r,address s,int n) private   returns (bool) {
      int allowance_x2 = allowance[o][s].n;
      if(n<=allowance_x2) {
        updateSpentTotalOnInsertTransferFrom_r21(o,s,n);
        updateTransferOnInsertTransferFrom_r23(o,r,n);
        emit TransferFrom(o,r,s,n);
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
  function updateIncreaseApprovalOnInsertRecv_increaseApproval_r10(address o,address s,int n) private   returns (bool) {
      if(n>0) {
        updateAllowanceTotalOnInsertIncreaseApproval_r2(o,s,n);
        emit IncreaseApproval(o,s,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r22(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateTotalInOnInsertTransfer_r7(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r24(p,delta0);
  }
  function updateTotalSupplyOnIncrementAllMint_r20(int m) private    {
      totalSupply.n += m;
  }
  function updateDecreaseApprovalOnInsertRecv_decreaseApproval_r3(address o,address s,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[s].n;
      if(n<balanceOf_x1) {
        updateDecreaseAllowanceTotalOnInsertDecreaseApproval_r1(o,s,n);
        emit DecreaseApproval(o,s,n);
        return true;
      }
      return false;
  }
  function updateTotalInOnInsertConstructor_r5(int n) private    {
      address s = msg.sender;
      // Empty()
  }
  function updateSpentTotalOnInsertTransferFrom_r21(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r22(o,s,delta0);
  }
  function updateTotalMintOnInsertConstructor_r8(int n) private    {
      address s = msg.sender;
      // Empty()
  }
  function updateAllMintOnInsertMint_r14(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r20(delta0);
  }
  function updateMintOnInsertRecv_mint_r29(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(s==o && amount>0) {
        updateAllMintOnInsertMint_r14(amount);
        updateTotalMintOnInsertMint_r11(p,amount);
        emit Mint(p,amount);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r24(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateDecreaseAllowanceTotalOnInsertDecreaseApproval_r1(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementDecreaseAllowanceTotal_r22(o,s,delta0);
  }
  function updateTotalBalancesOnInsertConstructor_r28(int n) private    {
      // Empty()
  }
  function updateAllBurnOnInsertBurn_r19(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r20(delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseApproval_r2(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r22(o,s,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTotalBurnOnInsertBurn_r16(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r24(p,delta0);
  }
  function updateBalanceOfOnInsertConstructor_r15(int n) private    {
      address p = msg.sender;
      balanceOf[p] = BalanceOfTuple(n,true);
  }
  function updateTransferOnInsertRecv_transfer_r6(address s,address r,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[s].n;
      if(0!=balanceOf_x1) {
        updateTotalOutOnInsertTransfer_r30(s,n);
        updateTotalInOnInsertTransfer_r7(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r20(int b) private    {
      totalSupply.n -= b;
  }
  function updateTransferOnInsertTransferFrom_r23(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r30(o,n);
      updateTotalInOnInsertTransfer_r7(r,n);
      emit Transfer(o,r,n);
  }
  function updateBalanceOfOnIncrementTotalMint_r24(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateTotalMintOnInsertMint_r11(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r24(p,delta0);
  }
  function updateBurnOnInsertRecv_burn_r4(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(s==o && amount<=balanceOf_x1) {
        updateAllBurnOnInsertBurn_r19(amount);
        updateTotalBurnOnInsertBurn_r16(p,amount);
        emit Burn(p,amount);
        return true;
      }
      return false;
  }
}