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
    updateOwnerOnInsertConstructor_r9(n);
    updateTotalSupplyOnInsertConstructor_r13(n);
    updateTotalInOnInsertConstructor_r6(n);
    updateTotalBalancesOnInsertConstructor_r27(n);
    updateBalanceOfOnInsertConstructor_r16(n);
    updateTotalMintOnInsertConstructor_r8(n);
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function transferFrom(address o,address r,address s,int n) public    {
      bool r3 = updateTransferFromOnInsertRecv_transferFrom_r3(o,r,s,n);
      if(r3==false) {
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
      bool r11 = updateIncreaseApprovalOnInsertRecv_increaseApproval_r11(o,s,n);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r28 = updateBurnOnInsertRecv_burn_r28(p,amount);
      if(r28==false) {
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
  function transfer(address s,address r,int n) public    {
      bool r31 = updateTransferOnInsertRecv_transfer_r31(s,r,n);
      if(r31==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function updateBalanceOfOnIncrementTotalIn_r24(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTotalSupplyOnIncrementAllBurn_r19(int b) private    {
      totalSupply.n -= b;
  }
  function updateTotalBalancesOnInsertConstructor_r27(int n) private    {
      // Empty()
  }
  function updateTransferOnInsertRecv_transfer_r31(address s,address r,int n) private   returns (bool) {
      address t_1 = address(this);
      int balanceOf_x1_1 = balanceOf[s].n;
      if(r!=address(0) && r!=t_1 && n>0 && n<=balanceOf_x1_1) {
        updateTotalOutOnInsertTransfer_r30(s,n);
        updateTotalInOnInsertTransfer_r7(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateAllMintOnInsertMint_r15(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r19(delta0);
  }
  function updateOwnerOnInsertConstructor_r9(int n) private    {
      address s = msg.sender;
      if(s>address(0) && n>=0) {
        owner = OwnerTuple(s,true);
      }
  }
  function updateDecreaseApprovalOnInsertRecv_decreaseApproval_r5(address o,address s,int n) private   returns (bool) {
      int allowance_x2 = allowance[o][s].n;
      if(n<allowance_x2) {
        updateDecreaseAllowanceTotalOnInsertDecreaseApproval_r2(o,s,n);
        emit DecreaseApproval(o,s,n);
        return true;
      }
      return false;
  }
  function updateTotalInOnInsertTransfer_r7(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r24(p,delta0);
  }
  function updateIncreaseApprovalOnInsertRecv_increaseApproval_r11(address o,address s,int n) private   returns (bool) {
      if(n>=0) {
        updateAllowanceTotalOnInsertIncreaseApproval_r4(o,s,n);
        emit IncreaseApproval(o,s,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnInsertConstructor_r16(int n) private    {
      address p = msg.sender;
      balanceOf[p] = BalanceOfTuple(n,true);
  }
  function updateTotalMintOnInsertConstructor_r8(int n) private    {
      address s = msg.sender;
      // Empty()
  }
  function updateBurnOnInsertRecv_burn_r28(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && n<=balanceOf_x1) {
        updateTotalBurnOnInsertBurn_r1(p,n);
        updateAllBurnOnInsertBurn_r18(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateTotalOutOnInsertTransfer_r30(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r24(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalBurn_r24(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r3(address o,address r,address s,int n) private   returns (bool) {
      int balanceOf_x1_2 = balanceOf[o].n;
      int allowance_x2_1 = allowance[o][s].n;
      if(n>0 && n<allowance_x2_1 && n<=balanceOf_x1_2) {
        updateSpentTotalOnInsertTransferFrom_r20(o,s,n);
        updateTransferOnInsertTransferFrom_r23(o,r,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r24(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateMintOnInsertRecv_mint_r29(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>0) {
        updateAllMintOnInsertMint_r15(n);
        updateTotalMintOnInsertMint_r12(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r13(int n) private    {
      totalSupply = TotalSupplyTuple(n,true);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r21(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateAllowanceOnIncrementSpentTotal_r21(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTotalBurnOnInsertBurn_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r24(p,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalSupplyOnIncrementAllMint_r19(int m) private    {
      totalSupply.n += m;
  }
  function updateDecreaseAllowanceTotalOnInsertDecreaseApproval_r2(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementDecreaseAllowanceTotal_r21(o,s,delta0);
  }
  function updateSpentTotalOnInsertTransferFrom_r20(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r21(o,s,delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseApproval_r4(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r21(o,s,delta0);
  }
  function updateAllowanceOnIncrementDecreaseAllowanceTotal_r21(address o,address s,int d) private    {
      allowance[o][s].n -= d;
  }
  function updateTotalMintOnInsertMint_r12(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r24(p,delta0);
  }
  function updateAllBurnOnInsertBurn_r18(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r19(delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTotalInOnInsertConstructor_r6(int n) private    {
      address s = msg.sender;
      // Empty()
  }
  function updateTransferOnInsertTransferFrom_r23(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r30(o,n);
      updateTotalInOnInsertTransfer_r7(r,n);
      emit Transfer(o,r,n);
  }
  function updateBalanceOfOnIncrementTotalMint_r24(address p,int n) private    {
      balanceOf[p].n += n;
  }
}