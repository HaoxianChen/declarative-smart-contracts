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
    updateTotalMintOnInsertConstructor_r9(n);
    updateTotalSupplyOnInsertConstructor_r13(n);
    updateBalanceOfOnInsertConstructor_r17(n);
    updateTotalBalancesOnInsertConstructor_r30(n);
    updateOwnerOnInsertConstructor_r25(n);
    updateTotalInOnInsertConstructor_r7(n);
  }
  function decreaseApproval(address o,address s,int n) public    {
      bool r6 = updateDecreaseApprovalOnInsertRecv_decreaseApproval_r6(o,s,n);
      if(r6==false) {
        revert("Rule condition failed");
      }
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
      bool r5 = updateMintOnInsertRecv_mint_r5(p,amount);
      if(r5==false) {
        revert("Rule condition failed");
      }
  }
  function increaseApproval(address o,address s,int n) public    {
      bool r11 = updateIncreaseApprovalOnInsertRecv_increaseApproval_r11(o,s,n);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address s,address r,int n) public    {
      bool r24 = updateTransferOnInsertRecv_transfer_r24(s,r,n);
      if(r24==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function burn(address p,int amount) public    {
      bool r31 = updateBurnOnInsertRecv_burn_r31(p,amount);
      if(r31==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function updateSpentTotalOnInsertTransferFrom_r21(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r22(o,s,delta0);
  }
  function updateAllowanceOnIncrementDecreaseAllowanceTotal_r22(address o,address s,int d) private    {
      allowance[o][s].n -= d;
  }
  function updateAllMintOnInsertMint_r16(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r20(delta0);
  }
  function updateBalanceOfOnIncrementTotalMint_r27(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateBalanceOfOnInsertConstructor_r17(int n) private    {
      address p = msg.sender;
      balanceOf[p] = BalanceOfTuple(n,true);
  }
  function updateOwnerOnInsertConstructor_r25(int n) private    {
      address s = msg.sender;
      if(s>address(0) && n>=0) {
        owner = OwnerTuple(s,true);
      }
  }
  function updateDecreaseAllowanceTotalOnInsertDecreaseApproval_r2(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementDecreaseAllowanceTotal_r22(o,s,delta0);
  }
  function updateAllBurnOnInsertBurn_r19(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r20(delta0);
  }
  function updateTotalSupplyOnInsertConstructor_r13(int n) private    {
      totalSupply = TotalSupplyTuple(n,true);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTransferOnInsertTransferFrom_r26(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r32(o,n);
      updateTotalInOnInsertTransfer_r8(r,n);
      emit Transfer(o,r,n);
  }
  function updateBalanceOfOnIncrementTotalIn_r27(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTotalMintOnInsertMint_r12(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r27(p,delta0);
  }
  function updateBurnOnInsertRecv_burn_r31(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && n<=balanceOf_x1) {
        updateTotalBurnOnInsertBurn_r1(p,n);
        updateAllBurnOnInsertBurn_r19(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r22(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateTotalBurnOnInsertBurn_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r27(p,delta0);
  }
  function updateAllowanceOnIncrementSpentTotal_r22(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTotalInOnInsertConstructor_r7(int n) private    {
      address s = msg.sender;
      // Empty()
  }
  function updateTotalMintOnInsertConstructor_r9(int n) private    {
      address s = msg.sender;
      // Empty()
  }
  function updateTotalInOnInsertTransfer_r8(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r27(p,delta0);
  }
  function updateTransferOnInsertRecv_transfer_r24(address s,address r,int n) private   returns (bool) {
      address t_1 = address(this);
      int balanceOf_x1_1 = balanceOf[s].n;
      if(r!=address(0) && r!=t_1 && n>=0 && n<balanceOf_x1_1) {
        updateTotalOutOnInsertTransfer_r32(s,n);
        updateTotalInOnInsertTransfer_r8(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateDecreaseApprovalOnInsertRecv_decreaseApproval_r6(address o,address s,int n) private   returns (bool) {
      int allowance_x2 = allowance[o][s].n;
      if(n<allowance_x2) {
        updateDecreaseAllowanceTotalOnInsertDecreaseApproval_r2(o,s,n);
        emit DecreaseApproval(o,s,n);
        return true;
      }
      return false;
  }
  function updateMintOnInsertRecv_mint_r5(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>=0) {
        updateAllMintOnInsertMint_r16(n);
        updateTotalMintOnInsertMint_r12(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateTotalBalancesOnInsertConstructor_r30(int n) private    {
      // Empty()
  }
  function updateTotalSupplyOnIncrementAllBurn_r20(int b) private    {
      totalSupply.n -= b;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r3(address o,address r,address s,int n) private   returns (bool) {
      int balanceOf_x1_2 = balanceOf[o].n;
      int allowance_x2_1 = allowance[o][s].n;
      if(n>0 && n<allowance_x2_1 && n<=balanceOf_x1_2) {
        updateTransferOnInsertTransferFrom_r26(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r21(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r27(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateAllowanceTotalOnInsertIncreaseApproval_r4(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r22(o,s,delta0);
  }
  function updateBalanceOfOnIncrementTotalBurn_r27(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateTotalOutOnInsertTransfer_r32(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r27(p,delta0);
  }
  function updateTotalSupplyOnIncrementAllMint_r20(int m) private    {
      totalSupply.n += m;
  }
  function updateIncreaseApprovalOnInsertRecv_increaseApproval_r11(address o,address s,int n) private   returns (bool) {
      if(n>=0) {
        updateAllowanceTotalOnInsertIncreaseApproval_r4(o,s,n);
        emit IncreaseApproval(o,s,n);
        return true;
      }
      return false;
  }
}