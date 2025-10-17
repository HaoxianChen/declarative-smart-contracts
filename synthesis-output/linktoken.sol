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
    updateOwnerOnInsertConstructor_r15();
  }
  function transfer(address s,address r,int n) public    {
      bool r8 = updateTransferOnInsertRecv_transfer_r8(s,r,n);
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function decreaseApproval(address o,address s,int n) public    {
      bool r6 = updateDecreaseApprovalOnInsertRecv_decreaseApproval_r6(o,s,n);
      if(r6==false) {
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
      bool r10 = updateIncreaseApprovalOnInsertRecv_increaseApproval_r10(o,s,n);
      if(r10==false) {
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
  function transferFrom(address o,address r,address s,int n) public    {
      bool r12 = updateTransferFromOnInsertRecv_transferFrom_r12(o,r,s,n);
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function burn(address p,int amount) public    {
      bool r26 = updateBurnOnInsertRecv_burn_r26(p,amount);
      if(r26==false) {
        revert("Rule condition failed");
      }
  }
  function updateIncreaseApprovalOnInsertRecv_increaseApproval_r10(address o,address s,int n) private   returns (bool) {
      if(n>=0) {
        updateAllowanceTotalOnInsertIncreaseApproval_r4(o,s,n);
        emit IncreaseApproval(o,s,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementDecreaseAllowanceTotal_r20(address o,address s,int d) private    {
      allowance[o][s].n -= d;
  }
  function updateBalanceOfOnIncrementTotalOut_r23(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateSpentTotalOnInsertTransferFrom_r19(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r20(o,s,delta0);
  }
  function updateAllowanceOnIncrementSpentTotal_r20(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateBurnOnInsertRecv_burn_r26(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && n<=balanceOf_x1) {
        updateTotalBurnOnInsertBurn_r2(p,n);
        updateAllBurnOnInsertBurn_r17(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r18(delta0);
  }
  function updateTransferOnInsertRecv_transfer_r8(address s,address r,int n) private   returns (bool) {
      address t_1 = address(this);
      int balanceOf_x1_1 = balanceOf[s].n;
      if(r!=address(0) && r!=t_1 && n>=0 && n<balanceOf_x1_1) {
        updateTotalInOnInsertTransfer_r7(r,n);
        updateTotalOutOnInsertTransfer_r27(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateAllBurnOnInsertBurn_r17(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r18(delta0);
  }
  function updateBalanceOfOnIncrementTotalBurn_r23(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateDecreaseAllowanceTotalOnInsertDecreaseApproval_r3(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementDecreaseAllowanceTotal_r20(o,s,delta0);
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
  function updateOwnerOnInsertConstructor_r15() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateBalanceOfOnIncrementTotalIn_r23(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateBalanceOfOnIncrementTotalMint_r23(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateDecreaseApprovalOnInsertRecv_decreaseApproval_r6(address o,address s,int n) private   returns (bool) {
      int allowance_x2 = allowance[o][s].n;
      if(n<allowance_x2) {
        updateDecreaseAllowanceTotalOnInsertDecreaseApproval_r3(o,s,n);
        emit DecreaseApproval(o,s,n);
        return true;
      }
      return false;
  }
  function updateMintOnInsertRecv_mint_r5(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>=0) {
        updateAllMintOnInsertMint_r0(n);
        updateTotalMintOnInsertMint_r11(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateTotalMintOnInsertMint_r11(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r23(p,delta0);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r20(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateTotalSupplyOnIncrementAllBurn_r18(int b) private    {
      totalSupply.n -= b;
  }
  function updateAllowanceTotalOnInsertIncreaseApproval_r4(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r20(o,s,delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r12(address o,address r,address s,int n) private   returns (bool) {
      int balanceOf_x1_2 = balanceOf[o].n;
      int allowance_x2_1 = allowance[o][s].n;
      if(n>=0 && n<allowance_x2_1 && n<balanceOf_x1_2) {
        updateTransferOnInsertTransferFrom_r22(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r19(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateTotalInOnInsertTransfer_r7(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r23(p,delta0);
  }
  function updateTotalBurnOnInsertBurn_r2(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r23(p,delta0);
  }
  function updateTotalOutOnInsertTransfer_r27(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r23(p,delta0);
  }
  function updateTransferOnInsertTransferFrom_r22(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r7(r,n);
      updateTotalOutOnInsertTransfer_r27(o,n);
      emit Transfer(o,r,n);
  }
}