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
    updateBalanceOfOnInsertConstructor_r19(n);
    updateTotalMintOnInsertConstructor_r9(n);
    updateTotalBalancesOnInsertConstructor_r30(n);
    updateTotalInOnInsertConstructor_r7(n);
    updateTotalSupplyOnInsertConstructor_r14(n);
    updateOwnerOnInsertConstructor_r10(n);
  }
  function transferFrom(address o,address r,address s,int n) public    {
      bool r5 = updateTransferFromOnInsertRecv_transferFrom_r5(o,r,s,n);
      if(r5==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function transfer(address s,address r,int n) public    {
      bool r16 = updateTransferOnInsertRecv_transfer_r16(s,r,n);
      if(r16==false) {
        revert("Rule condition failed");
      }
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
  function mint(address p,int amount) public    {
      bool r4 = updateMintOnInsertRecv_mint_r4(p,amount);
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function increaseApproval(address o,address s,int n) public    {
      bool r12 = updateIncreaseApprovalOnInsertRecv_increaseApproval_r12(o,s,n);
      if(r12==false) {
        revert("Rule condition failed");
      }
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
  function updateAllMintOnInsertMint_r18(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r22(delta0);
  }
  function updateSpentTotalOnInsertTransferFrom_r23(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r24(o,s,delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseApproval_r3(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r24(o,s,delta0);
  }
  function updateBalanceOfOnIncrementTotalIn_r27(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateBurnOnInsertRecv_burn_r31(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && n<=balanceOf_x1) {
        updateTotalBurnOnInsertBurn_r1(p,n);
        updateAllBurnOnInsertBurn_r21(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r22(int b) private    {
      totalSupply.n -= b;
  }
  function updateIncreaseApprovalOnInsertRecv_increaseApproval_r12(address o,address s,int n) private   returns (bool) {
      if(n>0) {
        updateAllowanceTotalOnInsertIncreaseApproval_r3(o,s,n);
        emit IncreaseApproval(o,s,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r27(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateTotalBurnOnInsertBurn_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r27(p,delta0);
  }
  function updateTransferOnInsertRecv_transfer_r16(address s,address r,int n) private   returns (bool) {
      address t_1 = address(this);
      int balanceOf_x1_1 = balanceOf[r].n;
      int allowance_x2_3 = allowance[r][s].n;
      int balanceOf_x1_2 = balanceOf[s].n;
      if(r!=address(0) && balanceOf_x1_1>0 && r!=t_1 && balanceOf_x1_2>0 && n<allowance_x2_3 && n>0) {
        updateTotalOutOnInsertTransfer_r32(s,n);
        updateTotalInOnInsertTransfer_r8(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r14(int n) private    {
      totalSupply = TotalSupplyTuple(n,true);
  }
  function updateTotalSupplyOnIncrementAllMint_r22(int m) private    {
      totalSupply.n += m;
  }
  function updateAllBurnOnInsertBurn_r21(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r22(delta0);
  }
  function updateBalanceOfOnIncrementTotalMint_r27(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateBalanceOfOnInsertConstructor_r19(int n) private    {
      address p = msg.sender;
      balanceOf[p] = BalanceOfTuple(n,true);
  }
  function updateTotalInOnInsertConstructor_r7(int n) private    {
      address s = msg.sender;
      // Empty()
  }
  function updateOwnerOnInsertConstructor_r10(int n) private    {
      address s = msg.sender;
      if(s>address(0) && n>=0) {
        owner = OwnerTuple(s,true);
      }
  }
  function updateMintOnInsertRecv_mint_r4(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>=0) {
        updateAllMintOnInsertMint_r18(n);
        updateTotalMintOnInsertMint_r13(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r24(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateDecreaseAllowanceTotalOnInsertDecreaseApproval_r2(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementDecreaseAllowanceTotal_r24(o,s,delta0);
  }
  function updateTransferOnInsertTransferFrom_r26(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r32(o,n);
      updateTotalInOnInsertTransfer_r8(r,n);
      emit Transfer(o,r,n);
  }
  function updateTotalMintOnInsertMint_r13(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r27(p,delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r5(address o,address r,address s,int n) private   returns (bool) {
      int allowance_x2_3 = allowance[r][s].n;
      int allowance_x2_2 = allowance[o][r].n;
      int allowance_x2_1 = allowance[s][o].n;
      if(n>0 && n<allowance_x2_1 && n<allowance_x2_2 && allowance_x2_3>0) {
        updateTransferOnInsertTransferFrom_r26(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r23(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementSpentTotal_r24(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTotalMintOnInsertConstructor_r9(int n) private    {
      address s = msg.sender;
      // Empty()
  }
  function updateTotalInOnInsertTransfer_r8(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r27(p,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateAllowanceOnIncrementDecreaseAllowanceTotal_r24(address o,address s,int d) private    {
      allowance[o][s].n -= d;
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
  function updateTotalBalancesOnInsertConstructor_r30(int n) private    {
      // Empty()
  }
  function updateBalanceOfOnIncrementTotalBurn_r27(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateTotalOutOnInsertTransfer_r32(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r27(p,delta0);
  }
}