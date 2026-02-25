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
    updateTotalSupplyOnInsertConstructor_r13(n);
    updateBalanceOfOnInsertConstructor_r17(n);
    updateTotalBalancesOnInsertConstructor_r30(n);
    updateTotalInOnInsertConstructor_r25(n);
    updateTotalMintOnInsertConstructor_r7(n);
    updateOwnerOnInsertConstructor_r8(n);
  }
  function increaseApproval(address o,address s,int n) public    {
      bool r10 = updateIncreaseApprovalOnInsertRecv_increaseApproval_r10(o,s,n);
      if(r10==false) {
        revert("Rule condition failed");
      }
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
  function transfer(address s,address r,int n) public    {
      bool r24 = updateTransferOnInsertRecv_transfer_r24(s,r,n);
      if(r24==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address o,address r,address s,int n) public    {
      bool r11 = updateTransferFromOnInsertRecv_transferFrom_r11(o,r,s,n);
      if(r11==false) {
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
  function updateTotalInOnInsertTransfer_r6(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r27(p,delta0);
  }
  function updateAllowanceOnIncrementSpentTotal_r22(address o,address s,int l) private    {
      allowance[o][s].n -= l;
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
  function updateBalanceOfOnIncrementTotalIn_r27(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTotalMintOnInsertMint_r12(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r27(p,delta0);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r22(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateTotalBalancesOnInsertConstructor_r30(int n) private    {
      // Empty()
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
  function updateTotalBurnOnInsertBurn_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r27(p,delta0);
  }
  function updateIncreaseApprovalOnInsertRecv_increaseApproval_r10(address o,address s,int n) private   returns (bool) {
      if(n>0) {
        updateAllowanceTotalOnInsertIncreaseApproval_r3(o,s,n);
        emit IncreaseApproval(o,s,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r20(int m) private    {
      totalSupply.n += m;
  }
  function updateMintOnInsertRecv_mint_r4(address p,int amount) private   returns (bool) {
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
  function updateOwnerOnInsertConstructor_r8(int n) private    {
      address s = msg.sender;
      if(s>address(0) && n>=0) {
        owner = OwnerTuple(s,true);
      }
  }
  function updateTotalMintOnInsertConstructor_r7(int n) private    {
      address s = msg.sender;
      // Empty()
  }
  function updateTotalInOnInsertConstructor_r25(int n) private    {
      address s = msg.sender;
      // Empty()
  }
  function updateTransferOnInsertRecv_transfer_r24(address s,address r,int n) private   returns (bool) {
      address msgSender = msg.sender;
      address t_1 = address(this);
      int allowance_x2_3 = allowance[s][msgSender].n;
      int allowance_x2_2 = allowance[r][s].n;
      int allowance_x2_1 = allowance[msgSender][r].n;
      if(r!=address(0) && 0!=allowance_x2_1 && r!=t_1 && n>=0 && n<=allowance_x2_3 && n<allowance_x2_2) {
        updateTotalInOnInsertTransfer_r6(r,n);
        updateTotalOutOnInsertTransfer_r32(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnInsertConstructor_r17(int n) private    {
      address p = msg.sender;
      balanceOf[p] = BalanceOfTuple(n,true);
  }
  function updateBurnOnInsertRecv_burn_r31(address p,int amount) private   returns (bool) {
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
  function updateTransferOnInsertTransferFrom_r26(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r6(r,n);
      updateTotalOutOnInsertTransfer_r32(o,n);
      emit Transfer(o,r,n);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r11(address o,address r,address s,int n) private   returns (bool) {
      int allowance_x2_2 = allowance[r][o].n;
      int allowance_x2_1 = allowance[o][s].n;
      int allowance_x2_0 = allowance[s][r].n;
      if(0!=allowance_x2_0 && n<=allowance_x2_1 && allowance_x2_2>0) {
        updateTransferOnInsertTransferFrom_r26(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r21(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateAllowanceTotalOnInsertIncreaseApproval_r3(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r22(o,s,delta0);
  }
  function updateTotalSupplyOnIncrementAllBurn_r20(int b) private    {
      totalSupply.n -= b;
  }
  function updateBalanceOfOnIncrementTotalOut_r27(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateBalanceOfOnIncrementTotalBurn_r27(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateTotalOutOnInsertTransfer_r32(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r27(p,delta0);
  }
}