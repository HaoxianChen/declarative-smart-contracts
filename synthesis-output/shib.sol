contract Shib {
  struct AllowanceTuple {
    int n;
    bool _valid;
  }
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
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalSupplyTuple totalSupply;
  OwnerTuple owner;
  event InvalidTx();
  event Transfer(address s,address r,int n);
  event Mint(address p,int amount);
  event Burn(address p,int amount);
  event TransferFrom(address o,address r,address s,int n);
  event BurnFrom(address p,address from,int n);
  event IncreaseAllowance(address p,address s,int d);
  constructor() public {
    updateTotalSupplyOnInsertConstructor_r5();
    updateOwnerOnInsertConstructor_r30();
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function burnFrom(address p,address from,int n) public    {
      bool r28 = updateBurnFromOnInsertRecv_burnFrom_r28(p,from,n);
      if(r28==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address o,address r,address s,int n) public    {
      bool r3 = updateTransferFromOnInsertRecv_transferFrom_r3(o,r,s,n);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r14 = updateMintOnInsertRecv_mint_r14(p,amount);
      if(r14==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function burn(address p,int amount) public    {
      bool r23 = updateBurnOnInsertRecv_burn_r23(p,amount);
      if(r23==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function increaseAllowance(address p,address s,int d) public    {
      bool r15 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r15(p,s,d);
      if(r15==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address s,address r,int n) public    {
      bool r12 = updateTransferOnInsertRecv_transfer_r12(s,r,n);
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function updateTotalMintOnInsertMint_r29(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r26(p,delta0);
  }
  function updateSpentTotalOnInsertTransferFrom_r20(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r32(o,s,delta0);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r32(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateOwnerOnInsertConstructor_r30() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTotalSupplyOnIncrementAllBurn_r19(int b) private    {
      totalSupply.n -= b;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r15(address p,address s,int d) private   returns (bool) {
      updateAllowanceTotalOnInsertIncreaseAllowance_r31(o,s,d);
      emit IncreaseAllowance(o,s,d);
      return true;
      return false;
  }
  function updateTotalOutOnInsertTransfer_r33(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r26(p,delta0);
  }
  function updateMintOnInsertRecv_mint_r14(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>=0) {
        updateAllMintOnInsertMint_r0(n);
        updateTotalMintOnInsertMint_r29(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalMint_r26(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateAllowanceOnIncrementSpentTotal_r32(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateBurnFromOnInsertRecv_burnFrom_r28(address p,address from,int n) private   returns (bool) {
      if(n>0) {
        updateTransferFromOnInsertBurnFrom_r27(s,p,n);
        updateBurnOnInsertBurnFrom_r7(p,n);
        emit BurnFrom(s,p,n);
        return true;
      }
      return false;
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r19(delta0);
  }
  function updateBalanceOfOnIncrementTotalOut_r26(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateTotalSupplyOnInsertConstructor_r5() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateTotalBurnOnInsertBurn_r2(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r26(p,delta0);
  }
  function updateTransferOnInsertTransferFrom_r24(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r33(o,n);
      updateTotalInOnInsertTransfer_r8(r,n);
      emit Transfer(o,r,n);
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
  function updateAllBurnOnInsertBurn_r18(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r19(delta0);
  }
  function updateBalanceOfOnIncrementTotalIn_r26(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTransferFromOnInsertBurnFrom_r27(address s,address p,int n) private    {
      updateTransferOnInsertTransferFrom_r24(s,p,n);
      updateSpentTotalOnInsertTransferFrom_r20(s,address(0),n);
      emit TransferFrom(s,p,address(0),n);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateBalanceOfOnIncrementTotalBurn_r26(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r31(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r32(o,s,delta0);
  }
  function updateBurnOnInsertBurnFrom_r7(address p,int n) private    {
      updateTotalBurnOnInsertBurn_r2(p,n);
      updateAllBurnOnInsertBurn_r18(n);
      emit Burn(p,n);
  }
  function updateBurnOnInsertRecv_burn_r23(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(n>0 && o_1==s_1) {
        updateTotalBurnOnInsertBurn_r2(p,n);
        updateAllBurnOnInsertBurn_r18(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertRecv_transfer_r12(address s,address r,int n) private   returns (bool) {
      int m_1 = balanceOf[s].n;
      if(n>0 && r!=address(0) && s!=address(0) && n<=m_1) {
        updateTotalOutOnInsertTransfer_r33(s,n);
        updateTotalInOnInsertTransfer_r8(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateTotalInOnInsertTransfer_r8(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r26(p,delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r3(address o,address r,address s,int n) private   returns (bool) {
      int m_1 = balanceOf[o].n;
      if(n>0 && n<=m_1) {
        updateSpentTotalOnInsertTransferFrom_r20(o,s,n);
        updateTransferOnInsertTransferFrom_r24(o,r,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
}