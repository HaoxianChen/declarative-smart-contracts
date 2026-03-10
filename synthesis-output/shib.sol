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
    updateTotalSupplyOnInsertConstructor_r2();
    updateOwnerOnInsertConstructor_r30();
  }
  function mint(address p,int amount) public    {
      bool r18 = updateMintOnInsertRecv_mint_r18(p,amount);
      if(r18==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int d) public    {
      bool r16 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r16(p,s,d);
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address o,address r,address s,int n) public    {
      bool r10 = updateTransferFromOnInsertRecv_transferFrom_r10(o,r,s,n);
      if(r10==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function transfer(address s,address r,int n) public    {
      bool r12 = updateTransferOnInsertRecv_transfer_r12(s,r,n);
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
  function burnFrom(address p,address from,int n) public    {
      bool r14 = updateBurnFromOnInsertRecv_burnFrom_r14(p,from,n);
      if(r14==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function updateAllowanceOnIncrementSpentTotal_r32(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTotalSupplyOnIncrementAllMint_r22(int m) private    {
      totalSupply.n += m;
  }
  function updateBurnFromOnInsertRecv_burnFrom_r14(address p,address from,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(n>0 && n<balanceOf_x1) {
        updateTransferFromOnInsertBurnFrom_r8(p,from,n);
        updateBurnOnInsertBurnFrom_r4(from,n);
        emit BurnFrom(p,from,n);
        return true;
      }
      return false;
  }
  function updateTotalOutOnInsertTransfer_r33(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r28(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalMint_r28(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateSpentTotalOnInsertTransferFrom_r23(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r32(o,s,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalSupplyOnIncrementAllBurn_r22(int b) private    {
      totalSupply.n -= b;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r31(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r32(o,s,delta0);
  }
  function updateTotalMintOnInsertMint_r29(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r28(p,delta0);
  }
  function updateTransferFromOnInsertBurnFrom_r8(address s,address p,int n) private    {
      updateTransferOnInsertTransferFrom_r25(s,p,n);
      updateSpentTotalOnInsertTransferFrom_r23(s,address(0),n);
      emit TransferFrom(s,p,address(0),n);
  }
  function updateTotalSupplyOnInsertConstructor_r2() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateOwnerOnInsertConstructor_r30() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTotalBurnOnInsertBurn_r0(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r28(p,delta0);
  }
  function updateAllBurnOnInsertBurn_r21(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r22(delta0);
  }
  function updateMintOnInsertRecv_mint_r18(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && amount>=0) {
        updateAllMintOnInsertMint_r13(amount);
        updateTotalMintOnInsertMint_r29(p,amount);
        emit Mint(p,amount);
        return true;
      }
      return false;
  }
  function updateBurnOnInsertBurnFrom_r4(address p,int n) private    {
      updateTotalBurnOnInsertBurn_r0(p,n);
      updateAllBurnOnInsertBurn_r21(n);
      emit Burn(p,n);
  }
  function updateBurnOnInsertRecv_burn_r26(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(amount>0 && o_1==s_1 && amount<=balanceOf_x1) {
        updateAllBurnOnInsertBurn_r21(amount);
        updateTotalBurnOnInsertBurn_r0(p,amount);
        emit Burn(p,amount);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r32(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateBalanceOfOnIncrementTotalIn_r28(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateAllMintOnInsertMint_r13(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r22(delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r10(address o,address r,address s,int n) private   returns (bool) {
      int allowance_x2 = allowance[o][s].n;
      int m_1 = balanceOf[o].n;
      if(n>0 && n<=m_1 && n<=allowance_x2) {
        updateTransferOnInsertTransferFrom_r25(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r23(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r28(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateTransferOnInsertTransferFrom_r25(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r6(r,n);
      updateTotalOutOnInsertTransfer_r33(o,n);
      emit Transfer(o,r,n);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r16(address p,address s,int d) private   returns (bool) {
      if(d>=0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r31(p,s,d);
        emit IncreaseAllowance(p,s,d);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertRecv_transfer_r12(address s,address r,int n) private   returns (bool) {
      int m_1 = balanceOf[s].n;
      if(n>0 && r!=address(0) && s!=address(0) && n<=m_1) {
        updateTotalInOnInsertTransfer_r6(r,n);
        updateTotalOutOnInsertTransfer_r33(s,n);
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
}