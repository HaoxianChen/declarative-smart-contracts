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
    updateTotalSupplyOnInsertConstructor_r4();
    updateOwnerOnInsertConstructor_r19();
  }
  function transferFrom(address o,address r,address s,int n) public    {
      bool r25 = updateTransferFromOnInsertRecv_transferFrom_r25(o,r,s,n);
      if(r25==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r21 = updateBurnOnInsertRecv_burn_r21(p,amount);
      if(r21==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function transfer(address s,address r,int n) public    {
      bool r24 = updateTransferOnInsertRecv_transfer_r24(s,r,n);
      if(r24==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int d) public    {
      bool r12 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r12(p,s,d);
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function mint(address p,int amount) public    {
      bool r3 = updateMintOnInsertRecv_mint_r3(p,amount);
      if(r3==false) {
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
  function updateOwnerOnInsertConstructor_r19() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateBalanceOfOnIncrementTotalOut_r17(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateBalanceOfOnIncrementTotalMint_r17(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateTransferOnInsertRecv_transfer_r24(address s,address r,int n) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[s].n;
      if(n>=0 && n<balanceOf_x1_1) {
        updateTotalInOnInsertTransfer_r15(r,n);
        updateTotalOutOnInsertTransfer_r23(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r25(address o,address r,address s,int n) private   returns (bool) {
      int balanceOf_x1_2 = balanceOf[o].n;
      int allowance_x2_1 = allowance[o][s].n;
      if(n>=0 && n<allowance_x2_1 && n<balanceOf_x1_2) {
        updateTransferOnInsertTransferFrom_r16(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r11(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateTotalBurnOnInsertBurn_r2(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r17(p,delta0);
  }
  function updateTransferOnInsertTransferFrom_r16(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r15(r,n);
      updateTotalOutOnInsertTransfer_r23(o,n);
      emit Transfer(o,r,n);
  }
  function updateBurnOnInsertRecv_burn_r21(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && n<=balanceOf_x1) {
        updateTotalBurnOnInsertBurn_r2(p,n);
        updateAllBurnOnInsertBurn_r9(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r10(delta0);
  }
  function updateBurnFromOnInsertRecv_burnFrom_r14(address s,address p,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[s].n;
      if(n<=balanceOf_x1) {
        updateTransferFromOnInsertBurnFrom_r7(s,p,n);
        updateBurnOnInsertBurnFrom_r5(p,n);
        emit BurnFrom(s,p,n);
        return true;
      }
      return false;
  }
  function updateTransferFromOnInsertBurnFrom_r7(address s,address p,int n) private    {
      updateTransferOnInsertTransferFrom_r16(s,p,n);
      updateSpentTotalOnInsertTransferFrom_r11(s,address(0),n);
      emit TransferFrom(s,p,address(0),n);
  }
  function updateSpentTotalOnInsertTransferFrom_r11(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r22(o,s,delta0);
  }
  function updateAllBurnOnInsertBurn_r9(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r10(delta0);
  }
  function updateTotalMintOnInsertMint_r18(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r17(p,delta0);
  }
  function updateTotalSupplyOnInsertConstructor_r4() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateAllowanceOnIncrementSpentTotal_r22(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalSupplyOnIncrementAllBurn_r10(int b) private    {
      totalSupply.n -= b;
  }
  function updateMintOnInsertRecv_mint_r3(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>=0) {
        updateAllMintOnInsertMint_r0(n);
        updateTotalMintOnInsertMint_r18(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalIn_r17(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateBurnOnInsertBurnFrom_r5(address p,int n) private    {
      updateTotalBurnOnInsertBurn_r2(p,n);
      updateAllBurnOnInsertBurn_r9(n);
      emit Burn(p,n);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r22(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateTotalSupplyOnIncrementAllMint_r10(int m) private    {
      totalSupply.n += m;
  }
  function updateBalanceOfOnIncrementTotalBurn_r17(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r12(address o,address s,int d) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r20(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateTotalOutOnInsertTransfer_r23(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r17(p,delta0);
  }
  function updateTotalInOnInsertTransfer_r15(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r17(p,delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r20(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r22(o,s,delta0);
  }
}