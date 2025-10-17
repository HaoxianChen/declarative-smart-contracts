contract Erc20 {
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
  event Mint(address p,int amount);
  event TransferFrom(address from,address to,address spender,int amount);
  event Burn(address p,int amount);
  event IncreaseAllowance(address p,address s,int n);
  event Transfer(address from,address to,int amount);
  event InvalidTx();
  constructor() public {
    updateTotalSupplyOnInsertConstructor_r3();
    updateOwnerOnInsertConstructor_r17();
    updateTotalBalancesOnInsertConstructor_r14();
  }
  function mint(address p,int amount) public    {
      bool r2 = updateMintOnInsertRecv_mint_r2(p,amount);
      if(r2==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r24 = updateTransferFromOnInsertRecv_transferFrom_r24(from,to,spender,amount);
      if(r24==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r20 = updateBurnOnInsertRecv_burn_r20(p,amount);
      if(r20==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r6 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r6(p,s,n);
      if(r6==false) {
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
  function transfer(address from,address to,int amount) public    {
      bool r25 = updateTransferOnInsertRecv_transfer_r25(from,to,amount);
      if(r25==false) {
        revert("Rule condition failed");
      }
  }
  function updateBurnOnInsertRecv_burn_r20(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && n<=balanceOf_x1) {
        updateTotalBurnOnInsertBurn_r1(p,n);
        updateAllBurnOnInsertBurn_r10(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateTotalBurnOnInsertBurn_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r5(p,delta0);
  }
  function updateMintOnInsertRecv_mint_r2(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>=0) {
        updateAllMintOnInsertMint_r0(n);
        updateTotalMintOnInsertMint_r16(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateTotalBalancesOnInsertConstructor_r14() private    {
      // Empty()
  }
  function updateTotalOutOnInsertTransfer_r23(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r5(p,delta0);
  }
  function updateTotalSupplyOnIncrementAllBurn_r11(int b) private    {
      totalSupply.n -= b;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalMintOnInsertMint_r16(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r5(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalMint_r5(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTotalSupplyOnInsertConstructor_r3() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateSpentTotalOnInsertTransferFrom_r19(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r21(o,s,delta0);
  }
  function updateTotalInOnInsertTransfer_r8(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r5(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalOut_r5(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r6(address o,address s,int d) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r15(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalIn_r5(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r15(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r21(o,s,delta0);
  }
  function updateBalanceOfOnIncrementTotalBurn_r5(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateAllBurnOnInsertBurn_r10(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r11(delta0);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r21(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateAllowanceOnIncrementSpentTotal_r21(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r11(delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r24(address o,address r,address s,int n) private   returns (bool) {
      int balanceOf_x1_2 = balanceOf[o].n;
      int allowance_x2_1 = allowance[o][s].n;
      if(n>=0 && n<=allowance_x2_1 && n<balanceOf_x1_2) {
        updateTransferOnInsertTransferFrom_r12(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r19(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertRecv_transfer_r25(address s,address r,int n) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[s].n;
      if(n>0 && n<=balanceOf_x1_1) {
        updateTotalOutOnInsertTransfer_r23(s,n);
        updateTotalInOnInsertTransfer_r8(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r11(int m) private    {
      totalSupply.n += m;
  }
  function updateOwnerOnInsertConstructor_r17() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTransferOnInsertTransferFrom_r12(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r23(o,n);
      updateTotalInOnInsertTransfer_r8(r,n);
      emit Transfer(o,r,n);
  }
}