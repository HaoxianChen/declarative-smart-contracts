contract Erc20burnable {
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
  event BurnFrom(address from,address spender,int amount);
  event Transfer(address from,address to,int amount);
  event InvalidTx();
  event IncreaseAllowance(address o,address s,int n);
  event Burn(address p,int amount);
  constructor() public {
    updateTotalSupplyOnInsertConstructor_r20();
    updateOwnerOnInsertConstructor_r26();
  }
  function transfer(address from,address to,int amount) public    {
      bool r11 = updateTransferOnInsertRecv_transfer_r11(from,to,amount);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function burnFrom(address from,address spender,int amount) public    {
      bool r5 = updateBurnFromOnInsertRecv_burnFrom_r5(from,spender,amount);
      if(r5==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address o,address s,int n) public    {
      bool r3 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r3(o,s,n);
      if(r3==false) {
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
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r19 = updateTransferFromOnInsertRecv_transferFrom_r19(from,to,spender,amount);
      if(r19==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r7 = updateBurnOnInsertRecv_burn_r7(p,amount);
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function updateBalanceOfOnIncrementTotalBurn_r12(address p,int b) private    {
      balanceOf[p].n -= b;
  }
  function updateTotalSupplyOnIncrementAllBurn_r23(int b) private    {
      totalSupply.n -= b;
  }
  function updateBalanceOfOnIncrementTotalOut_r12(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateTotalBurnOnInsertBurn_r16(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r12(p,delta0);
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r23(delta0);
  }
  function updateBurnFromTotalOnInsertBurnFrom_r2(address from,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementBurnFromTotal_r14(from,s,delta0);
  }
  function updateAllBurnOnInsertBurn_r22(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r23(delta0);
  }
  function updateBurnOnInsertBurnFrom_r25(address p,int n) private    {
      updateAllBurnOnInsertBurn_r22(n);
      updateTotalBurnOnInsertBurn_r16(p,n);
      emit Burn(p,n);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r19(address o,address r,address s,int n) private   returns (bool) {
      int allowance_x2 = allowance[o][s].n;
      if(n<allowance_x2) {
        updateSpentTotalOnInsertTransferFrom_r28(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r9(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r14(o,s,delta0);
  }
  function updateAllowanceOnIncrementSpentTotal_r14(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateOwnerOnInsertConstructor_r26() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTotalInOnInsertTransfer_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r12(p,delta0);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r3(address o,address s,int d) private   returns (bool) {
      if(d>=0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r9(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateSpentTotalOnInsertTransferFrom_r28(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r14(o,s,delta0);
  }
  function updateTotalSupplyOnIncrementAllMint_r23(int m) private    {
      totalSupply.n += m;
  }
  function updateTransferOnInsertRecv_transfer_r11(address s,address r,int n) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[s].n;
      if(n>0 && n<=balanceOf_x1_1) {
        updateTotalOutOnInsertTransfer_r10(s,n);
        updateTotalInOnInsertTransfer_r1(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r14(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateBalanceOfOnIncrementTotalMint_r12(address p,int m) private    {
      balanceOf[p].n += m;
  }
  function updateBurnOnInsertRecv_burn_r7(address p,int n) private   returns (bool) {
      address s = msg.sender;
      int balanceOf_x1 = balanceOf[p].n;
      if(p==s && n<=balanceOf_x1) {
        updateAllBurnOnInsertBurn_r22(n);
        updateTotalBurnOnInsertBurn_r16(p,n);
        emit Burn(p,n);
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
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateMintOnInsertRecv_mint_r4(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>=0) {
        updateAllMintOnInsertMint_r0(n);
        updateTotalMintOnInsertMint_r24(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementBurnFromTotal_r14(address o,address s,int bf) private    {
      allowance[o][s].n -= bf;
  }
  function updateBurnFromOnInsertRecv_burnFrom_r5(address from,address s,int n) private   returns (bool) {
      address msgSender = msg.sender;
      address s_p_1 = msg.sender;
      int allowance_x2_0 = allowance[s][from].n;
      int allowance_x2_1 = allowance[msgSender][from].n;
      if(n>0 && s==s_p_1 && n<=allowance_x2_0 && 0==allowance_x2_1) {
        updateBurnFromTotalOnInsertBurnFrom_r2(from,s,n);
        updateBurnOnInsertBurnFrom_r25(from,n);
        emit BurnFrom(from,s,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r20() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateBalanceOfOnIncrementTotalIn_r12(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTotalMintOnInsertMint_r24(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r12(p,delta0);
  }
  function updateTotalOutOnInsertTransfer_r10(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r12(p,delta0);
  }
}