contract FinalizableCrowdSale {
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
  struct RateTuple {
    int r;
    bool _valid;
  }
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  RateTuple rate;
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalSupplyTuple totalSupply;
  OwnerTuple owner;
  event Mint(address p,int amount);
  event TransferFrom(address from,address to,address spender,int amount);
  event BuyToken(address p,int amount);
  event Burn(address p,int amount);
  event IncreaseAllowance(address p,address s,int n);
  event Transfer(address from,address to,int amount);
  event InvalidTx();
  event Finalize();
  constructor() public {
    updateTotalSupplyOnInsertConstructor_r5();
    updateOwnerOnInsertConstructor_r24();
    updateRateOnInsertConstructor_r26();
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r18 = updateTransferFromOnInsertRecv_transferFrom_r18(from,to,spender,amount);
      if(r18==false) {
        revert("Rule condition failed");
      }
  }
  function buyToken(address p,int amount) public    {
      bool r28 = updateBuyTokenOnInsertRecv_buyToken_r28(p,amount);
      if(r28==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r9 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r9(p,s,n);
      if(r9==false) {
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
  function burn(address p,int amount) public    {
      bool r25 = updateBurnOnInsertRecv_burn_r25(p,amount);
      if(r25==false) {
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
  function finalize() public    {
      bool r6 = updateFinalizeOnInsertRecv_finalize_r6();
      if(r6==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r29 = updateTransferOnInsertRecv_transfer_r29(from,to,amount);
      if(r29==false) {
        revert("Rule condition failed");
      }
  }
  function updateBalanceOfOnIncrementTotalIn_r8(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateBalanceOfOnIncrementTotalMint_r8(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateAllBurnOnInsertBurn_r16(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r17(delta0);
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r17(delta0);
  }
  function updateFinalizeOnInsertRecv_finalize_r6() private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        emit Finalize();
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r17(int b) private    {
      totalSupply.n -= b;
  }
  function updateTotalSupplyOnInsertConstructor_r5() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateTotalInOnInsertTransfer_r14(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r8(p,delta0);
  }
  function updateTransferOnInsertRecv_transfer_r29(address s,address r,int n) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[s].n;
      if(n>0 && n<=balanceOf_x1_1) {
        updateTotalOutOnInsertTransfer_r12(s,n);
        updateTotalInOnInsertTransfer_r14(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateMintOnInsertBuyToken_r3(address p,int m) private    {
      int r = rate.r;
      int n = m*r;
      updateTotalMintOnInsertMint_r22(p,n);
      updateAllMintOnInsertMint_r0(n);
      emit Mint(p,n);
  }
  function updateTotalBurnOnInsertBurn_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r8(p,delta0);
  }
  function updateBurnOnInsertRecv_burn_r25(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && n<=balanceOf_x1) {
        updateTotalBurnOnInsertBurn_r1(p,n);
        updateAllBurnOnInsertBurn_r16(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateSpentTotalOnInsertTransferFrom_r11(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r27(o,s,delta0);
  }
  function updateAllowanceOnIncrementSpentTotal_r27(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r9(address o,address s,int d) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r10(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateBuyTokenOnInsertRecv_buyToken_r28(address p,int m) private   returns (bool) {
      if(m>0) {
        updateMintOnInsertBuyToken_r3(p,m);
        emit BuyToken(p,m);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r17(int m) private    {
      totalSupply.n += m;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r27(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateOwnerOnInsertConstructor_r24() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateBalanceOfOnIncrementTotalBurn_r8(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateMintOnInsertRecv_mint_r4(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>=0) {
        updateTotalMintOnInsertMint_r22(p,n);
        updateAllMintOnInsertMint_r0(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r18(address o,address r,address s,int n) private   returns (bool) {
      int balanceOf_x1_2 = balanceOf[o].n;
      int allowance_x2_1 = allowance[o][s].n;
      if(n>=0 && n<allowance_x2_1 && n<balanceOf_x1_2) {
        updateTransferOnInsertTransferFrom_r20(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r11(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTotalMintOnInsertMint_r22(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r8(p,delta0);
  }
  function updateRateOnInsertConstructor_r26() private    {
      rate = RateTuple(1,true);
  }
  function updateTotalOutOnInsertTransfer_r12(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r8(p,delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r10(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r27(o,s,delta0);
  }
  function updateTransferOnInsertTransferFrom_r20(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r14(r,n);
      updateTotalOutOnInsertTransfer_r12(o,n);
      emit Transfer(o,r,n);
  }
  function updateBalanceOfOnIncrementTotalOut_r8(address p,int o) private    {
      balanceOf[p].n -= o;
  }
}