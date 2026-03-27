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
  struct EndTuple {
    uint time;
    bool _valid;
  }
  struct RateTuple {
    int r;
    bool _valid;
  }
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  EndTuple end;
  RateTuple rate;
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalSupplyTuple totalSupply;
  OwnerTuple owner;
  event UnauthorizedFinalize();
  event Mint(address p,int amount);
  event TransferFrom(address from,address to,address spender,int amount);
  event UnauthorizedBurn();
  event Burn(address p,int amount);
  event IncreaseAllowance(address p,address s,int n);
  event Transfer(address from,address to,int amount);
  event Finalize();
  event UnauthorizedMint();
  event BuyToken(address p,int amount);
  constructor() public {
    updateOnceBuyOutsideSaleOnInsertConstructor_r22();
    updateOnceFinalizeBeforeEndOnInsertConstructor_r10();
    updateOwnerOnInsertConstructor_r29();
    updateTotalSupplyOnInsertConstructor_r19();
  }
  function finalize() public    {
      bool r4 = updateFinalizeOnInsertRecv_finalize_r4();
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r11 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r11(p,s,n);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r14 = updateTransferOnInsertRecv_transfer_r14(from,to,amount);
      if(r14==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r15 = updateTransferFromOnInsertRecv_transferFrom_r15(from,to,spender,amount);
      if(r15==false) {
        revert("Rule condition failed");
      }
  }
  function buyToken(address p,int amount) public    {
      bool r25 = updateBuyTokenOnInsertRecv_buyToken_r25(p,amount);
      if(r25==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r30 = updateBurnOnInsertRecv_burn_r30(p,amount);
      if(r30==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r31 = updateMintOnInsertRecv_mint_r31(p,amount);
      if(r31==false) {
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
  function updateBalanceOfOnIncrementTotalMint_r8(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r32(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateMintOnInsertBuyToken_r2(address p,int m) private    {
      int r = rate.r;
      int n = m*r;
      updateAllMintOnInsertMint_r0(n);
      updateTotalMintOnInsertMint_r27(p,n);
      emit Mint(p,n);
  }
  function updateTotalSupplyOnIncrementAllBurn_r24(int b) private    {
      totalSupply.n -= b;
  }
  function updateTransferOnInsertRecv_transfer_r14(address from,address to,int amount) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[from].n;
      if(amount>0 && amount<=balanceOf_x1_1) {
        updateTotalOutOnInsertTransfer_r13(from,amount);
        updateTotalInOnInsertTransfer_r18(to,amount);
        emit Transfer(from,to,amount);
        return true;
      }
      return false;
  }
  function updateAllBurnOnInsertBurn_r23(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r24(delta0);
  }
  function updateTotalInOnInsertTransfer_r18(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r8(p,delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r26(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r32(o,s,delta0);
  }
  function updateMintOnInsertRecv_mint_r31(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && amount>0) {
        updateAllMintOnInsertMint_r0(amount);
        updateTotalMintOnInsertMint_r27(p,amount);
        emit Mint(p,amount);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r8(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateOnceFinalizeBeforeEndOnInsertConstructor_r10() private    {
      // Empty()
  }
  function updateTotalSupplyOnInsertConstructor_r19() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateOnceBuyOutsideSaleOnInsertConstructor_r22() private    {
      // Empty()
  }
  function updateOwnerOnInsertConstructor_r29() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTotalSupplyOnIncrementAllMint_r24(int m) private    {
      totalSupply.n += m;
  }
  function updateBalanceOfOnIncrementTotalIn_r8(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateBuyTokenOnInsertRecv_buyToken_r25(address p,int amount) private   returns (bool) {
      updateMintOnInsertBuyToken_r2(p,amount);
      emit BuyToken(p,amount);
      return true;
      return false;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r15(address from,address to,address spender,int amount) private   returns (bool) {
      int balanceOf_x1_2 = balanceOf[from].n;
      int allowance_x2_1 = allowance[from][spender].n;
      if(amount>0 && amount<allowance_x2_1 && amount<=balanceOf_x1_2) {
        updateTransferOnInsertTransferFrom_r9(from,to,amount);
        updateSpentTotalOnInsertTransferFrom_r12(from,spender,amount);
        emit TransferFrom(from,to,spender,amount);
        return true;
      }
      return false;
  }
  function updateTotalOutOnInsertTransfer_r13(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r8(p,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r11(address p,address s,int n) private   returns (bool) {
      if(n>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r26(p,s,n);
        emit IncreaseAllowance(p,s,n);
        return true;
      }
      return false;
  }
  function updateTotalMintOnInsertMint_r27(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r8(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalBurn_r8(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateBurnOnInsertRecv_burn_r30(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && amount<=balanceOf_x1) {
        updateAllBurnOnInsertBurn_r23(amount);
        updateTotalBurnOnInsertBurn_r1(p,amount);
        emit Burn(p,amount);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertTransferFrom_r9(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r13(o,n);
      updateTotalInOnInsertTransfer_r18(r,n);
      emit Transfer(o,r,n);
  }
  function updateSpentTotalOnInsertTransferFrom_r12(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r32(o,s,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateFinalizeOnInsertRecv_finalize_r4() private   returns (bool) {
      uint end_time = end.time;
      address s = msg.sender;
      address o = owner.p;
      if(o==s && 0!=end_time) {
        emit Finalize();
        return true;
      }
      return false;
  }
  function updateTotalBurnOnInsertBurn_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r8(p,delta0);
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r24(delta0);
  }
}