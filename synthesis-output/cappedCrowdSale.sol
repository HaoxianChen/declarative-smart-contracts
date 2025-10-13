contract CappedCrowdSale {
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
  struct CapTuple {
    int n;
    bool _valid;
  }
  struct FinalizedTuple {
    bool b;
    bool _valid;
  }
  struct RateTuple {
    int r;
    bool _valid;
  }
  struct RaisedTuple {
    int n;
    bool _valid;
  }
  CapTuple cap;
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  RateTuple rate;
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalSupplyTuple totalSupply;
  RaisedTuple raised;
  FinalizedTuple finalized;
  event Mint(address p,int amount);
  event TransferFrom(address from,address to,address spender,int amount);
  event BuyToken(address p,int amount);
  event BuyAfterFinalize();
  event Transfer(address from,address to,int amount);
  event Finalize();
  event IncreaseAllowance(address o,address s,int n);
  event Burn(address p,int amount);
  constructor(int c) public {
    updateOwnerOnInsertConstructor_r26();
    updateOnceFinalizeOnInsertConstructor_r11();
    updateTotalSupplyOnInsertConstructor_r15();
    updateCapOnInsertConstructor_r2(c);
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
  function buyToken(address p,int amount) public    {
      bool r3 = updateBuyTokenOnInsertRecv_buyToken_r3(p,amount);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address o,address s,int n) public    {
      bool r21 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r21(o,s,n);
      if(r21==false) {
        revert("Rule condition failed");
      }
  }
  function getRaised() public view  returns (int) {
      int n = raised.n;
      return n;
  }
  function transfer(address from,address to,int amount) public    {
      bool r29 = updateTransferOnInsertRecv_transfer_r29(from,to,amount);
      if(r29==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r28 = updateBurnOnInsertRecv_burn_r28(p,amount);
      if(r28==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r14 = updateTransferFromOnInsertRecv_transferFrom_r14(from,to,spender,amount);
      if(r14==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function mint(address p,int amount) public    {
      bool r25 = updateMintOnInsertRecv_mint_r25(p,amount);
      if(r25==false) {
        revert("Rule condition failed");
      }
  }
  function getCap() public view  returns (int) {
      int n = cap.n;
      return n;
  }
  function getFinalized() public view  returns (bool) {
      bool b = finalized.b;
      return b;
  }
  function updateBurnOnInsertRecv_burn_r28(address p,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(n<=balanceOf_x1) {
        updateTotalBurnOnInsertBurn_r0(p,n);
        updateAllBurnOnInsertBurn_r16(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateSpentTotalOnInsertTransferFrom_r9(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r23(o,s,delta0);
  }
  function updateAllowanceOnIncrementSpentTotal_r23(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTotalSupplyOnIncrementAllBurn_r17(int b) private    {
      totalSupply.n -= b;
  }
  function updateTotalInOnInsertTransfer_r13(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r7(p,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateBuyTokenOnInsertRecv_buyToken_r3(address p,int m) private   returns (bool) {
      int r = rate.r;
      int n = m*r;
      updateRaisedOnInsertBuyToken_r24(n);
      updateMintOnInsertBuyToken_r1(p,n);
      emit BuyToken(p,n);
      return true;
      return false;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r14(address o,address r,address s,int n) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[o].n;
      if(n>=0 && n<balanceOf_x1_1) {
        updateTransferOnInsertTransferFrom_r19(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r9(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateOwnerOnInsertConstructor_r26() private    {
      address s = msg.sender;
      // Empty()
  }
  function updateMintOnInsertBuyToken_r1(address p,int n) private    {
      updateTotalMintOnInsertMint_r22(p,n);
      updateAllMintOnInsertMint_r10(n);
      emit Mint(p,n);
  }
  function updateOnceFinalizeOnInsertFinalize_r5() private    {
      // Empty()
  }
  function updateCapOnInsertConstructor_r2(int c) private    {
      if(c>0) {
        cap = CapTuple(c,true);
      }
  }
  function updateTotalOutOnInsertTransfer_r27(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r7(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalBurn_r7(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateAllBurnOnInsertBurn_r16(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r17(delta0);
  }
  function updateTotalBurnOnInsertBurn_r0(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r7(p,delta0);
  }
  function updateTotalSupplyOnIncrementAllMint_r17(int m) private    {
      totalSupply.n += m;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r8(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r23(o,s,delta0);
  }
  function updateFinalizedOnInsertFinalize_r6() private    {
      finalized = FinalizedTuple(true,true);
  }
  function updateTotalSupplyOnInsertConstructor_r15() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateBalanceOfOnIncrementTotalIn_r7(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTransferOnInsertRecv_transfer_r29(address s,address r,int n) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[s].n;
      if(n>=0 && n<=balanceOf_x1_1) {
        updateTotalOutOnInsertTransfer_r27(s,n);
        updateTotalInOnInsertTransfer_r13(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertTransferFrom_r19(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r27(o,n);
      updateTotalInOnInsertTransfer_r13(r,n);
      emit Transfer(o,r,n);
  }
  function updateMintOnInsertRecv_mint_r25(address p,int n) private   returns (bool) {
      if(n>=0) {
        updateTotalMintOnInsertMint_r22(p,n);
        updateAllMintOnInsertMint_r10(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateOnceFinalizeOnInsertConstructor_r11() private    {
      // Empty()
  }
  function updateAllMintOnInsertMint_r10(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r17(delta0);
  }
  function updateTotalMintOnInsertMint_r22(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r7(p,delta0);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r23(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateRaisedOnInsertBuyToken_r24(int n) private    {
      raised.n += n;
  }
  function updateBalanceOfOnIncrementTotalOut_r7(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateBalanceOfOnIncrementTotalMint_r7(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateFinalizeOnInsertRecv_finalize_r4() private   returns (bool) {
      int totalSupply_n = totalSupply.n;
      if(totalSupply_n<0) {
        updateOnceFinalizeOnInsertFinalize_r5();
        updateFinalizedOnInsertFinalize_r6();
        emit Finalize();
        return true;
      }
      return false;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r21(address o,address s,int d) private   returns (bool) {
      updateAllowanceTotalOnInsertIncreaseAllowance_r8(o,s,d);
      emit IncreaseAllowance(o,s,d);
      return true;
      return false;
  }
}