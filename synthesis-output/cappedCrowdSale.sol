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
    updateTotalSupplyOnInsertConstructor_r13();
    updateOwnerOnInsertConstructor_r26();
    updateOnceFinalizeOnInsertConstructor_r9();
    updateCapOnInsertConstructor_r2(c);
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r12 = updateTransferFromOnInsertRecv_transferFrom_r12(from,to,spender,amount);
      if(r12==false) {
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
  function transfer(address from,address to,int amount) public    {
      bool r29 = updateTransferOnInsertRecv_transfer_r29(from,to,amount);
      if(r29==false) {
        revert("Rule condition failed");
      }
  }
  function finalize() public    {
      bool r16 = updateFinalizeOnInsertRecv_finalize_r16();
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function updateTransferFromOnInsertRecv_transferFrom_r12(address o,address r,address s,int n) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[o].n;
      if(n>=0 && n<balanceOf_x1_1) {
        updateSpentTotalOnInsertTransferFrom_r7(o,s,n);
        updateTransferOnInsertTransferFrom_r19(o,r,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateCapOnInsertConstructor_r2(int c) private    {
      if(c>0) {
        cap = CapTuple(c,true);
      }
  }
  function updateTotalSupplyOnIncrementAllBurn_r15(int b) private    {
      totalSupply.n -= b;
  }
  function updateBalanceOfOnIncrementTotalOut_r17(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateBalanceOfOnIncrementTotalMint_r17(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateTransferOnInsertRecv_transfer_r29(address s,address r,int n) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[s].n;
      if(n>=0 && n<=balanceOf_x1_1) {
        updateTotalInOnInsertTransfer_r11(r,n);
        updateTotalOutOnInsertTransfer_r27(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementSpentTotal_r23(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateMintOnInsertRecv_mint_r25(address p,int n) private   returns (bool) {
      if(n>=0) {
        updateAllMintOnInsertMint_r8(n);
        updateTotalMintOnInsertMint_r22(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateFinalizeOnInsertRecv_finalize_r16() private   returns (bool) {
      bool finalized_b = finalized.b;
      if(finalized_b==true) {
        updateOnceFinalizeOnInsertFinalize_r4();
        updateFinalizedOnInsertFinalize_r5();
        emit Finalize();
        return true;
      }
      return false;
  }
  function updateFinalizedOnInsertFinalize_r5() private    {
      finalized = FinalizedTuple(true,true);
  }
  function updateTotalBurnOnInsertBurn_r0(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r17(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalIn_r17(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r6(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r23(o,s,delta0);
  }
  function updateBalanceOfOnIncrementTotalBurn_r17(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateTransferOnInsertTransferFrom_r19(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r11(r,n);
      updateTotalOutOnInsertTransfer_r27(o,n);
      emit Transfer(o,r,n);
  }
  function updateBurnOnInsertRecv_burn_r28(address p,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(n<=balanceOf_x1) {
        updateTotalBurnOnInsertBurn_r0(p,n);
        updateAllBurnOnInsertBurn_r14(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateAllBurnOnInsertBurn_r14(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r15(delta0);
  }
  function updateTotalOutOnInsertTransfer_r27(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r17(p,delta0);
  }
  function updateTotalMintOnInsertMint_r22(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r17(p,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateMintOnInsertBuyToken_r1(address p,int n) private    {
      updateAllMintOnInsertMint_r8(n);
      updateTotalMintOnInsertMint_r22(p,n);
      emit Mint(p,n);
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
  function updateOnceFinalizeOnInsertFinalize_r4() private    {
      // Empty()
  }
  function updateSpentTotalOnInsertTransferFrom_r7(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r23(o,s,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r21(address o,address s,int d) private   returns (bool) {
      updateAllowanceTotalOnInsertIncreaseAllowance_r6(o,s,d);
      emit IncreaseAllowance(o,s,d);
      return true;
      return false;
  }
  function updateOwnerOnInsertConstructor_r26() private    {
      address s = msg.sender;
      // Empty()
  }
  function updateTotalSupplyOnIncrementAllMint_r15(int m) private    {
      totalSupply.n += m;
  }
  function updateTotalSupplyOnInsertConstructor_r13() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateOnceFinalizeOnInsertConstructor_r9() private    {
      // Empty()
  }
  function updateTotalInOnInsertTransfer_r11(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r17(p,delta0);
  }
  function updateAllMintOnInsertMint_r8(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r15(delta0);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r23(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateRaisedOnInsertBuyToken_r24(int n) private    {
      raised.n += n;
  }
}