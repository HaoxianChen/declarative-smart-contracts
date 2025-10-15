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
  struct OwnerTuple {
    address p;
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
  OwnerTuple owner;
  event Mint(address p,int amount);
  event TransferFrom(address from,address to,address spender,int amount);
  event BuyToken(address p,int amount);
  event BuyAfterFinalize();
  event Transfer(address from,address to,int amount);
  event InvalidTx();
  event Finalize();
  event IncreaseAllowance(address o,address s,int n);
  event Burn(address p,int amount);
  constructor(int c,int r) public {
    updateCapOnInsertConstructor_r10(c);
    updateOwnerOnInsertConstructor_r3();
    updateTotalSupplyOnInsertConstructor_r6();
    updateRateOnInsertConstructor_r20(r);
    updateOnceFinalizeOnInsertConstructor_r26();
  }
  function finalize() public    {
      bool r27 = updateFinalizeOnInsertRecv_finalize_r27();
      if(r27==false) {
        revert("Rule condition failed");
      }
  }
  function getRaised() public view  returns (int) {
      int n = raised.n;
      return n;
  }
  function transfer(address from,address to,int amount) public    {
      bool r28 = updateTransferOnInsertRecv_transfer_r28(from,to,amount);
      if(r28==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function increaseAllowance(address o,address s,int n) public    {
      bool r29 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r29(o,s,n);
      if(r29==false) {
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
  function buyToken(address p,int amount) public    {
      bool r16 = updateBuyTokenOnInsertRecv_buyToken_r16(p,amount);
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r25 = updateTransferFromOnInsertRecv_transferFrom_r25(from,to,spender,amount);
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
  function updateTransferOnInsertTransferFrom_r23(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r15(r,n);
      updateTotalOutOnInsertTransfer_r13(o,n);
      emit Transfer(o,r,n);
  }
  function updateAllowanceOnIncrementSpentTotal_r32(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTransferOnInsertRecv_transfer_r28(address s,address r,int n) private   returns (bool) {
      int rate_r = rate.r;
      if(0!=rate_r) {
        updateTotalInOnInsertTransfer_r15(r,n);
        updateTotalOutOnInsertTransfer_r13(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r32(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateFinalizedOnInsertFinalize_r8() private    {
      finalized = FinalizedTuple(true,true);
  }
  function updateFinalizeOnInsertRecv_finalize_r27() private   returns (bool) {
      updateOnceFinalizeOnInsertFinalize_r7();
      updateFinalizedOnInsertFinalize_r8();
      emit Finalize();
      return true;
      return false;
  }
  function updateBalanceOfOnIncrementTotalBurn_r9(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r11(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r32(o,s,delta0);
  }
  function updateBalanceOfOnIncrementTotalOut_r9(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateBuyTokenOnInsertRecv_buyToken_r16(address p,int m) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(balanceOf_x1>0) {
        updateMintOnInsertBuyToken_r2(p,m);
        updateRaisedOnInsertBuyToken_r33(m);
        emit BuyToken(p,m);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r6() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateTotalOutOnInsertTransfer_r13(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r9(p,delta0);
  }
  function updateTotalBurnOnInsertBurn_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r9(p,delta0);
  }
  function updateTotalInOnInsertTransfer_r15(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r9(p,delta0);
  }
  function updateOnceFinalizeOnInsertFinalize_r7() private    {
      // Empty()
  }
  function updateBalanceOfOnIncrementTotalMint_r9(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateRaisedOnInsertBuyToken_r33(int n) private    {
      raised.n += n;
  }
  function updateRateOnInsertConstructor_r20(int r) private    {
      if(r>0) {
        rate = RateTuple(r,true);
      }
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r19(delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r25(address o,address r,address s,int n) private   returns (bool) {
      int allowance_x2 = allowance[o][r].n;
      if(allowance_x2>0) {
        updateSpentTotalOnInsertTransferFrom_r12(o,s,n);
        updateTransferOnInsertTransferFrom_r23(o,r,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateTotalMintOnInsertMint_r30(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r9(p,delta0);
  }
  function updateTotalSupplyOnIncrementAllMint_r19(int m) private    {
      totalSupply.n += m;
  }
  function updateBalanceOfOnIncrementTotalIn_r9(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalSupplyOnIncrementAllBurn_r19(int b) private    {
      totalSupply.n -= b;
  }
  function updateOnceFinalizeOnInsertConstructor_r26() private    {
      // Empty()
  }
  function updateOwnerOnInsertConstructor_r3() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateCapOnInsertConstructor_r10(int c) private    {
      if(c>0) {
        cap = CapTuple(c,true);
      }
  }
  function updateAllBurnOnInsertBurn_r18(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r19(delta0);
  }
  function updateSpentTotalOnInsertTransferFrom_r12(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r32(o,s,delta0);
  }
  function updateMintOnInsertRecv_mint_r4(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>=0) {
        updateAllMintOnInsertMint_r0(n);
        updateTotalMintOnInsertMint_r30(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateMintOnInsertBuyToken_r2(address p,int m) private    {
      int r = rate.r;
      int n = m*r;
      updateAllMintOnInsertMint_r0(n);
      updateTotalMintOnInsertMint_r30(p,n);
      emit Mint(p,n);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r29(address o,address s,int d) private   returns (bool) {
      updateAllowanceTotalOnInsertIncreaseAllowance_r11(o,s,d);
      emit IncreaseAllowance(o,s,d);
      return true;
      return false;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateBurnOnInsertRecv_burn_r31(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && n<=balanceOf_x1) {
        updateTotalBurnOnInsertBurn_r1(p,n);
        updateAllBurnOnInsertBurn_r18(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
}