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
    updateOnceFinalizeOnInsertConstructor_r9();
    updateOwnerOnInsertConstructor_r20();
    updateCapOnInsertConstructor_r10(c);
    updateTotalSupplyOnInsertConstructor_r5();
    updateRateOnInsertConstructor_r26(r);
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
  function transfer(address from,address to,int amount) public    {
      bool r16 = updateTransferOnInsertRecv_transfer_r16(from,to,amount);
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function getRaised() public view  returns (int) {
      int n = raised.n;
      return n;
  }
  function finalize() public    {
      bool r24 = updateFinalizeOnInsertRecv_finalize_r24();
      if(r24==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address o,address s,int n) public    {
      bool r19 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r19(o,s,n);
      if(r19==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function buyToken(address p,int amount) public    {
      bool r15 = updateBuyTokenOnInsertRecv_buyToken_r15(p,amount);
      if(r15==false) {
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
  function mint(address p,int amount) public    {
      bool r3 = updateMintOnInsertRecv_mint_r3(p,amount);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function updateBalanceOfOnIncrementTotalIn_r8(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateBalanceOfOnIncrementTotalMint_r8(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r32(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateOnceFinalizeOnInsertFinalize_r6() private    {
      // Empty()
  }
  function updateRaisedOnInsertBuyToken_r33(int n) private    {
      raised.n += n;
  }
  function updateTotalInOnInsertTransfer_r18(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r8(p,delta0);
  }
  function updateFinalizeOnInsertRecv_finalize_r24() private   returns (bool) {
      int raised_n = raised.n;
      if(raised_n<0) {
        updateFinalizedOnInsertFinalize_r7();
        updateOnceFinalizeOnInsertFinalize_r6();
        emit Finalize();
        return true;
      }
      return false;
  }
  function updateTotalMintOnInsertMint_r12(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r8(p,delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r25(address o,address r,address s,int n) private   returns (bool) {
      int balanceOf_x1_2 = balanceOf[o].n;
      int allowance_x2_1 = allowance[o][s].n;
      if(n>0 && n<=allowance_x2_1 && n<balanceOf_x1_2) {
        updateTransferOnInsertTransferFrom_r29(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r13(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateOnceFinalizeOnInsertConstructor_r9() private    {
      // Empty()
  }
  function updateBalanceOfOnIncrementTotalOut_r8(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateTotalOutOnInsertTransfer_r14(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r8(p,delta0);
  }
  function updateTransferOnInsertTransferFrom_r29(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r14(o,n);
      updateTotalInOnInsertTransfer_r18(r,n);
      emit Transfer(o,r,n);
  }
  function updateAllowanceOnIncrementSpentTotal_r32(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r19(address o,address s,int d) private   returns (bool) {
      if(d>=0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r11(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r23(int b) private    {
      totalSupply.n -= b;
  }
  function updateMintOnInsertBuyToken_r2(address p,int m) private    {
      int r = rate.r;
      int n = m*r;
      updateAllMintOnInsertMint_r0(n);
      updateTotalMintOnInsertMint_r12(p,n);
      emit Mint(p,n);
  }
  function updateRateOnInsertConstructor_r26(int r) private    {
      if(r>0) {
        rate = RateTuple(r,true);
      }
  }
  function updateTransferOnInsertRecv_transfer_r16(address s,address r,int n) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[s].n;
      if(n>=0 && n<=balanceOf_x1_1) {
        updateTotalOutOnInsertTransfer_r14(s,n);
        updateTotalInOnInsertTransfer_r18(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateSpentTotalOnInsertTransferFrom_r13(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r32(o,s,delta0);
  }
  function updateTotalSupplyOnInsertConstructor_r5() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateTotalSupplyOnIncrementAllMint_r23(int m) private    {
      totalSupply.n += m;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r11(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r32(o,s,delta0);
  }
  function updateMintOnInsertRecv_mint_r3(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>=0) {
        updateAllMintOnInsertMint_r0(n);
        updateTotalMintOnInsertMint_r12(p,n);
        emit Mint(p,n);
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
  function updateBurnOnInsertRecv_burn_r31(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && n<=balanceOf_x1) {
        updateTotalBurnOnInsertBurn_r1(p,n);
        updateAllBurnOnInsertBurn_r22(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r23(delta0);
  }
  function updateCapOnInsertConstructor_r10(int c) private    {
      if(c>0) {
        cap = CapTuple(c,true);
      }
  }
  function updateOwnerOnInsertConstructor_r20() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateBalanceOfOnIncrementTotalBurn_r8(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateAllBurnOnInsertBurn_r22(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r23(delta0);
  }
  function updateBuyTokenOnInsertRecv_buyToken_r15(address p,int m) private   returns (bool) {
      if(m>0) {
        updateMintOnInsertBuyToken_r2(p,m);
        updateRaisedOnInsertBuyToken_r33(m);
        emit BuyToken(p,m);
        return true;
      }
      return false;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTotalBurnOnInsertBurn_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r8(p,delta0);
  }
  function updateFinalizedOnInsertFinalize_r7() private    {
      finalized = FinalizedTuple(true,true);
  }
}