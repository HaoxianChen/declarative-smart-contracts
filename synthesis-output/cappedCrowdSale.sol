contract CappedCrowdSale {
  struct AllowanceTuple {
    int n;
    bool _valid;
  }
  struct StartTuple {
    uint time;
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
  struct EndTuple {
    uint time;
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
  EndTuple end;
  StartTuple start;
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
    updateOwnerOnInsertConstructor_r19();
    updateCapOnInsertConstructor_r28(c);
    updateRateOnInsertConstructor_r23(r);
    updateOnceFinalizeOnInsertConstructor_r10();
    updateTotalSupplyOnInsertConstructor_r6();
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function getEnd() public view  returns (uint) {
      uint time = end.time;
      return time;
  }
  function burn(address p,int amount) public    {
      bool r33 = updateBurnOnInsertRecv_burn_r33(p,amount);
      if(r33==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r15 = updateTransferOnInsertRecv_transfer_r15(from,to,amount);
      if(r15==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address o,address s,int n) public    {
      bool r18 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r18(o,s,n);
      if(r18==false) {
        revert("Rule condition failed");
      }
  }
  function getRaised() public view  returns (int) {
      int n = raised.n;
      return n;
  }
  function getStart() public view  returns (uint) {
      uint time = start.time;
      return time;
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r14 = updateTransferFromOnInsertRecv_transferFrom_r14(from,to,spender,amount);
      if(r14==false) {
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
      bool r30 = updateBuyTokenOnInsertRecv_buyToken_r30(p,amount);
      if(r30==false) {
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
  function finalize() public    {
      bool r5 = updateFinalizeOnInsertRecv_finalize_r5();
      if(r5==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function updateRaisedOnInsertBuyToken_r35(int n) private    {
      raised.n += n;
  }
  function updateAllowanceOnIncrementSpentTotal_r34(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateFinalizeOnInsertRecv_finalize_r5() private   returns (bool) {
      int totalSupply_n = totalSupply.n;
      if(totalSupply_n<0) {
        updateOnceFinalizeOnInsertFinalize_r7();
        updateFinalizedOnInsertFinalize_r8();
        emit Finalize();
        return true;
      }
      return false;
  }
  function updateMintOnInsertBuyToken_r2(address p,int m) private    {
      int r = rate.r;
      int n = m*r;
      updateAllMintOnInsertMint_r0(n);
      updateTotalMintOnInsertMint_r31(p,n);
      emit Mint(p,n);
  }
  function updateMintOnInsertRecv_mint_r3(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>=0) {
        updateAllMintOnInsertMint_r0(n);
        updateTotalMintOnInsertMint_r31(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTotalSupplyOnInsertConstructor_r6() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateTotalOutOnInsertTransfer_r13(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r9(p,delta0);
  }
  function updateBurnOnInsertRecv_burn_r33(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && n<=balanceOf_x1) {
        updateTotalBurnOnInsertBurn_r1(p,n);
        updateAllBurnOnInsertBurn_r21(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r22(int m) private    {
      totalSupply.n += m;
  }
  function updateOwnerOnInsertConstructor_r19() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateOnceFinalizeOnInsertFinalize_r7() private    {
      // Empty()
  }
  function updateBalanceOfOnIncrementTotalMint_r9(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateAllBurnOnInsertBurn_r21(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r22(delta0);
  }
  function updateOnceFinalizeOnInsertConstructor_r10() private    {
      // Empty()
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r11(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r34(o,s,delta0);
  }
  function updateFinalizedOnInsertFinalize_r8() private    {
      finalized = FinalizedTuple(true,true);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r14(address o,address r,address s,int n) private   returns (bool) {
      int balanceOf_x1_2 = balanceOf[o].n;
      int allowance_x2_1 = allowance[o][s].n;
      if(n>0 && n<=allowance_x2_1 && n<=balanceOf_x1_2) {
        updateTransferOnInsertTransferFrom_r26(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r12(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r18(address o,address s,int d) private   returns (bool) {
      if(d>=0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r11(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalBurn_r9(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateTotalMintOnInsertMint_r31(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r9(p,delta0);
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
  function updateTotalSupplyOnIncrementAllBurn_r22(int b) private    {
      totalSupply.n -= b;
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r22(delta0);
  }
  function updateBalanceOfOnIncrementTotalOut_r9(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateBuyTokenOnInsertRecv_buyToken_r30(address p,int m) private   returns (bool) {
      if(m>0 && p!=address(0)) {
        updateMintOnInsertBuyToken_r2(p,m);
        updateRaisedOnInsertBuyToken_r35(m);
        emit BuyToken(p,m);
        return true;
      }
      return false;
  }
  function updateSpentTotalOnInsertTransferFrom_r12(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r34(o,s,delta0);
  }
  function updateTransferOnInsertRecv_transfer_r15(address s,address r,int n) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[s].n;
      if(n>=0 && n<=balanceOf_x1_1) {
        updateTotalInOnInsertTransfer_r17(r,n);
        updateTotalOutOnInsertTransfer_r13(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateTotalInOnInsertTransfer_r17(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r9(p,delta0);
  }
  function updateRateOnInsertConstructor_r23(int r) private    {
      if(r>0) {
        rate = RateTuple(r,true);
      }
  }
  function updateAllowanceOnIncrementAllowanceTotal_r34(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateTransferOnInsertTransferFrom_r26(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r17(r,n);
      updateTotalOutOnInsertTransfer_r13(o,n);
      emit Transfer(o,r,n);
  }
  function updateTotalBurnOnInsertBurn_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r9(p,delta0);
  }
  function updateCapOnInsertConstructor_r28(int c) private    {
      if(c>0) {
        cap = CapTuple(c,true);
      }
  }
}