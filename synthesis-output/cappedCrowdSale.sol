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
    updateTotalSupplyOnInsertConstructor_r8();
    updateOwnerOnInsertConstructor_r3();
    updateCapOnInsertConstructor_r30(c);
    updateOnceFinalizeOnInsertConstructor_r11();
    updateRateOnInsertConstructor_r25(r);
  }
  function buyToken(address p,int amount) public    {
      bool r24 = updateBuyTokenOnInsertRecv_buyToken_r24(p,amount);
      if(r24==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r18 = updateTransferFromOnInsertRecv_transferFrom_r18(from,to,spender,amount);
      if(r18==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function getEnd() public view  returns (uint) {
      uint time = end.time;
      return time;
  }
  function mint(address p,int amount) public    {
      bool r19 = updateMintOnInsertRecv_mint_r19(p,amount);
      if(r19==false) {
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
  function increaseAllowance(address o,address s,int n) public    {
      bool r32 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r32(o,s,n);
      if(r32==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r15 = updateBurnOnInsertRecv_burn_r15(p,amount);
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
  function transfer(address from,address to,int amount) public    {
      bool r35 = updateTransferOnInsertRecv_transfer_r35(from,to,amount);
      if(r35==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
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
  function updateRateOnInsertConstructor_r25(int r) private    {
      if(r>0) {
        rate = RateTuple(r,true);
      }
  }
  function updateTotalInOnInsertTransfer_r17(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r10(p,delta0);
  }
  function updateTotalSupplyOnIncrementAllMint_r22(int m) private    {
      totalSupply.n += m;
  }
  function updateAllBurnOnInsertBurn_r21(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r22(delta0);
  }
  function updateBalanceOfOnIncrementTotalBurn_r10(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateRaisedOnInsertBuyToken_r34(int n) private    {
      raised.n += n;
  }
  function updateOnceFinalizeOnInsertFinalize_r23() private    {
      // Empty()
  }
  function updateBurnOnInsertRecv_burn_r15(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n<0) {
        updateTotalBurnOnInsertBurn_r1(p,n);
        updateAllBurnOnInsertBurn_r21(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalIn_r10(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTotalBurnOnInsertBurn_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r10(p,delta0);
  }
  function updateTotalOutOnInsertTransfer_r36(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r10(p,delta0);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r32(address o,address s,int n) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r12(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertRecv_transfer_r35(address from,address to,int amount) private   returns (bool) {
      if(0==n) {
        updateTotalInOnInsertTransfer_r17(r,n);
        updateTotalOutOnInsertTransfer_r36(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateTotalMintOnInsertMint_r13(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r10(p,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateBalanceOfOnIncrementTotalOut_r10(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateFinalizeOnInsertRecv_finalize_r5() private   returns (bool) {
      int raised_n = raised.n;
      if(raised_n<0) {
        updateOnceFinalizeOnInsertFinalize_r23();
        updateFinalizedOnInsertFinalize_r9();
        emit Finalize();
        return true;
      }
      return false;
  }
  function updateBuyTokenOnInsertRecv_buyToken_r24(address p,int amount) private   returns (bool) {
      uint e_2 = end.time;
      uint t_2 = block.timestamp;
      uint t_1 = block.timestamp;
      uint s_1 = start.time;
      if(p!=address(0) && t_1>=s_1 && t_2<=e_2 && m>0) {
        updateMintOnInsertBuyToken_r2(p,m);
        updateRaisedOnInsertBuyToken_r34(m);
        emit BuyToken(p,m);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementSpentTotal_r33(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateOwnerOnInsertConstructor_r3() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r33(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateMintOnInsertRecv_mint_r19(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>=0) {
        updateTotalMintOnInsertMint_r13(p,n);
        updateAllMintOnInsertMint_r0(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateFinalizedOnInsertFinalize_r9() private    {
      finalized = FinalizedTuple(true,true);
  }
  function updateOnceFinalizeOnInsertConstructor_r11() private    {
      // Empty()
  }
  function updateTransferOnInsertTransferFrom_r28(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r17(r,n);
      updateTotalOutOnInsertTransfer_r36(o,n);
      emit Transfer(o,r,n);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateSpentTotalOnInsertTransferFrom_r14(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r33(o,s,delta0);
  }
  function updateTotalSupplyOnIncrementAllBurn_r22(int b) private    {
      totalSupply.n -= b;
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r22(delta0);
  }
  function updateBalanceOfOnIncrementTotalMint_r10(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r18(address from,address to,address spender,int amount) private   returns (bool) {
      if(0==n) {
        updateTransferOnInsertTransferFrom_r28(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r14(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r12(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r33(o,s,delta0);
  }
  function updateCapOnInsertConstructor_r30(int c) private    {
      if(c>0) {
        cap = CapTuple(c,true);
      }
  }
  function updateMintOnInsertBuyToken_r2(address p,int m) private    {
      int r = rate.r;
      int n = m*r;
      updateTotalMintOnInsertMint_r13(p,n);
      updateAllMintOnInsertMint_r0(n);
      emit Mint(p,n);
  }
  function updateTotalSupplyOnInsertConstructor_r8() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
}