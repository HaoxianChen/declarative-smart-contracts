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
    updateOnceFinalizeOnInsertConstructor_r9();
    updateOwnerOnInsertConstructor_r18();
    updateRateOnInsertConstructor_r26(r);
    updateTotalSupplyOnInsertConstructor_r6();
    updateCapOnInsertConstructor_r31(c);
  }
  function buyToken(address p,int amount) public    {
      bool r25 = updateBuyTokenOnInsertRecv_buyToken_r25(p,amount);
      if(r25==false) {
        revert("Rule condition failed");
      }
  }
  function getEnd() public view  returns (uint) {
      uint time = end.time;
      return time;
  }
  function getRaised() public view  returns (int) {
      int n = raised.n;
      return n;
  }
  function finalize() public    {
      bool r17 = updateFinalizeOnInsertRecv_finalize_r17();
      if(r17==false) {
        revert("Rule condition failed");
      }
  }
  function getStart() public view  returns (uint) {
      uint time = start.time;
      return time;
  }
  function burn(address p,int amount) public    {
      bool r13 = updateBurnOnInsertRecv_burn_r13(p,amount);
      if(r13==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function getCap() public view  returns (int) {
      int n = cap.n;
      return n;
  }
  function getFinalized() public view  returns (bool) {
      bool b = finalized.b;
      return b;
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r19 = updateTransferFromOnInsertRecv_transferFrom_r19(from,to,spender,amount);
      if(r19==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address o,address s,int n) public    {
      bool r16 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r16(o,s,n);
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r20 = updateMintOnInsertRecv_mint_r20(p,amount);
      if(r20==false) {
        revert("Rule condition failed");
      }
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
  function updateBalanceOfOnIncrementTotalIn_r8(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTotalInOnInsertTransfer_r15(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r8(p,delta0);
  }
  function updateRaisedOnInsertBuyToken_r34(int n) private    {
      raised.n += n;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r10(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r33(o,s,delta0);
  }
  function updateTotalSupplyOnIncrementAllMint_r23(int m) private    {
      totalSupply.n += m;
  }
  function updateTransferOnInsertTransferFrom_r29(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r15(r,n);
      updateTotalOutOnInsertTransfer_r36(o,n);
      emit Transfer(o,r,n);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r33(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateOnceFinalizeOnInsertFinalize_r24() private    {
      // Empty()
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateBurnOnInsertRecv_burn_r13(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n<0) {
        updateTotalBurnOnInsertBurn_r1(p,n);
        updateAllBurnOnInsertBurn_r22(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateTotalOutOnInsertTransfer_r36(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r8(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalMint_r8(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateTotalMintOnInsertMint_r11(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r8(p,delta0);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r16(address o,address s,int n) private   returns (bool) {
      if(d>=0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r10(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r23(int b) private    {
      totalSupply.n -= b;
  }
  function updateCapOnInsertConstructor_r31(int c) private    {
      if(c>0) {
        cap = CapTuple(c,true);
      }
  }
  function updateOwnerOnInsertConstructor_r18() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateRateOnInsertConstructor_r26(int r) private    {
      if(r>0) {
        rate = RateTuple(r,true);
      }
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateMintOnInsertBuyToken_r2(address p,int m) private    {
      int r = rate.r;
      int n = m*r;
      updateAllMintOnInsertMint_r0(n);
      updateTotalMintOnInsertMint_r11(p,n);
      emit Mint(p,n);
  }
  function updateAllowanceOnIncrementSpentTotal_r33(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r23(delta0);
  }
  function updateBalanceOfOnIncrementTotalBurn_r8(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateAllBurnOnInsertBurn_r22(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r23(delta0);
  }
  function updateTotalBurnOnInsertBurn_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r8(p,delta0);
  }
  function updateTotalSupplyOnInsertConstructor_r6() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateFinalizedOnInsertFinalize_r7() private    {
      finalized = FinalizedTuple(true,true);
  }
  function updateSpentTotalOnInsertTransferFrom_r12(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r33(o,s,delta0);
  }
  function updateOnceFinalizeOnInsertConstructor_r9() private    {
      // Empty()
  }
  function updateTransferFromOnInsertRecv_transferFrom_r19(address from,address to,address spender,int amount) private   returns (bool) {
      if(0==n) {
        updateTransferOnInsertTransferFrom_r29(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r12(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r8(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateBuyTokenOnInsertRecv_buyToken_r25(address p,int amount) private   returns (bool) {
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
  function updateTransferOnInsertRecv_transfer_r35(address from,address to,int amount) private   returns (bool) {
      if(0==n) {
        updateTotalInOnInsertTransfer_r15(r,n);
        updateTotalOutOnInsertTransfer_r36(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateMintOnInsertRecv_mint_r20(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>=0) {
        updateAllMintOnInsertMint_r0(n);
        updateTotalMintOnInsertMint_r11(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateFinalizeOnInsertRecv_finalize_r17() private   returns (bool) {
      uint start_time = start.time;
      if(0!=start_time) {
        updateFinalizedOnInsertFinalize_r7();
        updateOnceFinalizeOnInsertFinalize_r24();
        emit Finalize();
        return true;
      }
      return false;
  }
}