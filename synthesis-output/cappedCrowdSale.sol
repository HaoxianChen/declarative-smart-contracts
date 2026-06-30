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
    updateTotalSupplyOnInsertConstructor_r7();
    updateOwnerOnInsertConstructor_r18();
    updateCapOnInsertConstructor_r27(c);
    updateOnceFinalizeOnInsertConstructor_r11();
    updateRateOnInsertConstructor_r22(r);
  }
  function getEnd() public view  returns (uint) {
      uint time = end.time;
      return time;
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r3 = updateTransferFromOnInsertRecv_transferFrom_r3(from,to,spender,amount);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function getRaised() public view  returns (int) {
      int n = raised.n;
      return n;
  }
  function finalize() public    {
      bool r6 = updateFinalizeOnInsertRecv_finalize_r6();
      if(r6==false) {
        revert("Rule condition failed");
      }
  }
  function getStart() public view  returns (uint) {
      uint time = start.time;
      return time;
  }
  function transfer(address from,address to,int amount) public    {
      bool r34 = updateTransferOnInsertRecv_transfer_r34(from,to,amount);
      if(r34==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function increaseAllowance(address o,address s,int n) public    {
      bool r17 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r17(o,s,n);
      if(r17==false) {
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
  function mint(address p,int amount) public    {
      bool r4 = updateMintOnInsertRecv_mint_r4(p,amount);
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r32 = updateBurnOnInsertRecv_burn_r32(p,amount);
      if(r32==false) {
        revert("Rule condition failed");
      }
  }
  function buyToken(address p,int amount) public    {
      bool r29 = updateBuyTokenOnInsertRecv_buyToken_r29(p,amount);
      if(r29==false) {
        revert("Rule condition failed");
      }
  }
  function updateTransferOnInsertTransferFrom_r25(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r16(r,n);
      updateTotalOutOnInsertTransfer_r14(o,n);
      emit Transfer(o,r,n);
  }
  function updateRaisedOnInsertBuyToken_r35(int n) private    {
      raised.n += n;
  }
  function updateTotalMintOnInsertMint_r30(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r10(p,delta0);
  }
  function updateTransferOnInsertRecv_transfer_r34(address s,address r,int n) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[s].n;
      if(n>=0 && n<balanceOf_x1_1) {
        updateTotalOutOnInsertTransfer_r14(s,n);
        updateTotalInOnInsertTransfer_r16(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateBurnOnInsertRecv_burn_r32(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && n<=balanceOf_x1) {
        updateTotalBurnOnInsertBurn_r1(p,n);
        updateAllBurnOnInsertBurn_r20(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateBuyTokenOnInsertRecv_buyToken_r29(address p,int m) private   returns (bool) {
      if(m>0 && p!=address(0)) {
        updateMintOnInsertBuyToken_r2(p,m);
        updateRaisedOnInsertBuyToken_r35(m);
        emit BuyToken(p,m);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r10(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r33(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateBalanceOfOnIncrementTotalIn_r10(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateRateOnInsertConstructor_r22(int r) private    {
      if(r>0) {
        rate = RateTuple(r,true);
      }
  }
  function updateOwnerOnInsertConstructor_r18() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateFinalizeOnInsertRecv_finalize_r6() private   returns (bool) {
      int totalSupply_n = totalSupply.n;
      if(totalSupply_n<0) {
        updateOnceFinalizeOnInsertFinalize_r8();
        updateFinalizedOnInsertFinalize_r9();
        emit Finalize();
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalBurn_r10(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r21(delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateAllowanceOnIncrementSpentTotal_r33(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateAllBurnOnInsertBurn_r20(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r21(delta0);
  }
  function updateSpentTotalOnInsertTransferFrom_r13(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r33(o,s,delta0);
  }
  function updateFinalizedOnInsertFinalize_r9() private    {
      finalized = FinalizedTuple(true,true);
  }
  function updateOnceFinalizeOnInsertConstructor_r11() private    {
      // Empty()
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
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateBalanceOfOnIncrementTotalMint_r10(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateCapOnInsertConstructor_r27(int c) private    {
      if(c>0) {
        cap = CapTuple(c,true);
      }
  }
  function updateTotalBurnOnInsertBurn_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r10(p,delta0);
  }
  function updateTotalInOnInsertTransfer_r16(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r10(p,delta0);
  }
  function updateTotalSupplyOnInsertConstructor_r7() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateTotalOutOnInsertTransfer_r14(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r10(p,delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r12(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r33(o,s,delta0);
  }
  function updateTotalSupplyOnIncrementAllMint_r21(int m) private    {
      totalSupply.n += m;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r3(address o,address r,address s,int n) private   returns (bool) {
      int balanceOf_x1_2 = balanceOf[o].n;
      int allowance_x2_1 = allowance[o][s].n;
      if(n>=0 && n<allowance_x2_1 && n<=balanceOf_x1_2) {
        updateTransferOnInsertTransferFrom_r25(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r13(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateOnceFinalizeOnInsertFinalize_r8() private    {
      // Empty()
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r17(address o,address s,int d) private   returns (bool) {
      if(d>=0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r12(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r21(int b) private    {
      totalSupply.n -= b;
  }
}