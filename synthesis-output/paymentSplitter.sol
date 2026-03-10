contract PaymentSplitter {
  struct InitializedTuple {
    bool b;
    bool _valid;
  }
  struct SharesTuple {
    uint n;
    bool _valid;
  }
  struct ReleasedTuple {
    uint n;
    bool _valid;
  }
  struct TotalReceivedTuple {
    uint n;
    bool _valid;
  }
  struct TotalSharesTuple {
    uint n;
    bool _valid;
  }
  InitializedTuple initialized;
  mapping(address=>SharesTuple) shares;
  mapping(address=>ReleasedTuple) released;
  TotalReceivedTuple totalReceived;
  TotalSharesTuple totalShares;
  event InvalidTx();
  event Release(address p);
  event Fund(uint n);
  constructor(address p1,address p2,uint s1,uint s2) public {
    updateTotalSharesOnInsertConstructor_r15(p1,p2,s1,s2);
    updateReleasedOnInsertConstructor_r3(p1,p2,s1,s2);
    updateInitializedOnInsertConstructor_r11(p1,p2,s1,s2);
    updateReleasedOnInsertConstructor_r14(p1,p2,s1,s2);
    updateSharesOnInsertConstructor_r13(p1,p2,s1,s2);
    updateSharesOnInsertConstructor_r5(p1,p2,s1,s2);
  }
  function getInitialized() public view  returns (bool) {
      bool b = initialized.b;
      return b;
  }
  function release(address p) public    {
      bool r10 = updateReleaseOnInsertRecv_release_r10(p);
      if(r10==false) {
        revert("Rule condition failed");
      }
  }
  function fund(uint n) public    {
      bool r0 = updateFundOnInsertRecv_fund_r0(n);
      if(r0==false) {
        revert("Rule condition failed");
      }
  }
  function updateTotalSharesOnInsertConstructor_r15(address p1,address p2,uint s1,uint s2) private    {
      uint s = s1+s2;
      if(s>0 && s<=1000) {
        totalShares = TotalSharesTuple(s,true);
      }
  }
  function updateFundOnInsertRecv_fund_r0(uint n) private   returns (bool) {
      if(n>=0 && n<=1000) {
        updateTotalReceivedOnInsertFund_r17(n);
        emit Fund(n);
        return true;
      }
      return false;
  }
  function updateReleasedOnInsertConstructor_r3(address p1,address p2,uint s1,uint s2) private    {
      released[p1] = ReleasedTuple(0,true);
  }
  function updateReleaseAmountOnInsertRelease_r12(address p) private    {
      uint r = totalReceived.n;
      uint s = totalShares.n;
      uint m = shares[p].n;
      uint e = released[p].n;
      if(s>0 && (r*m)/s>e) {
        uint n = ((r*m)/s)-e;
        updateSendOnInsertReleaseAmount_r9(p,n);
        updateReleasedOnInsertReleaseAmount_r2(p,n);
      }
  }
  function updateReleasedOnInsertReleaseAmount_r2(address p,uint n) private    {
      released[p].n += n;
  }
  function updateTotalReceivedOnInsertFund_r17(uint n) private    {
      totalReceived.n += n;
  }
  function updateSendOnInsertReleaseAmount_r9(address p,uint n) private    {
      payable(p).send(n);
  }
  function updateReleasedOnInsertConstructor_r14(address p1,address p2,uint s1,uint s2) private    {
      released[p2] = ReleasedTuple(0,true);
  }
  function updateSharesOnInsertConstructor_r5(address p1,address p2,uint s1,uint s2) private    {
      if(s2>0 && s2<=1000 && p2!=p1) {
        shares[p2] = SharesTuple(s2,true);
      }
  }
  function updateReleaseOnInsertRecv_release_r10(address p) private   returns (bool) {
      updateReleaseAmountOnInsertRelease_r12(p);
      emit Release(p);
      return true;
      return false;
  }
  function updateInitializedOnInsertConstructor_r11(address p1,address p2,uint s1,uint s2) private    {
      initialized = InitializedTuple(true,true);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateSharesOnInsertConstructor_r13(address p1,address p2,uint s1,uint s2) private    {
      if(s1>0 && s1<=1000) {
        shares[p1] = SharesTuple(s1,true);
      }
  }
}