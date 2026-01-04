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
  constructor() public {
    updateInitializedOnInsertConstructor_r9();
  }
  function getInitialized() public view  returns (bool) {
      bool b = initialized.b;
      return b;
  }
  function release(address p) public    {
      bool r7 = updateReleaseOnInsertRecv_release_r7(p);
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function updateReleaseAmountOnInsertRelease_r3(address p) private    {
      uint r = totalReceived.n;
      uint s = totalShares.n;
      uint m = shares[p].n;
      uint e = released[p].n;
      if(s>0 && (r*m)/s>e) {
        uint n = ((r*m)/s)-e;
        updateSendOnInsertReleaseAmount_r8(p,n);
        updateReleasedOnInsertReleaseAmount_r1(p,n);
        updateTotalReceivedOnInsertReleaseAmount_r0(n);
      }
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateReleasedOnInsertReleaseAmount_r1(address p,uint n) private    {
      released[p].n += n;
  }
  function updateInitializedOnInsertConstructor_r9() private    {
      initialized = InitializedTuple(true,true);
  }
  function updateSendOnInsertReleaseAmount_r8(address p,uint n) private    {
      payable(p).send(n);
  }
  function updateReleaseOnInsertRecv_release_r7(address p) private   returns (bool) {
      uint s_1 = totalShares.n;
      uint __1 = shares[p].n;
      if(p!=address(0) && s_1!=0) {
        updateReleaseAmountOnInsertRelease_r3(p);
        emit Release(p);
        return true;
      }
      return false;
  }
  function updateTotalReceivedOnInsertReleaseAmount_r0(uint e) private    {
      totalReceived.n += e;
  }
}