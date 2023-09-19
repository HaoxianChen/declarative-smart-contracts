contract PaymentSplitter {
  struct TotalReleasedTuple {
    uint n;
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
  struct ReleaseTuple {
    address p;
    uint n;
    bool _valid;
  }
  struct TotalSharesTuple {
    uint n;
    bool _valid;
  }
  TotalReleasedTuple totalReleased;
  mapping(address=>SharesTuple) shares;
  mapping(address=>ReleasedTuple) released;
  TotalReceivedTuple totalReceived;
  ReleaseTuple release;
  TotalSharesTuple totalShares;
  event Release(address p,uint n);
  function release(address p) public    {
      bool r6 = updateReleaseOnInsertRecv_release_r6(p);
      if(r6==false) {
        revert("Rule condition failed");
      }
  }
  function updateTotalReleasedOnInsertRelease_r1(uint e) private    {
      int delta0 = int(e);
      updateTotalReceivedOnIncrementTotalReleased_r0(delta0);
      totalReleased.n += e;
  }
  function updateSendOnInsertRelease_r3(address p,uint n) private    {
      payable(p).send(n);
  }
  function updateTotalReceivedOnIncrementTotalReleased_r0(int e) private    {
      int _delta = int(e);
      uint newValue = updateuintByint(totalReceived.n,_delta);
      totalReceived.n = newValue;
  }
  function updateReleaseOnInsertRecv_release_r6(address p) private   returns (bool) {
      uint r = totalReceived.n;
      uint s = totalShares.n;
      uint m = shares[p].n;
      uint e = released[p].n;
      if((r*m)/s>e) {
        uint n = ((r*m)/s)-e;
        updateReleasedOnInsertRelease_r4(p,n);
        updateTotalReleasedOnInsertRelease_r1(n);
        updateSendOnInsertRelease_r3(p,n);
        release = ReleaseTuple(p,n,true);
        emit Release(p,n);
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
  function updateReleasedOnInsertRelease_r4(address p,uint n) private    {
      released[p].n += n;
  }
}