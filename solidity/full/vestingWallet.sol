contract VestingWallet {
  struct BeneficiaryTuple {
    address p;
    bool _valid;
  }
  struct DurationTuple {
    uint t;
    bool _valid;
  }
  struct StartTuple {
    uint t;
    bool _valid;
  }
  struct ReleasedTuple {
    uint n;
    bool _valid;
  }
  ReleasedTuple released;
  BeneficiaryTuple beneficiary;
  DurationTuple duration;
  StartTuple start;
  event Release(uint n);
  constructor(uint s,uint d,address b) public {
    updateDurationOnInsertConstructor_r5(d);
    updateStartOnInsertConstructor_r1(s);
    updateBeneficiaryOnInsertConstructor_r4(b);
  }
  function release() public    {
      bool r7 = updateReleaseOnInsertRecv_release_r7();
      bool r6 = updateReleaseOnInsertRecv_release_r6();
      if(r7==false && r6==false) {
        revert("Rule condition failed");
      }
  }
  function getReleased() public view  returns (uint) {
      uint n = released.n;
      return n;
  }
  function updateDurationOnInsertConstructor_r5(uint d) private    {
      duration = DurationTuple(d,true);
  }
  function updateReleasedOnInsertRelease_r0(uint n) private    {
      released.n += n;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateStartOnInsertConstructor_r1(uint a) private    {
      uint t = block.timestamp;
      uint s = a+t;
      start = StartTuple(s,true);
  }
  function updateReleaseOnInsertRecv_release_r6() private   returns (bool) {
      uint d = duration.t;
      uint e = released.n;
      uint b = address(this).balance;
      uint a = start.t;
      uint t = block.timestamp;
      if(t>a && ((b+e)*(t-a))/d>e && d<a+d && b+e>=b && t<a+d && d>0 && b+e>=e && a<a+d) {
        uint n = (((b+e)*(t-a))/d)-e;
        updateSendOnInsertRelease_r2(n);
        updateReleasedOnInsertRelease_r0(n);
        emit Release(n);
        return true;
      }
      return false;
  }
  function updateSendOnInsertRelease_r2(uint n) private    {
      address b = beneficiary.p;
      if(n>0) {
        payable(b).send(n);
      }
  }
  function updateReleaseOnInsertRecv_release_r7() private   returns (bool) {
      uint d = duration.t;
      uint e = released.n;
      uint b = address(this).balance;
      uint a = start.t;
      uint t = block.timestamp;
      if(d<a+d && b>e && a<a+d && t>a+d) {
        uint n = b-e;
        updateSendOnInsertRelease_r2(n);
        updateReleasedOnInsertRelease_r0(n);
        emit Release(n);
        return true;
      }
      return false;
  }
  function updateBeneficiaryOnInsertConstructor_r4(address b) private    {
      beneficiary = BeneficiaryTuple(b,true);
  }
}