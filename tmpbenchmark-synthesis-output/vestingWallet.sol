contract VestingWallet {
  struct BeneficiaryTuple {
    address p;
    bool _valid;
  }
  struct FundsTuple {
    uint b;
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
  FundsTuple funds;
  event InvalidTx();
  event Release();
  constructor(uint s,uint d,address b) public {
    updateDurationOnInsertConstructor_r6(d);
    updateBeneficiaryOnInsertConstructor_r5(b);
    updateFundsOnInsertConstructor_r7();
    updateStartOnInsertConstructor_r2(s);
    updateReleasedOnInsertConstructor_r3();
  }
  function release() public    {
      bool r1 = updateReleaseOnInsertRecv_release_r1();
      if(r1==false) {
        revert("Rule condition failed");
      }
  }
  function getReleased() public view  returns (uint) {
      uint n = released.n;
      return n;
  }
  function updateSendOnInsertReleaseAmount_r8(uint n) private    {
      address b = beneficiary.p;
      if(n>0) {
        payable(b).send(n);
      }
  }
  function updateReleaseOnInsertRecv_release_r1() private   returns (bool) {
      uint released_n = released.n;
      uint a = start.t;
      uint t = block.timestamp;
      if(t>=a && released_n<0) {
        updateReleaseAmountOnInsertRelease_r9();
        emit Release();
        return true;
      }
      return false;
  }
  function updateDurationOnInsertConstructor_r6(uint d) private    {
      duration = DurationTuple(d,true);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateBeneficiaryOnInsertConstructor_r5(address b) private    {
      beneficiary = BeneficiaryTuple(b,true);
  }
  function updateFundsOnInsertConstructor_r7() private    {
      funds = FundsTuple(1000000,true);
  }
  function updateReleasedOnInsertReleaseAmount_r10(uint n) private    {
      released.n += n;
  }
  function updateReleaseAmountOnInsertRelease_r9() private    {
      uint d = duration.t;
      uint e = released.n;
      uint b = funds.b;
      uint a = start.t;
      uint t = block.timestamp;
      if(t>a+d && b>e) {
        uint n = b-e;
        updateSendOnInsertReleaseAmount_r8(n);
        updateReleasedOnInsertReleaseAmount_r10(n);
      }
  }
  function updateReleasedOnInsertConstructor_r3() private    {
      released = ReleasedTuple(0,true);
  }
  function updateStartOnInsertConstructor_r2(uint a) private    {
      uint t = block.timestamp;
      uint s = a+t;
      start = StartTuple(s,true);
  }
}