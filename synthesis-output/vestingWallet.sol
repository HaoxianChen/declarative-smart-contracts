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
    updateReleasedOnInsertConstructor_r2();
    updateStartOnInsertConstructor_r1(s);
    updateDurationOnInsertConstructor_r6(d);
    updateBeneficiaryOnInsertConstructor_r5(b);
    updateFundsOnInsertConstructor_r7();
  }
  function release() public    {
      bool r4 = updateReleaseOnInsertRecv_release_r4();
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function getReleased() public view  returns (uint) {
      uint n = released.n;
      return n;
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
  function updateStartOnInsertConstructor_r1(uint a) private    {
      uint t = block.timestamp;
      uint s = a+t;
      start = StartTuple(s,true);
  }
  function updateReleaseAmountOnInsertRelease_r10() private    {
      uint d = duration.t;
      uint e = released.n;
      uint b = funds.b;
      uint a = start.t;
      uint t = block.timestamp;
      if(t>a+d && b>e) {
        uint n = b-e;
        updateSendOnInsertReleaseAmount_r8(n);
        updateReleasedOnInsertReleaseAmount_r11(n);
      }
  }
  function updateSendOnInsertReleaseAmount_r8(uint n) private    {
      address b = beneficiary.p;
      if(n>0) {
        payable(b).send(n);
      }
  }
  function updateReleaseOnInsertRecv_release_r4() private   returns (bool) {
      uint released_n = released.n;
      uint b = funds.b;
      uint d = duration.t;
      uint e = released.n;
      uint a_1 = start.t;
      uint e_0 = released.n;
      uint t_1 = block.timestamp;
      uint a = start.t;
      uint t = block.timestamp;
      if(e_0>=0 && 0!=released_n && b>e && t_1>=a_1 && t>a+d) {
        updateReleaseAmountOnInsertRelease_r10();
        emit Release();
        return true;
      }
      return false;
  }
  function updateReleasedOnInsertReleaseAmount_r11(uint n) private    {
      released.n += n;
  }
  function updateReleasedOnInsertConstructor_r2() private    {
      released = ReleasedTuple(0,true);
  }
}