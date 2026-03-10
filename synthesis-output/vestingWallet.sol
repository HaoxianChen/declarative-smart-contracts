import "./vestingWallet_udf.sol";
contract VestingWallet is VestingWalletUDF {
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
    updateBeneficiaryOnInsertConstructor_r4(b);
    updateDurationOnInsertConstructor_r5(d);
    updateStartOnInsertConstructor_r1(s);
    updateReleasedOnInsertConstructor_r2();
    updateFundsOnInsertConstructor_r6();
  }
  function release() public    {
      bool r10 = updateReleaseOnInsertRecv_release_r10();
      if(r10==false) {
        revert("Rule condition failed");
      }
  }
  function getReleased() public view  returns (uint) {
      uint n = released.n;
      return n;
  }
  function updateFundsOnInsertConstructor_r6() private    {
      funds = FundsTuple(1000000,true);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateReleasedOnInsertConstructor_r2() private    {
      released = ReleasedTuple(0,true);
  }
  function updateReleasedOnInsertReleaseAmount_r12(uint n) private    {
      released.n += n;
  }
  function updateSendOnInsertReleaseAmount_r8(uint n) private    {
      address b = beneficiary.p;
      if(n>0) {
        payable(b).send(n);
      }
  }
  function updateReleaseAmountOnInsertRelease_r11() private    {
      uint d = duration.t;
      uint e = released.n;
      uint b = funds.b;
      uint a = start.t;
      uint t = block.timestamp;
      if(t>a+d && b>e) {
        uint n = b-e;
        updateSendOnInsertReleaseAmount_r8(n);
        updateReleasedOnInsertReleaseAmount_r12(n);
      }
  }
  function updateDurationOnInsertConstructor_r5(uint d) private    {
      duration = DurationTuple(d,true);
  }
  function updateReleaseOnInsertRecv_release_r10() private   returns (bool) {
      uint released_n = released.n;
      uint b = funds.b;
      uint a = start.t;
      uint d = duration.t;
      uint t_0 = block.timestamp;
      if(released_n>=0 && 0!=released_n && t_0>=a && b>released_n && t_0>a+d) {
        uint elapsed = t_0-a;
        uint total = b+released_n;
        uint vested_0 = vestedAmount(total,elapsed);
        if(vested_0>released_n) {
          updateReleaseAmountOnInsertRelease_r11();
          emit Release();
          return true;
        }
      }
      return false;
  }
  function updateStartOnInsertConstructor_r1(uint a) private    {
      uint t = block.timestamp;
      uint s = a+t;
      start = StartTuple(s,true);
  }
  function updateBeneficiaryOnInsertConstructor_r4(address b) private    {
      beneficiary = BeneficiaryTuple(b,true);
  }
}