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
  function updateReleaseAmountOnInsertRelease_r11() private    {
      uint d = duration.t;
      uint e = released.n;
      uint b = funds.b;
      uint a = start.t;
      uint t = block.timestamp;
      if(t>a+d && b>e) {
        uint n = b-e;
        updateReleasedOnInsertReleaseAmount_r12(n);
        updateSendOnInsertReleaseAmount_r9(n);
      }
  }
  function updateDurationOnInsertConstructor_r6(uint d) private    {
      duration = DurationTuple(d,true);
  }
  function updateSendOnInsertReleaseAmount_r9(uint n) private    {
      address b = beneficiary.p;
      if(n>0) {
        payable(b).send(n);
      }
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
  function updateReleasedOnInsertReleaseAmount_r12(uint n) private    {
      released.n += n;
  }
  function updateStartOnInsertConstructor_r1(uint a) private    {
      uint t = block.timestamp;
      uint s = a+t;
      start = StartTuple(s,true);
  }
  function updateReleasedOnInsertConstructor_r2() private    {
      released = ReleasedTuple(0,true);
  }
  function updateReleaseOnInsertRecv_release_r4() private   returns (bool) {
      uint released_n = released.n;
      uint b = funds.b;
      uint a = start.t;
      uint e_1 = released.n;
      uint d = duration.t;
      uint e = released.n;
      uint a_0 = start.t;
      uint e_0 = released.n;
      uint t_0 = block.timestamp;
      uint t_2 = block.timestamp;
      uint b_0 = funds.b;
      uint a_2 = start.t;
      uint t = block.timestamp;
      uint vested_0 = vestedAmount(total,elapsed);
      if(e_1>=0 && vested_0>e_0 && released_n>0 && t_2>=a_2 && b>e && t>a+d) {
        updateReleaseAmountOnInsertRelease_r11();
        emit Release();
        return true;
      }
      return false;
  }
}