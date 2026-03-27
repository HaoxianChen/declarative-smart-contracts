import "./jokintheboxstakin_udf.sol";
contract Jokintheboxstakin is JokintheboxstakinUDF {
  struct StakeStatusTuple {
    bool unstaked;
    bool _valid;
  }
  struct TotalStakedTuple {
    uint n;
    bool _valid;
  }
  struct StakeAmountTuple {
    uint amount;
    bool _valid;
  }
  struct StakeStakedDayTuple {
    uint stakedDay;
    bool _valid;
  }
  struct HundredTuple {
    uint value;
    bool _valid;
  }
  struct InitializedTuple {
    bool b;
    bool _valid;
  }
  struct EthTaxTuple {
    uint rate;
    bool _valid;
  }
  struct OnceUnstakeTuple {
    bool b;
    bool _valid;
  }
  struct MaxPercentageTuple {
    uint value;
    bool _valid;
  }
  struct StakeLockPeriodTuple {
    uint lockPeriod;
    bool _valid;
  }
  mapping(address=>mapping(uint=>StakeStatusTuple)) stakeStatus;
  TotalStakedTuple totalStaked;
  mapping(address=>mapping(uint=>StakeStakedDayTuple)) stakeStakedDay;
  mapping(address=>mapping(uint=>StakeLockPeriodTuple)) stakeLockPeriod;
  HundredTuple hundred;
  InitializedTuple initialized;
  EthTaxTuple ethTax;
  mapping(address=>mapping(uint=>StakeAmountTuple)) stakeAmount;
  mapping(address=>mapping(uint=>OnceUnstakeTuple)) onceUnstake;
  MaxPercentageTuple maxPercentage;
  event InvalidWithdrawSignature();
  event Withdraw(address sender,uint total);
  event RepeatedUnstake();
  event Unstake(address staker,uint stakeId,uint unstakedDay);
  event InvalidLockPeriod();
  event Stake(address staker,uint stakeId,uint amount,uint lockPeriod,uint stakedDay);
  constructor() public {
    updateMaxPercentageOnInsertConstructor_r9();
    updateInitializedOnInsertConstructor_r6();
    updateHundredOnInsertConstructor_r20();
    updateEthTaxOnInsertConstructor_r17();
    updateTotalStakedOnInsertConstructor_r0();
  }
  function withdraw(uint earnings,uint affiliateEarnings,bool inETH,bytes32 messageHash,uint v,bytes32 r,bytes32 s) public    {
      bool r13 = updateWithdrawOnInsertRecv_withdraw_r13(earnings,affiliateEarnings,inETH,messageHash,v,r,s);
      if(r13==false) {
        revert("Rule condition failed");
      }
  }
  function unstake(uint stakeId) public    {
      bool r7 = updateUnstakeOnInsertRecv_unstake_r7(stakeId);
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function stake(uint stakeId,uint amount,uint lockPeriod) public    {
      bool r2 = updateStakeOnInsertRecv_stake_r2(stakeId,amount,lockPeriod);
      if(r2==false) {
        revert("Rule condition failed");
      }
  }
  function getStakeLockPeriod(address staker,uint stakeId) public view  returns (uint) {
      uint lockPeriod = stakeLockPeriod[staker][stakeId].lockPeriod;
      return lockPeriod;
  }
  function getMaxPercentage() public view  returns (uint) {
      uint value = maxPercentage.value;
      return value;
  }
  function getEthTax() public view  returns (uint) {
      uint rate = ethTax.rate;
      return rate;
  }
  function getHundred() public view  returns (uint) {
      uint value = hundred.value;
      return value;
  }
  function getStakeAmount(address staker,uint stakeId) public view  returns (uint) {
      uint amount = stakeAmount[staker][stakeId].amount;
      return amount;
  }
  function getStakeStatus(address staker,uint stakeId) public view  returns (bool) {
      bool unstaked = stakeStatus[staker][stakeId].unstaked;
      return unstaked;
  }
  function getTotalStaked() public view  returns (uint) {
      uint n = totalStaked.n;
      return n;
  }
  function getStakeStakedDay(address staker,uint stakeId) public view  returns (uint) {
      uint stakedDay = stakeStakedDay[staker][stakeId].stakedDay;
      return stakedDay;
  }
  function getInitialized() public view  returns (bool) {
      bool b = initialized.b;
      return b;
  }
  function updateUnstakeOnInsertRecv_unstake_r7(uint stakeId) private   returns (bool) {
      uint day = block.timestamp;
      address p = msg.sender;
      uint staked = stakeStakedDay[p][stakeId].stakedDay;
      if(false==stakeStatus[p][stakeId].unstaked) {
        if(false==onceUnstake[p][stakeId].b) {
          uint lock = stakeLockPeriod[p][stakeId].lockPeriod;
          if(day>staked+lock) {
            updateOnceUnstakeOnInsertUnstake_r21(p,stakeId);
            updateStakeStatusOnInsertUnstake_r11(p,stakeId);
            emit Unstake(p,stakeId,day);
            return true;
          }
        }
      }
      return false;
  }
  function updateTotalStakedOnInsertStake_r19(uint a) private    {
      totalStaked.n += a;
  }
  function updateOnceUnstakeOnInsertStake_r5(address p,uint stakeId) private    {
      onceUnstake[p][stakeId] = OnceUnstakeTuple(false,true);
  }
  function updateOnceUnstakeOnInsertUnstake_r21(address p,uint stakeId) private    {
      onceUnstake[p][stakeId] = OnceUnstakeTuple(true,true);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateInitializedOnInsertConstructor_r6() private    {
      initialized = InitializedTuple(true,true);
  }
  function updateStakeOnInsertRecv_stake_r2(uint stakeId,uint amount,uint lockPeriod) private   returns (bool) {
      uint day = block.timestamp;
      address p = msg.sender;
      if(0!=stakeId) {
        updateStakeStakedDayOnInsertStake_r15(p,stakeId,day);
        updateOnceUnstakeOnInsertStake_r5(p,stakeId);
        updateStakeStatusOnInsertStake_r1(p,stakeId);
        updateStakeLockPeriodOnInsertStake_r4(p,stakeId,lockPeriod);
        updateTotalStakedOnInsertStake_r19(amount);
        updateStakeAmountOnInsertStake_r16(p,stakeId,amount);
        emit Stake(p,stakeId,amount,lockPeriod,day);
        return true;
      }
      return false;
  }
  function updateMaxPercentageOnInsertConstructor_r9() private    {
      maxPercentage = MaxPercentageTuple(10,true);
  }
  function updateTotalStakedOnInsertConstructor_r0() private    {
      totalStaked = TotalStakedTuple(0,true);
  }
  function updateStakeStatusOnInsertStake_r1(address p,uint stakeId) private    {
      stakeStatus[p][stakeId] = StakeStatusTuple(false,true);
  }
  function updateStakeAmountOnInsertStake_r16(address p,uint stakeId,uint amount) private    {
      stakeAmount[p][stakeId] = StakeAmountTuple(amount,true);
  }
  function updateStakeStatusOnInsertUnstake_r11(address p,uint stakeId) private    {
      stakeStatus[p][stakeId] = StakeStatusTuple(true,true);
  }
  function updateStakeLockPeriodOnInsertStake_r4(address p,uint stakeId,uint lockPeriod) private    {
      stakeLockPeriod[p][stakeId] = StakeLockPeriodTuple(lockPeriod,true);
  }
  function updateEthTaxOnInsertConstructor_r17() private    {
      ethTax = EthTaxTuple(5,true);
  }
  function updateWithdrawOnInsertRecv_withdraw_r13(uint earnings,uint affiliateEarnings,bool inETH,bytes32 messageHash,uint v,bytes32 r,bytes32 s) private   returns (bool) {
      address sender = msg.sender;
      uint total = earnings+affiliateEarnings;
      emit Withdraw(sender,total);
      return true;
      return false;
  }
  function updateHundredOnInsertConstructor_r20() private    {
      hundred = HundredTuple(100,true);
  }
  function updateStakeStakedDayOnInsertStake_r15(address p,uint stakeId,uint day) private    {
      stakeStakedDay[p][stakeId] = StakeStakedDayTuple(day,true);
  }
}