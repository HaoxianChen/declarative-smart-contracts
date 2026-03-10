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
  event Withdraw(address sender,uint total);
  event InvalidTx();
  event Unstake(address staker,uint stakeId,uint unstakedDay);
  event Stake(address staker,uint stakeId,uint amount,uint lockPeriod,uint stakedDay);
  constructor() public {
    updateHundredOnInsertConstructor_r24();
    updateMaxPercentageOnInsertConstructor_r10();
    updateEthTaxOnInsertConstructor_r18();
    updateTotalStakedOnInsertConstructor_r0();
    updateInitializedOnInsertConstructor_r8();
  }
  function withdraw(uint earnings,uint affiliateEarnings,bool inETH,bytes32 messageHash,uint v,bytes32 r,bytes32 s) public    {
      bool r12 = updateWithdrawOnInsertRecv_withdraw_r12(earnings,affiliateEarnings,inETH,messageHash,v,r,s);
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function stake(uint stakeId,uint amount,uint lockPeriod) public    {
      bool r1 = updateStakeOnInsertRecv_stake_r1(stakeId,amount,lockPeriod);
      if(r1==false) {
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
  function unstake(uint stakeId) public    {
      bool r13 = updateUnstakeOnInsertRecv_unstake_r13(stakeId);
      if(r13==false) {
        revert("Rule condition failed");
      }
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
  function updateStakeStakedDayOnInsertStake_r15(address p,uint stakeId,uint day) private    {
      stakeStakedDay[p][stakeId] = StakeStakedDayTuple(day,true);
  }
  function updateStakeStatusOnInsertStake_r2(address p,uint stakeId) private    {
      stakeStatus[p][stakeId] = StakeStatusTuple(false,true);
  }
  function updateStakeLockPeriodOnInsertStake_r6(address p,uint stakeId,uint lockPeriod) private    {
      stakeLockPeriod[p][stakeId] = StakeLockPeriodTuple(lockPeriod,true);
  }
  function updateWithdrawOnInsertRecv_withdraw_r12(uint earnings,uint affiliateEarnings,bool inETH,bytes32 messageHash,uint v,bytes32 r,bytes32 s) private   returns (bool) {
      address sender = msg.sender;
      if(inETH==true) {
        uint total = earnings+affiliateEarnings;
        bool valid = isValidSignature(sender,total,inETH,messageHash,v,r,s);
        if(valid!=false) {
          emit Withdraw(sender,total);
          return true;
        }
      }
      return false;
  }
  function updateOnceUnstakeOnInsertUnstake_r25(address p,uint stakeId) private    {
      onceUnstake[p][stakeId] = OnceUnstakeTuple(true,true);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateHundredOnInsertConstructor_r24() private    {
      hundred = HundredTuple(100,true);
  }
  function updateUnstakeOnInsertRecv_unstake_r13(uint stakeId) private   returns (bool) {
      uint day = block.timestamp;
      address p = msg.sender;
      uint staked = stakeStakedDay[p][stakeId].stakedDay;
      if(false==stakeStatus[p][stakeId].unstaked) {
        if(false==onceUnstake[p][stakeId].b) {
          uint lock = stakeLockPeriod[p][stakeId].lockPeriod;
          if(day>staked+lock && day>staked+lock) {
            updateStakeStatusOnInsertUnstake_r11(p,stakeId);
            updateOnceUnstakeOnInsertUnstake_r25(p,stakeId);
            emit Unstake(p,stakeId,day);
            return true;
          }
        }
      }
      return false;
  }
  function updateEthTaxOnInsertConstructor_r18() private    {
      ethTax = EthTaxTuple(5,true);
  }
  function updateMaxPercentageOnInsertConstructor_r10() private    {
      maxPercentage = MaxPercentageTuple(10,true);
  }
  function updateTotalStakedOnInsertStake_r22(uint a) private    {
      totalStaked.n += a;
  }
  function updateStakeAmountOnInsertStake_r17(address p,uint stakeId,uint amount) private    {
      stakeAmount[p][stakeId] = StakeAmountTuple(amount,true);
  }
  function updateStakeStatusOnInsertUnstake_r11(address p,uint stakeId) private    {
      stakeStatus[p][stakeId] = StakeStatusTuple(true,true);
  }
  function updateOnceUnstakeOnInsertStake_r7(address p,uint stakeId) private    {
      onceUnstake[p][stakeId] = OnceUnstakeTuple(false,true);
  }
  function updateTotalStakedOnInsertConstructor_r0() private    {
      totalStaked = TotalStakedTuple(0,true);
  }
  function updateStakeOnInsertRecv_stake_r1(uint stakeId,uint amount,uint lockPeriod) private   returns (bool) {
      uint day = block.timestamp;
      address p = msg.sender;
      if(amount>0) {
        bool ok_1 = this.isValidLockPeriod(lockPeriod);
        if(ok_1!=false) {
          updateStakeAmountOnInsertStake_r17(p,stakeId,amount);
          updateOnceUnstakeOnInsertStake_r7(p,stakeId);
          updateTotalStakedOnInsertStake_r22(amount);
          updateStakeStatusOnInsertStake_r2(p,stakeId);
          updateStakeStakedDayOnInsertStake_r15(p,stakeId,day);
          updateStakeLockPeriodOnInsertStake_r6(p,stakeId,lockPeriod);
          emit Stake(p,stakeId,amount,lockPeriod,day);
          return true;
        }
      }
      return false;
  }
  function updateInitializedOnInsertConstructor_r8() private    {
      initialized = InitializedTuple(true,true);
  }
}