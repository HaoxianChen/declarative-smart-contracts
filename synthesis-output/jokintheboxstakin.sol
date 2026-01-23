contract JokStaking {
  struct InitializedTuple {
    bool b;
    bool _valid;
  }
  struct StakeStatusTuple {
    bool unstaked;
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
  struct StakeLockPeriodTuple {
    uint lockPeriod;
    bool _valid;
  }
  mapping(address=>mapping(uint=>StakeStatusTuple)) stakeStatus;
  mapping(address=>mapping(uint=>StakeStakedDayTuple)) stakeStakedDay;
  mapping(address=>mapping(uint=>StakeLockPeriodTuple)) stakeLockPeriod;
  InitializedTuple initialized;
  mapping(address=>mapping(uint=>StakeAmountTuple)) stakeAmount;
  event InvalidTx();
  event Unstake(address staker,uint stakeId,uint unstakedDay);
  event Stake(address staker,uint stakeId,uint amount,uint lockPeriod,uint stakedDay);
  constructor() public {
    updateInitializedOnInsertConstructor_r3();
  }
  function stake(uint stakeId,uint amount,uint lockPeriod) public    {
      bool r6 = updateStakeOnInsertRecv_stake_r6(stakeId,amount,lockPeriod);
      if(r6==false) {
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
  function getStakeLockPeriod(address staker,uint stakeId) public view  returns (uint) {
      uint lockPeriod = stakeLockPeriod[staker][stakeId].lockPeriod;
      return lockPeriod;
  }
  function unstake(uint stakeId) public    {
      bool r2 = updateUnstakeOnInsertRecv_unstake_r2(stakeId);
      if(r2==false) {
        revert("Rule condition failed");
      }
  }
  function getStakeStakedDay(address staker,uint stakeId) public view  returns (uint) {
      uint stakedDay = stakeStakedDay[staker][stakeId].stakedDay;
      return stakedDay;
  }
  function getInitialized() public view  returns (bool) {
      bool b = initialized.b;
      return b;
  }
  function updateStakeStatusOnInsertStake_r0(address p,uint stakeId) private    {
      stakeStatus[p][stakeId] = StakeStatusTuple(false,true);
  }
  function updateUnstakeOnInsertRecv_unstake_r2(uint stakeId) private   returns (bool) {
      address p_1 = msg.sender;
      uint t_0 = block.timestamp;
      address p_0 = msg.sender;
      uint day = block.timestamp;
      address p = msg.sender;
      uint staked_0 = stakeStakedDay[p_0][stakeId].stakedDay;
      bool u_1 = stakeStatus[p_1][stakeId].unstaked;
      uint lock_0 = stakeLockPeriod[p_0][stakeId].lockPeriod;
      if(t_0>staked_0+lock_0 && u_1!=true) {
        updateStakeStatusOnInsertUnstake_r5(p,stakeId);
        emit Unstake(p,stakeId,day);
        return true;
      }
      return false;
  }
  function updateStakeLockPeriodOnInsertStake_r1(address p,uint stakeId,uint lockPeriod) private    {
      stakeLockPeriod[p][stakeId] = StakeLockPeriodTuple(lockPeriod,true);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateStakeStakedDayOnInsertStake_r7(address p,uint stakeId,uint day) private    {
      stakeStakedDay[p][stakeId] = StakeStakedDayTuple(day,true);
  }
  function updateStakeStatusOnInsertUnstake_r5(address p,uint stakeId) private    {
      stakeStatus[p][stakeId] = StakeStatusTuple(true,true);
  }
  function updateStakeOnInsertRecv_stake_r6(uint stakeId,uint amount,uint lockPeriod) private   returns (bool) {
      uint day = block.timestamp;
      address p = msg.sender;
      updateStakeStakedDayOnInsertStake_r7(p,stakeId,day);
      updateStakeLockPeriodOnInsertStake_r1(p,stakeId,lockPeriod);
      updateStakeAmountOnInsertStake_r9(p,stakeId,amount);
      updateStakeStatusOnInsertStake_r0(p,stakeId);
      emit Stake(p,stakeId,amount,lockPeriod,day);
      return true;
      return false;
  }
  function updateInitializedOnInsertConstructor_r3() private    {
      initialized = InitializedTuple(true,true);
  }
  function updateStakeAmountOnInsertStake_r9(address p,uint stakeId,uint amount) private    {
      stakeAmount[p][stakeId] = StakeAmountTuple(amount,true);
  }
}