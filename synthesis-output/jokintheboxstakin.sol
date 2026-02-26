import "./jokintheboxstakin_udf.sol";
contract Jokintheboxstakin is IERC20 {
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
  struct HundredTuple {
    uint value;
    bool _valid;
  }
  struct InitializedTuple {
    bool b;
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
  mapping(address=>mapping(uint=>StakeStatusTuple)) stakeStatus;
  mapping(address=>mapping(uint=>StakeStakedDayTuple)) stakeStakedDay;
  mapping(address=>mapping(uint=>StakeLockPeriodTuple)) stakeLockPeriod;
  HundredTuple hundred;
  InitializedTuple initialized;
  mapping(address=>mapping(uint=>StakeAmountTuple)) stakeAmount;
  mapping(address=>mapping(uint=>OnceUnstakeTuple)) onceUnstake;
  MaxPercentageTuple maxPercentage;
  event Withdraw(address sender,uint total);
  event InvalidTx();
  event Unstake(address staker,uint stakeId,uint unstakedDay);
  event Stake(address staker,uint stakeId,uint amount,uint lockPeriod,uint stakedDay);
  constructor() public {
    updateHundredOnInsertConstructor_r17();
    updateInitializedOnInsertConstructor_r8();
    updateMaxPercentageOnInsertConstructor_r9();
  }
  function getStakeLockPeriod(address staker,uint stakeId) public view  returns (uint) {
      uint lockPeriod = stakeLockPeriod[staker][stakeId].lockPeriod;
      return lockPeriod;
  }
  function getMaxPercentage() public view  returns (uint) {
      uint value = maxPercentage.value;
      return value;
  }
  function getHundred() public view  returns (uint) {
      uint value = hundred.value;
      return value;
  }
  function withdraw(uint earnings,uint affiliateEarnings,bool inETH,bytes32 messageHash,uint v,bytes32 r,bytes32 s) public  payable  {
      bool r0 = updateWithdrawOnInsertRecv_withdraw_r0(earnings,affiliateEarnings,inETH,messageHash,v,r,s);
      if(r0==false) {
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
  function stake(uint stakeId,uint amount,uint lockPeriod) public    {
      bool r11 = updateStakeOnInsertRecv_stake_r11(stakeId,amount,lockPeriod);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function getStakeStakedDay(address staker,uint stakeId) public view  returns (uint) {
      uint stakedDay = stakeStakedDay[staker][stakeId].stakedDay;
      return stakedDay;
  }
  function unstake(uint stakeId) public    {
      bool r1 = updateUnstakeOnInsertRecv_unstake_r1(stakeId);
      if(r1==false) {
        revert("Rule condition failed");
      }
  }
  function getInitialized() public view  returns (bool) {
      bool b = initialized.b;
      return b;
  }
  function updateStakeLockPeriodOnInsertStake_r6(address p,uint stakeId,uint lockPeriod) private    {
      stakeLockPeriod[p][stakeId] = StakeLockPeriodTuple(lockPeriod,true);
  }
  function updateStakeStatusOnInsertStake_r2(address p,uint stakeId) private    {
      stakeStatus[p][stakeId] = StakeStatusTuple(false,true);
  }
  function updateInitializedOnInsertConstructor_r8() private    {
      initialized = InitializedTuple(true,true);
  }
  function updateMaxPercentageOnInsertConstructor_r9() private    {
      maxPercentage = MaxPercentageTuple(10,true);
  }
  function updateStakeOnInsertRecv_stake_r11(uint stakeId,uint amount,uint lockPeriod) private   returns (bool) {
      uint day = block.timestamp;
      address p = msg.sender;
      updateStakeLockPeriodOnInsertStake_r6(p,stakeId,lockPeriod);
      updateStakeStakedDayOnInsertStake_r12(p,stakeId,day);
      updateOnceUnstakeOnInsertStake_r7(p,stakeId);
      updateStakeStatusOnInsertStake_r2(p,stakeId);
      updateStakeAmountOnInsertStake_r14(p,stakeId,amount);
      emit Stake(p,stakeId,amount,lockPeriod,day);
      return true;
      return false;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateHundredOnInsertConstructor_r17() private    {
      hundred = HundredTuple(100,true);
  }
  function updateOnceUnstakeOnInsertUnstake_r18(address p,uint stakeId) private    {
      onceUnstake[p][stakeId] = OnceUnstakeTuple(true,true);
  }
  function updateUnstakeOnInsertRecv_unstake_r1(uint stakeId) private   returns (bool) {
      uint day = block.timestamp;
      address p = msg.sender;
      uint t = block.timestamp;
      uint staked = stakeStakedDay[p][stakeId].stakedDay;
      if(false==stakeStatus[p][stakeId].unstaked) {
        if(false==onceUnstake[p][stakeId].b) {
          uint lock = stakeLockPeriod[p][stakeId].lockPeriod;
          if(day>staked+lock && t>staked+lock) {
            updateOnceUnstakeOnInsertUnstake_r18(p,stakeId);
            updateStakeStatusOnInsertUnstake_r10(p,stakeId);
            emit Unstake(p,stakeId,day);
            return true;
          }
        }
      }
      return false;
  }
  function updateWithdrawOnInsertRecv_withdraw_r0(uint earnings,uint affiliateEarnings,bool inETH,bytes32 messageHash,uint v,bytes32 r,bytes32 s) private   returns (bool) {
      address msgSender = msg.sender;
      uint bal_0 = address(this).balance;
      address sender_1 = msg.sender;
      uint maxPct_0 = maxPercentage.value;
      uint c_0 = hundred.value;
      uint msgValue = msg.value;
      uint c_2 = hundred.value;
      uint maxPercentage_value_0 = maxPercentage.value;
      uint maxPct_2 = maxPercentage.value;
      address sender = msg.sender;
      uint stakeStakedDay_x2_1 = stakeStakedDay[msgSender][earnings].stakedDay;
      uint total = earnings+affiliateEarnings;
      bool valid_1 = isValidSignature(sender_1,total,inETH,messageHash,v,r,s);
      uint tokenBal_2 = jokTokenBalance();
      if(total*c_2<tokenBal_2*maxPct_2 && total*c_0<bal_0*maxPct_0 && maxPercentage_value_0>=msgValue && valid_1!=false && stakeStakedDay_x2_1<msgValue) {
        emit Withdraw(sender,total);
        return true;
      }
      return false;
  }
  function updateStakeStakedDayOnInsertStake_r12(address p,uint stakeId,uint day) private    {
      stakeStakedDay[p][stakeId] = StakeStakedDayTuple(day,true);
  }
  function updateOnceUnstakeOnInsertStake_r7(address p,uint stakeId) private    {
      onceUnstake[p][stakeId] = OnceUnstakeTuple(false,true);
  }
  function updateStakeAmountOnInsertStake_r14(address p,uint stakeId,uint amount) private    {
      stakeAmount[p][stakeId] = StakeAmountTuple(amount,true);
  }
  function updateStakeStatusOnInsertUnstake_r10(address p,uint stakeId) private    {
      stakeStatus[p][stakeId] = StakeStatusTuple(true,true);
  }
}