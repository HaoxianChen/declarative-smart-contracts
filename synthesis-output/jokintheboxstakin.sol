import "./jokintheboxstakin_udf.sol";
contract Jokintheboxstakin is IERC20 {
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
    updateTotalStakedOnInsertConstructor_r0();
    updateEthTaxOnInsertConstructor_r17();
    updateMaxPercentageOnInsertConstructor_r11();
    updateHundredOnInsertConstructor_r24();
    updateInitializedOnInsertConstructor_r9();
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
  function withdraw(uint earnings,uint affiliateEarnings,bool inETH,bytes32 messageHash,uint v,bytes32 r,bytes32 s) public    {
      bool r23 = updateWithdrawOnInsertRecv_withdraw_r23(earnings,affiliateEarnings,inETH,messageHash,v,r,s);
      if(r23==false) {
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
  function updateHundredOnInsertConstructor_r24() private    {
      hundred = HundredTuple(100,true);
  }
  function updateEthTaxOnInsertConstructor_r17() private    {
      ethTax = EthTaxTuple(5,true);
  }
  function updateWithdrawOnInsertRecv_withdraw_r23(uint earnings,uint affiliateEarnings,bool inETH,bytes32 messageHash,uint v,bytes32 r,bytes32 s) private   returns (bool) {
      uint bal_0 = address(this).balance;
      address sender_1 = msg.sender;
      uint maxPct_0 = maxPercentage.value;
      uint c_0 = hundred.value;
      uint c_2 = hundred.value;
      uint maxPct_2 = maxPercentage.value;
      address sender = msg.sender;
      uint total = earnings+affiliateEarnings;
      bool valid_1 = isValidSignature(sender_1,total,inETH,messageHash,v,r,s);
      uint tokenBal_2 = jokTokenBalance();
      if(inETH==true && total*c_2<tokenBal_2*maxPct_2 && total*c_0<bal_0*maxPct_0 && valid_1!=false) {
        emit Withdraw(sender,total);
        return true;
      }
      return false;
  }
  function updateStakeAmountOnInsertStake_r16(address p,uint stakeId,uint amount) private    {
      stakeAmount[p][stakeId] = StakeAmountTuple(amount,true);
  }
  function updateOnceUnstakeOnInsertStake_r8(address p,uint stakeId) private    {
      onceUnstake[p][stakeId] = OnceUnstakeTuple(false,true);
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
            updateOnceUnstakeOnInsertUnstake_r25(p,stakeId);
            updateStakeStatusOnInsertUnstake_r12(p,stakeId);
            emit Unstake(p,stakeId,day);
            return true;
          }
        }
      }
      return false;
  }
  function updateMaxPercentageOnInsertConstructor_r11() private    {
      maxPercentage = MaxPercentageTuple(10,true);
  }
  function updateOnceUnstakeOnInsertUnstake_r25(address p,uint stakeId) private    {
      onceUnstake[p][stakeId] = OnceUnstakeTuple(true,true);
  }
  function updateStakeStatusOnInsertStake_r3(address p,uint stakeId) private    {
      stakeStatus[p][stakeId] = StakeStatusTuple(false,true);
  }
  function updateInitializedOnInsertConstructor_r9() private    {
      initialized = InitializedTuple(true,true);
  }
  function updateStakeStatusOnInsertUnstake_r12(address p,uint stakeId) private    {
      stakeStatus[p][stakeId] = StakeStatusTuple(true,true);
  }
  function updateTotalStakedOnInsertStake_r21(uint a) private    {
      totalStaked.n += a;
  }
  function updateStakeStakedDayOnInsertStake_r14(address p,uint stakeId,uint day) private    {
      stakeStakedDay[p][stakeId] = StakeStakedDayTuple(day,true);
  }
  function updateTotalStakedOnInsertConstructor_r0() private    {
      totalStaked = TotalStakedTuple(0,true);
  }
  function updateStakeOnInsertRecv_stake_r2(uint stakeId,uint amount,uint lockPeriod) private   returns (bool) {
      uint day = block.timestamp;
      address p = msg.sender;
      bool ok_1 = isValidLockPeriod(lockPeriod);
      if(amount>0 && ok_1!=false) {
        updateStakeLockPeriodOnInsertStake_r7(p,stakeId,lockPeriod);
        updateOnceUnstakeOnInsertStake_r8(p,stakeId);
        updateStakeStatusOnInsertStake_r3(p,stakeId);
        updateTotalStakedOnInsertStake_r21(amount);
        updateStakeStakedDayOnInsertStake_r14(p,stakeId,day);
        updateStakeAmountOnInsertStake_r16(p,stakeId,amount);
        emit Stake(p,stakeId,amount,lockPeriod,day);
        return true;
      }
      return false;
  }
  function updateStakeLockPeriodOnInsertStake_r7(address p,uint stakeId,uint lockPeriod) private    {
      stakeLockPeriod[p][stakeId] = StakeLockPeriodTuple(lockPeriod,true);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
}