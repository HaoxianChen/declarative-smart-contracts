contract StakingRewards {
  struct StakedBalanceTuple {
    int n;
    bool _valid;
  }
  struct RewardsDistributorTuple {
    address p;
    bool _valid;
  }
  struct RewardPoolTuple {
    int n;
    bool _valid;
  }
  struct TotalStakedTuple {
    int n;
    bool _valid;
  }
  mapping(address=>StakedBalanceTuple) stakedBalance;
  RewardsDistributorTuple rewardsDistributor;
  RewardPoolTuple rewardPool;
  TotalStakedTuple totalStaked;
  event Stake(address p,int amount);
  event InvalidTx();
  event Unstake(address p,int amount);
  event AddRewards(int n);
  constructor() public {
    updateRewardsDistributorOnInsertConstructor_r17();
    updateOwnerOnInsertConstructor_r16();
  }
  function getStakedBalance(address p) public view  returns (int) {
      int n = stakedBalance[p].n;
      return n;
  }
  function getRewardPool() public view  returns (int) {
      int n = rewardPool.n;
      return n;
  }
  function addRewards(int n) public    {
      bool r4 = updateAddRewardsOnInsertRecv_addRewards_r4(n);
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function stake(address p,int amount) public    {
      bool r0 = updateStakeOnInsertRecv_stake_r0(p,amount);
      if(r0==false) {
        revert("Rule condition failed");
      }
  }
  function unstake(address p,int amount) public    {
      bool r10 = updateUnstakeOnInsertRecv_unstake_r10(p,amount);
      if(r10==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalStaked() public view  returns (int) {
      int n = totalStaked.n;
      return n;
  }
  function updateUnstakeOnInsertRecv_unstake_r10(address p,int n) private   returns (bool) {
      address s_1 = msg.sender;
      int stakedBalance_x1 = stakedBalance[p].n;
      if(n>0 && p==s_1 && n<stakedBalance_x1) {
        updateAllUnstakeOutOnInsertUnstake_r6(n);
        updateTotalUnstakeOutOnInsertUnstake_r13(p,n);
        emit Unstake(p,n);
        return true;
      }
      return false;
  }
  function updateStakedBalanceOnIncrementTotalStakeIn_r5(address p,int i) private    {
      stakedBalance[p].n += i;
  }
  function updateStakeOnInsertRecv_stake_r0(address p,int n) private   returns (bool) {
      address s_1 = msg.sender;
      if(n>0 && p==s_1) {
        updateTotalStakeInOnInsertStake_r7(p,n);
        updateAllStakeInOnInsertStake_r2(n);
        emit Stake(p,n);
        return true;
      }
      return false;
  }
  function updateTotalStakeInOnInsertStake_r7(address p,int m) private    {
      int delta0 = int(m);
      updateStakedBalanceOnIncrementTotalStakeIn_r5(p,delta0);
  }
  function updateAllStakeInOnInsertStake_r2(int n) private    {
      int delta0 = int(n);
      updateTotalStakedOnIncrementAllStakeIn_r14(delta0);
  }
  function updateRewardPoolOnIncrementAllRewards_r3(int s) private    {
      rewardPool.n += s;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateAllRewardsOnInsertAddRewards_r1(int n) private    {
      int delta0 = int(n);
      updateRewardPoolOnIncrementAllRewards_r3(delta0);
  }
  function updateTotalStakedOnIncrementAllUnstakeOut_r14(int o) private    {
      totalStaked.n -= o;
  }
  function updateOwnerOnInsertConstructor_r16() private    {
      address s = msg.sender;
      // Empty()
  }
  function updateAddRewardsOnInsertRecv_addRewards_r4(int n) private   returns (bool) {
      address s = msg.sender;
      address d = rewardsDistributor.p;
      if(d==s) {
        updateAllRewardsOnInsertAddRewards_r1(n);
        emit AddRewards(n);
        return true;
      }
      return false;
  }
  function updateTotalUnstakeOutOnInsertUnstake_r13(address p,int m) private    {
      int delta0 = int(m);
      updateStakedBalanceOnIncrementTotalUnstakeOut_r5(p,delta0);
  }
  function updateTotalStakedOnIncrementAllStakeIn_r14(int i) private    {
      totalStaked.n += i;
  }
  function updateAllUnstakeOutOnInsertUnstake_r6(int n) private    {
      int delta0 = int(n);
      updateTotalStakedOnIncrementAllUnstakeOut_r14(delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateStakedBalanceOnIncrementTotalUnstakeOut_r5(address p,int o) private    {
      stakedBalance[p].n -= o;
  }
  function updateRewardsDistributorOnInsertConstructor_r17() private    {
      address s = msg.sender;
      rewardsDistributor = RewardsDistributorTuple(s,true);
  }
}