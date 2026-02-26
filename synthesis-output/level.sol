contract Level {
  struct CurrentEpochTuple {
    uint e;
    bool _valid;
  }
  struct ClaimableTuple {
    uint amount;
    bool _valid;
  }
  struct TotalRewardTuple {
    uint amount;
    bool _valid;
  }
  struct ClaimedTuple {
    uint amount;
    bool _valid;
  }
  mapping(uint=>mapping(address=>ClaimableTuple)) claimable;
  mapping(uint=>mapping(address=>TotalRewardTuple)) totalReward;
  mapping(uint=>mapping(address=>ClaimedTuple)) claimed;
  CurrentEpochTuple currentEpoch;
  event ClaimMultiple(uint epoch,address user,address to,uint reward);
  event InvalidTx();
  event Allocate(uint epoch,address user,uint amount);
  constructor() public {
    updateCurrentEpochOnInsertConstructor_r9();
  }
  function getCurrentEpoch() public view  returns (uint) {
      uint e = currentEpoch.e;
      return e;
  }
  function claimMultiple(uint epoch,address to) public    {
      bool r7 = updateClaimMultipleOnInsertRecv_claimMultiple_r7(epoch,to);
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalReward(uint epoch,address user) public view  returns (uint) {
      uint amount = totalReward[epoch][user].amount;
      return amount;
  }
  function getClaimable(uint epoch,address user) public view  returns (uint) {
      uint amount = claimable[epoch][user].amount;
      return amount;
  }
  function allocate(uint epoch,address user,uint amount) public    {
      bool r3 = updateAllocateOnInsertRecv_allocate_r3(epoch,user,amount);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function getClaimed(uint epoch,address user) public view  returns (uint) {
      uint amount = claimed[epoch][user].amount;
      return amount;
  }
  function updateTotalRewardOnInsertAllocateEvent_r10(uint epoch,address user,uint amount) private    {
      int delta0 = int(amount);
      updateClaimableOnIncrementTotalReward_r5(epoch,user,delta0);
      totalReward[epoch][user].amount += amount;
  }
  function updateClaimMultipleOnInsertRecv_claimMultiple_r7(uint epoch,address to) private   returns (bool) {
      address user_0 = msg.sender;
      address user = msg.sender;
      uint e_1 = currentEpoch.e;
      uint e = currentEpoch.e;
      uint reward = claimable[epoch][user].amount;
      uint total_0 = totalReward[epoch][user_0].amount;
      uint c_0 = claimed[epoch][user_0].amount;
      if(epoch<e && total_0>c_0 && epoch<e_1) {
        updateClaimEventOnInsertClaimMultiple_r2(epoch,user,reward);
        emit ClaimMultiple(epoch,user,to,reward);
        return true;
      }
      return false;
  }
  function updateAllocateEventOnInsertAllocate_r8(uint epoch,address user,uint amount) private    {
      updateClaimEventOnInsertAllocateEvent_r0(epoch,user);
      updateTotalRewardOnInsertAllocateEvent_r10(epoch,user,amount);
  }
  function updateClaimedOnInsertClaimEvent_r6(uint epoch,address user,uint amount) private    {
      int delta0 = int(amount);
      updateClaimableOnIncrementClaimed_r5(epoch,user,delta0);
      claimed[epoch][user].amount += amount;
  }
  function updateClaimableOnIncrementTotalReward_r5(uint epoch,address user,int total) private    {
      int _delta = int(total);
      uint x_claimable_epoch_user_amount = claimable[epoch][user].amount;
      uint newValue = updateuintByint(x_claimable_epoch_user_amount,_delta);
      claimable[epoch][user].amount = newValue;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateCurrentEpochOnInsertConstructor_r9() private    {
      currentEpoch = CurrentEpochTuple(10,true);
  }
  function updateClaimEventOnInsertAllocateEvent_r0(uint epoch,address user) private    {
      uint z = 0;
      updateClaimedOnInsertClaimEvent_r6(epoch,user,z);
  }
  function updateClaimableOnIncrementClaimed_r5(uint epoch,address user,int c) private    {
      int _delta = int(-c);
      uint x_claimable_epoch_user_amount = claimable[epoch][user].amount;
      uint newValue = updateuintByint(x_claimable_epoch_user_amount,_delta);
      claimable[epoch][user].amount = newValue;
  }
  function updateClaimEventOnInsertClaimMultiple_r2(uint epoch,address user,uint reward) private    {
      updateClaimedOnInsertClaimEvent_r6(epoch,user,reward);
  }
  function updateAllocateOnInsertRecv_allocate_r3(uint epoch,address user,uint amount) private   returns (bool) {
      updateAllocateEventOnInsertAllocate_r8(epoch,user,amount);
      emit Allocate(epoch,user,amount);
      return true;
      return false;
  }
}