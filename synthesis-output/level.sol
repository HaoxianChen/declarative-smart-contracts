import "./level_udf.sol";
contract Level is LevelUDF {
  struct CurrentEpochTuple {
    uint e;
    bool _valid;
  }
  struct UpdaterTuple {
    address p;
    bool _valid;
  }
  struct ReferredByTuple {
    address referrer;
    bool _valid;
  }
  UpdaterTuple updater;
  mapping(address=>ReferredByTuple) referredBy;
  CurrentEpochTuple currentEpoch;
  event ClaimMultiple(uint epoch,address user,address to,uint reward);
  event InvalidTx();
  event SetReferrer(address trader,address referrer);
  event NextEpoch(uint epoch,uint twap,uint allocationTime,uint vestingDuration);
  event UpdatePoint(uint epoch,address trader,address referrer,uint point);
  constructor() public {
    updateOwnerOnInsertConstructor_r10();
    updateCurrentEpochOnInsertConstructor_r4();
    updateUpdaterOnInsertConstructor_r20();
  }
  function getCurrentEpoch() public view  returns (uint) {
      uint e = currentEpoch.e;
      return e;
  }
  function getTradingPoint(uint epoch,address user) public view  returns (uint) {
      uint amount = tradingPoint[epoch][user].amount;
      return amount;
  }
  function getReferralPoint(uint epoch,address user) public view  returns (uint) {
      uint amount = referralPoint[epoch][user].amount;
      return amount;
  }
  function updatePoint(uint epoch,address trader,uint point) public    {
      bool r15 = updateUpdatePointOnInsertRecv_updatePoint_r15(epoch,trader,point);
      if(r15==false) {
        revert("Rule condition failed");
      }
  }
  function getClaimed(uint epoch,address user) public view  returns (uint) {
      uint amount = claimed[epoch][user].amount;
      return amount;
  }
  function claimMultiple(uint epoch,address to) public    {
      bool r18 = updateClaimMultipleOnInsertRecv_claimMultiple_r18(epoch,to);
      if(r18==false) {
        revert("Rule condition failed");
      }
  }
  function getEpochTWAP(uint epoch) public view  returns (uint) {
      uint twap = epochTWAP[epoch].twap;
      return twap;
  }
  function nextEpoch(uint epoch,uint twap,uint vestingDuration) public    {
      bool r17 = updateNextEpochOnInsertRecv_nextEpoch_r17(epoch,twap,vestingDuration);
      if(r17==false) {
        revert("Rule condition failed");
      }
  }
  function setReferrer(address referrer) public    {
      bool r21 = updateSetReferrerOnInsertRecv_setReferrer_r21(referrer);
      if(r21==false) {
        revert("Rule condition failed");
      }
  }
  function updateClaimMultipleOnInsertRecv_claimMultiple_r18(uint epoch,address to) private   returns (bool) {
      address user = msg.sender;
      uint e_1 = currentEpoch.e;
      if(epoch<e_1 && to!=address(0) && epoch<e_1) {
        uint reward = computeClaimable(epoch,user);
        if(reward>0) {
          updateClaimEventOnInsertClaimMultiple_r13(epoch,user,reward);
          emit ClaimMultiple(epoch,user,to,reward);
          return true;
        }
      }
      return false;
  }
  function updateOwnerOnInsertConstructor_r10() private    {
      address s = msg.sender;
      // Empty()
  }
  function updateClaimEventOnInsertReferralPointEvent_r2(uint epoch,address user) private    {
      updateClaimedOnInsertClaimEvent_r19(epoch,user,uint(0));
  }
  function updateTradingPointOnInsertTradingPointEvent_r14(uint epoch,address user,uint p) private    {
      tradingPoint[epoch][user].amount += p;
  }
  function updateCurrentEpochOnInsertConstructor_r4() private    {
      currentEpoch = CurrentEpochTuple(10,true);
  }
  function updateReferralPointOnInsertReferralPointEvent_r0(uint epoch,address user,uint p) private    {
      referralPoint[epoch][user].amount += p;
  }
  function updateTradingPointEventOnInsertUpdatePoint_r12(uint epoch,address trader,uint point) private    {
      updateClaimEventOnInsertTradingPointEvent_r9(epoch,trader);
      updateTradingPointOnInsertTradingPointEvent_r14(epoch,trader,point);
  }
  function updateClaimedOnInsertClaimEvent_r19(uint epoch,address user,uint a) private    {
      claimed[epoch][user].amount += a;
  }
  function updateUpdaterOnInsertConstructor_r20() private    {
      address s = msg.sender;
      updater = UpdaterTuple(s,true);
  }
  function updateReferredByOnInsertSetReferrer_r1(address trader,address referrer) private    {
      referredBy[trader] = ReferredByTuple(referrer,true);
  }
  function updateSetReferrerOnInsertRecv_setReferrer_r21(address referrer) private   returns (bool) {
      address msgSender = msg.sender;
      address referredBy_x1 = referredBy[referrer].referrer;
      if(referrer!=address(0) && msgSender!=referrer && referredBy_x1==msgSender) {
        updateReferredByOnInsertSetReferrer_r1(msgSender,referrer);
        emit SetReferrer(msgSender,referrer);
        return true;
      }
      return false;
  }
  function updateEpochTWAPOnInsertNextEpoch_r5(uint epoch,uint twap) private    {
      epochTWAP[epoch] = EpochTWAPTuple(twap,true);
  }
  function updateClaimEventOnInsertClaimMultiple_r13(uint epoch,address user,uint reward) private    {
      updateClaimedOnInsertClaimEvent_r19(epoch,user,reward);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateUpdatePointOnInsertRecv_updatePoint_r15(uint epoch,address trader,uint point) private   returns (bool) {
      address s_1 = msg.sender;
      address u_1 = updater.p;
      address referrer = referredBy[trader].referrer;
      if(u_1==s_1 && trader!=address(0) && point>0 && u_1==s_1) {
        updateReferralPointEventOnInsertUpdatePoint_r16(epoch,referrer,point);
        updateTradingPointEventOnInsertUpdatePoint_r12(epoch,trader,point);
        emit UpdatePoint(epoch,trader,referrer,point);
        return true;
      }
      return false;
  }
  function updateReferralPointEventOnInsertUpdatePoint_r16(uint epoch,address referrer,uint point) private    {
      updateReferralPointOnInsertReferralPointEvent_r0(epoch,referrer,point);
      updateClaimEventOnInsertReferralPointEvent_r2(epoch,referrer);
  }
  function updateNextEpochOnInsertRecv_nextEpoch_r17(uint epoch,uint twap,uint vestingDuration) private   returns (bool) {
      uint t = block.timestamp;
      if(twap>0) {
        updateEpochTWAPOnInsertNextEpoch_r5(epoch,twap);
        emit NextEpoch(epoch,twap,t,vestingDuration);
        return true;
      }
      return false;
  }
  function updateClaimEventOnInsertTradingPointEvent_r9(uint epoch,address user) private    {
      updateClaimedOnInsertClaimEvent_r19(epoch,user,uint(0));
  }
}