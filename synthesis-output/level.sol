import "./level_udf.sol";
contract Level is LevelUDF {
  struct ReferralPointTuple {
    uint amount;
    bool _valid;
  }
  struct EpochTWAPTuple {
    uint twap;
    bool _valid;
  }
  struct CurrentEpochTuple {
    uint e;
    bool _valid;
  }
  struct TradingPointTuple {
    uint amount;
    bool _valid;
  }
  struct ClaimedTuple {
    uint amount;
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
  mapping(uint=>mapping(address=>ReferralPointTuple)) referralPoint;
  mapping(uint=>mapping(address=>TradingPointTuple)) tradingPoint;
  mapping(uint=>mapping(address=>ClaimedTuple)) claimed;
  UpdaterTuple updater;
  mapping(uint=>EpochTWAPTuple) epochTWAP;
  mapping(address=>ReferredByTuple) referredBy;
  CurrentEpochTuple currentEpoch;
  event ClaimMultiple(uint epoch,address user,address to,uint reward);
  event InvalidTx();
  event SetReferrer(address trader,address referrer);
  event NextEpoch(uint epoch,uint twap,uint allocationTime,uint vestingDuration);
  event UpdatePoint(uint epoch,address trader,address referrer,uint point);
  constructor() public {
    updateCurrentEpochOnInsertConstructor_r3();
    updateUpdaterOnInsertConstructor_r21();
    updateOwnerOnInsertConstructor_r9();
  }
  function getCurrentEpoch() public view  returns (uint) {
      uint e = currentEpoch.e;
      return e;
  }
  function getTradingPoint(uint epoch,address user) public view  returns (uint) {
      uint amount = tradingPoint[epoch][user].amount;
      return amount;
  }
  function getEpochTWAP(uint epoch) public view  returns (uint) {
      uint twap = epochTWAP[epoch].twap;
      return twap;
  }
  function setReferrer(address referrer) public    {
      bool r12 = updateSetReferrerOnInsertRecv_setReferrer_r12(referrer);
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function nextEpoch(uint epoch,uint twap,uint vestingDuration) public    {
      bool r18 = updateNextEpochOnInsertRecv_nextEpoch_r18(epoch,twap,vestingDuration);
      if(r18==false) {
        revert("Rule condition failed");
      }
  }
  function claimMultiple(uint epoch,address to) public    {
      bool r19 = updateClaimMultipleOnInsertRecv_claimMultiple_r19(epoch,to);
      if(r19==false) {
        revert("Rule condition failed");
      }
  }
  function getReferralPoint(uint epoch,address user) public view  returns (uint) {
      uint amount = referralPoint[epoch][user].amount;
      return amount;
  }
  function updatePoint(uint epoch,address trader,uint point) public    {
      bool r16 = updateUpdatePointOnInsertRecv_updatePoint_r16(epoch,trader,point);
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function getClaimed(uint epoch,address user) public view  returns (uint) {
      uint amount = claimed[epoch][user].amount;
      return amount;
  }
  function updateNextEpochOnInsertRecv_nextEpoch_r18(uint epoch,uint twap,uint vestingDuration) private   returns (bool) {
      uint t = block.timestamp;
      if(twap>0) {
        updateEpochTWAPOnInsertNextEpoch_r4(epoch,twap);
        emit NextEpoch(epoch,twap,t,vestingDuration);
        return true;
      }
      return false;
  }
  function updateTradingPointOnInsertTradingPointEvent_r15(uint epoch,address user,uint p) private    {
      tradingPoint[epoch][user].amount += p;
  }
  function updateReferralPointOnInsertReferralPointEvent_r0(uint epoch,address user,uint p) private    {
      referralPoint[epoch][user].amount += p;
  }
  function updateUpdatePointOnInsertRecv_updatePoint_r16(uint epoch,address trader,uint point) private   returns (bool) {
      address s_1 = msg.sender;
      address s = msg.sender;
      address u_1 = updater.p;
      address u = updater.p;
      address referrer = referredBy[trader].referrer;
      if(u==s && point>0 && trader!=address(0) && u_1==s_1) {
        updateTradingPointEventOnInsertUpdatePoint_r11(epoch,trader,point);
        updateReferralPointEventOnInsertUpdatePoint_r17(epoch,referrer,point);
        emit UpdatePoint(epoch,trader,referrer,point);
        return true;
      }
      return false;
  }
  function updateClaimedOnInsertClaimEvent_r20(uint epoch,address user,uint a) private    {
      claimed[epoch][user].amount += a;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateClaimMultipleOnInsertRecv_claimMultiple_r19(uint epoch,address to) private   returns (bool) {
      address user = msg.sender;
      uint e_1 = currentEpoch.e;
      uint e = currentEpoch.e;
      uint reward = computeClaimable(epoch,user);
      if(epoch<e && reward>0 && to!=address(0) && epoch<e_1) {
        updateClaimEventOnInsertClaimMultiple_r14(epoch,user,reward);
        emit ClaimMultiple(epoch,user,to,reward);
        return true;
      }
      return false;
  }
  function updateClaimEventOnInsertClaimMultiple_r14(uint epoch,address user,uint reward) private    {
      updateClaimedOnInsertClaimEvent_r20(epoch,user,reward);
  }
  function updateClaimEventOnInsertTradingPointEvent_r8(uint epoch,address user) private    {
      updateClaimedOnInsertClaimEvent_r20(epoch,user,uint(0));
  }
  function updateCurrentEpochOnInsertConstructor_r3() private    {
      currentEpoch = CurrentEpochTuple(10,true);
  }
  function updateTradingPointEventOnInsertUpdatePoint_r11(uint epoch,address trader,uint point) private    {
      updateClaimEventOnInsertTradingPointEvent_r8(epoch,trader);
      updateTradingPointOnInsertTradingPointEvent_r15(epoch,trader,point);
  }
  function updateUpdaterOnInsertConstructor_r21() private    {
      address s = msg.sender;
      updater = UpdaterTuple(s,true);
  }
  function updateOwnerOnInsertConstructor_r9() private    {
      address s = msg.sender;
      // Empty()
  }
  function updateSetReferrerOnInsertRecv_setReferrer_r12(address referrer) private   returns (bool) {
      address msgSender = msg.sender;
      address s_1 = msg.sender;
      address trader = msg.sender;
      address referredBy_x1 = referredBy[msgSender].referrer;
      if(referrer!=address(0) && s_1!=referrer && referredBy_x1==msgSender) {
        updateReferredByOnInsertSetReferrer_r1(trader,referrer);
        emit SetReferrer(trader,referrer);
        return true;
      }
      return false;
  }
  function updateReferralPointEventOnInsertUpdatePoint_r17(uint epoch,address referrer,uint point) private    {
      updateReferralPointOnInsertReferralPointEvent_r0(epoch,referrer,point);
      updateClaimEventOnInsertReferralPointEvent_r13(epoch,referrer);
  }
  function updateReferredByOnInsertSetReferrer_r1(address trader,address referrer) private    {
      referredBy[trader] = ReferredByTuple(referrer,true);
  }
  function updateClaimEventOnInsertReferralPointEvent_r13(uint epoch,address user) private    {
      updateClaimedOnInsertClaimEvent_r20(epoch,user,uint(0));
  }
  function updateEpochTWAPOnInsertNextEpoch_r4(uint epoch,uint twap) private    {
      epochTWAP[epoch] = EpochTWAPTuple(twap,true);
  }
}