contract Mana {
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct CrowdsaleFinalizedTuple {
    bool b;
    bool _valid;
  }
  struct WeiRaised_1Tuple {
    int a;
    bool _valid;
  }
  struct ContinuousSaleStartedTuple {
    bool b;
    bool _valid;
  }
  struct IssuanceTuple {
    int a;
    bool _valid;
  }
  struct CapTuple {
    int a;
    bool _valid;
  }
  struct BucketAmountTuple {
    int a;
    bool _valid;
  }
  WeiRaised_1Tuple weiRaised_1;
  ContinuousSaleStartedTuple continuousSaleStarted;
  CrowdsaleFinalizedTuple crowdsaleFinalized;
  BucketAmountTuple bucketAmount;
  OwnerTuple owner;
  IssuanceTuple issuance;
  CapTuple cap;
  event BuyTokens(address p,int v);
  event BeginContinuousSale();
  event BuyToken1(address p,int v);
  event InvalidTx();
  event Finalize();
  constructor() public {
    updateBucketAmountOnInsertConstructor_r26();
    updateContinuousSaleStartedOnInsertConstructor_r41();
    updateTotalSupplyOnInsertConstructor_r22();
    updateCapOnInsertConstructor_r8();
    updateOwnerOnInsertConstructor_r6();
    updateCrowdsaleFinalizedOnInsertConstructor_r14();
    updateWeiRaised_2OnInsertConstructor_r3();
    updateIssuanceOnInsertConstructor_r29();
    updateWeiRaised_1OnInsertConstructor_r0();
  }
  function buyTokens(address p,int v) public    {
      bool r37 = updateBuyTokensOnInsertRecv_buyTokens_r37(p,v);
      if(r37==false) {
        revert("Rule condition failed");
      }
  }
  function finalize() public    {
      bool r16 = updateFinalizeOnInsertRecv_finalize_r16();
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function beginContinuousSale() public    {
      bool r28 = updateBeginContinuousSaleOnInsertRecv_beginContinuousSale_r28();
      if(r28==false) {
        revert("Rule condition failed");
      }
  }
  function buyToken1(address p,int v) public    {
      bool r32 = updateBuyToken1OnInsertRecv_buyToken1_r32(p,v);
      if(r32==false) {
        revert("Rule condition failed");
      }
  }
  function updateStartOnInsertBeginContinuousSale_r5() private    {
      updateContinuousSaleStartedOnInsertStart_r23();
  }
  function updateOwnerOnInsertConstructor_r6() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateProcessPurchaseOnInsertBuyToken1_r7(address p,int a) private    {
      updateBucketAmount_increaseOnInsertProcessPurchase_r27(a);
  }
  function updateIssuanceOnInsertConstructor_r29() private    {
      issuance = IssuanceTuple(1000,true);
  }
  function updateWeiRaised_1OnInsertConstructor_r0() private    {
      weiRaised_1 = WeiRaised_1Tuple(0,true);
  }
  function updateBuyTokensOnInsertRecv_buyTokens_r37(address p,int v) private   returns (bool) {
      int c_1 = cap.a;
      int b_1 = weiRaised_1.a;
      if(v>=0 && v!=0 && v>0 && p!=address(0) && b_1+v<=c_1) {
        updateWeiRaised_1OnInsertBuyTokens_r15(v);
        emit BuyTokens(p,v);
        return true;
      }
      return false;
  }
  function updateCrowdsaleFinalizedOnInsertFinalize_r40() private    {
      crowdsaleFinalized = CrowdsaleFinalizedTuple(true,true);
  }
  function updateBucketAmountOnInsertBucketAmount_increase_r24(int a) private    {
      bucketAmount = BucketAmountTuple(a,true);
  }
  function updateBucketAmountOnInsertConstructor_r26() private    {
      bucketAmount = BucketAmountTuple(0,true);
  }
  function updateCapOnInsertConstructor_r8() private    {
      cap = CapTuple(100,true);
  }
  function updateContinuousSaleStartedOnInsertStart_r23() private    {
      continuousSaleStarted = ContinuousSaleStartedTuple(true,true);
  }
  function updateBuyToken1OnInsertRecv_buyToken1_r32(address p,int v) private   returns (bool) {
      bool b_1 = continuousSaleStarted.b;
      int b_2 = bucketAmount.a;
      int c_2 = issuance.a;
      if(v>=0 && p!=address(0) && v!=0 && b_1!=false && b_2+v<=c_2) {
        updateProcessPurchaseOnInsertBuyToken1_r7(p,v);
        emit BuyToken1(p,v);
        return true;
      }
      return false;
  }
  function updateContinuousSaleStartedOnInsertConstructor_r41() private    {
      continuousSaleStarted = ContinuousSaleStartedTuple(false,true);
  }
  function updateTransferOwnershipOnInsertBeginContinuousSale_r34() private    {
      address p = address(10);
      updateOwnerOnInsertTransferOwnership_r19(p);
  }
  function updateWeiRaised_1OnInsertBuyTokens_r15(int v) private    {
      weiRaised_1.a += v;
  }
  function updateWeiRaised_2OnInsertConstructor_r3() private    {
      // Empty()
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateBeginContinuousSaleOnInsertRecv_beginContinuousSale_r28() private   returns (bool) {
      bool b_1 = crowdsaleFinalized.b;
      address s_0 = msg.sender;
      address o_0 = owner.p;
      if(o_0==s_0 && b_1!=false) {
        updateStartOnInsertBeginContinuousSale_r5();
        updateTransferOwnershipOnInsertBeginContinuousSale_r34();
        emit BeginContinuousSale();
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r22() private    {
      // Empty()
  }
  function updateCrowdsaleFinalizedOnInsertConstructor_r14() private    {
      crowdsaleFinalized = CrowdsaleFinalizedTuple(false,true);
  }
  function updateOwnerOnInsertTransferOwnership_r19(address s) private    {
      owner = OwnerTuple(s,true);
  }
  function updateBucketAmount_increaseOnInsertProcessPurchase_r27(int a) private    {
      updateBucketAmountOnInsertBucketAmount_increase_r24(a);
  }
  function updateFinalizeOnInsertRecv_finalize_r16() private   returns (bool) {
      address s_2 = msg.sender;
      int m_1 = weiRaised_1.a;
      int n_1 = cap.a;
      bool b_0 = crowdsaleFinalized.b;
      address o_2 = owner.p;
      if(b_0!=true && m_1>=n_1 && o_2==s_2) {
        updateCrowdsaleFinalizedOnInsertFinalize_r40();
        emit Finalize();
        return true;
      }
      return false;
  }
}