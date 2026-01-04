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
  struct TotalSupplyTuple {
    int a;
    bool _valid;
  }
  struct BucketAmountTuple {
    int a;
    bool _valid;
  }
  WeiRaised_1Tuple weiRaised_1;
  ContinuousSaleStartedTuple continuousSaleStarted;
  TotalSupplyTuple totalSupply;
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
    updateCrowdsaleFinalizedOnInsertConstructor_r19();
    updateWeiRaised_1OnInsertConstructor_r0();
    updateContinuousSaleStartedOnInsertConstructor_r38();
    updateCapOnInsertConstructor_r36();
    updateBucketAmountOnInsertConstructor_r5();
    updateWeiRaised_2OnInsertConstructor_r6();
    updateTotalSupplyOnInsertConstructor_r4();
    updateOwnerOnInsertConstructor_r10();
  }
  function buyTokens(address p,int v) public    {
      bool r33 = updateBuyTokensOnInsertRecv_buyTokens_r33(p,v);
      if(r33==false) {
        revert("Rule condition failed");
      }
  }
  function finalize() public    {
      bool r12 = updateFinalizeOnInsertRecv_finalize_r12();
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function beginContinuousSale() public    {
      bool r27 = updateBeginContinuousSaleOnInsertRecv_beginContinuousSale_r27();
      if(r27==false) {
        revert("Rule condition failed");
      }
  }
  function buyToken1(address p,int v) public    {
      bool r8 = updateBuyToken1OnInsertRecv_buyToken1_r8(p,v);
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function updateStartOnInsertBeginContinuousSale_r28() private    {
      int n = issuance.a;
      int a = totalSupply.a;
      int n = (((a*8)/100)*12)/1;
      updateContinuousSaleStartedOnInsertStart_r24();
  }
  function updateTotalMintOnInsertMint_r23(address p,int m) private    {
      int delta0 = int(m);
      updateTotalSupplyOnIncrementTotalMint_r30(delta0);
  }
  function updateTotalSupplyOnIncrementTotalMint_r30(int m) private    {
      totalSupply.a += m;
  }
  function updateTotalSupplyOnInsertConstructor_r4() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateWeiRaised_1OnInsertConstructor_r0() private    {
      weiRaised_1 = WeiRaised_1Tuple(0,true);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateOwnerOnInsertTransferOwnership_r22(address s) private    {
      owner = OwnerTuple(s,true);
  }
  function updateMintOnInsertProcessPurchase_r16(address p,int a) private    {
      updateTotalMintOnInsertMint_r23(p,a);
  }
  function updateWeiRaised_1OnInsertBuyTokens_r20(int v) private    {
      weiRaised_1.a += v;
  }
  function updateTransferOwnershipOnInsertBeginContinuousSale_r32() private    {
      address p = address(10);
      updateOwnerOnInsertTransferOwnership_r22(p);
  }
  function updateBucketAmount_increaseOnInsertProcessPurchase_r26(int a) private    {
      updateBucketAmountOnInsertBucketAmount_increase_r25(a);
  }
  function updateContinuousSaleStartedOnInsertStart_r24() private    {
      continuousSaleStarted = ContinuousSaleStartedTuple(true,true);
  }
  function updateOwnerOnInsertConstructor_r10() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateBeginContinuousSaleOnInsertRecv_beginContinuousSale_r27() private   returns (bool) {
      bool b_1 = crowdsaleFinalized.b;
      address s_0 = msg.sender;
      address o_0 = owner.p;
      if(o_0==s_0 && b_1!=false) {
        updateTransferOwnershipOnInsertBeginContinuousSale_r32();
        updateStartOnInsertBeginContinuousSale_r28();
        emit BeginContinuousSale();
        return true;
      }
      return false;
  }
  function updateWeiRaised_2OnInsertConstructor_r6() private    {
      // Empty()
  }
  function updateBuyToken1OnInsertRecv_buyToken1_r8(address p,int v) private   returns (bool) {
      int c_2 = issuance.a;
      int b_2 = bucketAmount.a;
      bool b_1 = continuousSaleStarted.b;
      if(p!=address(0) && v!=0 && b_1!=false && b_2+v<=c_2) {
        updateProcessPurchaseOnInsertBuyToken1_r11(p,v);
        emit BuyToken1(p,v);
        return true;
      }
      return false;
  }
  function updateBucketAmountOnInsertConstructor_r5() private    {
      bucketAmount = BucketAmountTuple(0,true);
  }
  function updateBucketAmountOnInsertBucketAmount_increase_r25(int a) private    {
      bucketAmount = BucketAmountTuple(a,true);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateBuyTokensOnInsertRecv_buyTokens_r33(address p,int v) private   returns (bool) {
      int c_1 = cap.a;
      int b_1 = weiRaised_1.a;
      if(v!=0 && p!=address(0) && b_1+v<=c_1 && 0==v) {
        updateWeiRaised_1OnInsertBuyTokens_r20(v);
        updateMintOnInsertBuyTokens_r34(p,v);
        emit BuyTokens(p,v);
        return true;
      }
      return false;
  }
  function updateCrowdsaleFinalizedOnInsertFinalize_r37() private    {
      crowdsaleFinalized = CrowdsaleFinalizedTuple(true,true);
  }
  function updateFinalizeOnInsertRecv_finalize_r12() private   returns (bool) {
      address s_2 = msg.sender;
      int m_1 = weiRaised_1.a;
      int n_1 = cap.a;
      bool b_0 = crowdsaleFinalized.b;
      address o_2 = owner.p;
      if(b_0!=true && m_1>n_1 && o_2==s_2) {
        updateCrowdsaleFinalizedOnInsertFinalize_r37();
        emit Finalize();
        return true;
      }
      return false;
  }
  function updateCapOnInsertConstructor_r36() private    {
      cap = CapTuple(86206,true);
  }
  function updateProcessPurchaseOnInsertBuyToken1_r11(address p,int a) private    {
      updateBucketAmount_increaseOnInsertProcessPurchase_r26(a);
      updateMintOnInsertProcessPurchase_r16(p,a);
  }
  function updateContinuousSaleStartedOnInsertConstructor_r38() private    {
      continuousSaleStarted = ContinuousSaleStartedTuple(false,true);
  }
  function updateMintOnInsertBuyTokens_r34(address p,int a) private    {
      updateTotalMintOnInsertMint_r23(p,a);
  }
  function updateCrowdsaleFinalizedOnInsertConstructor_r19() private    {
      crowdsaleFinalized = CrowdsaleFinalizedTuple(false,true);
  }
}