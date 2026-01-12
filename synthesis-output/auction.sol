contract Auction {
  struct HighestBidAmountTuple {
    uint n;
    bool _valid;
  }
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct BeneficiaryTuple {
    address p;
    bool _valid;
  }
  struct EndTuple {
    bool b;
    bool _valid;
  }
  struct BalanceTuple {
    uint n;
    bool _valid;
  }
  HighestBidAmountTuple highestBidAmount;
  mapping(address=>BalanceTuple) balance;
  BeneficiaryTuple beneficiary;
  OwnerTuple owner;
  EndTuple end;
  event InvalidTx();
  event Withdraw(address bidder,uint amount);
  event EndAuction();
  event Bid(address bidder,uint amount);
  constructor(address beneficiary,int biddingTime) public {
    updateBeneficiaryOnInsertConstructor_r12(beneficiary);
    updateHighestBidAmountOnInsertConstructor_r17();
    updateHighestBidderOnInsertConstructor_r28();
    updateEndOnInsertConstructor_r16();
    updateOwnerOnInsertConstructor_r24();
    updateEndTimeOnInsertConstructor_r8(biddingTime);
  }
  function bid(address bidder,uint amount) public    {
      bool r13 = updateBidOnInsertRecv_bid_r13(bidder,amount);
      if(r13==false) {
        revert("Rule condition failed");
      }
  }
  function withdraw(address bidder,uint amount) public    {
      bool r14 = updateWithdrawOnInsertRecv_withdraw_r14(bidder,amount);
      if(r14==false) {
        revert("Rule condition failed");
      }
  }
  function endAuction() public    {
      bool r4 = updateEndAuctionOnInsertRecv_endAuction_r4();
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function getBalance(address p) public view  returns (uint) {
      uint n = balance[p].n;
      return n;
  }
  function updateSendOnInsertEndAuction_r0() private    {
      address p = beneficiary.p;
      uint n = highestBidAmount.n;
      payable(p).send(n);
  }
  function updateBalanceOnIncrementWithdrawTotal_r9(address p,int w) private    {
      int _delta = int(-w);
      uint x_balance_p_n = balance[p].n;
      uint newValue = updateuintByint(x_balance_p_n,_delta);
      balance[p].n = newValue;
  }
  function updateSendOnInsertWithdraw_r10(address p,uint n) private    {
      payable(p).send(n);
  }
  function updateHighestBidAmountOnInsertBid_r23() private    {
      uint _max = highestBidAmount.n;
      if(_>_max) {
        highestBidAmount = HighestBidAmountTuple(m,true);
      }
  }
  function updateBidTotalOnInsertBid_r25(address p,uint m) private    {
      int delta0 = int(m);
      updateBalanceOnIncrementBidTotal_r9(p,delta0);
  }
  function updateEndOnInsertConstructor_r16() private    {
      end = EndTuple(false,true);
  }
  function updateHighestBidAmountOnInsertConstructor_r17() private    {
      highestBidAmount = HighestBidAmountTuple(0,true);
  }
  function updateEndOnInsertEndAuction_r26() private    {
      end = EndTuple(true,true);
  }
  function updateWithdrawTotalOnInsertWithdraw_r21(address p,uint m) private    {
      int delta0 = int(m);
      updateBalanceOnIncrementWithdrawTotal_r9(p,delta0);
  }
  function updateBalanceOnIncrementBidTotal_r9(address p,int b) private    {
      int _delta = int(b);
      uint x_balance_p_n = balance[p].n;
      uint newValue = updateuintByint(x_balance_p_n,_delta);
      balance[p].n = newValue;
  }
  function updateBeneficiaryOnInsertConstructor_r12(address p) private    {
      beneficiary = BeneficiaryTuple(p,true);
  }
  function updateBidOnInsertRecv_bid_r13(address bidder,uint amount) private   returns (bool) {
      uint m_1 = highestBidAmount.n;
      bool b_0 = end.b;
      if(b_0!=true && n>m_1) {
        updateBidTotalOnInsertBid_r25(p,n);
        updateHighestBidAmountOnInsertBid_r23();
        emit Bid(p,n);
        return true;
      }
      return false;
  }
  function updateEndTimeOnInsertConstructor_r8(int d) private    {
      int t2 = d;
      // Empty()
  }
  function updateHighestBidderOnInsertConstructor_r28() private    {
      // Empty()
  }
  function updateOwnerOnInsertConstructor_r24() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateWithdrawOnInsertRecv_withdraw_r14(address bidder,uint amount) private   returns (bool) {
      bool b_1 = end.b;
      uint b_2 = balance[p].n;
      if(n<=b_2 && n==b_2 && p!=address(0) && n>0 && b_1!=false && b_2!=0) {
        updateWithdrawTotalOnInsertWithdraw_r21(p,n);
        updateSendOnInsertWithdraw_r10(p,n);
        emit Withdraw(p,n);
        return true;
      }
      return false;
  }
  function updateEndAuctionOnInsertRecv_endAuction_r4() private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        updateEndOnInsertEndAuction_r26();
        updateSendOnInsertEndAuction_r0();
        emit EndAuction();
        return true;
      }
      return false;
  }
}