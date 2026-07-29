contract Auction {
  struct AuctionEndedTuple {
    bool b;
    bool _valid;
  }
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct BalanceTuple {
    int n;
    bool _valid;
  }
  struct HighestBidTuple {
    int n;
    bool _valid;
  }
  struct HighestBidderTuple {
    address p;
    bool _valid;
  }
  AuctionEndedTuple auctionEnded;
  mapping(address=>BalanceTuple) balance;
  HighestBidTuple highestBid;
  HighestBidderTuple highestBidder;
  OwnerTuple owner;
  event InvalidTx();
  event EndAuction();
  event Withdraw(address bidder,int amount);
  event Bid(address bidder,int amount);
  constructor(address beneficiary) public {
    updateBeneficiaryOnInsertConstructor_r4(beneficiary);
    updateOwnerOnInsertConstructor_r13();
  }
  function bid(address bidder,int amount) public    {
      bool r5 = updateBidOnInsertRecv_bid_r5(bidder,amount);
      if(r5==false) {
        revert("Rule condition failed");
      }
  }
  function getBalance(address p) public view  returns (int) {
      int n = balance[p].n;
      return n;
  }
  function getHighestBidder() public view  returns (address) {
      address p = highestBidder.p;
      return p;
  }
  function endAuction() public    {
      bool r3 = updateEndAuctionOnInsertRecv_endAuction_r3();
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function getAuctionEnded() public view  returns (bool) {
      bool b = auctionEnded.b;
      return b;
  }
  function getHighestBid() public view  returns (int) {
      int n = highestBid.n;
      return n;
  }
  function withdraw(address bidder,int amount) public    {
      bool r11 = updateWithdrawOnInsertRecv_withdraw_r11(bidder,amount);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function updateBalanceOnIncrementWithdrawTotal_r8(address p,int w) private    {
      balance[p].n -= w;
  }
  function updateWithdrawOnInsertRecv_withdraw_r11(address p,int n) private   returns (bool) {
      bool auctionEnded_b = auctionEnded.b;
      bool b = auctionEnded.b;
      if(b==true && auctionEnded_b==false) {
        updateWithdrawTotalOnInsertWithdraw_r16(p,n);
        emit Withdraw(p,n);
        return true;
      }
      return false;
  }
  function updateOwnerOnInsertConstructor_r13() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateBalanceOnIncrementBidTotal_r8(address p,int b) private    {
      balance[p].n += b;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateEndAuctionOnInsertRecv_endAuction_r3() private   returns (bool) {
      bool b_1 = auctionEnded.b;
      address s_0 = msg.sender;
      address o_0 = owner.p;
      if(o_0==s_0 && b_1!=true) {
        updateAuctionEndedOnInsertEndAuction_r0();
        emit EndAuction();
        return true;
      }
      return false;
  }
  function updateHighestBidderOnInsertBid_r10(address p,int m) private    {
      int n = highestBid.n;
      if(m==n) {
        highestBidder = HighestBidderTuple(p,true);
      }
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateBidTotalOnInsertBid_r7(address p,int m) private    {
      int delta0 = int(m);
      updateBalanceOnIncrementBidTotal_r8(p,delta0);
  }
  function updateBeneficiaryOnInsertConstructor_r4(address p) private    {
      // Empty()
  }
  function updateBidOnInsertRecv_bid_r5(address p,int n) private   returns (bool) {
      int m_1 = highestBid.n;
      bool b_0 = auctionEnded.b;
      if(b_0!=true && n>m_1) {
        updateHighestBidderOnInsertBid_r10(p,n);
        updateBidTotalOnInsertBid_r7(p,n);
        updateHighestBidOnInsertBid_r12(n);
        emit Bid(p,n);
        return true;
      }
      return false;
  }
  function updateHighestBidOnInsertBid_r12(int m) private    {
      int _max = highestBid.n;
      if(m>_max) {
        highestBid = HighestBidTuple(m,true);
      }
  }
  function updateWithdrawTotalOnInsertWithdraw_r16(address p,int m) private    {
      int delta0 = int(m);
      updateBalanceOnIncrementWithdrawTotal_r8(p,delta0);
  }
  function updateAuctionEndedOnInsertEndAuction_r0() private    {
      auctionEnded = AuctionEndedTuple(true,true);
  }
}