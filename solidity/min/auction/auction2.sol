contract Auction {
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct BeneficiaryTuple {
    address p;
    bool _valid;
  }
  struct WithdrawTotalTuple {
    uint n;
    bool _valid;
  }
  struct HighestBidTuple {
    address bidder;
    uint amount;
    bool _valid;
  }
  struct EndTimeTuple {
    uint t;
    bool _valid;
  }
  struct EndTuple {
    bool b;
    bool _valid;
  }
  struct BidTotalTuple {
    uint n;
    bool _valid;
  }
  OwnerTuple owner;
  BeneficiaryTuple beneficiary;
  mapping(address=>WithdrawTotalTuple) withdrawTotal;
  HighestBidTuple highestBid;
  EndTimeTuple endTime;
  mapping(address=>BidTotalTuple) bidTotal;
  EndTuple end;
  event Withdraw(address bidder,uint amount);
  event EndAuction(bool b);
  event Send(address p,uint amount);
  event Bid(address bidder,uint amount);
  constructor(address beneficiary,uint biddingTime) public {
    updateEndTimeOnInsertConstructor_r13(biddingTime);
    updateOwnerOnInsertConstructor_r7();
    updateBeneficiaryOnInsertConstructor_r9(beneficiary);
  }
  function bid() public  payable  {
      bool r8 = updateBidOnInsertRecv_bid_r8();
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function withdraw() public    {
      bool r4 = updateWithdrawOnInsertRecv_withdraw_r4();
      bool r1 = updateWithdrawOnInsertRecv_withdraw_r1();
      if(r4==false && r1==false) {
        revert("Rule condition failed");
      }
  }
  function endAuction() public    {
      bool r11 = updateEndAuctionOnInsertRecv_endAuction_r11();
      bool r2 = updateSendOnInsertRecv_endAuction_r2();
      if(r11==false && r2==false) {
        revert("Rule condition failed");
      }
  }
  function getBalance(address p) public view  returns (uint) {
      uint n = balance[p].n;
      return n;
  }
  function updateWithdrawOnInsertRecv_withdraw_r1() private   returns (bool) {
      address p = highestBid.bidder;
      uint m = highestBid.amount;
      if(true==end.b) {
        if(p==msg.sender) {
          if(n>m && balance(p,n)) {
            uint s = n-m;
            updateSendOnInsertWithdraw_r6(p,s);
            updateWithdrawTotalOnInsertWithdraw_r3(p,s);
            emit Withdraw(p,s);
            return true;
          }
        }
      }
      return false;
  }
  function updateWithdrawOnInsertRecv_withdraw_r4() private   returns (bool) {
      if(true==end.b) {
        address h = highestBid.bidder;
        address p = msg.sender;
        if(p!=h && n>0 && balance(p,n)) {
          updateSendOnInsertWithdraw_r6(p,n);
          updateWithdrawTotalOnInsertWithdraw_r3(p,n);
          emit Withdraw(p,n);
          return true;
        }
      }
      return false;
  }
  function balance(address p,uint n) private view  returns (bool) {
      uint w = withdrawTotal[p].n;
      uint b = bidTotal[p].n;
      return true;
      return false;
  }
  function updateEndOnInsertEndAuction_r15(bool true) private    {
      end = EndTuple(true,true);
  }
  function updateBidOnInsertRecv_bid_r8() private   returns (bool) {
      uint t1 = block.timestamp;
      uint t2 = endTime.t;
      uint m = highestBid.amount;
      if(false==end.b) {
        uint n = msg.value;
        address p = msg.sender;
        if(n>m && t1<t2) {
          updateHighestBidOnInsertBid_r0(p,n);
          updateBidTotalOnInsertBid_r10(p,n);
          emit Bid(p,n);
          return true;
        }
      }
      return false;
  }
  function updateBidTotalOnInsertBid_r10(address p,uint m) private    {
      bidTotal[p].n += m;
  }
  function updateOwnerOnInsertConstructor_r7() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateSendOnInsertRecv_endAuction_r2() private   returns (bool) {
      uint t1 = block.timestamp;
      address s = msg.sender;
      address p = beneficiary.p;
      uint t2 = endTime.t;
      uint n = highestBid.amount;
      if(false==end.b) {
        if(s==owner.p) {
          if(t1>=t2) {
            payable(p).send(n);
            emit Send(p,n);
            return true;
          }
        }
      }
      return false;
  }
  function updateSendOnInsertWithdraw_r6(address p,uint n) private    {
      payable(p).send(n);
      emit Send(p,n);
  }
  function updateBeneficiaryOnInsertConstructor_r9(address p) private    {
      beneficiary = BeneficiaryTuple(p,true);
  }
  function updateEndAuctionOnInsertRecv_endAuction_r11() private   returns (bool) {
      uint t1 = block.timestamp;
      address s = msg.sender;
      uint t2 = endTime.t;
      if(false==end.b) {
        if(s==owner.p) {
          if(t1>=t2) {
            updateEndOnInsertEndAuction_r15(bool(true));
            emit EndAuction(true);
            return true;
          }
        }
      }
      return false;
  }
  function updateHighestBidOnInsertBid_r0(address p,uint m) private    {
      uint _max = highestBid.amount;
      if(m>_max) {
        highestBid = HighestBidTuple(p,m,true);
      }
  }
  function updateEndTimeOnInsertConstructor_r13(uint d) private    {
      uint t = block.timestamp;
      uint t2 = t+d;
      endTime = EndTimeTuple(t2,true);
  }
  function updateWithdrawTotalOnInsertWithdraw_r3(address p,uint m) private    {
      withdrawTotal[p].n += m;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
}