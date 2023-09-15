contract Auction {
  struct BalanceTuple {
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
  mapping(address=>BalanceTuple) balance;
  BeneficiaryTuple beneficiary;
  HighestBidTuple highestBid;
  EndTimeTuple endTime;
  OwnerTuple owner;
  EndTuple end;
  event Withdraw(address bidder,uint amount);
  event Send(address p,uint amount);
  event Bid(address bidder,uint amount);
  event End(bool b);
  constructor(address beneficiary,uint biddingTime) public {
    updateBeneficiaryOnInsertConstructor_r11(beneficiary);
    updateEndTimeOnInsertConstructor_r14(biddingTime);
    updateOwnerOnInsertConstructor_r9();
  }
  function bid() public  payable  {
      bool r10 = updateBidOnInsertRecv_bid_r10();
      if(r10==false) {
        revert("Rule condition failed");
      }
  }
  function withdraw() public    {
      bool r1 = updateWithdrawOnInsertRecv_withdraw_r1();
      bool r5 = updateWithdrawOnInsertRecv_withdraw_r5();
      if(r1==false && r5==false) {
        revert("Rule condition failed");
      }
  }
  function endAuction() public    {
      bool r4 = updateEndOnInsertRecv_endAuction_r4();
      bool r2 = updateSendOnInsertRecv_endAuction_r2();
      if(r4==false && r2==false) {
        revert("Rule condition failed");
      }
  }
  function getBalance(address p) public view  returns (uint) {
      uint n = balance[p].n;
      return n;
  }
  function updateEndTimeOnInsertConstructor_r14(uint d) private    {
      uint t = block.timestamp;
      uint t2 = t+d;
      endTime = EndTimeTuple(t2,true);
  }
  function updateWithdrawTotalOnInsertWithdraw_r3(address p,uint m) private    {
      int delta0 = int(m);
      updateBalanceOnIncrementWithdrawTotal_r7(p,delta0);
  }
  function updateHighestBidOnInsertBid_r0(address p,uint m) private    {
      uint _max = highestBid.amount;
      if(m>_max) {
        highestBid = HighestBidTuple(p,m,true);
      }
  }
  function updateOwnerOnInsertConstructor_r9() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateBalanceOnIncrementBidTotal_r7(address p,int b) private    {
      int _delta = int(b);
      uint newValue = updateuintByint(balance[p].n,_delta);
      balance[p].n = newValue;
  }
  function updateEndOnInsertRecv_endAuction_r4() private   returns (bool) {
      uint t1 = block.timestamp;
      address s = msg.sender;
      uint t2 = endTime.t;
      if(false==end.b) {
        if(s==owner.p) {
          if(t1>=t2) {
            end = EndTuple(true,true);
            emit End(true);
            return true;
          }
        }
      }
      return false;
  }
  function updateWithdrawOnInsertRecv_withdraw_r1() private   returns (bool) {
      address p = highestBid.bidder;
      uint m = highestBid.amount;
      if(true==end.b) {
        if(p==msg.sender) {
          uint n = balance[p].n;
          if(n>m) {
            uint s = n-m;
            updateWithdrawTotalOnInsertWithdraw_r3(p,s);
            updateSendOnInsertWithdraw_r8(p,s);
            emit Withdraw(p,s);
            return true;
          }
        }
      }
      return false;
  }
  function updateWithdrawOnInsertRecv_withdraw_r5() private   returns (bool) {
      if(true==end.b) {
        address h = highestBid.bidder;
        address p = msg.sender;
        uint n = balance[p].n;
        if(p!=h && n>0) {
          updateSendOnInsertWithdraw_r8(p,n);
          updateWithdrawTotalOnInsertWithdraw_r3(p,n);
          emit Withdraw(p,n);
          return true;
        }
      }
      return false;
  }
  function updateBeneficiaryOnInsertConstructor_r11(address p) private    {
      beneficiary = BeneficiaryTuple(p,true);
  }
  function updateSendOnInsertWithdraw_r8(address p,uint n) private    {
      payable(p).send(n);
      emit Send(p,n);
  }
  function updateBalanceOnIncrementWithdrawTotal_r7(address p,int w) private    {
      int _delta = int(-w);
      uint newValue = updateuintByint(balance[p].n,_delta);
      balance[p].n = newValue;
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
  function updateBidOnInsertRecv_bid_r10() private   returns (bool) {
      uint t1 = block.timestamp;
      uint t2 = endTime.t;
      uint m = highestBid.amount;
      if(false==end.b) {
        uint n = msg.value;
        address p = msg.sender;
        if(n>m && t1<t2) {
          updateHighestBidOnInsertBid_r0(p,n);
          updateBidTotalOnInsertBid_r12(p,n);
          emit Bid(p,n);
          return true;
        }
      }
      return false;
  }
  function updateBidTotalOnInsertBid_r12(address p,uint m) private    {
      int delta0 = int(m);
      updateBalanceOnIncrementBidTotal_r7(p,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
}