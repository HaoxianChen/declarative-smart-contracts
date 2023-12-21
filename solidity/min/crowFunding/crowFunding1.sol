contract CrowFunding {
  struct TargetTuple {
    uint t;
    bool _valid;
  }
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct RaisedTuple {
    uint n;
    bool _valid;
  }
  struct ClosedTuple {
    bool b;
    bool _valid;
  }
  struct InvestTotalTuple {
    uint n;
    bool _valid;
  }
  struct RefundTotalTuple {
    uint n;
    bool _valid;
  }
  struct TotalBalanceTuple {
    uint m;
    bool _valid;
  }
  struct BeneficiaryTuple {
    address p;
    bool _valid;
  }
  TargetTuple target;
  RaisedTuple raised;
  ClosedTuple closed;
  BeneficiaryTuple beneficiary;
  mapping(address=>InvestTotalTuple) investTotal;
  mapping(address=>RefundTotalTuple) refundTotal;
  TotalBalanceTuple totalBalance;
  OwnerTuple owner;
  event Refund(address p,uint n);
  event Invest(address p,uint n);
  event Closed(bool b);
  event Withdraw(address p,uint n);
  constructor(uint t,address b) public {
    updateRaisedOnInsertConstructor_r6();
    updateBeneficiaryOnInsertConstructor_r16(b);
    updateTotalBalanceOnInsertConstructor_r12();
    updateOwnerOnInsertConstructor_r7();
    updateTargetOnInsertConstructor_r11(t);
  }
  function getClosed() public view  returns (bool) {
      bool b = closed.b;
      return b;
  }
  function close() public    {
      bool r9 = updateClosedOnInsertRecv_close_r9();
      if(r9==false) {
        revert("Rule condition failed");
      }
  }
  function invest() public  payable  {
      bool r4 = updateInvestOnInsertRecv_invest_r4();
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function withdraw() public    {
      bool r8 = updateWithdrawOnInsertRecv_withdraw_r8();
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function refund() public    {
      bool r3 = updateRefundOnInsertRecv_refund_r3();
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function getRaised() public view  returns (uint) {
      uint n = raised.n;
      return n;
  }
  function updateBeneficiaryOnInsertConstructor_r16(address p) private    {
      beneficiary = BeneficiaryTuple(p,true);
  }
  function updateTargetOnInsertConstructor_r11(uint t) private    {
      target = TargetTuple(t,true);
  }
  function updateSendOnInsertRefund_r1(address p,uint n) private    {
      payable(p).send(n);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateInvestOnInsertRecv_invest_r4() private   returns (bool) {
      if(false==closed.b) {
        uint s = raised.n;
        uint t = target.t;
        uint n = msg.value;
        address p = msg.sender;
        if(s<t) {
          updateInvestTotalOnInsertInvest_r5(p,n);
          updateRaisedOnInsertInvest_r13(n);
          emit Invest(p,n);
          return true;
        }
      }
      return false;
  }
  function updateRaisedOnInsertConstructor_r6() private    {
      raised = RaisedTuple(0,true);
  }
  function updateWithdrawOnInsertRecv_withdraw_r8() private   returns (bool) {
      address p = beneficiary.p;
      uint t = target.t;
      uint r = raised.n;
      if(p==msg.sender) {
        if(r>=t) {
          updateSendOnInsertWithdraw_r0(p,r);
          emit Withdraw(p,r);
          return true;
        }
      }
      return false;
  }
  function updateRaisedOnInsertInvest_r13(uint m) private    {
      raised.n += m;
  }
  function updateOwnerOnInsertConstructor_r7() private    {
      address p = msg.sender;
      owner = OwnerTuple(p,true);
  }
  function updateTotalBalanceOnInsertConstructor_r12() private    {
      totalBalance = TotalBalanceTuple(0,true);
  }
  function updateRefundOnInsertRecv_refund_r3() private   returns (bool) {
      if(true==closed.b) {
        address p = msg.sender;
        uint t = target.t;
        uint r = raised.n;
        uint n = balanceOf(p);
        if(r<t && n>0) {
          updateRefundTotalOnInsertRefund_r14(p,n);
          updateSendOnInsertRefund_r1(p,n);
          emit Refund(p,n);
          return true;
        }
      }
      return false;
  }
  function updateInvestTotalOnInsertInvest_r5(address p,uint m) private    {
      investTotal[p].n += m;
  }
  function balanceOf(address p) private view  returns (uint) {
      uint r = refundTotal[p].n;
      uint i = investTotal[p].n;
      uint s = i-r;
      return s;
  }
  function updateRefundTotalOnInsertRefund_r14(address p,uint m) private    {
      refundTotal[p].n += m;
  }
  function updateClosedOnInsertRecv_close_r9() private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        closed = ClosedTuple(true,true);
        emit Closed(true);
        return true;
      }
      return false;
  }
  function updateSendOnInsertWithdraw_r0(address p,uint r) private    {
      payable(p).send(r);
  }
}