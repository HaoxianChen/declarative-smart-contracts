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
  struct InvestTotalTuple {
    uint n;
    bool _valid;
  }
  struct RefundTotalTuple {
    uint n;
    bool _valid;
  }
  struct BeneficiaryTuple {
    address p;
    bool _valid;
  }
  struct BalanceOfTuple {
    uint n;
    bool _valid;
  }
  TargetTuple target;
  OwnerTuple owner;
  RaisedTuple raised;
  BeneficiaryTuple beneficiary;
  mapping(address=>InvestTotalTuple) investTotal;
  mapping(address=>RefundTotalTuple) refundTotal;
  mapping(address=>BalanceOfTuple) balanceOf;
  event Refund(address p,uint n);
  event Invest(address p,uint n);
  event Closed(bool b);
  event Withdraw(address p,uint n);
  constructor(uint t,address b) public {
    updateTotalBalanceOnInsertConstructor_r12();
    updateTargetOnInsertConstructor_r11(t);
    updateRaisedOnInsertConstructor_r6();
    updateOwnerOnInsertConstructor_r7();
    updateBeneficiaryOnInsertConstructor_r16(b);
  }
  function getClosed() public view  returns (bool) {
      bool b = closed.b;
      return b;
  }
  function invest() public  payable  {
      bool r4 = updateInvestOnInsertRecv_invest_r4();
      if(r4==false) {
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
  function close() public    {
      revert("Rule condition failed");
  }
  function withdraw() public    {
      bool r8 = updateWithdrawOnInsertRecv_withdraw_r8();
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function updateBeneficiaryOnInsertConstructor_r16(address p) private    {
      beneficiary = BeneficiaryTuple(p,true);
  }
  function updateSendOnInsertRefund_r1(address p,uint n) private    {
      payable(p).send(n);
  }
  function closed(bool b) private view  returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        return true;
      }
      return false;
  }
  function updateInvestTotalOnInsertInvest_r5(address p,uint m) private    {
      int delta0 = int(m);
      updateBalanceOfOnIncrementInvestTotal_r2(p,delta0);
      investTotal[p].n += m;
  }
  function updateTotalBalanceOnInsertConstructor_r12() private    {
      // Empty()
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateRefundTotalOnInsertRefund_r14(address p,uint m) private    {
      int delta0 = int(m);
      updateBalanceOfOnIncrementRefundTotal_r2(p,delta0);
      refundTotal[p].n += m;
  }
  function updateInvestOnInsertRecv_invest_r4() private   returns (bool) {
      uint s = raised.n;
      uint t = target.t;
      uint n = msg.value;
      address p = msg.sender;
      if(s<t && closed(false)) {
        updateInvestTotalOnInsertInvest_r5(p,n);
        updateRaisedOnInsertInvest_r13(n);
        emit Invest(p,n);
        return true;
      }
      return false;
  }
  function updateRaisedOnInsertInvest_r13(uint m) private    {
      raised.n += m;
  }
  function updateTargetOnInsertConstructor_r11(uint t) private    {
      target = TargetTuple(t,true);
  }
  function updateBalanceOfOnIncrementRefundTotal_r2(address p,int r) private    {
      int _delta = int(-r);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
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
  function updateSendOnInsertWithdraw_r0(address p,uint r) private    {
      payable(p).send(r);
  }
  function updateRefundOnInsertRecv_refund_r3() private   returns (bool) {
      uint t = target.t;
      uint r = raised.n;
      address p = msg.sender;
      uint n = balanceOf[p].n;
      if(r<t && n>0 && closed(true)) {
        updateRefundTotalOnInsertRefund_r14(p,n);
        updateSendOnInsertRefund_r1(p,n);
        emit Refund(p,n);
        return true;
      }
      return false;
  }
  function updateRaisedOnInsertConstructor_r6() private    {
      raised = RaisedTuple(0,true);
  }
  function updateOwnerOnInsertConstructor_r7() private    {
      address p = msg.sender;
      owner = OwnerTuple(p,true);
  }
  function updateBalanceOfOnIncrementInvestTotal_r2(address p,int i) private    {
      int _delta = int(i);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
}