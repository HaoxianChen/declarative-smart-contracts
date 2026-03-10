contract CrowdFunding {
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
  struct BeneficiaryTuple {
    address p;
    bool _valid;
  }
  struct BalanceOfTuple {
    uint n;
    bool _valid;
  }
  TargetTuple target;
  RaisedTuple raised;
  ClosedTuple closed;
  BeneficiaryTuple beneficiary;
  mapping(address=>BalanceOfTuple) balanceOf;
  OwnerTuple owner;
  event Refund(address p,uint n);
  event InvalidTx();
  event Close();
  event Invest(address p,uint n);
  event Withdraw(address p,uint n);
  constructor(uint t,address b) public {
    updateTotalBalanceOnInsertConstructor_r24();
    updateTargetOnInsertConstructor_r14(t);
    updateBeneficiaryOnInsertConstructor_r26(b);
    updateOnceRefundOnInsertConstructor_r11();
    updateRaisedOnInsertConstructor_r6();
    updateOwnerOnInsertConstructor_r7();
    updateOnceWithdrawOnInsertConstructor_r21();
  }
  function getClosed() public view  returns (bool) {
      bool b = closed.b;
      return b;
  }
  function getRaised() public view  returns (uint) {
      uint n = raised.n;
      return n;
  }
  function refund(address p,uint n) public    {
      bool r16 = updateRefundOnInsertRecv_refund_r16(p,n);
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (uint) {
      uint n = balanceOf[p].n;
      return n;
  }
  function invest(address p,uint n) public    {
      bool r3 = updateInvestOnInsertRecv_invest_r3(p,n);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function withdraw(address p,uint n) public    {
      bool r2 = updateWithdrawOnInsertRecv_withdraw_r2(p,n);
      if(r2==false) {
        revert("Rule condition failed");
      }
  }
  function close() public    {
      bool r22 = updateCloseOnInsertRecv_close_r22();
      if(r22==false) {
        revert("Rule condition failed");
      }
  }
  function getTarget() public view  returns (uint) {
      uint t = target.t;
      return t;
  }
  function updateSendOnInsertRefund_r1(address p,uint n) private    {
      payable(p).send(n);
  }
  function updateCloseOnInsertRecv_close_r22() private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        updateClosedOnInsertClose_r0();
        emit Close();
        return true;
      }
      return false;
  }
  function updateRefundTotalOnInsertRefund_r19(address p,uint m) private    {
      int delta0 = int(m);
      updateBalanceOfOnIncrementRefundTotal_r5(p,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateClosedOnInsertClose_r0() private    {
      closed = ClosedTuple(true,true);
  }
  function updateSendOnInsertWithdraw_r10(address p,uint r) private    {
      payable(p).send(r);
  }
  function updateRaisedOnInsertInvest_r15(uint m) private    {
      raised.n += m;
  }
  function updateInvestOnInsertRecv_invest_r3(address p,uint n) private   returns (bool) {
      bool closed_b = closed.b;
      if(closed_b==false) {
        updateInvestTotalOnInsertInvest_r20(p,n);
        updateRaisedOnInsertInvest_r15(n);
        emit Invest(p,n);
        return true;
      }
      return false;
  }
  function updateOwnerOnInsertConstructor_r7() private    {
      address p = msg.sender;
      owner = OwnerTuple(p,true);
  }
  function updateInvestTotalOnInsertInvest_r20(address p,uint m) private    {
      int delta0 = int(m);
      updateBalanceOfOnIncrementInvestTotal_r5(p,delta0);
  }
  function updateRaisedOnInsertConstructor_r6() private    {
      raised = RaisedTuple(0,true);
  }
  function updateRefundOnInsertRecv_refund_r16(address p,uint n) private   returns (bool) {
      bool b = closed.b;
      uint t = target.t;
      uint r = raised.n;
      if(b!=false && r<t && b==false) {
        updateRefundTotalOnInsertRefund_r19(p,n);
        updateSendOnInsertRefund_r1(p,n);
        emit Refund(p,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementInvestTotal_r5(address p,int i) private    {
      int _delta = int(i);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateTargetOnInsertConstructor_r14(uint t) private    {
      target = TargetTuple(t,true);
  }
  function updateBalanceOfOnIncrementRefundTotal_r5(address p,int r) private    {
      int _delta = int(-r);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateOnceWithdrawOnInsertConstructor_r21() private    {
      // Empty()
  }
  function updateTotalBalanceOnInsertConstructor_r24() private    {
      // Empty()
  }
  function updateBeneficiaryOnInsertConstructor_r26(address p) private    {
      beneficiary = BeneficiaryTuple(p,true);
  }
  function updateOnceRefundOnInsertConstructor_r11() private    {
      // Empty()
  }
  function updateWithdrawOnInsertRecv_withdraw_r2(address p,uint n) private   returns (bool) {
      uint t_1 = target.t;
      uint r_1 = raised.n;
      address b_0 = beneficiary.p;
      if(p==b_0 && r_1>=t_1) {
        updateSendOnInsertWithdraw_r10(p,n);
        emit Withdraw(p,n);
        return true;
      }
      return false;
  }
}