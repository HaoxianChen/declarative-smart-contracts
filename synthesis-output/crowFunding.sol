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
    updateOnceWithdrawOnInsertConstructor_r23();
    updateTargetOnInsertConstructor_r14(t);
    updateOnceRefundOnInsertConstructor_r11();
    updateOwnerOnInsertConstructor_r22();
    updateBeneficiaryOnInsertConstructor_r28(b);
    updateRaisedOnInsertConstructor_r7();
    updateTotalBalanceOnInsertConstructor_r26();
  }
  function getClosed() public view  returns (bool) {
      bool b = closed.b;
      return b;
  }
  function close() public    {
      bool r24 = updateCloseOnInsertRecv_close_r24();
      if(r24==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (uint) {
      uint n = balanceOf[p].n;
      return n;
  }
  function refund(address p,uint n) public    {
      bool r21 = updateRefundOnInsertRecv_refund_r21(p,n);
      if(r21==false) {
        revert("Rule condition failed");
      }
  }
  function invest(address p,uint n) public    {
      bool r4 = updateInvestOnInsertRecv_invest_r4(p,n);
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function getTarget() public view  returns (uint) {
      uint t = target.t;
      return t;
  }
  function withdraw(address p,uint n) public    {
      bool r3 = updateWithdrawOnInsertRecv_withdraw_r3(p,n);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function getRaised() public view  returns (uint) {
      uint n = raised.n;
      return n;
  }
  function updateClosedOnInsertClose_r0() private    {
      closed = ClosedTuple(true,true);
  }
  function updateWithdrawOnInsertRecv_withdraw_r3(address p,uint n) private   returns (bool) {
      uint t_1 = target.t;
      uint r_1 = raised.n;
      address b_0 = beneficiary.p;
      if(p==b_0 && r_1>=t_1) {
        updateSendOnInsertWithdraw_r10(p,r);
        emit Withdraw(p,r);
        return true;
      }
      return false;
  }
  function updateSendOnInsertRefund_r1(address p,uint n) private    {
      payable(p).send(n);
  }
  function updateTotalBalanceOnInsertConstructor_r26() private    {
      // Empty()
  }
  function updateRaisedOnInsertConstructor_r7() private    {
      raised = RaisedTuple(0,true);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateBalanceOfOnIncrementRefundTotal_r6(address p,int r) private    {
      int _delta = int(-r);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateBeneficiaryOnInsertConstructor_r28(address p) private    {
      beneficiary = BeneficiaryTuple(p,true);
  }
  function updateRaisedOnInsertInvest_r15(uint m) private    {
      raised.n += m;
  }
  function updateSendOnInsertWithdraw_r10(address p,uint r) private    {
      payable(p).send(r);
  }
  function updateBalanceOfOnIncrementInvestTotal_r6(address p,int i) private    {
      int _delta = int(i);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateRefundOnInsertRecv_refund_r21(address p,uint n) private   returns (bool) {
      bool closed_b = closed.b;
      uint r_0 = raised.n;
      uint t_0 = target.t;
      bool b_1 = closed.b;
      if(r_0<t_0 && b_1!=false && closed_b==false) {
        updateRefundTotalOnInsertRefund_r19(p,n);
        updateSendOnInsertRefund_r1(p,n);
        emit Refund(p,n);
        return true;
      }
      return false;
  }
  function updateTargetOnInsertConstructor_r14(uint t) private    {
      target = TargetTuple(t,true);
  }
  function updateCloseOnInsertRecv_close_r24() private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        updateClosedOnInsertClose_r0();
        emit Close();
        return true;
      }
      return false;
  }
  function updateInvestOnInsertRecv_invest_r4(address p,uint n) private   returns (bool) {
      bool closed_b = closed.b;
      if(closed_b==false) {
        updateInvestTotalOnInsertInvest_r20(p,n);
        updateRaisedOnInsertInvest_r15(n);
        emit Invest(p,n);
        return true;
      }
      return false;
  }
  function updateOnceWithdrawOnInsertConstructor_r23() private    {
      // Empty()
  }
  function updateOnceRefundOnInsertConstructor_r11() private    {
      // Empty()
  }
  function updateInvestTotalOnInsertInvest_r20(address p,uint m) private    {
      int delta0 = int(m);
      updateBalanceOfOnIncrementInvestTotal_r6(p,delta0);
  }
  function updateOwnerOnInsertConstructor_r22() private    {
      address p = msg.sender;
      owner = OwnerTuple(p,true);
  }
  function updateRefundTotalOnInsertRefund_r19(address p,uint m) private    {
      int delta0 = int(m);
      updateBalanceOfOnIncrementRefundTotal_r6(p,delta0);
  }
}