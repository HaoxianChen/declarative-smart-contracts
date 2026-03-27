contract CrowdFunding {
  struct TargetTuple {
    uint t;
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
  struct OwnerTuple {
    address p;
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
  event Close();
  event Invest(address p,uint n);
  event CloseNotByOwner();
  event Withdraw(address p,uint n);
  event WithdrawNotByBeneficiary();
  constructor(uint t,address b) public {
    updateTargetOnInsertConstructor_r18(t);
    updateOnceRefundBeforeCloseOnInsertConstructor_r1();
    updateOnceWithdrawBeforeTargetOnInsertConstructor_r25();
    updateOnceRefundOnInsertConstructor_r14();
    updateClosedOnInsertConstructor_r27();
    updateBeneficiaryOnInsertConstructor_r36(b);
    updateTotalBalanceOnInsertConstructor_r33();
    updateOwnerOnInsertConstructor_r29();
    updateRaisedOnInsertConstructor_r9();
    updateOnceWithdrawOnInsertConstructor_r31();
    updateOnceInvestAfterCloseOnInsertConstructor_r35();
  }
  function getClosed() public view  returns (bool) {
      bool b = closed.b;
      return b;
  }
  function getRaised() public view  returns (uint) {
      uint n = raised.n;
      return n;
  }
  function invest(address p,uint n) public    {
      bool r22 = updateInvestOnInsertRecv_invest_r22(p,n);
      if(r22==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (uint) {
      uint n = balanceOf[p].n;
      return n;
  }
  function close() public    {
      bool r7 = updateCloseOnInsertRecv_close_r7();
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function withdraw(address p,uint n) public    {
      bool r30 = updateWithdrawOnInsertRecv_withdraw_r30(p,n);
      if(r30==false) {
        revert("Rule condition failed");
      }
  }
  function refund(address p,uint n) public    {
      bool r12 = updateRefundOnInsertRecv_refund_r12(p,n);
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function getTarget() public view  returns (uint) {
      uint t = target.t;
      return t;
  }
  function updateRefundOnInsertRecv_refund_r12(address p,uint n) private   returns (bool) {
      bool closed_b = closed.b;
      if(closed_b==true) {
        updateSendOnInsertRefund_r0(p,n);
        updateRefundTotalOnInsertRefund_r26(p,n);
        emit Refund(p,n);
        return true;
      }
      return false;
  }
  function updateOnceWithdrawBeforeTargetOnInsertConstructor_r25() private    {
      // Empty()
  }
  function updateRaisedOnInsertConstructor_r9() private    {
      raised = RaisedTuple(0,true);
  }
  function updateRefundTotalOnInsertRefund_r26(address p,uint m) private    {
      int delta0 = int(m);
      updateBalanceOfOnIncrementRefundTotal_r5(p,delta0);
  }
  function updateOnceWithdrawOnInsertConstructor_r31() private    {
      // Empty()
  }
  function updateOnceInvestAfterCloseOnInsertConstructor_r35() private    {
      // Empty()
  }
  function updateInvestTotalOnInsertInvest_r28(address p,uint m) private    {
      int delta0 = int(m);
      updateBalanceOfOnIncrementInvestTotal_r5(p,delta0);
  }
  function updateBalanceOfOnIncrementRefundTotal_r5(address p,int r) private    {
      int _delta = int(-r);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateClosedOnInsertClose_r11() private    {
      closed = ClosedTuple(true,true);
  }
  function updateOnceRefundOnInsertConstructor_r14() private    {
      // Empty()
  }
  function updateOnceRefundBeforeCloseOnInsertConstructor_r1() private    {
      // Empty()
  }
  function updateTotalBalanceOnInsertConstructor_r33() private    {
      // Empty()
  }
  function updateRaisedOnInsertInvest_r2(uint m) private    {
      raised.n += m;
  }
  function updateOwnerOnInsertConstructor_r29() private    {
      address p = msg.sender;
      owner = OwnerTuple(p,true);
  }
  function updateBalanceOfOnIncrementInvestTotal_r5(address p,int i) private    {
      int _delta = int(i);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateInvestOnInsertRecv_invest_r22(address p,uint n) private   returns (bool) {
      bool closed_b = closed.b;
      if(closed_b==false) {
        updateInvestTotalOnInsertInvest_r28(p,n);
        updateRaisedOnInsertInvest_r2(n);
        emit Invest(p,n);
        return true;
      }
      return false;
  }
  function updateSendOnInsertRefund_r0(address p,uint n) private    {
      payable(p).send(n);
  }
  function updateSendOnInsertWithdraw_r13(address p,uint r) private    {
      payable(p).send(r);
  }
  function updateWithdrawOnInsertRecv_withdraw_r30(address p,uint n) private   returns (bool) {
      uint raised_x = raised.n;
      uint target_x = target.t;
      address b = beneficiary.p;
      if(p==b && target_x<raised_x) {
        updateSendOnInsertWithdraw_r13(p,n);
        emit Withdraw(p,n);
        return true;
      }
      return false;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateClosedOnInsertConstructor_r27() private    {
      closed = ClosedTuple(false,true);
  }
  function updateBeneficiaryOnInsertConstructor_r36(address p) private    {
      beneficiary = BeneficiaryTuple(p,true);
  }
  function updateTargetOnInsertConstructor_r18(uint t) private    {
      target = TargetTuple(t,true);
  }
  function updateCloseOnInsertRecv_close_r7() private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(s==o) {
        updateClosedOnInsertClose_r11();
        emit Close();
        return true;
      }
      return false;
  }
}