contract CrowFunding {
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct BeneficiaryTuple {
    address p;
    bool _valid;
  }
  struct BalanceOfTuple {
    int n;
    bool _valid;
  }
  struct ClosedTuple {
    bool b;
    bool _valid;
  }
  struct TargetTuple {
    int t;
    bool _valid;
  }
  struct RaisedTuple {
    int n;
    bool _valid;
  }
  BeneficiaryTuple beneficiary;
  mapping(address=>BalanceOfTuple) balanceOf;
  RaisedTuple raised;
  OwnerTuple owner;
  ClosedTuple closed;
  TargetTuple target;
  event Withdraw(address p,int n);
  event InvalidTx();
  event Close();
  event Refund(address p,int n);
  event Invest(address p,int n);
  constructor(int t,address b) public {
    updateBeneficiaryOnInsertConstructor_r6(b);
    updateTargetOnInsertConstructor_r24(t);
    updateOnceWithdrawOnInsertConstructor_r22();
    updateOwnerOnInsertConstructor_r21();
    updateRaisedOnInsertConstructor_r3();
    updateOnceRefundOnInsertConstructor_r11();
    updateTotalBalanceOnInsertConstructor_r27();
  }
  function getClosed() public view  returns (bool) {
      bool b = closed.b;
      return b;
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function refund(address p,int n) public    {
      bool r5 = updateRefundOnInsertRecv_refund_r5(p,n);
      if(r5==false) {
        revert("Rule condition failed");
      }
  }
  function withdraw(address p,int n) public    {
      bool r29 = updateWithdrawOnInsertRecv_withdraw_r29(p,n);
      if(r29==false) {
        revert("Rule condition failed");
      }
  }
  function getRaised() public view  returns (int) {
      int n = raised.n;
      return n;
  }
  function invest(address p,int n) public    {
      bool r8 = updateInvestOnInsertRecv_invest_r8(p,n);
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function close() public    {
      bool r9 = updateCloseOnInsertRecv_close_r9();
      if(r9==false) {
        revert("Rule condition failed");
      }
  }
  function getTarget() public view  returns (int) {
      int t = target.t;
      return t;
  }
  function updateRaisedOnInsertInvest_r18(int m) private    {
      raised.n += m;
  }
  function updateOnceWithdrawOnInsertConstructor_r22() private    {
      // Empty()
  }
  function updateClosedOnInsertClose_r0() private    {
      closed = ClosedTuple(true,true);
  }
  function updateSendOnInsertWithdraw_r13(address p,int r) private    {
      payable(p).send(uint(r));
  }
  function updateTargetOnInsertConstructor_r24(int t) private    {
      target = TargetTuple(t,true);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateWithdrawOnInsertRecv_withdraw_r29(address p,int r) private   returns (bool) {
      address b_1 = beneficiary.p;
      int t_0 = target.t;
      int r_p_0 = raised.n;
      if(r_p_0>=t_0 && p==b_1) {
        updateSendOnInsertWithdraw_r13(p,r);
        emit Withdraw(p,r);
        return true;
      }
      return false;
  }
  function updateSendOnInsertRefund_r15(address p,int n) private    {
      payable(p).send(uint(n));
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateRaisedOnInsertConstructor_r3() private    {
      raised = RaisedTuple(0,true);
  }
  function updateRefundOnInsertRecv_refund_r5(address p,int n) private   returns (bool) {
      int t_1 = target.t;
      int r_1 = raised.n;
      bool b_0 = closed.b;
      int balanceOf_x1 = balanceOf[p].n;
      if(b_0!=false && r_1<t_1 && n<balanceOf_x1) {
        updateSendOnInsertRefund_r15(p,n);
        updateRefundTotalOnInsertRefund_r14(p,n);
        emit Refund(p,n);
        return true;
      }
      return false;
  }
  function updateInvestOnInsertRecv_invest_r8(address p,int n) private   returns (bool) {
      bool closed_b_1 = closed.b;
      if(n>=0 && closed_b_1==false) {
        updateRaisedOnInsertInvest_r18(n);
        updateInvestTotalOnInsertInvest_r26(p,n);
        emit Invest(p,n);
        return true;
      }
      return false;
  }
  function updateInvestTotalOnInsertInvest_r26(address p,int m) private    {
      int delta0 = int(m);
      updateBalanceOfOnIncrementInvestTotal_r16(p,delta0);
  }
  function updateOwnerOnInsertConstructor_r21() private    {
      address p = msg.sender;
      owner = OwnerTuple(p,true);
  }
  function updateRefundTotalOnInsertRefund_r14(address p,int m) private    {
      int delta0 = int(m);
      updateBalanceOfOnIncrementRefundTotal_r16(p,delta0);
  }
  function updateOnceRefundOnInsertConstructor_r11() private    {
      // Empty()
  }
  function updateCloseOnInsertRecv_close_r9() private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        updateClosedOnInsertClose_r0();
        emit Close();
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementInvestTotal_r16(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateBalanceOfOnIncrementRefundTotal_r16(address p,int r) private    {
      balanceOf[p].n -= r;
  }
  function updateBeneficiaryOnInsertConstructor_r6(address p) private    {
      beneficiary = BeneficiaryTuple(p,true);
  }
  function updateTotalBalanceOnInsertConstructor_r27() private    {
      // Empty()
  }
}