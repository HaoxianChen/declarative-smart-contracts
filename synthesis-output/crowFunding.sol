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
    updateOnceRefundOnInsertConstructor_r12();
    updateOnceWithdrawOnInsertConstructor_r23();
    updateBeneficiaryOnInsertConstructor_r7(b);
    updateRaisedOnInsertConstructor_r3();
    updateTotalBalanceOnInsertConstructor_r28();
    updateOwnerOnInsertConstructor_r22();
    updateTargetOnInsertConstructor_r25(t);
  }
  function getClosed() public view  returns (bool) {
      bool b = closed.b;
      return b;
  }
  function withdraw(address p,int n) public    {
      bool r6 = updateWithdrawOnInsertRecv_withdraw_r6(p,n);
      if(r6==false) {
        revert("Rule condition failed");
      }
  }
  function invest(address p,int n) public    {
      bool r9 = updateInvestOnInsertRecv_invest_r9(p,n);
      if(r9==false) {
        revert("Rule condition failed");
      }
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
  function getRaised() public view  returns (int) {
      int n = raised.n;
      return n;
  }
  function getTarget() public view  returns (int) {
      int t = target.t;
      return t;
  }
  function close() public    {
      bool r10 = updateCloseOnInsertRecv_close_r10();
      if(r10==false) {
        revert("Rule condition failed");
      }
  }
  function updateOnceWithdrawOnInsertConstructor_r23() private    {
      // Empty()
  }
  function updateClosedOnInsertClose_r0() private    {
      closed = ClosedTuple(true,true);
  }
  function updateRefundTotalOnInsertRefund_r15(address p,int m) private    {
      int delta0 = int(m);
      updateBalanceOfOnIncrementRefundTotal_r17(p,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateCloseOnInsertRecv_close_r10() private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        updateClosedOnInsertClose_r0();
        emit Close();
        return true;
      }
      return false;
  }
  function updateBeneficiaryOnInsertConstructor_r7(address p) private    {
      beneficiary = BeneficiaryTuple(p,true);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateRaisedOnInsertInvest_r19(int m) private    {
      raised.n += m;
  }
  function updateSendOnInsertRefund_r16(address p,int n) private    {
      payable(p).send(n);
  }
  function updateBalanceOfOnIncrementInvestTotal_r17(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateOnceRefundOnInsertConstructor_r12() private    {
      // Empty()
  }
  function updateTargetOnInsertConstructor_r25(int t) private    {
      target = TargetTuple(t,true);
  }
  function updateWithdrawOnInsertRecv_withdraw_r6(address p,int r) private   returns (bool) {
      int t_1 = target.t;
      int r_1 = raised.n;
      address b_0 = beneficiary.p;
      if(p==b_0 && r_1>=t_1) {
        updateSendOnInsertWithdraw_r14(p,r);
        emit Withdraw(p,r);
        return true;
      }
      return false;
  }
  function updateRefundOnInsertRecv_refund_r5(address p,int n) private   returns (bool) {
      int t_1 = target.t;
      int r_1 = raised.n;
      bool b_0 = closed.b;
      int balanceOf_x1 = balanceOf[p].n;
      if(b_0!=false && r_1<t_1 && n<balanceOf_x1) {
        updateSendOnInsertRefund_r16(p,n);
        updateRefundTotalOnInsertRefund_r15(p,n);
        emit Refund(p,n);
        return true;
      }
      return false;
  }
  function updateRaisedOnInsertConstructor_r3() private    {
      raised = RaisedTuple(0,true);
  }
  function updateTotalBalanceOnInsertConstructor_r28() private    {
      // Empty()
  }
  function updateBalanceOfOnIncrementRefundTotal_r17(address p,int r) private    {
      balanceOf[p].n -= r;
  }
  function updateOwnerOnInsertConstructor_r22() private    {
      address p = msg.sender;
      owner = OwnerTuple(p,true);
  }
  function updateSendOnInsertWithdraw_r14(address p,int r) private    {
      payable(p).send(r);
  }
  function updateInvestOnInsertRecv_invest_r9(address p,int n) private   returns (bool) {
      bool closed_b_1 = closed.b;
      if(n>=0 && closed_b_1==false) {
        updateInvestTotalOnInsertInvest_r27(p,n);
        updateRaisedOnInsertInvest_r19(n);
        emit Invest(p,n);
        return true;
      }
      return false;
  }
  function updateInvestTotalOnInsertInvest_r27(address p,int m) private    {
      int delta0 = int(m);
      updateBalanceOfOnIncrementInvestTotal_r17(p,delta0);
  }
}