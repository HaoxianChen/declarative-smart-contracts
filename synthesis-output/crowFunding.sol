contract CrowFunding {
  struct OwnerTuple {
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
    updateRaisedOnInsertConstructor_r15();
    updateBeneficiaryOnInsertConstructor_r5(b);
    updateTargetOnInsertConstructor_r22(t);
    updateOwnerOnInsertConstructor_r19();
    updateOnceRefundOnInsertConstructor_r9();
    updateTotalBalanceOnInsertConstructor_r25();
    updateOnceWithdrawOnInsertConstructor_r20();
  }
  function refund(address p,int n) public    {
      bool r4 = updateRefundOnInsertRecv_refund_r4(p,n);
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function withdraw(address p,int n) public    {
      bool r3 = updateWithdrawOnInsertRecv_withdraw_r3(p,n);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function getRaised() public view  returns (int) {
      int n = raised.n;
      return n;
  }
  function close() public    {
      bool r7 = updateCloseOnInsertRecv_close_r7();
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function getTarget() public view  returns (int) {
      int t = target.t;
      return t;
  }
  function invest(address p,int n) public    {
      bool r16 = updateInvestOnInsertRecv_invest_r16(p,n);
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function getClosed() public view  returns (bool) {
      bool b = closed.b;
      return b;
  }
  function updateRaisedOnInsertConstructor_r15() private    {
      raised = RaisedTuple(0,true);
  }
  function updateWithdrawOnInsertRecv_withdraw_r3(address p,int r) private   returns (bool) {
      int t = target.t;
      if(r==raised.n) {
        if(r>=t) {
          emit Withdraw(p,r);
          return true;
        }
      }
      return false;
  }
  function updateTargetOnInsertConstructor_r22(int t) private    {
      target = TargetTuple(t,true);
  }
  function updateRaisedOnInsertInvest_r14(int m) private    {
      raised.n += m;
  }
  function updateBalanceOfOnIncrementRefundTotal_r12(address p,int r) private    {
      balanceOf[p].n -= r;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateRefundTotalOnInsertRefund_r11(address p,int m) private    {
      int delta0 = int(m);
      updateBalanceOfOnIncrementRefundTotal_r12(p,delta0);
  }
  function updateOwnerOnInsertConstructor_r19() private    {
      address p = msg.sender;
      owner = OwnerTuple(p,true);
  }
  function updateRefundOnInsertRecv_refund_r4(address p,int n) private   returns (bool) {
      int t_1 = target.t;
      int r_1 = raised.n;
      bool b_0 = closed.b;
      int balanceOf_x1 = balanceOf[p].n;
      if(b_0!=false && r_1<t_1 && n<balanceOf_x1) {
        updateRefundTotalOnInsertRefund_r11(p,n);
        emit Refund(p,n);
        return true;
      }
      return false;
  }
  function updateBeneficiaryOnInsertConstructor_r5(address p) private    {
      // Empty()
  }
  function updateTotalBalanceOnInsertConstructor_r25() private    {
      // Empty()
  }
  function updateOnceRefundOnInsertConstructor_r9() private    {
      // Empty()
  }
  function updateClosedOnInsertClose_r0() private    {
      closed = ClosedTuple(true,true);
  }
  function updateInvestTotalOnInsertInvest_r24(address p,int m) private    {
      int delta0 = int(m);
      updateBalanceOfOnIncrementInvestTotal_r12(p,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateInvestOnInsertRecv_invest_r16(address p,int n) private   returns (bool) {
      bool closed_b_1 = closed.b;
      if(n>0 && closed_b_1==false) {
        updateRaisedOnInsertInvest_r14(n);
        updateInvestTotalOnInsertInvest_r24(p,n);
        emit Invest(p,n);
        return true;
      }
      return false;
  }
  function updateOnceWithdrawOnInsertConstructor_r20() private    {
      // Empty()
  }
  function updateBalanceOfOnIncrementInvestTotal_r12(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateCloseOnInsertRecv_close_r7() private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        updateClosedOnInsertClose_r0();
        emit Close();
        return true;
      }
      return false;
  }
}