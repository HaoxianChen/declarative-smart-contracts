contract Tether {
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct TotalSupplyTuple {
    uint n;
    bool _valid;
  }
  struct BalanceOfTuple {
    uint n;
    bool _valid;
  }
  struct AllowanceTuple {
    uint n;
    bool _valid;
  }
  TotalSupplyTuple totalSupply;
  mapping(address=>BalanceOfTuple) balanceOf;
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  OwnerTuple owner;
  event Issue(address p,uint amount);
  event AddBlackList(address p);
  event TransferFromWithFee(address from,address to,address spender,uint fee,uint amount);
  event Redeem(address p,uint amount);
  event IncreaseAllowance(address p,address s,uint n);
  event TransferWithFee(address from,address to,uint fee,uint amount);
  event DestroyBlackFund(address p,uint n);
  constructor(uint n) public {
    updateTotalSupplyOnInsertConstructor_r24(n);
    updateTotalBalancesOnInsertConstructor_r29(n);
    updateOwnerOnInsertConstructor_r26();
    updateBalanceOfOnInsertConstructor_r7(n);
  }
  function approve(address s,uint n) public    {
      bool r6 = updateIncreaseAllowanceOnInsertRecv_approve_r6(s,n);
      if(r6==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (uint) {
      uint n = totalSupply.n;
      return n;
  }
  function issue(address p,uint amount) public    {
      bool r3 = updateIssueOnInsertRecv_issue_r3(p,amount);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (uint) {
      uint n = allowance[p][s].n;
      return n;
  }
  function getBalanceOf(address p) public view  returns (uint) {
      uint n = balanceOf[p].n;
      return n;
  }
  function transferFrom(address from,address to,uint amount) public    {
      bool r5 = updateTransferFromWithFeeOnInsertRecv_transferFrom_r5(from,to,amount);
      if(r5==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address to,uint amount) public    {
      bool r12 = updateTransferWithFeeOnInsertRecv_transfer_r12(to,amount);
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function redeem(address p,uint amount) public    {
      bool r13 = updateRedeemOnInsertRecv_redeem_r13(p,amount);
      if(r13==false) {
        revert("Rule condition failed");
      }
  }
  function updateTotalOutOnInsertTransfer_r21(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r16(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalRedeem_r16(address p,int m) private    {
      int _delta = int(-m);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateTransferFromWithFeeOnInsertRecv_transferFrom_r5(address o,address r,uint n) private   returns (bool) {
      updateTransferFromOnInsertTransferFromWithFee_r20(o,r,s,f,n);
      updateTransferFromOnInsertTransferFromWithFee_r9(o,r,s,f);
      emit TransferFromWithFee(o,r,s,f,n);
      return true;
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r16(address p,int o) private    {
      int _delta = int(-o);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateTransferFromOnInsertTransferFromWithFee_r20(address o,address r,address s,uint f,uint n) private    {
      uint m = n-f;
      updateTransferOnInsertTransferFrom_r0(o,r,m);
      updateSpentTotalOnInsertTransferFrom_r23(o,s,m);
  }
  function updateTotalInOnInsertTransfer_r28(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r16(p,delta0);
  }
  function updateTotalSupplyOnIncrementAllIssue_r14(int m) private    {
      int _delta = int(m);
      uint x_totalSupply__n = totalSupply.n;
      uint newValue = updateuintByint(x_totalSupply__n,_delta);
      totalSupply.n = newValue;
  }
  function updateTransferOnInsertTransferWithFee_r19(address s,address r,uint f,uint n) private    {
      uint m = n-f;
      updateTotalInOnInsertTransfer_r28(r,m);
      updateTotalOutOnInsertTransfer_r21(s,m);
  }
  function updateTotalRedeemOnInsertRedeem_r17(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalRedeem_r16(p,delta0);
  }
  function updateAllRedeemOnInsertRedeem_r1(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllRedeem_r14(delta0);
  }
  function updateAllIssueOnInsertIssue_r27(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllIssue_r14(delta0);
  }
  function updateTransferWithFeeOnInsertRecv_transfer_r12(address r,uint n) private   returns (bool) {
      updateTransferOnInsertTransferWithFee_r19(s,r,f,n);
      updateTransferOnInsertTransferWithFee_r2(s,r,f);
      emit TransferWithFee(s,r,f,n);
      return true;
      return false;
  }
  function updateIncreaseAllowanceOnInsertRecv_approve_r6(address s,uint n) private   returns (bool) {
      updateAllowanceTotalOnInsertIncreaseAllowance_r30(o,s,d);
      emit IncreaseAllowance(o,s,d);
      return true;
      return false;
  }
  function updateTotalSupplyOnIncrementAllRedeem_r14(int b) private    {
      int _delta = int(-b);
      uint x_totalSupply__n = totalSupply.n;
      uint newValue = updateuintByint(x_totalSupply__n,_delta);
      totalSupply.n = newValue;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r30(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r10(o,s,delta0);
  }
  function updateAllowanceOnIncrementSpentTotal_r10(address o,address s,int l) private    {
      int _delta = int(-l);
      uint x_allowance_o_s_n = allowance[o][s].n;
      uint newValue = updateuintByint(x_allowance_o_s_n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateSpentTotalOnInsertTransferFrom_r23(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r10(o,s,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalBalancesOnInsertConstructor_r29(uint n) private    {
      // Empty()
  }
  function updateBalanceOfOnIncrementTotalIssue_r16(address p,int n) private    {
      int _delta = int(n);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateTotalIssueOnInsertIssue_r25(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIssue_r16(p,delta0);
  }
  function updateTransferFromOnInsertTransferFromWithFee_r9(address o,address r,address s,uint f) private    {
      address p = owner.p;
      updateSpentTotalOnInsertTransferFrom_r23(o,s,f);
      updateTransferOnInsertTransferFrom_r0(o,p,f);
  }
  function updateIssueOnInsertRecv_issue_r3(address p,uint n) private   returns (bool) {
      updateTotalIssueOnInsertIssue_r25(p,n);
      updateAllIssueOnInsertIssue_r27(n);
      emit Issue(p,n);
      return true;
      return false;
  }
  function updateRedeemOnInsertRecv_redeem_r13(address p,uint n) private   returns (bool) {
      updateAllRedeemOnInsertRedeem_r1(n);
      updateTotalRedeemOnInsertRedeem_r17(p,n);
      emit Redeem(p,n);
      return true;
      return false;
  }
  function updateTransferOnInsertTransferWithFee_r2(address s,address r,uint f) private    {
      address o = owner.p;
      updateTotalOutOnInsertTransfer_r21(s,f);
      updateTotalInOnInsertTransfer_r28(o,f);
  }
  function updateOwnerOnInsertConstructor_r26() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTransferOnInsertTransferFrom_r0(address o,address r,uint n) private    {
      updateTotalInOnInsertTransfer_r28(r,n);
      updateTotalOutOnInsertTransfer_r21(o,n);
  }
  function updateBalanceOfOnIncrementTotalIn_r16(address p,int i) private    {
      int _delta = int(i);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateBalanceOfOnInsertConstructor_r7(uint n) private    {
      address s = msg.sender;
      balanceOf[s] = BalanceOfTuple(n,true);
  }
  function updateTotalSupplyOnInsertConstructor_r24(uint n) private    {
      totalSupply = TotalSupplyTuple(n,true);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r10(address o,address s,int m) private    {
      int _delta = int(m);
      uint x_allowance_o_s_n = allowance[o][s].n;
      uint newValue = updateuintByint(x_allowance_o_s_n,_delta);
      allowance[o][s].n = newValue;
  }
}