contract Tether {
  struct IsBlackListedTuple {
    bool b;
    bool _valid;
  }
  struct TotalSupplyTuple {
    int n;
    bool _valid;
  }
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct AllowanceTuple {
    int n;
    bool _valid;
  }
  struct BalanceOfTuple {
    int n;
    bool _valid;
  }
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  mapping(address=>IsBlackListedTuple) isBlackListed;
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalSupplyTuple totalSupply;
  OwnerTuple owner;
  event Redeem(address p,int amount);
  event TransferFrom(address from,address to,address spender,int amount);
  event AddBlackList(address p);
  event Issue(address p,int amount);
  event IncreaseAllowance(address p,address s,int n);
  event DestroyBlackFund(address p,int n);
  event Transfer(address from,address to,int amount);
  event InvalidTx();
  constructor(int n) public {
    updateOwnerOnInsertConstructor_r16();
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r8 = updateTransferFromOnInsertRecv_transferFrom_r8(from,to,spender,amount);
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function getIsBlackListed(address p) public view  returns (bool) {
      bool b = isBlackListed[p].b;
      return b;
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r20 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r20(p,s,n);
      if(r20==false) {
        revert("Rule condition failed");
      }
  }
  function redeem(address p,int amount) public    {
      bool r18 = updateRedeemOnInsertRecv_redeem_r18(p,amount);
      if(r18==false) {
        revert("Rule condition failed");
      }
  }
  function issue(address p,int amount) public    {
      bool r22 = updateIssueOnInsertRecv_issue_r22(p,amount);
      if(r22==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function addBlackList(address p) public    {
      bool r1 = updateAddBlackListOnInsertRecv_addBlackList_r1(p);
      if(r1==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r9 = updateTransferOnInsertRecv_transfer_r9(from,to,amount);
      if(r9==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function updateBalanceOfOnIncrementTotalOut_r3(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateAllRedeemOnInsertRedeem_r15(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllRedeem_r14(delta0);
  }
  function updateOwnerOnInsertConstructor_r16() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateAddBlackListOnInsertRecv_addBlackList_r1(address p) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        updateIsBlackListedOnInsertAddBlackList_r4(p);
        emit AddBlackList(p);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r25(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateIssueOnInsertRecv_issue_r22(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>0) {
        updateTotalIssueOnInsertIssue_r19(p,n);
        updateAllIssueOnInsertIssue_r2(n);
        emit Issue(p,n);
        return true;
      }
      return false;
  }
  function updateRedeemOnInsertRecv_redeem_r18(address p,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(n<=balanceOf_x1) {
        updateTotalRedeemOnInsertRedeem_r0(p,n);
        updateAllRedeemOnInsertRedeem_r15(n);
        emit Redeem(p,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementSpentTotal_r25(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalOutOnInsertTransfer_r27(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r3(p,delta0);
  }
  function updateTotalIssueOnInsertIssue_r19(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIssue_r3(p,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTotalInOnInsertTransfer_r13(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r3(p,delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r8(address o,address r,address s,int n) private   returns (bool) {
      int balanceOf_x1_2 = balanceOf[o].n;
      int allowance_x2_1 = allowance[o][s].n;
      if(n>=0 && n<=allowance_x2_1 && n<=balanceOf_x1_2) {
        updateTransferOnInsertTransferFrom_r6(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r23(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateAllIssueOnInsertIssue_r2(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllIssue_r14(delta0);
  }
  function updateIsBlackListedOnInsertAddBlackList_r4(address p) private    {
      isBlackListed[p] = IsBlackListedTuple(true,true);
  }
  function updateSpentTotalOnInsertTransferFrom_r23(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r25(o,s,delta0);
  }
  function updateTotalSupplyOnIncrementAllIssue_r14(int m) private    {
      totalSupply.n += m;
  }
  function updateTotalRedeemOnInsertRedeem_r0(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalRedeem_r3(p,delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r7(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r25(o,s,delta0);
  }
  function updateBalanceOfOnIncrementTotalIssue_r3(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateBalanceOfOnIncrementTotalIn_r3(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTransferOnInsertRecv_transfer_r9(address s,address r,int n) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[s].n;
      if(n>0 && n<=balanceOf_x1_1) {
        updateTotalOutOnInsertTransfer_r27(s,n);
        updateTotalInOnInsertTransfer_r13(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r20(address o,address s,int d) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r7(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllRedeem_r14(int b) private    {
      totalSupply.n -= b;
  }
  function updateBalanceOfOnIncrementTotalRedeem_r3(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateTransferOnInsertTransferFrom_r6(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r27(o,n);
      updateTotalInOnInsertTransfer_r13(r,n);
      emit Transfer(o,r,n);
  }
}