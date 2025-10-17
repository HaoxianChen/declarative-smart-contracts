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
    updateOwnerOnInsertConstructor_r14();
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r18 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r18(p,s,n);
      if(r18==false) {
        revert("Rule condition failed");
      }
  }
  function redeem(address p,int amount) public    {
      bool r16 = updateRedeemOnInsertRecv_redeem_r16(p,amount);
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function issue(address p,int amount) public    {
      bool r20 = updateIssueOnInsertRecv_issue_r20(p,amount);
      if(r20==false) {
        revert("Rule condition failed");
      }
  }
  function getIsBlackListed(address p) public view  returns (bool) {
      bool b = isBlackListed[p].b;
      return b;
  }
  function transfer(address from,address to,int amount) public    {
      bool r27 = updateTransferOnInsertRecv_transfer_r27(from,to,amount);
      if(r27==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r26 = updateTransferFromOnInsertRecv_transferFrom_r26(from,to,spender,amount);
      if(r26==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function addBlackList(address p) public    {
      bool r1 = updateAddBlackListOnInsertRecv_addBlackList_r1(p);
      if(r1==false) {
        revert("Rule condition failed");
      }
  }
  function updateBalanceOfOnIncrementTotalOut_r3(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateRedeemOnInsertRecv_redeem_r16(address p,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(n<=balanceOf_x1) {
        updateAllRedeemOnInsertRedeem_r13(n);
        updateTotalRedeemOnInsertRedeem_r0(p,n);
        emit Redeem(p,n);
        return true;
      }
      return false;
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
  function updateTotalRedeemOnInsertRedeem_r0(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalRedeem_r3(p,delta0);
  }
  function updateAllowanceOnIncrementSpentTotal_r23(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTotalSupplyOnIncrementAllIssue_r12(int m) private    {
      totalSupply.n += m;
  }
  function updateTotalOutOnInsertTransfer_r25(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r3(p,delta0);
  }
  function updateAllIssueOnInsertIssue_r2(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllIssue_r12(delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTransferOnInsertRecv_transfer_r27(address s,address r,int n) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[s].n;
      if(n>=0 && n<=balanceOf_x1_1) {
        updateTotalInOnInsertTransfer_r11(r,n);
        updateTotalOutOnInsertTransfer_r25(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateSpentTotalOnInsertTransferFrom_r21(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r23(o,s,delta0);
  }
  function updateIsBlackListedOnInsertAddBlackList_r4(address p) private    {
      isBlackListed[p] = IsBlackListedTuple(true,true);
  }
  function updateTotalSupplyOnIncrementAllRedeem_r12(int b) private    {
      totalSupply.n -= b;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r7(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r23(o,s,delta0);
  }
  function updateTransferOnInsertTransferFrom_r6(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r11(r,n);
      updateTotalOutOnInsertTransfer_r25(o,n);
      emit Transfer(o,r,n);
  }
  function updateIssueOnInsertRecv_issue_r20(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>0) {
        updateTotalIssueOnInsertIssue_r17(p,n);
        updateAllIssueOnInsertIssue_r2(n);
        emit Issue(p,n);
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
  function updateBalanceOfOnIncrementTotalIssue_r3(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateBalanceOfOnIncrementTotalIn_r3(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateOwnerOnInsertConstructor_r14() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTotalIssueOnInsertIssue_r17(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIssue_r3(p,delta0);
  }
  function updateAllRedeemOnInsertRedeem_r13(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllRedeem_r12(delta0);
  }
  function updateTotalInOnInsertTransfer_r11(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r3(p,delta0);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r23(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r26(address o,address r,address s,int n) private   returns (bool) {
      int balanceOf_x1_2 = balanceOf[o].n;
      int allowance_x2_1 = allowance[o][s].n;
      if(n>0 && n<allowance_x2_1 && n<=balanceOf_x1_2) {
        updateTransferOnInsertTransferFrom_r6(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r21(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalRedeem_r3(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r18(address o,address s,int d) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r7(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
}