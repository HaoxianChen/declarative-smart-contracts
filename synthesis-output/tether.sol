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
  function issue(address p,int amount) public    {
      bool r21 = updateIssueOnInsertRecv_issue_r21(p,amount);
      if(r21==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function redeem(address p,int amount) public    {
      bool r24 = updateRedeemOnInsertRecv_redeem_r24(p,amount);
      if(r24==false) {
        revert("Rule condition failed");
      }
  }
  function getIsBlackListed(address p) public view  returns (bool) {
      bool b = isBlackListed[p].b;
      return b;
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r16 = updateTransferFromOnInsertRecv_transferFrom_r16(from,to,spender,amount);
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r19 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r19(p,s,n);
      if(r19==false) {
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
  function transfer(address from,address to,int amount) public    {
      bool r4 = updateTransferOnInsertRecv_transfer_r4(from,to,amount);
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function updateTotalRedeemOnInsertRedeem_r0(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalRedeem_r3(p,delta0);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r25(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateTotalSupplyOnIncrementAllRedeem_r12(int b) private    {
      totalSupply.n -= b;
  }
  function updateTotalOutOnInsertTransfer_r27(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r3(p,delta0);
  }
  function updateRedeemOnInsertRecv_redeem_r24(address p,int amount) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(balanceOf_x1>0) {
        updateAllRedeemOnInsertRedeem_r13(n);
        updateTotalRedeemOnInsertRedeem_r0(p,n);
        emit Redeem(p,n);
        return true;
      }
      return false;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateAddBlackListOnInsertRecv_addBlackList_r1(address p) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        updateIsBlackListedOnInsertAddBlackList_r17(p);
        emit AddBlackList(p);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalRedeem_r3(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateBalanceOfOnIncrementTotalOut_r3(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateIsBlackListedOnInsertAddBlackList_r17(address p) private    {
      isBlackListed[p] = IsBlackListedTuple(true,true);
  }
  function updateTransferOnInsertTransferFrom_r6(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r11(r,n);
      updateTotalOutOnInsertTransfer_r27(o,n);
      emit Transfer(o,r,n);
  }
  function updateTotalSupplyOnIncrementAllIssue_r12(int m) private    {
      totalSupply.n += m;
  }
  function updateSpentTotalOnInsertTransferFrom_r22(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r25(o,s,delta0);
  }
  function updateAllowanceOnIncrementSpentTotal_r25(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r16(address from,address to,address spender,int amount) private   returns (bool) {
      int balanceOf_x1 = balanceOf[o].n;
      if(balanceOf_x1>0) {
        updateTransferOnInsertTransferFrom_r6(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r22(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateAllIssueOnInsertIssue_r2(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllIssue_r12(delta0);
  }
  function updateTransferOnInsertRecv_transfer_r4(address from,address to,int amount) private   returns (bool) {
      int balanceOf_x1 = balanceOf[r].n;
      if(0!=balanceOf_x1) {
        updateTotalInOnInsertTransfer_r11(r,n);
        updateTotalOutOnInsertTransfer_r27(s,n);
        emit Transfer(s,r,n);
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
  function updateIssueOnInsertRecv_issue_r21(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>0) {
        updateTotalIssueOnInsertIssue_r18(p,n);
        updateAllIssueOnInsertIssue_r2(n);
        emit Issue(p,n);
        return true;
      }
      return false;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r19(address p,address s,int n) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r7(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateTotalIssueOnInsertIssue_r18(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIssue_r3(p,delta0);
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
  function updateOwnerOnInsertConstructor_r14() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateAllRedeemOnInsertRedeem_r13(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllRedeem_r12(delta0);
  }
  function updateTotalInOnInsertTransfer_r11(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r3(p,delta0);
  }
}