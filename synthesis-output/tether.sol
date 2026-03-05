import "./tether_udf.sol";
contract Tether is TetherUDF {
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
    updateOwnerOnInsertConstructor_r23();
  }
  function redeem(address p,int amount) public    {
      bool r3 = updateRedeemOnInsertRecv_redeem_r3(p,amount);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r28 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r28(p,s,n);
      if(r28==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function addBlackList(address p) public    {
      bool r0 = updateAddBlackListOnInsertRecv_addBlackList_r0(p);
      if(r0==false) {
        revert("Rule condition failed");
      }
  }
  function getIsBlackListed(address p) public view  returns (bool) {
      bool b = isBlackListed[p].b;
      return b;
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r24 = updateTransferFromOnInsertRecv_transferFrom_r24(from,to,spender,amount);
      if(r24==false) {
        revert("Rule condition failed");
      }
  }
  function destroyBlackFund(address p,int n) public    {
      bool r10 = updateDestroyBlackFundOnInsertRecv_destroyBlackFund_r10(p,n);
      if(r10==false) {
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
  function transfer(address from,address to,int amount) public    {
      bool r20 = updateTransferOnInsertRecv_transfer_r20(from,to,amount);
      if(r20==false) {
        revert("Rule condition failed");
      }
  }
  function issue(address p,int amount) public    {
      bool r30 = updateIssueOnInsertRecv_issue_r30(p,amount);
      if(r30==false) {
        revert("Rule condition failed");
      }
  }
  function updateIsBlackListedOnInsertAddBlackList_r5(address p) private    {
      isBlackListed[p] = IsBlackListedTuple(true,true);
  }
  function updateBalanceOfOnIncrementTotalOut_r2(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateAllRedeemOnInsertRedeem_r22(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllRedeem_r18(delta0);
  }
  function updateSpentTotalOnInsertTransferFrom_r31(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r33(o,s,delta0);
  }
  function updateBalanceOfOnIncrementTotalIn_r2(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTotalSupplyOnIncrementAllRedeem_r18(int b) private    {
      totalSupply.n -= b;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateAddBlackListOnInsertRecv_addBlackList_r0(address p) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        updateIsBlackListedOnInsertAddBlackList_r5(p);
        emit AddBlackList(p);
        return true;
      }
      return false;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateIssueOnInsertRecv_issue_r30(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>0) {
        updateTotalIssueOnInsertIssue_r27(p,n);
        updateAllIssueOnInsertIssue_r1(n);
        emit Issue(p,n);
        return true;
      }
      return false;
  }
  function updateRedeemOnInsertDestroyBlackFund_r32(address p,int n) private    {
      updateAllRedeemOnInsertRedeem_r22(n);
      updateTotalRedeemOnInsertRedeem_r17(p,n);
      emit Redeem(p,n);
  }
  function updateBalanceOfOnIncrementTotalRedeem_r2(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r24(address from,address to,address spender,int amount) private   returns (bool) {
      int m_1 = balanceOf[o].n;
      if(n>0 && n<=m_1) {
        updateTransferOnInsertTransferFrom_r7(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r31(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateTotalIssueOnInsertIssue_r27(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIssue_r2(p,delta0);
  }
  function updateTotalRedeemOnInsertRedeem_r17(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalRedeem_r2(p,delta0);
  }
  function updateTotalSupplyOnIncrementAllIssue_r18(int m) private    {
      totalSupply.n += m;
  }
  function updateTotalOutOnInsertTransfer_r35(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r2(p,delta0);
  }
  function updateRedeemOnInsertRecv_redeem_r3(address p,int amount) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(0!=balanceOf_x1) {
        updateAllRedeemOnInsertRedeem_r22(n);
        updateTotalRedeemOnInsertRedeem_r17(p,n);
        emit Redeem(p,n);
        return true;
      }
      return false;
  }
  function updateTotalInOnInsertTransfer_r15(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r2(p,delta0);
  }
  function updateAllIssueOnInsertIssue_r1(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllIssue_r18(delta0);
  }
  function updateTransferOnInsertRecv_transfer_r20(address from,address to,int amount) private   returns (bool) {
      int balanceOf_x1 = balanceOf[s].n;
      int m_1 = balanceOf[s].n;
      int fee_2 = computeFee(n);
      if(r!=address(0) && n<=m_1 && s!=address(0) && fee_2<n && n<=balanceOf_x1 && n>0) {
        updateTotalInOnInsertTransfer_r15(r,n);
        updateTotalOutOnInsertTransfer_r35(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r8(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r33(o,s,delta0);
  }
  function updateAllowanceOnIncrementSpentTotal_r33(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateBalanceOfOnIncrementTotalIssue_r2(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateTransferOnInsertTransferFrom_r7(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r15(r,n);
      updateTotalOutOnInsertTransfer_r35(o,n);
      emit Transfer(o,r,n);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r33(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateOwnerOnInsertConstructor_r23() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r28(address p,address s,int n) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r8(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateDestroyBlackFundOnInsertRecv_destroyBlackFund_r10(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && balanceOf_x1>0) {
        updateRedeemOnInsertDestroyBlackFund_r32(p,n);
        emit DestroyBlackFund(p,n);
        return true;
      }
      return false;
  }
}