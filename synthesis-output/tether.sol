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
    updateOwnerOnInsertConstructor_r21();
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
  function redeem(address p,int amount) public    {
      bool r27 = updateRedeemOnInsertRecv_redeem_r27(p,amount);
      if(r27==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r22 = updateTransferFromOnInsertRecv_transferFrom_r22(from,to,spender,amount);
      if(r22==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r13 = updateTransferOnInsertRecv_transfer_r13(from,to,amount);
      if(r13==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r26 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r26(p,s,n);
      if(r26==false) {
        revert("Rule condition failed");
      }
  }
  function issue(address p,int amount) public    {
      bool r30 = updateIssueOnInsertRecv_issue_r30(p,amount);
      if(r30==false) {
        revert("Rule condition failed");
      }
  }
  function destroyBlackFund(address p,int n) public    {
      bool r8 = updateDestroyBlackFundOnInsertRecv_destroyBlackFund_r8(p,n);
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function updateOwnerOnInsertConstructor_r21() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateBalanceOfOnIncrementTotalOut_r2(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateTotalOutOnInsertTransfer_r35(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r2(p,delta0);
  }
  function updateTotalRedeemOnInsertRedeem_r16(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalRedeem_r2(p,delta0);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r26(address p,address s,int n) private   returns (bool) {
      if(n>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r7(p,s,n);
        emit IncreaseAllowance(p,s,n);
        return true;
      }
      return false;
  }
  function updateSpentTotalOnInsertTransferFrom_r31(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r33(o,s,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateAllowanceOnIncrementSpentTotal_r33(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r22(address from,address to,address spender,int amount) private   returns (bool) {
      int m_1 = balanceOf[from].n;
      if(amount>0 && amount<=m_1) {
        updateTransferOnInsertTransferFrom_r6(from,to,amount);
        updateSpentTotalOnInsertTransferFrom_r31(from,spender,amount);
        emit TransferFrom(from,to,spender,amount);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertTransferFrom_r6(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r14(r,n);
      updateTotalOutOnInsertTransfer_r35(o,n);
      emit Transfer(o,r,n);
  }
  function updateBalanceOfOnIncrementTotalRedeem_r2(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateAllIssueOnInsertIssue_r1(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllIssue_r17(delta0);
  }
  function updateIssueOnInsertRecv_issue_r30(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && amount>0) {
        updateAllIssueOnInsertIssue_r1(amount);
        updateTotalIssueOnInsertIssue_r25(p,amount);
        emit Issue(p,amount);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllIssue_r17(int m) private    {
      totalSupply.n += m;
  }
  function updateRedeemOnInsertRecv_redeem_r27(address p,int amount) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(amount<balanceOf_x1) {
        updateAllRedeemOnInsertRedeem_r20(amount);
        updateTotalRedeemOnInsertRedeem_r16(p,amount);
        emit Redeem(p,amount);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllRedeem_r17(int b) private    {
      totalSupply.n -= b;
  }
  function updateBalanceOfOnIncrementTotalIn_r2(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTransferOnInsertRecv_transfer_r13(address from,address to,int amount) private   returns (bool) {
      int m_1 = balanceOf[from].n;
      if(to!=address(0) && amount<=m_1 && from!=address(0) && amount>0) {
        int fee_2 = computeFee(amount);
        if(fee_2<amount) {
          updateTotalInOnInsertTransfer_r14(to,amount);
          updateTotalOutOnInsertTransfer_r35(from,amount);
          emit Transfer(from,to,amount);
          return true;
        }
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalIssue_r2(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r33(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateAllRedeemOnInsertRedeem_r20(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllRedeem_r17(delta0);
  }
  function updateRedeemOnInsertDestroyBlackFund_r32(address p,int n) private    {
      updateTotalRedeemOnInsertRedeem_r16(p,n);
      updateAllRedeemOnInsertRedeem_r20(n);
      emit Redeem(p,n);
  }
  function updateIsBlackListedOnInsertAddBlackList_r4(address p) private    {
      isBlackListed[p] = IsBlackListedTuple(true,true);
  }
  function updateTotalInOnInsertTransfer_r14(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r2(p,delta0);
  }
  function updateTotalIssueOnInsertIssue_r25(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIssue_r2(p,delta0);
  }
  function updateDestroyBlackFundOnInsertRecv_destroyBlackFund_r8(address p,int n) private   returns (bool) {
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
  function updateAddBlackListOnInsertRecv_addBlackList_r0(address p) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        updateIsBlackListedOnInsertAddBlackList_r4(p);
        emit AddBlackList(p);
        return true;
      }
      return false;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r7(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r33(o,s,delta0);
  }
}