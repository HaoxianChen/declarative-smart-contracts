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
  event UnauthorizedDestroyBlackFund();
  event Transfer(address from,address to,int amount);
  event TransferFrom(address from,address to,address spender,int amount);
  event AddBlackList(address p);
  event Issue(address p,int amount);
  event UnauthorizedRedeem();
  event UnauthorizedAddBlackList();
  event IncreaseAllowance(address p,address s,int n);
  event DestroyBlackFund(address p,int n);
  event Redeem(address p,int amount);
  event UnauthorizedIssue();
  constructor(int n) public {
    updateOnceBlacklistedTransferOnInsertConstructor_r20();
    updateOwnerOnInsertConstructor_r18();
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function redeem(address p,int amount) public    {
      bool r27 = updateRedeemOnInsertRecv_redeem_r27(p,amount);
      if(r27==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r24 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r24(p,s,n);
      if(r24==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r6 = updateTransferOnInsertRecv_transfer_r6(from,to,amount);
      if(r6==false) {
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
  function issue(address p,int amount) public    {
      bool r25 = updateIssueOnInsertRecv_issue_r25(p,amount);
      if(r25==false) {
        revert("Rule condition failed");
      }
  }
  function destroyBlackFund(address p,int n) public    {
      bool r4 = updateDestroyBlackFundOnInsertRecv_destroyBlackFund_r4(p,n);
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function getIsBlackListed(address p) public view  returns (bool) {
      bool b = isBlackListed[p].b;
      return b;
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r9 = updateTransferFromOnInsertRecv_transferFrom_r9(from,to,spender,amount);
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
  function updateIsBlackListedOnInsertAddBlackList_r22(address p) private    {
      isBlackListed[p] = IsBlackListedTuple(true,true);
  }
  function updateTotalRedeemOnInsertRedeem_r15(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalRedeem_r3(p,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTransferOnInsertTransferFrom_r7(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r11(r,n);
      updateTotalOutOnInsertTransfer_r33(o,n);
      emit Transfer(o,r,n);
  }
  function updateOnceBlacklistedTransferOnInsertConstructor_r20() private    {
      // Empty()
  }
  function updateAllIssueOnInsertIssue_r2(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllIssue_r16(delta0);
  }
  function updateTotalSupplyOnIncrementAllRedeem_r16(int b) private    {
      totalSupply.n -= b;
  }
  function updateTotalOutOnInsertTransfer_r33(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r3(p,delta0);
  }
  function updateOwnerOnInsertConstructor_r18() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateRedeemOnInsertDestroyBlackFund_r30(address p,int n) private    {
      updateAllRedeemOnInsertRedeem_r17(n);
      updateTotalRedeemOnInsertRedeem_r15(p,n);
      emit Redeem(p,n);
  }
  function updateDestroyBlackFundOnInsertRecv_destroyBlackFund_r4(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(s==o && n<=balanceOf_x1) {
        updateRedeemOnInsertDestroyBlackFund_r30(p,n);
        emit DestroyBlackFund(p,n);
        return true;
      }
      return false;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r26(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r31(o,s,delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r9(address from,address to,address spender,int amount) private   returns (bool) {
      address msgSender = msg.sender;
      bool isBlackListed_x1_3 = isBlackListed[msgSender].b;
      int allowance_x2_1 = allowance[spender][to].n;
      int balanceOf_x1_2 = balanceOf[from].n;
      if(amount>0 && amount<allowance_x2_1 && amount<=balanceOf_x1_2 && isBlackListed_x1_3==false) {
        updateSpentTotalOnInsertTransferFrom_r29(from,spender,amount);
        updateTransferOnInsertTransferFrom_r7(from,to,amount);
        emit TransferFrom(from,to,spender,amount);
        return true;
      }
      return false;
  }
  function updateTotalIssueOnInsertIssue_r23(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIssue_r3(p,delta0);
  }
  function updateSpentTotalOnInsertTransferFrom_r29(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r31(o,s,delta0);
  }
  function updateBalanceOfOnIncrementTotalIssue_r3(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r24(address p,address s,int n) private   returns (bool) {
      if(n>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r26(p,s,n);
        emit IncreaseAllowance(p,s,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalIn_r3(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateAllRedeemOnInsertRedeem_r17(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllRedeem_r16(delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateAllowanceOnIncrementSpentTotal_r31(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r31(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateTransferOnInsertRecv_transfer_r6(address from,address to,int amount) private   returns (bool) {
      bool isBlackListed_x1 = isBlackListed[from].b;
      if(isBlackListed_x1==false) {
        updateTotalOutOnInsertTransfer_r33(from,amount);
        updateTotalInOnInsertTransfer_r11(to,amount);
        emit Transfer(from,to,amount);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllIssue_r16(int m) private    {
      totalSupply.n += m;
  }
  function updateTotalInOnInsertTransfer_r11(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r3(p,delta0);
  }
  function updateAddBlackListOnInsertRecv_addBlackList_r1(address p) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        updateIsBlackListedOnInsertAddBlackList_r22(p);
        emit AddBlackList(p);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalRedeem_r3(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateIssueOnInsertRecv_issue_r25(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(s==o && amount>0) {
        updateTotalIssueOnInsertIssue_r23(p,amount);
        updateAllIssueOnInsertIssue_r2(amount);
        emit Issue(p,amount);
        return true;
      }
      return false;
  }
  function updateRedeemOnInsertRecv_redeem_r27(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(s==o && amount<=balanceOf_x1) {
        updateTotalRedeemOnInsertRedeem_r15(p,amount);
        updateAllRedeemOnInsertRedeem_r17(amount);
        emit Redeem(p,amount);
        return true;
      }
      return false;
  }
}