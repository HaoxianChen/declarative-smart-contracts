contract Tether {
  struct TotalOutTuple {
    uint n;
    bool _valid;
  }
  struct TotalIssueTuple {
    uint n;
    bool _valid;
  }
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct AllIssueTuple {
    uint n;
    bool _valid;
  }
  struct TotalSupplyTuple {
    uint n;
    bool _valid;
  }
  struct IsBlackListedTuple {
    bool b;
    bool _valid;
  }
  struct AllowanceTotalTuple {
    uint m;
    bool _valid;
  }
  struct TransferTuple {
    address from;
    address to;
    uint amount;
    bool _valid;
  }
  struct MaxFeeTuple {
    uint m;
    bool _valid;
  }
  struct AllRedeemTuple {
    uint n;
    bool _valid;
  }
  struct BalanceOfTuple {
    uint n;
    bool _valid;
  }
  struct TotalInTuple {
    uint n;
    bool _valid;
  }
  struct RateTuple {
    uint r;
    bool _valid;
  }
  struct TransferFromTuple {
    address from;
    address to;
    address spender;
    uint amount;
    bool _valid;
  }
  struct TotalRedeemTuple {
    uint n;
    bool _valid;
  }
  struct SpentTotalTuple {
    uint m;
    bool _valid;
  }
  struct AllowanceTuple {
    uint n;
    bool _valid;
  }
  mapping(address=>TotalInTuple) totalIn;
  mapping(address=>TotalOutTuple) totalOut;
  mapping(address=>TotalIssueTuple) totalIssue;
  OwnerTuple owner;
  AllIssueTuple allIssue;
  RateTuple rate;
  TotalSupplyTuple totalSupply;
  mapping(address=>IsBlackListedTuple) isBlackListed;
  mapping(address=>mapping(address=>AllowanceTotalTuple)) allowanceTotal;
  TransferTuple transfer;
  TransferFromTuple transferFrom;
  MaxFeeTuple maxFee;
  AllRedeemTuple allRedeem;
  mapping(address=>BalanceOfTuple) balanceOf;
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  mapping(address=>TotalRedeemTuple) totalRedeem;
  mapping(address=>mapping(address=>SpentTotalTuple)) spentTotal;
  event Issue(address p,uint amount);
  event AddBlackList(address p);
  event TransferFromWithFee(address from,address to,address spender,uint fee,uint amount);
  event Redeem(address p,uint amount);
  event IncreaseAllowance(address p,address s,uint n);
  event TransferWithFee(address from,address to,uint fee,uint amount);
  event DestroyBlackFund(address p,uint n);
  constructor(uint n) public {
    updateOwnerOnInsertConstructor_r24();
    updateBalanceOfOnInsertConstructor_r8(n);
    updateTotalSupplyOnInsertConstructor_r22(n);
    updateTotalBalancesOnInsertConstructor_r29(n);
  }
  function transfer(address to,uint amount) public    {
      bool r7 = updateTransferWithFeeOnInsertRecv_transfer_r7(to,amount);
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function approve(address s,uint n) public    {
      bool r26 = updateIncreaseAllowanceOnInsertRecv_approve_r26(s,n);
      if(r26==false) {
        revert("Rule condition failed");
      }
  }
  function issue(address p,uint amount) public    {
      bool r11 = updateIssueOnInsertRecv_issue_r11(p,amount);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (uint) {
      uint n = totalSupply.n;
      return n;
  }
  function redeem(address p,uint amount) public    {
      bool r25 = updateRedeemOnInsertRecv_redeem_r25(p,amount);
      if(r25==false) {
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
      bool r0 = updateTransferFromWithFeeOnInsertRecv_transferFrom_r0(from,to,amount);
      if(r0==false) {
        revert("Rule condition failed");
      }
  }
  function updateTransferFromWithFeeOnInsertRecv_transferFrom_r0(address o,address r,uint n) private   returns (bool) {
      uint rt = rate.r;
      uint mf = maxFee.m;
      address s = msg.sender;
      uint m = balanceOf[o].n;
      if(false==isBlackListed[o].b) {
        uint k = allowance[o][s].n;
        if(m>=n && k>=n) {
          uint f = (rt*n)/10000 < mf ? (rt*n)/10000 : mf;
          updateTransferFromOnInsertTransferFromWithFee_r10(o,r,s,f);
          updateTransferFromOnInsertTransferFromWithFee_r18(o,r,s,f,n);
          emit TransferFromWithFee(o,r,s,f,n);
          return true;
        }
      }
      return false;
  }
  function updateBalanceOfOnInsertConstructor_r8(uint n) private    {
      address s = msg.sender;
      balanceOf[s] = BalanceOfTuple(n,true);
  }
  function updateIncreaseAllowanceOnInsertRecv_approve_r26(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      uint m = allowance[o][s].n;
      uint d = n-m;
      updateAllowanceTotalOnInsertIncreaseAllowance_r30(o,s,d);
      emit IncreaseAllowance(o,s,d);
      return true;
      return false;
  }
  function updateTransferOnInsertTransferFrom_r1(address o,address r,uint n) private    {
      updateTotalInOnInsertTransfer_r28(r,n);
      updateTotalOutOnInsertTransfer_r19(o,n);
      transfer = TransferTuple(o,r,n,true);
  }
  function updateTotalBalancesOnInsertConstructor_r29(uint n) private    {
      // Empty()
  }
  function updateAllRedeemOnInsertRedeem_r13(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllRedeem_r2(delta0);
      allRedeem.n += n;
  }
  function updateTotalOutOnInsertTransfer_r19(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r6(p,delta0);
      totalOut[p].n += n;
  }
  function updateTotalSupplyOnIncrementAllIssue_r2(int m) private    {
      int _delta = int(m);
      uint newValue = updateuintByint(totalSupply.n,_delta);
      totalSupply.n = newValue;
  }
  function updateAllIssueOnInsertIssue_r27(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllIssue_r2(delta0);
      allIssue.n += n;
  }
  function updateTransferOnInsertTransferWithFee_r3(address s,address r,uint f) private    {
      address o = owner.p;
      updateTotalInOnInsertTransfer_r28(o,f);
      updateTotalOutOnInsertTransfer_r19(s,f);
      transfer = TransferTuple(s,o,f,true);
  }
  function updateTransferFromOnInsertTransferFromWithFee_r18(address o,address r,address s,uint f,uint n) private    {
      uint m = n-f;
      updateSpentTotalOnInsertTransferFrom_r21(o,s,m);
      updateTransferOnInsertTransferFrom_r1(o,r,m);
      transferFrom = TransferFromTuple(o,r,s,m,true);
  }
  function updateTransferOnInsertTransferWithFee_r17(address s,address r,uint f,uint n) private    {
      uint m = n-f;
      updateTotalInOnInsertTransfer_r28(r,m);
      updateTotalOutOnInsertTransfer_r19(s,m);
      transfer = TransferTuple(s,r,m,true);
  }
  function updateTotalSupplyOnIncrementAllRedeem_r2(int b) private    {
      int _delta = int(-b);
      uint newValue = updateuintByint(totalSupply.n,_delta);
      totalSupply.n = newValue;
  }
  function updateTotalRedeemOnInsertRedeem_r15(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalRedeem_r6(p,delta0);
      totalRedeem[p].n += n;
  }
  function updateTransferFromOnInsertTransferFromWithFee_r10(address o,address r,address s,uint f) private    {
      address p = owner.p;
      updateSpentTotalOnInsertTransferFrom_r21(o,s,f);
      updateTransferOnInsertTransferFrom_r1(o,p,f);
      transferFrom = TransferFromTuple(o,p,s,f,true);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r30(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r12(o,s,delta0);
      allowanceTotal[o][s].m += n;
  }
  function updateTotalIssueOnInsertIssue_r23(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIssue_r6(p,delta0);
      totalIssue[p].n += n;
  }
  function updateBalanceOfOnIncrementTotalRedeem_r6(address p,int m) private    {
      int _delta = int(-m);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateRedeemOnInsertRecv_redeem_r25(address p,uint n) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        uint m = balanceOf[p].n;
        if(p!=address(0) && n<=m) {
          updateTotalRedeemOnInsertRedeem_r15(p,n);
          updateAllRedeemOnInsertRedeem_r13(n);
          emit Redeem(p,n);
          return true;
        }
      }
      return false;
  }
  function updateOwnerOnInsertConstructor_r24() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateSpentTotalOnInsertTransferFrom_r21(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r12(o,s,delta0);
      spentTotal[o][s].m += n;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r12(address o,address s,int m) private    {
      int _delta = int(m);
      uint newValue = updateuintByint(allowance[o][s].n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateTotalInOnInsertTransfer_r28(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r6(p,delta0);
      totalIn[p].n += n;
  }
  function updateIssueOnInsertRecv_issue_r11(address p,uint n) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        if(p!=address(0)) {
          updateTotalIssueOnInsertIssue_r23(p,n);
          updateAllIssueOnInsertIssue_r27(n);
          emit Issue(p,n);
          return true;
        }
      }
      return false;
  }
  function updateAllowanceOnIncrementSpentTotal_r12(address o,address s,int l) private    {
      int _delta = int(-l);
      uint newValue = updateuintByint(allowance[o][s].n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateTransferWithFeeOnInsertRecv_transfer_r7(address r,uint n) private   returns (bool) {
      uint rt = rate.r;
      uint mf = maxFee.m;
      address s = msg.sender;
      uint m = balanceOf[s].n;
      if(false==isBlackListed[s].b) {
        if(n<=m) {
          uint f = (rt*n)/10000 < mf ? (rt*n)/10000 : mf;
          updateTransferOnInsertTransferWithFee_r3(s,r,f);
          updateTransferOnInsertTransferWithFee_r17(s,r,f,n);
          emit TransferWithFee(s,r,f,n);
          return true;
        }
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r6(address p,int o) private    {
      int _delta = int(-o);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateTotalSupplyOnInsertConstructor_r22(uint n) private    {
      totalSupply = TotalSupplyTuple(n,true);
  }
  function updateBalanceOfOnIncrementTotalIn_r6(address p,int i) private    {
      int _delta = int(i);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateBalanceOfOnIncrementTotalIssue_r6(address p,int n) private    {
      int _delta = int(n);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
}