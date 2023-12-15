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
  struct TotalSupplyTuple {
    uint n;
    bool _valid;
  }
  struct AllowanceTotalTuple {
    uint m;
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
  struct AllIssueTuple {
    uint n;
    bool _valid;
  }
  struct RateTuple {
    uint r;
    bool _valid;
  }
  struct IsBlackListedTuple {
    bool b;
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
  event SetParams(uint b,uint f);
  event Redeem(address p,uint amount);
  event IncreaseAllowance(address p,address s,uint n);
  event TransferWithFee(address from,address to,uint fee,uint amount);
  event DestroyBlackFunds(address p,uint n);
  event TransferOwnership(address o);
  event RemoveBlackList(address p);
  event Paused(bool b);
  constructor(uint n) public {
    updateTotalSupplyOnInsertConstructor_r33(n);
    updateTotalBalancesOnInsertConstructor_r41(n);
    updateRateOnInsertConstructor_r19();
    updateOwnerOnInsertConstructor_r35();
    updateBalanceOfOnInsertConstructor_r7(n);
    updateMaxFeeOnInsertConstructor_r38();
    updatePausedOnInsertConstructor_r8();
  }
  function transferOwnership(address o) public    {
      bool r25 = updateTransferOwnershipOnInsertRecv_transferOwnership_r25(o);
      if(r25==false) {
        revert("Rule condition failed");
      }
  }
  function addBlackList(address p) public    {
      bool r16 = updateAddBlackListOnInsertRecv_addBlackList_r16(p);
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function pause() public    {
      bool r36 = updatePausedOnInsertRecv_pause_r36();
      if(r36==false) {
        revert("Rule condition failed");
      }
  }
  function redeem(uint amount) public    {
      bool r24 = updateRedeemOnInsertRecv_redeem_r24(amount);
      if(r24==false) {
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
  function approve(address s,uint n) public    {
      bool r11 = updateIncreaseAllowanceOnInsertRecv_approve_r11(s,n);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address to,uint amount) public    {
      bool r17 = updateTransferWithFeeOnInsertRecv_transfer_r17(to,amount);
      if(r17==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,uint amount) public    {
      bool r0 = updateTransferFromWithFeeOnInsertRecv_transferFrom_r0(from,to,amount);
      if(r0==false) {
        revert("Rule condition failed");
      }
  }
  function removeBlackList(address p) public    {
      bool r9 = updateRemoveBlackListOnInsertRecv_removeBlackList_r9(p);
      if(r9==false) {
        revert("Rule condition failed");
      }
  }
  function issue(uint amount) public    {
      bool r27 = updateIssueOnInsertRecv_issue_r27(amount);
      if(r27==false) {
        revert("Rule condition failed");
      }
  }
  function unpause() public    {
      bool r31 = updatePausedOnInsertRecv_unpause_r31();
      if(r31==false) {
        revert("Rule condition failed");
      }
  }
  function setParams(uint b,uint f) public    {
      bool r15 = updateSetParamsOnInsertRecv_setParams_r15(b,f);
      if(r15==false) {
        revert("Rule condition failed");
      }
  }
  function destroyBlackFunds(address p) public    {
      bool r4 = updateDestroyBlackFundsOnInsertRecv_destroyBlackFunds_r4(p);
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (uint) {
      uint n = totalSupply.n;
      return n;
  }
  function updateTransferOwnershipOnInsertRecv_transferOwnership_r25(address o) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        if(o!=address(0)) {
          updateOwnerOnInsertTransferOwnership_r6(o);
          emit TransferOwnership(o);
          return true;
        }
      }
      return false;
  }
  function updatePausedOnInsertRecv_unpause_r31() private   returns (bool) {
      if(true==paused.b) {
        address s = owner.p;
        if(s==msg.sender) {
          emit Paused(false);
          return true;
        }
      }
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r33(uint n) private    {
      totalSupply = TotalSupplyTuple(n,true);
  }
  function updateIssueOnInsertRecv_issue_r27(uint n) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        if(s!=address(0)) {
          updateAllIssueOnInsertIssue_r39(n);
          updateTotalIssueOnInsertIssue_r34(s,n);
          emit Issue(s,n);
          return true;
        }
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllRedeem_r2(int b) private    {
      int _delta = int(-b);
      uint newValue = updateuintByint(totalSupply.n,_delta);
      totalSupply.n = newValue;
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
          updateTransferFromOnInsertTransferFromWithFee_r23(o,r,s,f);
          updateTransferFromOnInsertTransferFromWithFee_r28(o,r,s,f,n);
          emit TransferFromWithFee(o,r,s,f,n);
          return true;
        }
      }
      return false;
  }
  function updateTotalOutOnInsertTransfer_r29(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r5(p,delta0);
      totalOut[p].n += n;
  }
  function updateBalanceOfOnIncrementTotalIn_r5(address p,int i) private    {
      int _delta = int(i);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateTotalRedeemOnInsertRedeem_r21(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalRedeem_r5(p,delta0);
      totalRedeem[p].n += n;
  }
  function updateTotalBalancesOnInsertConstructor_r41(uint n) private    {
      // Empty()
  }
  function updateRateOnInsertSetParams_r18(uint b) private    {
      rate = RateTuple(b,true);
  }
  function updateTransferFromOnInsertTransferFromWithFee_r28(address o,address r,address s,uint f,uint n) private    {
      uint m = n-f;
      updateSpentTotalOnInsertTransferFrom_r32(o,s,m);
      updateTransferOnInsertTransferFrom_r1(o,r,m);
      transferFrom = TransferFromTuple(o,r,s,m,true);
  }
  function updateTransferFromOnInsertOwner_r23(address p) private    {
      address o = transferFromWithFee.from;
      address r = transferFromWithFee.to;
      address s = transferFromWithFee.spender;
      uint f = transferFromWithFee.fee;
      updateSpentTotalOnInsertTransferFrom_r32(o,s,f);
      updateTransferOnInsertTransferFrom_r1(o,p,f);
      transferFrom = TransferFromTuple(o,p,s,f,true);
  }
  function updateIsBlackListedOnInsertRemoveBlackList_r37(address p) private    {
      isBlackListed[p] = IsBlackListedTuple(false,true);
  }
  function updateTransferOnInsertTransferWithFee_r26(address s,address r,uint f,uint n) private    {
      uint m = n-f;
      updateTotalInOnInsertTransfer_r40(r,m);
      updateTotalOutOnInsertTransfer_r29(s,m);
  }
  function updateDestroyBlackFundsOnInsertRecv_destroyBlackFunds_r4(address p) private   returns (bool) {
      address s = msg.sender;
      if(s==owner.p) {
        uint n = balanceOf[p].n;
        if(true==isBlackListed[p].b) {
          if(p!=address(0) && n>0) {
            updateRedeemOnInsertDestroyBlackFunds_r13(p,n);
            emit DestroyBlackFunds(p,n);
            return true;
          }
        }
      }
      return false;
  }
  function updateRedeemOnInsertDestroyBlackFunds_r13(address p,uint n) private    {
      updateTotalRedeemOnInsertRedeem_r21(p,n);
      updateAllRedeemOnInsertRedeem_r12(n);
      emit Redeem(p,n);
  }
  function updateTransferOnInsertTransferFrom_r1(address o,address r,uint n) private    {
      updateTotalOutOnInsertTransfer_r29(o,n);
      updateTotalInOnInsertTransfer_r40(r,n);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r42(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r10(o,s,delta0);
      allowanceTotal[o][s].m += n;
  }
  function updatePausedOnInsertRecv_pause_r36() private   returns (bool) {
      if(false==paused.b) {
        address s = owner.p;
        if(s==msg.sender) {
          emit Paused(true);
          return true;
        }
      }
      return false;
  }
  function updateAddBlackListOnInsertRecv_addBlackList_r16(address p) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        updateIsBlackListedOnInsertAddBlackList_r22(p);
        emit AddBlackList(p);
        return true;
      }
      return false;
  }
  function updateRemoveBlackListOnInsertRecv_removeBlackList_r9(address p) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        updateIsBlackListedOnInsertRemoveBlackList_r37(p);
        emit RemoveBlackList(p);
        return true;
      }
      return false;
  }
  function updateRedeemOnInsertRecv_redeem_r24(uint n) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        uint m = balanceOf[s].n;
        if(s!=address(0) && n<=m) {
          updateTotalRedeemOnInsertRedeem_r21(s,n);
          updateAllRedeemOnInsertRedeem_r12(n);
          emit Redeem(s,n);
          return true;
        }
      }
      return false;
  }
  function updateIncreaseAllowanceOnInsertRecv_approve_r11(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      uint m = allowance[o][s].n;
      uint d = n-m;
      updateAllowanceTotalOnInsertIncreaseAllowance_r42(o,s,d);
      emit IncreaseAllowance(o,s,d);
      return true;
      return false;
  }
  function updateAllRedeemOnInsertRedeem_r12(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllRedeem_r2(delta0);
      allRedeem.n += n;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r10(address o,address s,int m) private    {
      int _delta = int(m);
      uint newValue = updateuintByint(allowance[o][s].n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateOwnerOnInsertTransferOwnership_r6(address s) private    {
      updateTransferFromOnInsertOwner_r23(s);
      updateTransferOnInsertOwner_r3(s);
      owner = OwnerTuple(s,true);
  }
  function updateIsBlackListedOnInsertAddBlackList_r22(address p) private    {
      isBlackListed[p] = IsBlackListedTuple(true,true);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateBalanceOfOnIncrementTotalRedeem_r5(address p,int m) private    {
      int _delta = int(-m);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateMaxFeeOnInsertConstructor_r38() private    {
      maxFee = MaxFeeTuple(0,true);
  }
  function updateRateOnInsertConstructor_r19() private    {
      rate = RateTuple(0,true);
  }
  function updateBalanceOfOnIncrementTotalIssue_r5(address p,int n) private    {
      int _delta = int(n);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateTotalIssueOnInsertIssue_r34(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIssue_r5(p,delta0);
      totalIssue[p].n += n;
  }
  function updateSpentTotalOnInsertTransferFrom_r32(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r10(o,s,delta0);
      spentTotal[o][s].m += n;
  }
  function updateTotalSupplyOnIncrementAllIssue_r2(int m) private    {
      int _delta = int(m);
      uint newValue = updateuintByint(totalSupply.n,_delta);
      totalSupply.n = newValue;
  }
  function updateTransferOnInsertOwner_r3(address o) private    {
      address s = transferWithFee.from;
      address r = transferWithFee.to;
      uint f = transferWithFee.fee;
      updateTotalInOnInsertTransfer_r40(o,f);
      updateTotalOutOnInsertTransfer_r29(s,f);
  }
  function updateTransferOnInsertTransferWithFee_r3(address s,address r,uint f) private    {
      address o = owner.p;
      updateTotalInOnInsertTransfer_r40(o,f);
      updateTotalOutOnInsertTransfer_r29(s,f);
  }
  function updateAllIssueOnInsertIssue_r39(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllIssue_r2(delta0);
      allIssue.n += n;
  }
  function updateTotalInOnInsertTransfer_r40(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r5(p,delta0);
      totalIn[p].n += n;
  }
  function updateMaxFeeOnInsertSetParams_r14(uint f) private    {
      maxFee = MaxFeeTuple(f,true);
  }
  function updateBalanceOfOnInsertConstructor_r7(uint n) private    {
      address s = msg.sender;
      balanceOf[s] = BalanceOfTuple(n,true);
  }
  function updateTransferFromOnInsertTransferFromWithFee_r23(address o,address r,address s,uint f) private    {
      address p = owner.p;
      updateSpentTotalOnInsertTransferFrom_r32(o,s,f);
      updateTransferOnInsertTransferFrom_r1(o,p,f);
      transferFrom = TransferFromTuple(o,p,s,f,true);
  }
  function updateTransferWithFeeOnInsertRecv_transfer_r17(address r,uint n) private   returns (bool) {
      uint rt = rate.r;
      uint mf = maxFee.m;
      address s = msg.sender;
      uint m = balanceOf[s].n;
      if(false==isBlackListed[s].b) {
        if(n<=m) {
          uint f = (rt*n)/10000 < mf ? (rt*n)/10000 : mf;
          updateTransferOnInsertTransferWithFee_r3(s,r,f);
          updateTransferOnInsertTransferWithFee_r26(s,r,f,n);
          emit TransferWithFee(s,r,f,n);
          return true;
        }
      }
      return false;
  }
  function updateOwnerOnInsertConstructor_r35() private    {
      address s = msg.sender;
      updateTransferFromOnInsertOwner_r23(s);
      updateTransferOnInsertOwner_r3(s);
      owner = OwnerTuple(s,true);
  }
  function updateBalanceOfOnIncrementTotalOut_r5(address p,int o) private    {
      int _delta = int(-o);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updatePausedOnInsertConstructor_r8() private    {
      emit Paused(false);
  }
  function updateAllowanceOnIncrementSpentTotal_r10(address o,address s,int l) private    {
      int _delta = int(-l);
      uint newValue = updateuintByint(allowance[o][s].n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateSetParamsOnInsertRecv_setParams_r15(uint b,uint f) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        if(b<20 && f<50) {
          updateRateOnInsertSetParams_r18(b);
          updateMaxFeeOnInsertSetParams_r14(f);
          emit SetParams(b,f);
          return true;
        }
      }
      return false;
  }
}