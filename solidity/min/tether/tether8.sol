contract Tether {
  struct OwnerTuple {
    address p;
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
  struct TransferFromWithFeeTuple {
    address from;
    address to;
    address spender;
    uint fee;
    uint amount;
    bool _valid;
  }
  struct AllIssueTuple {
    uint n;
    bool _valid;
  }
  struct TransferWithFeeTuple {
    address from;
    address to;
    uint fee;
    uint amount;
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
  struct TotalBalancesTuple {
    uint m;
    bool _valid;
  }
  struct AllowanceTuple {
    uint n;
    bool _valid;
  }
  struct PausedTuple {
    bool b;
    bool _valid;
  }
  TransferFromWithFeeTuple transferFromWithFee;
  AllIssueTuple allIssue;
  mapping(address=>IsBlackListedTuple) isBlackListed;
  TotalBalancesTuple totalBalances;
  MaxFeeTuple maxFee;
  AllRedeemTuple allRedeem;
  mapping(address=>BalanceOfTuple) balanceOf;
  OwnerTuple owner;
  TransferWithFeeTuple transferWithFee;
  RateTuple rate;
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  PausedTuple paused;
  event RemoveBlackList(address p);
  event Issue(address p,uint amount);
  event SetParams(uint b,uint f);
  event Redeem(address p,uint amount);
  event Transfer(address from,address to,uint fee,uint amount);
  event DestroyBlackFunds(address p,uint n);
  event TransferOwnership(address o);
  event Unpause(bool b);
  event AddBlackList(address p);
  event TransferFrom(address from,address to,address spender,uint fee,uint amount);
  event IncreaseAllowance(address p,address s,uint n);
  event Pause(bool b);
  constructor(uint n) public {
    updateBalanceOfOnInsertConstructor_r24(n);
    updateOwnerOnInsertConstructor_r38();
    updateTotalBalancesOnInsertConstructor_r45(n);
    updateRateOnInsertConstructor_r26();
    updatePausedOnInsertConstructor_r5();
    updateMaxFeeOnInsertConstructor_r43();
    updateTotalSupplyOnInsertConstructor_r6(n);
  }
  function transferOwnership(address o) public    {
      bool r32 = updateTransferOwnershipOnInsertRecv_transferOwnership_r32(o);
      if(r32==false) {
        revert("Rule condition failed");
      }
  }
  function pause() public    {
      bool r40 = updatePauseOnInsertRecv_pause_r40();
      if(r40==false) {
        revert("Rule condition failed");
      }
  }
  function removeBlackList(address p) public    {
      bool r36 = updateRemoveBlackListOnInsertRecv_removeBlackList_r36(p);
      if(r36==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,uint amount) public    {
      bool r0 = updateTransferFromOnInsertRecv_transferFrom_r0(from,to,amount);
      if(r0==false) {
        revert("Rule condition failed");
      }
  }
  function destroyBlackFunds(address p) public    {
      bool r13 = updateDestroyBlackFundsOnInsertRecv_destroyBlackFunds_r13(p);
      if(r13==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (uint) {
      uint n = totalSupply();
      return n;
  }
  function setParams(uint b,uint f) public    {
      bool r17 = updateSetParamsOnInsertRecv_setParams_r17(b,f);
      if(r17==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (uint) {
      uint n = allowance[p][s].n;
      return n;
  }
  function addBlackList(address p) public    {
      bool r18 = updateAddBlackListOnInsertRecv_addBlackList_r18(p);
      if(r18==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address to,uint amount) public    {
      bool r1 = updateTransferOnInsertRecv_transfer_r1(to,amount);
      if(r1==false) {
        revert("Rule condition failed");
      }
  }
  function approve(address s,uint n) public    {
      bool r42 = updateIncreaseAllowanceOnInsertRecv_approve_r42(s,n);
      if(r42==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (uint) {
      uint n = balanceOf[p].n;
      return n;
  }
  function redeem(uint amount) public    {
      bool r31 = updateRedeemOnInsertRecv_redeem_r31(amount);
      if(r31==false) {
        revert("Rule condition failed");
      }
  }
  function unpause() public    {
      bool r4 = updateUnpauseOnInsertRecv_unpause_r4();
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function issue(uint amount) public    {
      bool r33 = updateIssueOnInsertRecv_issue_r33(amount);
      if(r33==false) {
        revert("Rule condition failed");
      }
  }
  function updatePausedOnInsertPause_r22(bool p) private    {
      paused = PausedTuple(p,true);
  }
  function updateAddBlackListOnInsertRecv_addBlackList_r18(address p) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        updateIsBlackListedOnInsertAddBlackList_r30(p);
        emit AddBlackList(p);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r6(uint n) private    {
      // Empty()
  }
  function updateIsBlackListedOnInsertAddBlackList_r30(address p) private    {
      isBlackListed[p] = IsBlackListedTuple(true,true);
  }
  function updateAllRedeemOnInsertRedeem_r9(uint n) private    {
      allRedeem.n += n;
  }
  function updatePausedOnInsertConstructor_r5() private    {
      paused = PausedTuple(false,true);
  }
  function updateTransferWithoutFeeOnInsertTransferWithFee_r7(address s,address r,uint f) private    {
      address o = owner.p;
      updateTotalInOnInsertTransferWithoutFee_r12(o,f);
      updateTotalOutOnInsertTransferWithoutFee_r14(s,f);
  }
  function updateTransferFromWithoutFeeOnInsertTransferFromWithFee_r35(address o,address r,address s,uint f) private    {
      address p = owner.p;
      updateTransferWithoutFeeOnInsertTransferFromWithoutFee_r19(o,p,f);
      updateSpentTotalOnInsertTransferFromWithoutFee_r23(o,s,f);
  }
  function updateTotalInOnInsertTransferWithoutFee_r12(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r20(p,delta0);
  }
  function updateRedeemOnInsertRecv_redeem_r31(uint n) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        uint m = balanceOf[s].n;
        if(s!=address(0) && n<=m) {
          updateTotalRedeemOnInsertRedeem_r28(s,n);
          updateAllRedeemOnInsertRedeem_r9(n);
          emit Redeem(s,n);
          return true;
        }
      }
      return false;
  }
  function updateTotalIssueOnInsertIssue_r37(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIssue_r20(p,delta0);
  }
  function updateTransferFromWithFeeOnInsertTransferFrom_r16(address o,address r,address s,uint f,uint n) private    {
      updateTransferFromWithoutFeeOnInsertTransferFromWithFee_r35(o,r,s,f);
      updateTransferFromWithoutFeeOnInsertTransferFromWithFee_r2(o,r,s,f,n);
      transferFromWithFee = TransferFromWithFeeTuple(o,r,s,f,n,true);
  }
  function updateTransferWithFeeOnInsertTransfer_r29(address s,address r,uint f,uint n) private    {
      updateTransferWithoutFeeOnInsertTransferWithFee_r41(s,r,f,n);
      updateTransferWithoutFeeOnInsertTransferWithFee_r7(s,r,f);
      transferWithFee = TransferWithFeeTuple(s,r,f,n,true);
  }
  function updateIsBlackListedOnInsertRemoveBlackList_r39(address p) private    {
      isBlackListed[p] = IsBlackListedTuple(false,true);
  }
  function updateTransferOnInsertRecv_transfer_r1(address r,uint n) private   returns (bool) {
      uint rt = rate.r;
      uint mf = maxFee.m;
      address s = msg.sender;
      uint m = balanceOf[s].n;
      if(false==isBlackListed[s].b) {
        if(n<=m) {
          uint f = (rt*n)/10000 < mf ? (rt*n)/10000 : mf;
          updateTransferWithFeeOnInsertTransfer_r29(s,r,f,n);
          emit Transfer(s,r,f,n);
          return true;
        }
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalIn_r20(address p,int i) private    {
      int _delta = int(i);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateOwnerOnInsertConstructor_r38() private    {
      address s = msg.sender;
      updateTransferFromWithoutFeeOnInsertOwner_r35(s);
      updateTransferWithoutFeeOnInsertOwner_r7(s);
      owner = OwnerTuple(s,true);
  }
  function updateTransferFromWithoutFeeOnInsertOwner_r35(address p) private    {
      address o = transferFromWithFee.from;
      address r = transferFromWithFee.to;
      address s = transferFromWithFee.spender;
      uint f = transferFromWithFee.fee;
      updateTransferWithoutFeeOnInsertTransferFromWithoutFee_r19(o,p,f);
      updateSpentTotalOnInsertTransferFromWithoutFee_r23(o,s,f);
  }
  function updateIncreaseAllowanceOnInsertRecv_approve_r42(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      uint m = allowance[o][s].n;
      uint d = n-m;
      updateAllowanceTotalOnInsertIncreaseAllowance_r46(o,s,d);
      emit IncreaseAllowance(o,s,d);
      return true;
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r20(address p,int o) private    {
      int _delta = int(-o);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateTransferOwnershipOnInsertRecv_transferOwnership_r32(address o) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        if(o!=address(0)) {
          updateOwnerOnInsertTransferOwnership_r3(o);
          emit TransferOwnership(o);
          return true;
        }
      }
      return false;
  }
  function updatePausedOnInsertUnpause_r25(bool p) private    {
      paused = PausedTuple(p,true);
  }
  function updateBalanceOfOnInsertConstructor_r24(uint n) private    {
      address s = msg.sender;
      balanceOf[s] = BalanceOfTuple(n,true);
  }
  function updateBalanceOfOnIncrementTotalRedeem_r20(address p,int m) private    {
      int _delta = int(-m);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateDestroyBlackFundsOnInsertRecv_destroyBlackFunds_r13(address p) private   returns (bool) {
      address s = msg.sender;
      if(s==owner.p) {
        uint n = balanceOf[p].n;
        if(true==isBlackListed[p].b) {
          if(p!=address(0) && n>0) {
            updateRedeemOnInsertDestroyBlackFunds_r10(p,n);
            emit DestroyBlackFunds(p,n);
            return true;
          }
        }
      }
      return false;
  }
  function updateRateOnInsertSetParams_r21(uint b) private    {
      rate = RateTuple(b,true);
  }
  function updatePauseOnInsertRecv_pause_r40() private   returns (bool) {
      if(false==paused.b) {
        address s = owner.p;
        if(s==msg.sender) {
          updatePausedOnInsertPause_r22(bool(true));
          emit Pause(true);
          return true;
        }
      }
      return false;
  }
  function updateSetParamsOnInsertRecv_setParams_r17(uint b,uint f) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        if(b<20 && f<50) {
          updateMaxFeeOnInsertSetParams_r15(f);
          updateRateOnInsertSetParams_r21(b);
          emit SetParams(b,f);
          return true;
        }
      }
      return false;
  }
  function updateSpentTotalOnInsertTransferFromWithoutFee_r23(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r8(o,s,delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r46(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r8(o,s,delta0);
  }
  function updateRedeemOnInsertDestroyBlackFunds_r10(address p,uint n) private    {
      updateTotalRedeemOnInsertRedeem_r28(p,n);
      updateAllRedeemOnInsertRedeem_r9(n);
      emit Redeem(p,n);
  }
  function updateTransferWithoutFeeOnInsertTransferFromWithoutFee_r19(address o,address r,uint n) private    {
      updateTotalOutOnInsertTransferWithoutFee_r14(o,n);
      updateTotalInOnInsertTransferWithoutFee_r12(r,n);
  }
  function updateTotalRedeemOnInsertRedeem_r28(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalRedeem_r20(p,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateAllIssueOnInsertIssue_r44(uint n) private    {
      allIssue.n += n;
  }
  function updateBalanceOfOnIncrementTotalIssue_r20(address p,int n) private    {
      int _delta = int(n);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateMaxFeeOnInsertConstructor_r43() private    {
      maxFee = MaxFeeTuple(0,true);
  }
  function updateUnpauseOnInsertRecv_unpause_r4() private   returns (bool) {
      if(true==paused.b) {
        address s = owner.p;
        if(s==msg.sender) {
          updatePausedOnInsertUnpause_r25(bool(false));
          emit Unpause(false);
          return true;
        }
      }
      return false;
  }
  function totalSupply() private view  returns (uint) {
      uint b = allRedeem.n;
      uint m = allIssue.n;
      uint n = m-b;
      return n;
  }
  function updateOwnerOnInsertTransferOwnership_r3(address s) private    {
      updateTransferFromWithoutFeeOnInsertOwner_r35(s);
      updateTransferWithoutFeeOnInsertOwner_r7(s);
      owner = OwnerTuple(s,true);
  }
  function updateTransferFromWithoutFeeOnInsertTransferFromWithFee_r2(address o,address r,address s,uint f,uint n) private    {
      uint m = n-f;
      updateTransferWithoutFeeOnInsertTransferFromWithoutFee_r19(o,r,m);
      updateSpentTotalOnInsertTransferFromWithoutFee_r23(o,s,m);
  }
  function updateTotalBalancesOnInsertConstructor_r45(uint n) private    {
      totalBalances = TotalBalancesTuple(n,true);
  }
  function updateRemoveBlackListOnInsertRecv_removeBlackList_r36(address p) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        updateIsBlackListedOnInsertRemoveBlackList_r39(p);
        emit RemoveBlackList(p);
        return true;
      }
      return false;
  }
  function updateTransferWithoutFeeOnInsertOwner_r7(address o) private    {
      address s = transferWithFee.from;
      address r = transferWithFee.to;
      uint f = transferWithFee.fee;
      updateTotalInOnInsertTransferWithoutFee_r12(o,f);
      updateTotalOutOnInsertTransferWithoutFee_r14(s,f);
  }
  function updateIssueOnInsertRecv_issue_r33(uint n) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        if(s!=address(0)) {
          updateAllIssueOnInsertIssue_r44(n);
          updateTotalIssueOnInsertIssue_r37(s,n);
          emit Issue(s,n);
          return true;
        }
      }
      return false;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r0(address o,address r,uint n) private   returns (bool) {
      uint rt = rate.r;
      uint mf = maxFee.m;
      address s = msg.sender;
      uint m = balanceOf[o].n;
      if(false==isBlackListed[o].b) {
        uint k = allowance[o][s].n;
        if(m>=n && k>=n) {
          uint f = (rt*n)/10000 < mf ? (rt*n)/10000 : mf;
          updateTransferFromWithFeeOnInsertTransferFrom_r16(o,r,s,f,n);
          emit TransferFrom(o,r,s,f,n);
          return true;
        }
      }
      return false;
  }
  function updateRateOnInsertConstructor_r26() private    {
      rate = RateTuple(0,true);
  }
  function updateAllowanceOnIncrementSpentTotal_r8(address o,address s,int l) private    {
      int _delta = int(-l);
      uint newValue = updateuintByint(allowance[o][s].n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateMaxFeeOnInsertSetParams_r15(uint f) private    {
      maxFee = MaxFeeTuple(f,true);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r8(address o,address s,int m) private    {
      int _delta = int(m);
      uint newValue = updateuintByint(allowance[o][s].n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateTotalOutOnInsertTransferWithoutFee_r14(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r20(p,delta0);
  }
  function updateTransferWithoutFeeOnInsertTransferWithFee_r41(address s,address r,uint f,uint n) private    {
      uint m = n-f;
      updateTotalInOnInsertTransferWithoutFee_r12(r,m);
      updateTotalOutOnInsertTransferWithoutFee_r14(s,m);
  }
}