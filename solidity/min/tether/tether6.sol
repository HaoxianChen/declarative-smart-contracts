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
  struct TotalInTuple {
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
  struct TotalRedeemTuple {
    uint n;
    bool _valid;
  }
  struct SpentTotalTuple {
    uint m;
    bool _valid;
  }
  struct PausedTuple {
    bool b;
    bool _valid;
  }
  mapping(address=>TotalOutTuple) totalOut;
  mapping(address=>TotalIssueTuple) totalIssue;
  TotalSupplyTuple totalSupply;
  mapping(address=>IsBlackListedTuple) isBlackListed;
  mapping(address=>mapping(address=>AllowanceTotalTuple)) allowanceTotal;
  TotalBalancesTuple totalBalances;
  MaxFeeTuple maxFee;
  PausedTuple paused;
  mapping(address=>TotalInTuple) totalIn;
  TransferFromWithFeeTuple transferFromWithFee;
  OwnerTuple owner;
  TransferWithFeeTuple transferWithFee;
  RateTuple rate;
  mapping(address=>TotalRedeemTuple) totalRedeem;
  mapping(address=>mapping(address=>SpentTotalTuple)) spentTotal;
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
  function setParams(uint b,uint f) public    {
      bool r17 = updateSetParamsOnInsertRecv_setParams_r17(b,f);
      if(r17==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (uint) {
      uint n = totalSupply.n;
      return n;
  }
  function approve(address s,uint n) public    {
      bool r42 = updateIncreaseAllowanceOnInsertRecv_approve_r42(s,n);
      if(r42==false) {
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
  function getBalanceOf(address p) public view  returns (uint) {
      uint n = balanceOf(p);
      return n;
  }
  function getAllowance(address p,address s) public view  returns (uint) {
      uint n = allowance(p,s);
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
  function updateTransferFromWithFeeOnInsertTransferFrom_r16(address o,address r,address s,uint f,uint n) private    {
      updateTransferFromWithoutFeeOnInsertTransferFromWithFee_r35(o,r,s,f);
      updateTransferFromWithoutFeeOnInsertTransferFromWithFee_r2(o,r,s,f,n);
      transferFromWithFee = TransferFromWithFeeTuple(o,r,s,f,n,true);
  }
  function updateOwnerOnInsertConstructor_r38() private    {
      address s = msg.sender;
      updateTransferFromWithoutFeeOnInsertOwner_r35(s);
      updateTransferWithoutFeeOnInsertOwner_r7(s);
      owner = OwnerTuple(s,true);
  }
  function updatePausedOnInsertPause_r22(bool p) private    {
      paused = PausedTuple(p,true);
  }
  function updateTotalSupplyOnInsertConstructor_r6(uint n) private    {
      totalSupply = TotalSupplyTuple(n,true);
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
  function updateIsBlackListedOnInsertAddBlackList_r30(address p) private    {
      isBlackListed[p] = IsBlackListedTuple(true,true);
  }
  function updateRedeemOnInsertDestroyBlackFunds_r10(address p,uint n) private    {
      updateTotalRedeemOnInsertRedeem_r28(p,n);
      updateAllRedeemOnInsertRedeem_r9(n);
      emit Redeem(p,n);
  }
  function updateTotalRedeemOnInsertRedeem_r28(address p,uint n) private    {
      totalRedeem[p].n += n;
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
  function updateAllIssueOnInsertIssue_r44(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllIssue_r11(delta0);
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
  function updateTransferWithoutFeeOnInsertTransferWithFee_r41(address s,address r,uint f,uint n) private    {
      uint m = n-f;
      updateTotalInOnInsertTransferWithoutFee_r12(r,m);
      updateTotalOutOnInsertTransferWithoutFee_r14(s,m);
  }
  function updateTotalInOnInsertTransferWithoutFee_r12(address p,uint n) private    {
      totalIn[p].n += n;
  }
  function updateIncreaseAllowanceOnInsertRecv_approve_r42(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      uint m = allowance(o,s);
      uint d = n-m;
      updateAllowanceTotalOnInsertIncreaseAllowance_r46(o,s,d);
      emit IncreaseAllowance(o,s,d);
      return true;
      return false;
  }
  function updateTotalOutOnInsertTransferWithoutFee_r14(address p,uint n) private    {
      totalOut[p].n += n;
  }
  function updateTransferOnInsertRecv_transfer_r1(address r,uint n) private   returns (bool) {
      uint rt = rate.r;
      uint mf = maxFee.m;
      address s = msg.sender;
      if(false==isBlackListed[s].b) {
        uint m = balanceOf(s);
        if(n<=m) {
          uint f = (rt*n)/10000 < mf ? (rt*n)/10000 : mf;
          updateTransferWithFeeOnInsertTransfer_r29(s,r,f,n);
          emit Transfer(s,r,f,n);
          return true;
        }
      }
      return false;
  }
  function updateTransferWithFeeOnInsertTransfer_r29(address s,address r,uint f,uint n) private    {
      updateTransferWithoutFeeOnInsertTransferWithFee_r41(s,r,f,n);
      updateTransferWithoutFeeOnInsertTransferWithFee_r7(s,r,f);
      transferWithFee = TransferWithFeeTuple(s,r,f,n,true);
  }
  function updateIsBlackListedOnInsertRemoveBlackList_r39(address p) private    {
      isBlackListed[p] = IsBlackListedTuple(false,true);
  }
  function updateTransferFromWithoutFeeOnInsertOwner_r35(address p) private    {
      address o = transferFromWithFee.from;
      address r = transferFromWithFee.to;
      address s = transferFromWithFee.spender;
      uint f = transferFromWithFee.fee;
      updateTransferWithoutFeeOnInsertTransferFromWithoutFee_r19(o,p,f);
      updateSpentTotalOnInsertTransferFromWithoutFee_r23(o,s,f);
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
  function updateAllowanceTotalOnInsertIncreaseAllowance_r46(address o,address s,uint n) private    {
      allowanceTotal[o][s].m += n;
  }
  function updateAllRedeemOnInsertRedeem_r9(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllRedeem_r11(delta0);
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
  function updateTotalSupplyOnIncrementAllIssue_r11(int m) private    {
      int _delta = int(m);
      uint newValue = updateuintByint(totalSupply.n,_delta);
      totalSupply.n = newValue;
  }
  function updateSpentTotalOnInsertTransferFromWithoutFee_r23(address o,address s,uint n) private    {
      spentTotal[o][s].m += n;
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
  function allowance(address o,address s) private view  returns (uint) {
      uint l = spentTotal[o][s].m;
      uint m = allowanceTotal[o][s].m;
      uint n = m-l;
      return n;
  }
  function updateTransferWithoutFeeOnInsertTransferFromWithoutFee_r19(address o,address r,uint n) private    {
      updateTotalOutOnInsertTransferWithoutFee_r14(o,n);
      updateTotalInOnInsertTransferWithoutFee_r12(r,n);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateDestroyBlackFundsOnInsertRecv_destroyBlackFunds_r13(address p) private   returns (bool) {
      address s = msg.sender;
      if(s==owner.p) {
        if(true==isBlackListed[p].b) {
          uint n = balanceOf(p);
          if(p!=address(0) && n>0) {
            updateRedeemOnInsertDestroyBlackFunds_r10(p,n);
            emit DestroyBlackFunds(p,n);
            return true;
          }
        }
      }
      return false;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r0(address o,address r,uint n) private   returns (bool) {
      uint rt = rate.r;
      uint mf = maxFee.m;
      address s = msg.sender;
      if(false==isBlackListed[o].b) {
        uint m = balanceOf(o);
        uint k = allowance(o,s);
        if(m>=n && k>=n) {
          uint f = (rt*n)/10000 < mf ? (rt*n)/10000 : mf;
          updateTransferFromWithFeeOnInsertTransferFrom_r16(o,r,s,f,n);
          emit TransferFrom(o,r,s,f,n);
          return true;
        }
      }
      return false;
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
  function updateBalanceOfOnInsertConstructor_r24(uint n) private    {
      address s = msg.sender;
      // Empty()
  }
  function updateTotalBalancesOnInsertConstructor_r45(uint n) private    {
      totalBalances = TotalBalancesTuple(n,true);
  }
  function balanceOf(address p) private view  returns (uint) {
      uint i = totalIn[p].n;
      uint o = totalOut[p].n;
      uint m = totalRedeem[p].n;
      uint n = totalIssue[p].n;
      uint s = ((n+i)-m)-o;
      return s;
  }
  function updatePausedOnInsertConstructor_r5() private    {
      paused = PausedTuple(false,true);
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
  function updateRateOnInsertConstructor_r26() private    {
      rate = RateTuple(0,true);
  }
  function updateRedeemOnInsertRecv_redeem_r31(uint n) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        uint m = balanceOf(s);
        if(s!=address(0) && n<=m) {
          updateTotalRedeemOnInsertRedeem_r28(s,n);
          updateAllRedeemOnInsertRedeem_r9(n);
          emit Redeem(s,n);
          return true;
        }
      }
      return false;
  }
  function updateMaxFeeOnInsertSetParams_r15(uint f) private    {
      maxFee = MaxFeeTuple(f,true);
  }
  function updateTotalSupplyOnIncrementAllRedeem_r11(int b) private    {
      int _delta = int(-b);
      uint newValue = updateuintByint(totalSupply.n,_delta);
      totalSupply.n = newValue;
  }
  function updateTotalIssueOnInsertIssue_r37(address p,uint n) private    {
      totalIssue[p].n += n;
  }
}