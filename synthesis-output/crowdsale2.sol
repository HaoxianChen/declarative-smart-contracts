import "./crowdsale2_udf.sol";
contract Crowdsale2 is CrowdsaleUDF {
  struct AllowanceTuple {
    int n;
    bool _valid;
  }
  struct BalanceOfTuple {
    int n;
    bool _valid;
  }
  struct _primaryTuple {
    address p;
    bool _valid;
  }
  struct _depositsTuple {
    int a;
    bool _valid;
  }
  struct TotalSupplyTuple {
    int n;
    bool _valid;
  }
  _primaryTuple _primary;
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  mapping(address=>_depositsTuple) _deposits;
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalSupplyTuple totalSupply;
  event Withdraw(address p);
  event FinalizeNotByPrimary();
  event Deposit(address p,int a);
  event Burn(address p,int amount);
  event TransferPrimaryNotByPrimary();
  event Transfer(address from,address to,int amount);
  event Finalize();
  event Mint(address p,int amount);
  event BuyToken(address p,int v);
  event TransferFrom(address from,address to,address spender,int amount);
  event ClaimRefund(address p);
  event TransferPrimary(address p);
  event IncreaseAllowance(address p,address s,int n);
  constructor(uint t1,uint t2,address p,int cap,int goal) public {
    updateTotalSupplyOnInsertConstructor_r19();
    update_openingTimeOnInsertConstructor_r13(t1);
    updateMintOnInsertConstructor_r34();
    update_closingTimeOnInsertConstructor_r46(t2);
    update_finalizedOnInsertConstructor_r35();
    updateOnceWithdrawOutsideSuccessOnInsertConstructor_r23();
    update_goalOnInsertConstructor_r36(cap,goal);
    updateOnceBuyAfterFinalizeOnInsertConstructor_r49();
    updateStateOnInsertConstructor_r14();
    updateOnceClaimRefundOutsideRefundOnInsertConstructor_r7();
    update_walletOnInsertConstructor_r18(p);
    update_capOnInsertConstructor_r3(cap,goal);
    update_primaryOnInsertConstructor_r22();
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r17 = updateTransferFromOnInsertRecv_transferFrom_r17(from,to,spender,amount);
      if(r17==false) {
        revert("Rule condition failed");
      }
  }
  function withdraw(address p) public    {
      bool r58 = updateWithdrawOnInsertRecv_withdraw_r58(p);
      if(r58==false) {
        revert("Rule condition failed");
      }
  }
  function get_deposits(address p) public view  returns (int) {
      int a = _deposits[p].a;
      return a;
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function finalize() public    {
      bool r29 = updateFinalizeOnInsertRecv_finalize_r29();
      if(r29==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r20 = updateMintOnInsertRecv_mint_r20(p,amount);
      if(r20==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r4 = updateBurnOnInsertRecv_burn_r4(p,amount);
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function buyToken(address p,int v) public    {
      bool r55 = updateBuyTokenOnInsertRecv_buyToken_r55(p,v);
      if(r55==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r11 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r11(p,s,n);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function deposit(address p,int a) public    {
      bool r33 = updateDepositOnInsertRecv_deposit_r33(p,a);
      if(r33==false) {
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
  function transferPrimary(address p) public    {
      bool r52 = updateTransferPrimaryOnInsertRecv_transferPrimary_r52(p);
      if(r52==false) {
        revert("Rule condition failed");
      }
  }
  function claimRefund(address p) public    {
      bool r39 = updateClaimRefundOnInsertRecv_claimRefund_r39(p);
      if(r39==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r51 = updateTransferOnInsertRecv_transfer_r51(from,to,amount);
      if(r51==false) {
        revert("Rule condition failed");
      }
  }
  function updateTransferPrimaryOnInsertRecv_transferPrimary_r52(address p) private   returns (bool) {
      address msgSender = msg.sender;
      if(p==_primary.p) {
        int _deposits_x1 = _deposits[msgSender].a;
        if(msgSender==p && _deposits_x1>0) {
          update_primaryOnInsertTransferPrimary_r1(p);
          emit TransferPrimary(p);
          return true;
        }
      }
      return false;
  }
  function update_closingTimeOnInsertConstructor_r46(uint t2) private    {
      // Empty()
  }
  function update_finalizedOnInsertConstructor_r35() private    {
      // Empty()
  }
  function updateOnceBuyAfterFinalizeOnInsertConstructor_r49() private    {
      // Empty()
  }
  function updateDepositOnInsertBuyToken_r16(address p,int n) private    {
      update_depositsOnInsertDeposit_r30(p,n);
      emit Deposit(p,n);
  }
  function updateTotalBurnOnInsertBurn_r15(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r38(p,delta0);
  }
  function update_walletOnInsertConstructor_r18(address p) private    {
      // Empty()
  }
  function updateBuyTokenOnInsertRecv_buyToken_r55(address p,int v) private   returns (bool) {
      updateDepositOnInsertBuyToken_r16(p,v);
      updateMintOnInsertBuyToken_r53(p,v);
      emit BuyToken(p,v);
      return true;
      return false;
  }
  function updateBalanceOfOnIncrementTotalBurn_r38(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateTotalMintOnInsertMint_r5(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r38(p,delta0);
  }
  function updateTransferOnInsertTransferFrom_r40(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r56(o,n);
      updateTotalInOnInsertTransfer_r12(r,n);
      emit Transfer(o,r,n);
  }
  function updateSpentTotalOnInsertTransferFrom_r6(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r50(o,s,delta0);
  }
  function updateTransferOnInsertRecv_transfer_r51(address from,address to,int amount) private   returns (bool) {
      int balanceOf_x1 = balanceOf[from].n;
      if(amount<=balanceOf_x1) {
        updateTotalOutOnInsertTransfer_r56(from,amount);
        updateTotalInOnInsertTransfer_r12(to,amount);
        emit Transfer(from,to,amount);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r19() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateMintOnInsertRecv_mint_r20(address p,int amount) private   returns (bool) {
      address msgSender = msg.sender;
      int _deposits_x1 = _deposits[msgSender].a;
      if(0!=_deposits_x1) {
        updateTotalMintOnInsertMint_r5(p,amount);
        updateAllMintOnInsertMint_r8(amount);
        emit Mint(p,amount);
        return true;
      }
      return false;
  }
  function updateWithdrawOnInsertRecv_withdraw_r58(address p) private   returns (bool) {
      address msgSender = msg.sender;
      int _deposits_x1 = _deposits[msgSender].a;
      if(0!=_deposits_x1) {
        update_depositsOnInsertWithdraw_r43(p);
        emit Withdraw(p);
        return true;
      }
      return false;
  }
  function update_primaryOnInsertTransferPrimary_r1(address p) private    {
      _primary = _primaryTuple(p,true);
  }
  function update_openingTimeOnInsertConstructor_r13(uint t1) private    {
      // Empty()
  }
  function updateTotalSupplyOnIncrementAllMint_r25(int m) private    {
      totalSupply.n += m;
  }
  function updateBurnOnInsertRecv_burn_r4(address p,int amount) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[p].n;
      if(amount>0 && amount<=balanceOf_x1_1) {
        updateTotalBurnOnInsertBurn_r15(p,amount);
        updateAllBurnOnInsertBurn_r24(amount);
        emit Burn(p,amount);
        return true;
      }
      return false;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r48(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r50(o,s,delta0);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r11(address p,address s,int n) private   returns (bool) {
      if(0!=n) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r48(p,s,n);
        emit IncreaseAllowance(p,s,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalIn_r38(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function update_depositsOnInsertWithdraw_r43(address p) private    {
      int a = 0;
      _deposits[p] = _depositsTuple(a,true);
  }
  function update_capOnInsertConstructor_r3(int n,int a) private    {
      if(a<=n) {
        // Empty()
      }
  }
  function updateMintOnInsertConstructor_r34() private    {
      address s = msg.sender;
      updateAllMintOnInsertMint_r8(int(1));
      updateTotalMintOnInsertMint_r5(s,int(1));
      emit Mint(s,1);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r17(address from,address to,address spender,int amount) private   returns (bool) {
      int allowance_x2 = allowance[from][spender].n;
      if(0!=allowance_x2) {
        updateSpentTotalOnInsertTransferFrom_r6(from,spender,amount);
        updateTransferOnInsertTransferFrom_r40(from,to,amount);
        emit TransferFrom(from,to,spender,amount);
        return true;
      }
      return false;
  }
  function updateAllBurnOnInsertBurn_r24(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r25(delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateClaimRefundOnInsertRecv_claimRefund_r39(address p) private   returns (bool) {
      address msgSender = msg.sender;
      int _deposits_x1 = _deposits[msgSender].a;
      if(0!=_deposits_x1) {
        emit ClaimRefund(p);
        return true;
      }
      return false;
  }
  function update_primaryOnInsertConstructor_r22() private    {
      address s = msg.sender;
      _primary = _primaryTuple(s,true);
  }
  function updateDepositOnInsertRecv_deposit_r33(address p,int a) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(a<=balanceOf_x1) {
        update_depositsOnInsertDeposit_r30(p,a);
        emit Deposit(p,a);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r38(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateAllowanceOnIncrementSpentTotal_r50(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateOnceClaimRefundOutsideRefundOnInsertConstructor_r7() private    {
      // Empty()
  }
  function update_depositsOnInsertDeposit_r30(address p,int n) private    {
      _deposits[p].a += n;
  }
  function updateMintOnInsertBuyToken_r53(address p,int v) private    {
      int tokens = getTokenAmount(v);
      updateAllMintOnInsertMint_r8(tokens);
      updateTotalMintOnInsertMint_r5(p,tokens);
      emit Mint(p,tokens);
  }
  function updateTotalSupplyOnIncrementAllBurn_r25(int b) private    {
      totalSupply.n -= b;
  }
  function updateOnceWithdrawOutsideSuccessOnInsertConstructor_r23() private    {
      // Empty()
  }
  function updateFinalizeOnInsertRecv_finalize_r29() private   returns (bool) {
      address s = msg.sender;
      address p = _primary.p;
      if(s==p) {
        emit Finalize();
        return true;
      }
      return false;
  }
  function updateAllMintOnInsertMint_r8(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r25(delta0);
  }
  function updateTotalOutOnInsertTransfer_r56(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r38(p,delta0);
  }
  function update_goalOnInsertConstructor_r36(int n,int a) private    {
      if(a<=n) {
        // Empty()
      }
  }
  function updateTotalInOnInsertTransfer_r12(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r38(p,delta0);
  }
  function updateStateOnInsertConstructor_r14() private    {
      // Empty()
  }
  function updateBalanceOfOnIncrementTotalMint_r38(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r50(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
}