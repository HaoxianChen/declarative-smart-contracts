contract Crowdsale {
  struct AllowanceTuple {
    int n;
    bool _valid;
  }
  struct _openingTimeTuple {
    uint a;
    bool _valid;
  }
  struct BalanceOfTuple {
    int n;
    bool _valid;
  }
  struct _capTuple {
    int a;
    bool _valid;
  }
  struct _finalizedTuple {
    bool b;
    bool _valid;
  }
  struct _primaryTuple {
    address p;
    bool _valid;
  }
  struct _goalTuple {
    int a;
    bool _valid;
  }
  struct _weiRaisedTuple {
    int a;
    bool _valid;
  }
  struct _closingTimeTuple {
    uint a;
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
  struct StateTuple {
    int a;
    bool _valid;
  }
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  _goalTuple _goal;
  _weiRaisedTuple _weiRaised;
  _openingTimeTuple _openingTime;
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalSupplyTuple totalSupply;
  _capTuple _cap;
  StateTuple state;
  _finalizedTuple _finalized;
  _primaryTuple _primary;
  _closingTimeTuple _closingTime;
  mapping(address=>_depositsTuple) _deposits;
  event Withdraw(address p);
  event Deposit(address p,int a);
  event Burn(address p,int amount);
  event Transfer(address from,address to,int amount);
  event InvalidTx();
  event Finalize();
  event Mint(address p,int amount);
  event BuyToken(address p,int v);
  event TransferFrom(address from,address to,address spender,int amount);
  event ClaimRefund(address p);
  event TransferPrimary(address p);
  event IncreaseAllowance(address p,address s,int n);
  constructor(uint t1,uint t2,address p,int cap,int goal) public {
    update_walletOnInsertConstructor_r26(p);
    update_openingTimeOnInsertConstructor_r11(t1);
    update_capOnInsertConstructor_r51(cap,goal);
    update_closingTimeOnInsertConstructor_r61(t2);
    update_primaryOnInsertConstructor_r0();
    update_finalizedOnInsertConstructor_r40();
    update_goalOnInsertConstructor_r46(cap,goal);
    updateStateOnInsertConstructor_r12();
    updateTotalSupplyOnInsertConstructor_r29();
  }
  function claimRefund(address p) public    {
      bool r63 = updateClaimRefundOnInsertRecv_claimRefund_r63(p);
      if(r63==false) {
        revert("Rule condition failed");
      }
  }
  function get_deposits(address p) public view  returns (int) {
      int a = _deposits[p].a;
      return a;
  }
  function transferPrimary(address p) public    {
      bool r5 = updateTransferPrimaryOnInsertRecv_transferPrimary_r5(p);
      if(r5==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r58 = updateTransferOnInsertRecv_transfer_r58(from,to,amount);
      if(r58==false) {
        revert("Rule condition failed");
      }
  }
  function finalize() public    {
      bool r27 = updateFinalizeOnInsertRecv_finalize_r27();
      if(r27==false) {
        revert("Rule condition failed");
      }
  }
  function buyToken(address p,int v) public    {
      bool r38 = updateBuyTokenOnInsertRecv_buyToken_r38(p,v);
      if(r38==false) {
        revert("Rule condition failed");
      }
  }
  function withdraw(address p) public    {
      bool r44 = updateWithdrawOnInsertRecv_withdraw_r44(p);
      if(r44==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r49 = updateBurnOnInsertRecv_burn_r49(p,amount);
      if(r49==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r22 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r22(p,s,n);
      if(r22==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r1 = updateTransferFromOnInsertRecv_transferFrom_r1(from,to,spender,amount);
      if(r1==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function mint(address p,int amount) public    {
      bool r4 = updateMintOnInsertRecv_mint_r4(p,amount);
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function deposit(address p,int a) public    {
      bool r25 = updateDepositOnInsertRecv_deposit_r25(p,a);
      if(r25==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function updateBuyTokenOnInsertRecv_buyToken_r38(address p,int v) private   returns (bool) {
      int c_1 = _cap.a;
      uint t_5 = block.timestamp;
      bool b_4 = _finalized.b;
      int h_3 = state.a;
      int r_1 = _weiRaised.a;
      uint o_2 = _openingTime.a;
      uint t_2 = block.timestamp;
      uint c_5 = _closingTime.a;
      int balanceOf_x1 = balanceOf[p].n;
      if(balanceOf_x1>0 && p!=address(0) && v!=0 && t_2>=o_2 && r_1<=c_1 && b_4!=true && h_3==0 && t_5<=c_5) {
        updateDepositOnInsertBuyToken_r17(p,v);
        updateRaisedOnInsertBuyToken_r30(v);
        updateMintOnInsertBuyToken_r21(p,v);
        emit BuyToken(p,v);
        return true;
      }
      return false;
  }
  function update_goalOnInsertConstructor_r46(int n,int a) private    {
      if(a<=n) {
        _goal = _goalTuple(a,true);
      }
  }
  function updateBurnOnInsertRecv_burn_r49(address p,int n) private   returns (bool) {
      int m_1 = balanceOf[p].n;
      if(p!=address(0) && n<=m_1 && n>0) {
        updateTotalBurnOnInsertBurn_r16(p,n);
        updateAllBurnOnInsertBurn_r34(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateMintOnInsertBuyToken_r21(address p,int n) private    {
      updateTotalMintOnInsertMint_r65(p,n);
      updateAllMintOnInsertMint_r7(n);
      emit Mint(p,n);
  }
  function updateTotalSupplyOnInsertConstructor_r29() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function update_walletOnInsertConstructor_r26(address p) private    {
      // Empty()
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r22(address o,address s,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[s].n;
      if(0!=balanceOf_x1) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r64(o,s,n);
        emit IncreaseAllowance(o,s,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r70(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateRaisedOnInsertBuyToken_r30(int a) private    {
      update_weiRaisedOnInsertRaised_r54(a);
  }
  function updateBalanceOfOnIncrementTotalIn_r3(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateSpentTotalOnInsertTransferFrom_r67(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r70(o,s,delta0);
  }
  function updateTransferPrimaryOnInsertRecv_transferPrimary_r5(address p) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(balanceOf_x1>0) {
        update_primaryOnInsertTransferPrimary_r2(p);
        emit TransferPrimary(p);
        return true;
      }
      return false;
  }
  function updateStateOnInsertFinalize_r36() private    {
      int n = _weiRaised.a;
      int a = _goal.a;
      if(n<a) {
        state = StateTuple(1,true);
      }
  }
  function update_finalizedOnInsertFinalize_r41() private    {
      _finalized = _finalizedTuple(true,true);
  }
  function updateFinalizeOnInsertRecv_finalize_r27() private   returns (bool) {
      bool b_2 = _finalized.b;
      address p_0 = _primary.p;
      uint c_3 = _closingTime.a;
      int h_1 = state.a;
      uint t_3 = block.timestamp;
      address s_0 = msg.sender;
      if(s_0==p_0 && h_1==0 && b_2!=true && t_3>=c_3) {
        update_finalizedOnInsertFinalize_r41();
        updateStateOnInsertFinalize_r75();
        updateStateOnInsertFinalize_r36();
        emit Finalize();
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r3(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateStateOnInsertConstructor_r12() private    {
      state = StateTuple(0,true);
  }
  function update_depositsOnInsertDeposit_r39(address p,int n) private    {
      _deposits[p].a += n;
  }
  function update_closingTimeOnInsertConstructor_r61(uint t2) private    {
      _closingTime = _closingTimeTuple(t2,true);
  }
  function updateBalanceOfOnIncrementTotalBurn_r3(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateTransferOnInsertRecv_transfer_r58(address s,address r,int n) private   returns (bool) {
      int m_1 = balanceOf[s].n;
      if(r!=address(0) && s!=address(0) && n<=m_1 && n>0) {
        updateTotalInOnInsertTransfer_r15(r,n);
        updateTotalOutOnInsertTransfer_r72(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r1(address o,address r,address sp,int n) private   returns (bool) {
      int k_2 = allowance[o][sp].n;
      int m_0 = balanceOf[o].n;
      if(r!=address(0) && n<=k_2 && n<=m_0 && o!=address(0) && n>0) {
        updateSpentTotalOnInsertTransferFrom_r67(o,sp,n);
        updateTransferOnInsertTransferFrom_r53(o,r,n);
        emit TransferFrom(o,r,sp,n);
        return true;
      }
      return false;
  }
  function update_capOnInsertConstructor_r51(int n,int a) private    {
      if(a<=n) {
        _cap = _capTuple(n,true);
      }
  }
  function updateAllBurnOnInsertBurn_r34(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r35(delta0);
  }
  function updateDepositOnInsertBuyToken_r17(address p,int n) private    {
      update_depositsOnInsertDeposit_r39(p,n);
      emit Deposit(p,n);
  }
  function update_primaryOnInsertTransferPrimary_r2(address p) private    {
      _primary = _primaryTuple(p,true);
  }
  function update_finalizedOnInsertConstructor_r40() private    {
      _finalized = _finalizedTuple(false,true);
  }
  function updateTotalOutOnInsertTransfer_r72(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r3(p,delta0);
  }
  function updateMintOnInsertRecv_mint_r4(address p,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(p!=address(0) && balanceOf_x1>0) {
        updateTotalMintOnInsertMint_r65(p,n);
        updateAllMintOnInsertMint_r7(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r35(int b) private    {
      totalSupply.n -= b;
  }
  function updateDepositOnInsertRecv_deposit_r25(address p,int a) private   returns (bool) {
      int h_1 = state.a;
      int balanceOf_x1 = balanceOf[p].n;
      if(a!=0 && p!=address(0) && h_1==0 && balanceOf_x1>0) {
        update_depositsOnInsertDeposit_r39(p,a);
        emit Deposit(p,a);
        return true;
      }
      return false;
  }
  function updateClaimRefundOnInsertRecv_claimRefund_r63(address p) private   returns (bool) {
      int r_0 = _weiRaised.a;
      bool b_3 = _finalized.b;
      int h_2 = state.a;
      int g_0 = _goal.a;
      int a_1 = _deposits[p].a;
      if(r_0<g_0 && a_1!=0 && h_2==1 && b_3!=false) {
        emit ClaimRefund(p);
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
  function update_primaryOnInsertConstructor_r0() private    {
      address s = msg.sender;
      _primary = _primaryTuple(s,true);
  }
  function updateTotalMintOnInsertMint_r65(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r3(p,delta0);
  }
  function update_depositsOnInsertWithdraw_r59(address p) private    {
      int a = 0;
      _deposits[p] = _depositsTuple(a,true);
  }
  function updateTotalBurnOnInsertBurn_r16(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r3(p,delta0);
  }
  function updateAllowanceOnIncrementSpentTotal_r70(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r64(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r70(o,s,delta0);
  }
  function updateBalanceOfOnIncrementTotalMint_r3(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateWithdrawOnInsertRecv_withdraw_r44(address p) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(balanceOf_x1>0) {
        update_depositsOnInsertWithdraw_r59(p);
        emit Withdraw(p);
        return true;
      }
      return false;
  }
  function updateStateOnInsertFinalize_r75() private    {
      int n = _weiRaised.a;
      int a = _goal.a;
      if(n>=a) {
        state = StateTuple(2,true);
      }
  }
  function updateAllMintOnInsertMint_r7(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r35(delta0);
  }
  function update_weiRaisedOnInsertRaised_r54(int n) private    {
      _weiRaised.a += n;
  }
  function updateTransferOnInsertTransferFrom_r53(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r15(r,n);
      updateTotalOutOnInsertTransfer_r72(o,n);
      emit Transfer(o,r,n);
  }
  function update_openingTimeOnInsertConstructor_r11(uint t1) private    {
      _openingTime = _openingTimeTuple(t1,true);
  }
  function updateTotalInOnInsertTransfer_r15(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r3(p,delta0);
  }
  function updateTotalSupplyOnIncrementAllMint_r35(int m) private    {
      totalSupply.n += m;
  }
}