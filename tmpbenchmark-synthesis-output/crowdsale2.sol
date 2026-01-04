contract Crowdsale2 {
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
    updateStateOnInsertConstructor_r14();
    update_goalOnInsertConstructor_r47(cap,goal);
    update_closingTimeOnInsertConstructor_r65(t2);
    updateTotalSupplyOnInsertConstructor_r29();
    update_openingTimeOnInsertConstructor_r13(t1);
    update_primaryOnInsertConstructor_r37();
    update_finalizedOnInsertConstructor_r40();
    update_capOnInsertConstructor_r52(cap,goal);
    update_walletOnInsertConstructor_r26(p);
  }
  function get_deposits(address p) public view  returns (int) {
      int a = _deposits[p].a;
      return a;
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function mint(address p,int amount) public    {
      bool r5 = updateMintOnInsertRecv_mint_r5(p,amount);
      if(r5==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r35 = updateTransferFromOnInsertRecv_transferFrom_r35(from,to,spender,amount);
      if(r35==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r50 = updateBurnOnInsertRecv_burn_r50(p,amount);
      if(r50==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r58 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r58(p,s,n);
      if(r58==false) {
        revert("Rule condition failed");
      }
  }
  function transferPrimary(address p) public    {
      bool r36 = updateTransferPrimaryOnInsertRecv_transferPrimary_r36(p);
      if(r36==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function buyToken(address p,int v) public    {
      bool r78 = updateBuyTokenOnInsertRecv_buyToken_r78(p,v);
      if(r78==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function claimRefund(address p) public    {
      bool r53 = updateClaimRefundOnInsertRecv_claimRefund_r53(p);
      if(r53==false) {
        revert("Rule condition failed");
      }
  }
  function deposit(address p,int a) public    {
      bool r30 = updateDepositOnInsertRecv_deposit_r30(p,a);
      if(r30==false) {
        revert("Rule condition failed");
      }
  }
  function finalize() public    {
      bool r27 = updateFinalizeOnInsertRecv_finalize_r27();
      if(r27==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r62 = updateTransferOnInsertRecv_transfer_r62(from,to,amount);
      if(r62==false) {
        revert("Rule condition failed");
      }
  }
  function withdraw(address p) public    {
      bool r68 = updateWithdrawOnInsertRecv_withdraw_r68(p);
      if(r68==false) {
        revert("Rule condition failed");
      }
  }
  function updateTotalOutOnInsertTransfer_r77(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r4(p,delta0);
  }
  function update_capOnInsertConstructor_r52(int n,int a) private    {
      if(a<=n) {
        _cap = _capTuple(n,true);
      }
  }
  function updateTransferOnInsertTransferFrom_r55(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r12(r,n);
      updateTotalOutOnInsertTransfer_r77(o,n);
      emit Transfer(o,r,n);
  }
  function updateAllMintOnInsertMint_r8(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r39(delta0);
  }
  function updateTotalSupplyOnIncrementAllBurn_r39(int b) private    {
      totalSupply.n -= b;
  }
  function updateRaisedOnInsertBuyToken_r31(int a) private    {
      update_weiRaisedOnInsertRaised_r56(a);
  }
  function updateBuyTokenOnInsertRecv_buyToken_r78(address p,int v) private   returns (bool) {
      int c_1 = _cap.a;
      uint t_5 = block.timestamp;
      bool b_4 = _finalized.b;
      int h_3 = state.a;
      int r_1 = _weiRaised.a;
      uint o_2 = _openingTime.a;
      uint t_2 = block.timestamp;
      uint c_5 = _closingTime.a;
      int balanceOf_x1 = balanceOf[p].n;
      if(balanceOf_x1>0 && p!=address(0) && v!=0 && r_1+v<=c_1 && t_2>=o_2 && b_4!=true && h_3==0 && t_5<=c_5) {
        updateMintOnInsertBuyToken_r24(p,v);
        updateDepositOnInsertBuyToken_r17(p,v);
        updateRaisedOnInsertBuyToken_r31(v);
        emit BuyToken(p,v);
        return true;
      }
      return false;
  }
  function updateTotalBurnOnInsertBurn_r16(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r4(p,delta0);
  }
  function update_closingTimeOnInsertConstructor_r65(uint t2) private    {
      _closingTime = _closingTimeTuple(t2,true);
  }
  function updateBalanceOfOnIncrementTotalIn_r4(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTotalSupplyOnIncrementAllMint_r39(int m) private    {
      totalSupply.n += m;
  }
  function updateStateOnInsertFinalize_r2() private    {
      int n = _weiRaised.a;
      int a = _goal.a;
      if(n<a) {
        state = StateTuple(1,true);
      }
  }
  function update_openingTimeOnInsertConstructor_r13(uint t1) private    {
      _openingTime = _openingTimeTuple(t1,true);
  }
  function updateTotalSupplyOnInsertConstructor_r29() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateBalanceOfOnIncrementTotalMint_r4(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateDepositOnInsertRecv_deposit_r30(address p,int a) private   returns (bool) {
      address s_1 = msg.sender;
      int h_2 = state.a;
      address p_1 = _primary.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(balanceOf_x1>0 && p!=address(0) && s_1==p_1 && a!=0 && h_2==0) {
        update_depositsOnInsertDeposit_r3(p,a);
        emit Deposit(p,a);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r4(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateAllowanceOnIncrementSpentTotal_r74(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function update_walletOnInsertConstructor_r26(address p) private    {
      // Empty()
  }
  function updateStateOnInsertConstructor_r14() private    {
      state = StateTuple(0,true);
  }
  function update_primaryOnInsertTransferPrimary_r43(address p) private    {
      _primary = _primaryTuple(p,true);
  }
  function update_finalizedOnInsertConstructor_r40() private    {
      _finalized = _finalizedTuple(false,true);
  }
  function updateBurnOnInsertRecv_burn_r50(address p,int n) private   returns (bool) {
      int m_1 = balanceOf[p].n;
      if(p!=address(0) && n<=m_1 && n>0) {
        updateTotalBurnOnInsertBurn_r16(p,n);
        updateAllBurnOnInsertBurn_r38(n);
        emit Burn(p,n);
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
  function updateAllowanceTotalOnInsertIncreaseAllowance_r67(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r74(o,s,delta0);
  }
  function update_weiRaisedOnInsertRaised_r56(int n) private    {
      _weiRaised.a += n;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r74(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateTotalMintOnInsertMint_r69(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r4(p,delta0);
  }
  function update_primaryOnInsertConstructor_r37() private    {
      address s = msg.sender;
      _primary = _primaryTuple(s,true);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r58(address o,address s,int d) private   returns (bool) {
      int balanceOf_x1 = balanceOf[s].n;
      if(d>=0 && d<balanceOf_x1) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r67(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateTotalInOnInsertTransfer_r12(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r4(p,delta0);
  }
  function updateMintOnInsertBuyToken_r24(address p,int n) private    {
      updateTotalMintOnInsertMint_r69(p,n);
      updateAllMintOnInsertMint_r8(n);
      emit Mint(p,n);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateStateOnInsertFinalize_r81() private    {
      int n = _weiRaised.a;
      int a = _goal.a;
      if(n>=a) {
        state = StateTuple(2,true);
      }
  }
  function updateMintOnInsertRecv_mint_r5(address p,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(p!=address(0) && balanceOf_x1>0) {
        updateTotalMintOnInsertMint_r69(p,n);
        updateAllMintOnInsertMint_r8(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function update_depositsOnInsertWithdraw_r63(address p) private    {
      int a = 0;
      _deposits[p] = _depositsTuple(a,true);
  }
  function update_goalOnInsertConstructor_r47(int n,int a) private    {
      if(a<=n) {
        _goal = _goalTuple(a,true);
      }
  }
  function updateFinalizeOnInsertRecv_finalize_r27() private   returns (bool) {
      bool b_2 = _finalized.b;
      address p_0 = _primary.p;
      uint c_3 = _closingTime.a;
      int h_1 = state.a;
      uint t_3 = block.timestamp;
      address s_0 = msg.sender;
      if(s_0==p_0 && h_1==0 && b_2!=true && t_3>=c_3) {
        updateStateOnInsertFinalize_r2();
        updateStateOnInsertFinalize_r81();
        update_finalizedOnInsertFinalize_r41();
        emit Finalize();
        return true;
      }
      return false;
  }
  function updateAllBurnOnInsertBurn_r38(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r39(delta0);
  }
  function updateWithdrawOnInsertRecv_withdraw_r68(address p) private   returns (bool) {
      if(p==_primary.p) {
        address s = msg.sender;
        int balanceOf_x1 = balanceOf[p].n;
        if(s==p && balanceOf_x1>0) {
          update_depositsOnInsertWithdraw_r63(p);
          emit Withdraw(p);
          return true;
        }
      }
      return false;
  }
  function updateTransferOnInsertRecv_transfer_r62(address s,address r,int n) private   returns (bool) {
      int m_1 = balanceOf[s].n;
      if(r!=address(0) && s!=address(0) && n<=m_1 && n>0) {
        updateTotalOutOnInsertTransfer_r77(s,n);
        updateTotalInOnInsertTransfer_r12(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateDepositOnInsertBuyToken_r17(address p,int n) private    {
      update_depositsOnInsertDeposit_r3(p,n);
      emit Deposit(p,n);
  }
  function updateTransferPrimaryOnInsertRecv_transferPrimary_r36(address p) private   returns (bool) {
      address p_1 = _primary.p;
      address s_1 = msg.sender;
      int balanceOf_x1 = balanceOf[p].n;
      if(p!=address(0) && s_1==p_1 && balanceOf_x1>0) {
        update_primaryOnInsertTransferPrimary_r43(p);
        emit TransferPrimary(p);
        return true;
      }
      return false;
  }
  function update_depositsOnInsertDeposit_r3(address p,int n) private    {
      _deposits[p].a += n;
  }
  function updateBalanceOfOnIncrementTotalBurn_r4(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r35(address o,address r,address sp,int n) private   returns (bool) {
      int k_2 = allowance[o][sp].n;
      int m_0 = balanceOf[o].n;
      if(r!=address(0) && sp!=address(0) && n<=k_2 && n<=m_0 && o!=address(0) && n>0) {
        updateSpentTotalOnInsertTransferFrom_r71(o,sp,n);
        updateTransferOnInsertTransferFrom_r55(o,r,n);
        emit TransferFrom(o,r,sp,n);
        return true;
      }
      return false;
  }
  function update_finalizedOnInsertFinalize_r41() private    {
      _finalized = _finalizedTuple(true,true);
  }
  function updateSpentTotalOnInsertTransferFrom_r71(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r74(o,s,delta0);
  }
  function updateClaimRefundOnInsertRecv_claimRefund_r53(address p) private   returns (bool) {
      int h_2 = state.a;
      int s_1 = _weiRaised.a;
      bool b_3 = _finalized.b;
      int g_1 = _goal.a;
      int a_0 = _deposits[p].a;
      if(a_0!=0 && s_1<g_1 && h_2==1 && b_3!=false) {
        emit ClaimRefund(p);
        return true;
      }
      return false;
  }
}