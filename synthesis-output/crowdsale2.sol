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
    update_goalOnInsertConstructor_r50(cap,goal);
    updateStateOnInsertConstructor_r13();
    update_capOnInsertConstructor_r55(cap,goal);
    update_primaryOnInsertConstructor_r1();
    updateTotalSupplyOnInsertConstructor_r30();
    update_walletOnInsertConstructor_r27(p);
    updateMintOnInsertConstructor_r42();
    update_closingTimeOnInsertConstructor_r67(t2);
    update_finalizedOnInsertConstructor_r43();
    update_openingTimeOnInsertConstructor_r12(t1);
  }
  function transferPrimary(address p) public    {
      bool r15 = updateTransferPrimaryOnInsertRecv_transferPrimary_r15(p);
      if(r15==false) {
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
  function claimRefund(address p) public    {
      bool r56 = updateClaimRefundOnInsertRecv_claimRefund_r56(p);
      if(r56==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r72 = updateMintOnInsertRecv_mint_r72(p,amount);
      if(r72==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r35 = updateTransferFromOnInsertRecv_transferFrom_r35(from,to,spender,amount);
      if(r35==false) {
        revert("Rule condition failed");
      }
  }
  function buyToken(address p,int v) public    {
      bool r16 = updateBuyTokenOnInsertRecv_buyToken_r16(p,v);
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function withdraw(address p) public    {
      bool r47 = updateWithdrawOnInsertRecv_withdraw_r47(p);
      if(r47==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r36 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r36(p,s,n);
      if(r36==false) {
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
  function finalize() public    {
      bool r28 = updateFinalizeOnInsertRecv_finalize_r28();
      if(r28==false) {
        revert("Rule condition failed");
      }
  }
  function deposit(address p,int a) public    {
      bool r40 = updateDepositOnInsertRecv_deposit_r40(p,a);
      if(r40==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r64 = updateTransferOnInsertRecv_transfer_r64(from,to,amount);
      if(r64==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r53 = updateBurnOnInsertRecv_burn_r53(p,amount);
      if(r53==false) {
        revert("Rule condition failed");
      }
  }
  function update_depositsOnInsertDeposit_r39(address p,int n) private    {
      _deposits[p].a += n;
  }
  function updateDepositOnInsertRecv_deposit_r40(address p,int a) private   returns (bool) {
      address s_1 = msg.sender;
      int h_2 = state.a;
      int _goal_a = _goal.a;
      address p_1 = _primary.p;
      if(0==_goal_a && p!=address(0) && s_1==p_1 && a!=0 && h_2==0) {
        update_depositsOnInsertDeposit_r39(p,a);
        emit Deposit(p,a);
        return true;
      }
      return false;
  }
  function updateTotalBurnOnInsertBurn_r17(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r4(p,delta0);
  }
  function updateTransferPrimaryOnInsertRecv_transferPrimary_r15(address p) private   returns (bool) {
      int _goal_a = _goal.a;
      address p_1 = _primary.p;
      address s_1 = msg.sender;
      if(p!=address(0) && s_1==p_1 && _goal_a>=0) {
        update_primaryOnInsertTransferPrimary_r45(p);
        emit TransferPrimary(p);
        return true;
      }
      return false;
  }
  function updateTotalInOnInsertTransfer_r11(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r4(p,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalMintOnInsertMint_r70(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r4(p,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function update_capOnInsertConstructor_r55(int n,int a) private    {
      if(a<=n) {
        _cap = _capTuple(n,true);
      }
  }
  function updateTotalOutOnInsertTransfer_r79(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r4(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalBurn_r4(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function update_openingTimeOnInsertConstructor_r12(uint t1) private    {
      _openingTime = _openingTimeTuple(t1,true);
  }
  function updateMintOnInsertRecv_mint_r72(address p,int amount) private   returns (bool) {
      address msgSender = msg.sender;
      int _goal_a_1 = _goal.a;
      int balanceOf_x1_0 = balanceOf[msgSender].n;
      if(p!=address(0) && n>balanceOf_x1_0 && 0==_goal_a_1) {
        updateTotalMintOnInsertMint_r70(p,n);
        updateAllMintOnInsertMint_r7(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r76(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function update_depositsOnInsertWithdraw_r65(address p) private    {
      int a = 0;
      _deposits[p] = _depositsTuple(a,true);
  }
  function updateBuyTokenOnInsertRecv_buyToken_r16(address p,int v) private   returns (bool) {
      uint t_5 = block.timestamp;
      bool b_4 = _finalized.b;
      int h_3 = state.a;
      int r_1 = _weiRaised.a;
      uint o_2 = _openingTime.a;
      uint t_2 = block.timestamp;
      uint c_5 = _closingTime.a;
      int c_1 = _cap.a;
      if(p!=address(0) && v!=0 && r_1+v<=c_1 && t_2>=o_2 && b_4!=true && h_3==0 && t_5<=c_5) {
        updateMintOnInsertBuyToken_r25(p,v);
        updateRaisedOnInsertBuyToken_r31(v);
        updateDepositOnInsertBuyToken_r18(p,v);
        emit BuyToken(p,v);
        return true;
      }
      return false;
  }
  function updateMintOnInsertConstructor_r42() private    {
      address s = msg.sender;
      updateAllMintOnInsertMint_r7(int(1));
      updateTotalMintOnInsertMint_r70(s,int(1));
      emit Mint(s,1);
  }
  function updateBalanceOfOnIncrementTotalMint_r4(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function update_walletOnInsertConstructor_r27(address p) private    {
      // Empty()
  }
  function updateAllMintOnInsertMint_r7(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r38(delta0);
  }
  function updateWithdrawOnInsertRecv_withdraw_r47(address p) private   returns (bool) {
      int _goal_a = _goal.a;
      if(p==_primary.p) {
        address s = msg.sender;
        if(s==p && 0==_goal_a) {
          update_depositsOnInsertWithdraw_r65(p);
          emit Withdraw(p);
          return true;
        }
      }
      return false;
  }
  function updateDepositOnInsertBuyToken_r18(address p,int n) private    {
      update_depositsOnInsertDeposit_r39(p,n);
      emit Deposit(p,n);
  }
  function updateBalanceOfOnIncrementTotalOut_r4(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function update_weiRaisedOnInsertRaised_r59(int n) private    {
      _weiRaised.a += n;
  }
  function updateStateOnInsertConstructor_r13() private    {
      state = StateTuple(0,true);
  }
  function updateTotalSupplyOnIncrementAllBurn_r38(int b) private    {
      totalSupply.n -= b;
  }
  function updateBurnOnInsertRecv_burn_r53(address p,int amount) private   returns (bool) {
      int m_1 = balanceOf[p].n;
      if(p!=address(0) && n<=m_1 && n>0) {
        updateTotalBurnOnInsertBurn_r17(p,n);
        updateAllBurnOnInsertBurn_r37(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateMintOnInsertBuyToken_r25(address p,int n) private    {
      updateTotalMintOnInsertMint_r70(p,n);
      updateAllMintOnInsertMint_r7(n);
      emit Mint(p,n);
  }
  function update_primaryOnInsertConstructor_r1() private    {
      address s = msg.sender;
      _primary = _primaryTuple(s,true);
  }
  function updateStateOnInsertFinalize_r82() private    {
      int n = _weiRaised.a;
      int a = _goal.a;
      if(n>=a) {
        state = StateTuple(2,true);
      }
  }
  function updateTotalSupplyOnInsertConstructor_r30() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateTotalSupplyOnIncrementAllMint_r38(int m) private    {
      totalSupply.n += m;
  }
  function updateSpentTotalOnInsertTransferFrom_r73(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r76(o,s,delta0);
  }
  function updateClaimRefundOnInsertRecv_claimRefund_r56(address p) private   returns (bool) {
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
  function updateTransferOnInsertRecv_transfer_r64(address from,address to,int amount) private   returns (bool) {
      int m_1 = balanceOf[s].n;
      if(r!=address(0) && s!=address(0) && n<=m_1 && n>0) {
        updateTotalInOnInsertTransfer_r11(r,n);
        updateTotalOutOnInsertTransfer_r79(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementSpentTotal_r76(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function update_primaryOnInsertTransferPrimary_r45(address p) private    {
      _primary = _primaryTuple(p,true);
  }
  function updateStateOnInsertFinalize_r3() private    {
      int n = _weiRaised.a;
      int a = _goal.a;
      if(n<a) {
        state = StateTuple(1,true);
      }
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r69(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r76(o,s,delta0);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r36(address p,address s,int n) private   returns (bool) {
      int _goal_a = _goal.a;
      if(d>=0 && 0==_goal_a) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r69(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertTransferFrom_r58(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r11(r,n);
      updateTotalOutOnInsertTransfer_r79(o,n);
      emit Transfer(o,r,n);
  }
  function update_finalizedOnInsertConstructor_r43() private    {
      _finalized = _finalizedTuple(false,true);
  }
  function updateBalanceOfOnIncrementTotalIn_r4(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function update_goalOnInsertConstructor_r50(int n,int a) private    {
      if(a<=n) {
        _goal = _goalTuple(a,true);
      }
  }
  function update_finalizedOnInsertFinalize_r41() private    {
      _finalized = _finalizedTuple(true,true);
  }
  function update_closingTimeOnInsertConstructor_r67(uint t2) private    {
      _closingTime = _closingTimeTuple(t2,true);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r35(address from,address to,address spender,int amount) private   returns (bool) {
      int k_2 = allowance[o][sp].n;
      int m_0 = balanceOf[o].n;
      if(r!=address(0) && sp!=address(0) && n<=k_2 && n<=m_0 && o!=address(0) && n>0) {
        updateTransferOnInsertTransferFrom_r58(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r73(o,sp,n);
        emit TransferFrom(o,r,sp,n);
        return true;
      }
      return false;
  }
  function updateFinalizeOnInsertRecv_finalize_r28() private   returns (bool) {
      bool b_2 = _finalized.b;
      address p_0 = _primary.p;
      uint c_3 = _closingTime.a;
      int h_1 = state.a;
      uint t_3 = block.timestamp;
      address s_0 = msg.sender;
      if(s_0==p_0 && h_1==0 && b_2!=true && t_3>=c_3) {
        updateStateOnInsertFinalize_r82();
        update_finalizedOnInsertFinalize_r41();
        updateStateOnInsertFinalize_r3();
        emit Finalize();
        return true;
      }
      return false;
  }
  function updateRaisedOnInsertBuyToken_r31(int a) private    {
      update_weiRaisedOnInsertRaised_r59(a);
  }
  function updateAllBurnOnInsertBurn_r37(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r38(delta0);
  }
}