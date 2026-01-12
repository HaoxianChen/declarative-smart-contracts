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
    update_finalizedOnInsertConstructor_r37();
    updateTotalSupplyOnInsertConstructor_r24();
    update_capOnInsertConstructor_r47(cap,goal);
    update_openingTimeOnInsertConstructor_r9(t1);
    updateMintOnInsertConstructor_r36();
    update_primaryOnInsertConstructor_r1();
    update_goalOnInsertConstructor_r43(cap,goal);
    updateStateOnInsertConstructor_r10();
    update_closingTimeOnInsertConstructor_r60(t2);
    update_walletOnInsertConstructor_r21(p);
  }
  function transfer(address from,address to,int amount) public    {
      bool r56 = updateTransferOnInsertRecv_transfer_r56(from,to,amount);
      if(r56==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r2 = updateBurnOnInsertRecv_burn_r2(p,amount);
      if(r2==false) {
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
  function buyToken(address p,int v) public    {
      bool r74 = updateBuyTokenOnInsertRecv_buyToken_r74(p,v);
      if(r74==false) {
        revert("Rule condition failed");
      }
  }
  function withdraw(address p) public    {
      bool r59 = updateWithdrawOnInsertRecv_withdraw_r59(p);
      if(r59==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r66 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r66(p,s,n);
      if(r66==false) {
        revert("Rule condition failed");
      }
  }
  function finalize() public    {
      bool r22 = updateFinalizeOnInsertRecv_finalize_r22();
      if(r22==false) {
        revert("Rule condition failed");
      }
  }
  function claimRefund(address p) public    {
      bool r62 = updateClaimRefundOnInsertRecv_claimRefund_r62(p);
      if(r62==false) {
        revert("Rule condition failed");
      }
  }
  function deposit(address p,int a) public    {
      bool r27 = updateDepositOnInsertRecv_deposit_r27(p,a);
      if(r27==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function mint(address p,int amount) public    {
      bool r49 = updateMintOnInsertRecv_mint_r49(p,amount);
      if(r49==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function transferPrimary(address p) public    {
      bool r38 = updateTransferPrimaryOnInsertRecv_transferPrimary_r38(p);
      if(r38==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r30 = updateTransferFromOnInsertRecv_transferFrom_r30(from,to,spender,amount);
      if(r30==false) {
        revert("Rule condition failed");
      }
  }
  function updateTotalSupplyOnIncrementAllBurn_r32(int b) private    {
      totalSupply.n -= b;
  }
  function updateTotalBurnOnInsertBurn_r14(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r50(p,delta0);
  }
  function updateDepositOnInsertBuyToken_r15(address p,int n) private    {
      update_depositsOnInsertDeposit_r3(p,n);
      emit Deposit(p,n);
  }
  function update_capOnInsertConstructor_r47(int n,int a) private    {
      if(a<=n) {
        _cap = _capTuple(n,true);
      }
  }
  function updateBalanceOfOnIncrementTotalMint_r50(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateTotalMintOnInsertMint_r64(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r50(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalBurn_r50(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateDepositOnInsertRecv_deposit_r27(address p,int a) private   returns (bool) {
      int h_1 = state.a;
      if(a!=0 && p!=address(0) && h_1==0) {
        update_depositsOnInsertDeposit_r3(p,a);
        emit Deposit(p,a);
        return true;
      }
      return false;
  }
  function update_goalOnInsertConstructor_r43(int n,int a) private    {
      if(a<=n) {
        _goal = _goalTuple(a,true);
      }
  }
  function updateTotalSupplyOnInsertConstructor_r24() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateAllMintOnInsertMint_r5(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r32(delta0);
  }
  function updateStateOnInsertFinalize_r76() private    {
      int n = _weiRaised.a;
      int a = _goal.a;
      if(n>=a) {
        state = StateTuple(2,true);
      }
  }
  function update_depositsOnInsertWithdraw_r57(address p) private    {
      int a = 0;
      _deposits[p] = _depositsTuple(a,true);
  }
  function update_finalizedOnInsertConstructor_r37() private    {
      _finalized = _finalizedTuple(false,true);
  }
  function update_primaryOnInsertTransferPrimary_r39(address p) private    {
      _primary = _primaryTuple(p,true);
  }
  function update_finalizedOnInsertFinalize_r35() private    {
      _finalized = _finalizedTuple(true,true);
  }
  function updateStateOnInsertConstructor_r10() private    {
      state = StateTuple(0,true);
  }
  function update_walletOnInsertConstructor_r21(address p) private    {
      // Empty()
  }
  function update_openingTimeOnInsertConstructor_r9(uint t1) private    {
      _openingTime = _openingTimeTuple(t1,true);
  }
  function updateBurnOnInsertRecv_burn_r2(address p,int amount) private   returns (bool) {
      int m_1 = balanceOf[p].n;
      if(p!=address(0) && n<=m_1) {
        updateAllBurnOnInsertBurn_r31(n);
        updateTotalBurnOnInsertBurn_r14(p,n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateBuyTokenOnInsertRecv_buyToken_r74(address p,int v) private   returns (bool) {
      uint t_5 = block.timestamp;
      bool b_4 = _finalized.b;
      address msgSender = msg.sender;
      int h_3 = state.a;
      int r_1 = _weiRaised.a;
      uint o_2 = _openingTime.a;
      uint t_2 = block.timestamp;
      uint c_5 = _closingTime.a;
      int c_1 = _cap.a;
      int balanceOf_x1 = balanceOf[msgSender].n;
      if(p!=address(0) && v!=0 && v>balanceOf_x1 && t_2>=o_2 && r_1<=c_1 && b_4!=true && h_3==0 && t_5<=c_5) {
        updateMintOnInsertBuyToken_r19(p,v);
        updateRaisedOnInsertBuyToken_r25(v);
        updateDepositOnInsertBuyToken_r15(p,v);
        emit BuyToken(p,v);
        return true;
      }
      return false;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r66(address p,address s,int n) private   returns (bool) {
      updateAllowanceTotalOnInsertIncreaseAllowance_r63(o,s,n);
      emit IncreaseAllowance(o,s,n);
      return true;
      return false;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r30(address from,address to,address spender,int amount) private   returns (bool) {
      int k_2 = allowance[o][sp].n;
      int m_0 = balanceOf[o].n;
      if(n<=m_0 && r!=address(0) && o!=address(0) && n<=k_2) {
        updateTransferOnInsertTransferFrom_r51(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r67(o,sp,n);
        emit TransferFrom(o,r,sp,n);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertTransferFrom_r51(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r72(o,n);
      updateTotalInOnInsertTransfer_r13(r,n);
      emit Transfer(o,r,n);
  }
  function updateFinalizeOnInsertRecv_finalize_r22() private   returns (bool) {
      bool b_2 = _finalized.b;
      address p_0 = _primary.p;
      uint c_3 = _closingTime.a;
      int h_1 = state.a;
      uint t_3 = block.timestamp;
      address s_0 = msg.sender;
      if(s_0==p_0 && h_1==0 && b_2!=true && t_3>=c_3) {
        updateStateOnInsertFinalize_r76();
        updateStateOnInsertFinalize_r33();
        update_finalizedOnInsertFinalize_r35();
        emit Finalize();
        return true;
      }
      return false;
  }
  function update_primaryOnInsertConstructor_r1() private    {
      address s = msg.sender;
      _primary = _primaryTuple(s,true);
  }
  function updateTransferPrimaryOnInsertRecv_transferPrimary_r38(address p) private   returns (bool) {
      int _goal_a = _goal.a;
      if(_goal_a>=0) {
        update_primaryOnInsertTransferPrimary_r39(p);
        emit TransferPrimary(p);
        return true;
      }
      return false;
  }
  function updateMintOnInsertConstructor_r36() private    {
      address s = msg.sender;
      updateTotalMintOnInsertMint_r64(s,int(1));
      updateAllMintOnInsertMint_r5(int(1));
      emit Mint(s,1);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r70(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function update_weiRaisedOnInsertRaised_r52(int n) private    {
      _weiRaised.a += n;
  }
  function updateWithdrawOnInsertRecv_withdraw_r59(address p) private   returns (bool) {
      update_depositsOnInsertWithdraw_r57(p);
      emit Withdraw(p);
      return true;
      return false;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function update_closingTimeOnInsertConstructor_r60(uint t2) private    {
      _closingTime = _closingTimeTuple(t2,true);
  }
  function updateAllowanceOnIncrementSpentTotal_r70(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateMintOnInsertRecv_mint_r49(address p,int amount) private   returns (bool) {
      if(p!=address(0)) {
        updateAllMintOnInsertMint_r5(n);
        updateTotalMintOnInsertMint_r64(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateAllBurnOnInsertBurn_r31(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r32(delta0);
  }
  function updateRaisedOnInsertBuyToken_r25(int a) private    {
      update_weiRaisedOnInsertRaised_r52(a);
  }
  function updateBalanceOfOnIncrementTotalOut_r50(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateTransferOnInsertRecv_transfer_r56(address from,address to,int amount) private   returns (bool) {
      int m_1 = balanceOf[s].n;
      if(r!=address(0) && s!=address(0) && n<=m_1 && n>0) {
        updateTotalOutOnInsertTransfer_r72(s,n);
        updateTotalInOnInsertTransfer_r13(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalIn_r50(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateStateOnInsertFinalize_r33() private    {
      int n = _weiRaised.a;
      int a = _goal.a;
      if(n<a) {
        state = StateTuple(1,true);
      }
  }
  function updateSpentTotalOnInsertTransferFrom_r67(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r70(o,s,delta0);
  }
  function updateTotalSupplyOnIncrementAllMint_r32(int m) private    {
      totalSupply.n += m;
  }
  function updateMintOnInsertBuyToken_r19(address p,int n) private    {
      updateAllMintOnInsertMint_r5(n);
      updateTotalMintOnInsertMint_r64(p,n);
      emit Mint(p,n);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r63(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r70(o,s,delta0);
  }
  function updateClaimRefundOnInsertRecv_claimRefund_r62(address p) private   returns (bool) {
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
  function updateTotalInOnInsertTransfer_r13(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r50(p,delta0);
  }
  function updateTotalOutOnInsertTransfer_r72(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r50(p,delta0);
  }
  function update_depositsOnInsertDeposit_r3(address p,int n) private    {
      _deposits[p].a += n;
  }
}