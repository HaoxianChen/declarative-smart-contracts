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
    updateMintOnInsertConstructor_r41();
    updateStateOnInsertConstructor_r14();
    updateTotalSupplyOnInsertConstructor_r31();
    update_primaryOnInsertConstructor_r1();
    update_openingTimeOnInsertConstructor_r13(t1);
    update_closingTimeOnInsertConstructor_r66(t2);
    update_finalizedOnInsertConstructor_r42();
    update_capOnInsertConstructor_r53(cap,goal);
    update_goalOnInsertConstructor_r48(cap,goal);
    update_walletOnInsertConstructor_r28(p);
  }
  function transferPrimary(address p) public    {
      bool r27 = updateTransferPrimaryOnInsertRecv_transferPrimary_r27(p);
      if(r27==false) {
        revert("Rule condition failed");
      }
  }
  function get_deposits(address p) public view  returns (int) {
      int a = _deposits[p].a;
      return a;
  }
  function withdraw(address p) public    {
      bool r73 = updateWithdrawOnInsertRecv_withdraw_r73(p);
      if(r73==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r36 = updateMintOnInsertRecv_mint_r36(p,amount);
      if(r36==false) {
        revert("Rule condition failed");
      }
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
  function transfer(address from,address to,int amount) public    {
      bool r62 = updateTransferOnInsertRecv_transfer_r62(from,to,amount);
      if(r62==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r80 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r80(p,s,n);
      if(r80==false) {
        revert("Rule condition failed");
      }
  }
  function deposit(address p,int a) public    {
      bool r64 = updateDepositOnInsertRecv_deposit_r64(p,a);
      if(r64==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r37 = updateTransferFromOnInsertRecv_transferFrom_r37(from,to,spender,amount);
      if(r37==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r51 = updateBurnOnInsertRecv_burn_r51(p,amount);
      if(r51==false) {
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
  function buyToken(address p,int v) public    {
      bool r16 = updateBuyTokenOnInsertRecv_buyToken_r16(p,v);
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function claimRefund(address p) public    {
      bool r54 = updateClaimRefundOnInsertRecv_claimRefund_r54(p);
      if(r54==false) {
        revert("Rule condition failed");
      }
  }
  function updateMintOnInsertBuyToken_r25(address p,int n) private    {
      updateTotalMintOnInsertMint_r69(p,n);
      updateAllMintOnInsertMint_r8(n);
      emit Mint(p,n);
  }
  function updateBalanceOfOnIncrementTotalIn_r5(address p,int i) private    {
      balanceOf[p].n += i;
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
        updateRaisedOnInsertBuyToken_r32(v);
        updateMintOnInsertBuyToken_r25(p,v);
        updateDepositOnInsertBuyToken_r18(p,v);
        emit BuyToken(p,v);
        return true;
      }
      return false;
  }
  function updateAllMintOnInsertMint_r8(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r39(delta0);
  }
  function updateStateOnInsertFinalize_r82() private    {
      int n = _weiRaised.a;
      int a = _goal.a;
      if(n>=a) {
        state = StateTuple(2,true);
      }
  }
  function updateTotalSupplyOnInsertConstructor_r31() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function update_depositsOnInsertWithdraw_r63(address p) private    {
      int a = 0;
      _deposits[p] = _depositsTuple(a,true);
  }
  function updateClaimRefundOnInsertRecv_claimRefund_r54(address p) private   returns (bool) {
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
  function updateStateOnInsertFinalize_r3() private    {
      int n = _weiRaised.a;
      int a = _goal.a;
      if(n<a) {
        state = StateTuple(1,true);
      }
  }
  function updateBurnOnInsertRecv_burn_r51(address p,int amount) private   returns (bool) {
      int m_1 = balanceOf[p].n;
      if(p!=address(0) && n<=m_1 && n>0) {
        updateTotalBurnOnInsertBurn_r17(p,n);
        updateAllBurnOnInsertBurn_r38(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r5(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateAllowanceOnIncrementSpentTotal_r75(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateMintOnInsertRecv_mint_r36(address p,int amount) private   returns (bool) {
      int _goal_a = _goal.a;
      if(p!=address(0) && _goal_a>=0) {
        updateTotalMintOnInsertMint_r69(p,n);
        updateAllMintOnInsertMint_r8(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r39(int m) private    {
      totalSupply.n += m;
  }
  function updateSpentTotalOnInsertTransferFrom_r71(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r75(o,s,delta0);
  }
  function updateDepositOnInsertRecv_deposit_r64(address p,int a) private   returns (bool) {
      address s_1 = msg.sender;
      int h_2 = state.a;
      int _goal_a = _goal.a;
      address p_1 = _primary.p;
      if(p!=address(0) && s_1==p_1 && a!=0 && h_2==0 && _goal_a>0) {
        update_depositsOnInsertDeposit_r4(p,a);
        emit Deposit(p,a);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertTransferFrom_r56(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r12(r,n);
      updateTotalOutOnInsertTransfer_r78(o,n);
      emit Transfer(o,r,n);
  }
  function updateTotalMintOnInsertMint_r69(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r5(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalBurn_r5(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateMintOnInsertConstructor_r41() private    {
      address s = msg.sender;
      updateAllMintOnInsertMint_r8(int(1));
      updateTotalMintOnInsertMint_r69(s,int(1));
      emit Mint(s,1);
  }
  function updateRaisedOnInsertBuyToken_r32(int a) private    {
      update_weiRaisedOnInsertRaised_r57(a);
  }
  function update_weiRaisedOnInsertRaised_r57(int n) private    {
      _weiRaised.a += n;
  }
  function update_openingTimeOnInsertConstructor_r13(uint t1) private    {
      _openingTime = _openingTimeTuple(t1,true);
  }
  function updateFinalizeOnInsertRecv_finalize_r29() private   returns (bool) {
      bool b_2 = _finalized.b;
      address p_0 = _primary.p;
      uint c_3 = _closingTime.a;
      int h_1 = state.a;
      uint t_3 = block.timestamp;
      address s_0 = msg.sender;
      if(s_0==p_0 && h_1==0 && b_2!=true && t_3>=c_3) {
        update_finalizedOnInsertFinalize_r40();
        updateStateOnInsertFinalize_r82();
        updateStateOnInsertFinalize_r3();
        emit Finalize();
        return true;
      }
      return false;
  }
  function updateStateOnInsertConstructor_r14() private    {
      state = StateTuple(0,true);
  }
  function updateWithdrawOnInsertRecv_withdraw_r73(address p) private   returns (bool) {
      int _goal_a = _goal.a;
      if(p==_primary.p) {
        address s = msg.sender;
        if(s==p && _goal_a>0) {
          update_depositsOnInsertWithdraw_r63(p);
          emit Withdraw(p);
          return true;
        }
      }
      return false;
  }
  function update_goalOnInsertConstructor_r48(int n,int a) private    {
      if(a<=n) {
        _goal = _goalTuple(a,true);
      }
  }
  function update_depositsOnInsertDeposit_r4(address p,int n) private    {
      _deposits[p].a += n;
  }
  function updateTransferOnInsertRecv_transfer_r62(address from,address to,int amount) private   returns (bool) {
      int m_1 = balanceOf[s].n;
      if(r!=address(0) && s!=address(0) && n<=m_1 && n>0) {
        updateTotalOutOnInsertTransfer_r78(s,n);
        updateTotalInOnInsertTransfer_r12(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r37(address from,address to,address spender,int amount) private   returns (bool) {
      int k_2 = allowance[o][sp].n;
      int m_0 = balanceOf[o].n;
      if(r!=address(0) && sp!=address(0) && n<=k_2 && n<=m_0 && o!=address(0) && n>0) {
        updateSpentTotalOnInsertTransferFrom_r71(o,sp,n);
        updateTransferOnInsertTransferFrom_r56(o,r,n);
        emit TransferFrom(o,r,sp,n);
        return true;
      }
      return false;
  }
  function update_closingTimeOnInsertConstructor_r66(uint t2) private    {
      _closingTime = _closingTimeTuple(t2,true);
  }
  function update_primaryOnInsertConstructor_r1() private    {
      address s = msg.sender;
      _primary = _primaryTuple(s,true);
  }
  function updateTotalSupplyOnIncrementAllBurn_r39(int b) private    {
      totalSupply.n -= b;
  }
  function update_finalizedOnInsertFinalize_r40() private    {
      _finalized = _finalizedTuple(true,true);
  }
  function update_finalizedOnInsertConstructor_r42() private    {
      _finalized = _finalizedTuple(false,true);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateBalanceOfOnIncrementTotalMint_r5(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r80(address p,address s,int n) private   returns (bool) {
      int _goal_a = _goal.a;
      if(d>=0 && _goal_a>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r68(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateDepositOnInsertBuyToken_r18(address p,int n) private    {
      update_depositsOnInsertDeposit_r4(p,n);
      emit Deposit(p,n);
  }
  function update_capOnInsertConstructor_r53(int n,int a) private    {
      if(a<=n) {
        _cap = _capTuple(n,true);
      }
  }
  function updateTotalInOnInsertTransfer_r12(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r5(p,delta0);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r75(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateTotalOutOnInsertTransfer_r78(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r5(p,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTotalBurnOnInsertBurn_r17(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r5(p,delta0);
  }
  function updateAllBurnOnInsertBurn_r38(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r39(delta0);
  }
  function update_walletOnInsertConstructor_r28(address p) private    {
      // Empty()
  }
  function updateTransferPrimaryOnInsertRecv_transferPrimary_r27(address p) private   returns (bool) {
      address msgSender = msg.sender;
      address s_1 = msg.sender;
      address p_1 = _primary.p;
      int balanceOf_x1 = balanceOf[msgSender].n;
      if(p!=address(0) && s_1==p_1 && 0==balanceOf_x1) {
        update_primaryOnInsertTransferPrimary_r44(p);
        emit TransferPrimary(p);
        return true;
      }
      return false;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r68(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r75(o,s,delta0);
  }
  function update_primaryOnInsertTransferPrimary_r44(address p) private    {
      _primary = _primaryTuple(p,true);
  }
}