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
    update_goalOnInsertConstructor_r48(cap,goal);
    update_capOnInsertConstructor_r52(cap,goal);
    update_openingTimeOnInsertConstructor_r11(t1);
    updateStateOnInsertConstructor_r12();
    updateTotalSupplyOnInsertConstructor_r30();
    update_walletOnInsertConstructor_r27(p);
    update_closingTimeOnInsertConstructor_r64(t2);
    update_finalizedOnInsertConstructor_r42();
    update_primaryOnInsertConstructor_r0();
  }
  function claimRefund(address p) public    {
      bool r53 = updateClaimRefundOnInsertRecv_claimRefund_r53(p);
      if(r53==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r32 = updateMintOnInsertRecv_mint_r32(p,amount);
      if(r32==false) {
        revert("Rule condition failed");
      }
  }
  function get_deposits(address p) public view  returns (int) {
      int a = _deposits[p].a;
      return a;
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r81 = updateTransferFromOnInsertRecv_transferFrom_r81(from,to,spender,amount);
      if(r81==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r65 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r65(p,s,n);
      if(r65==false) {
        revert("Rule condition failed");
      }
  }
  function withdraw(address p) public    {
      bool r80 = updateWithdrawOnInsertRecv_withdraw_r80(p);
      if(r80==false) {
        revert("Rule condition failed");
      }
  }
  function finalize() public    {
      bool r28 = updateFinalizeOnInsertRecv_finalize_r28();
      if(r28==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function buyToken(address p,int v) public    {
      bool r72 = updateBuyTokenOnInsertRecv_buyToken_r72(p,v);
      if(r72==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function burn(address p,int amount) public    {
      bool r38 = updateBurnOnInsertRecv_burn_r38(p,amount);
      if(r38==false) {
        revert("Rule condition failed");
      }
  }
  function deposit(address p,int a) public    {
      bool r54 = updateDepositOnInsertRecv_deposit_r54(p,a);
      if(r54==false) {
        revert("Rule condition failed");
      }
  }
  function transferPrimary(address p) public    {
      bool r26 = updateTransferPrimaryOnInsertRecv_transferPrimary_r26(p);
      if(r26==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r25 = updateTransferOnInsertRecv_transfer_r25(from,to,amount);
      if(r25==false) {
        revert("Rule condition failed");
      }
  }
  function updateTotalSupplyOnIncrementAllMint_r37(int m) private    {
      totalSupply.n += m;
  }
  function updateStateOnInsertFinalize_r1() private    {
      int n = _weiRaised.a;
      int a = _goal.a;
      if(n<a) {
        state = StateTuple(1,true);
      }
  }
  function update_finalizedOnInsertFinalize_r40() private    {
      _finalized = _finalizedTuple(true,true);
  }
  function updateDepositOnInsertBuyToken_r15(address p,int n) private    {
      update_depositsOnInsertDeposit_r2(p,n);
      emit Deposit(p,n);
  }
  function updateBalanceOfOnIncrementTotalIn_r3(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r65(address p,address s,int n) private   returns (bool) {
      address msgSender = msg.sender;
      int balanceOf_x1 = balanceOf[msgSender].n;
      if(d>=0 && 0==balanceOf_x1) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r67(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateMintOnInsertRecv_mint_r32(address p,int amount) private   returns (bool) {
      address msgSender = msg.sender;
      int balanceOf_x1 = balanceOf[msgSender].n;
      if(p!=address(0) && balanceOf_x1<=0) {
        updateAllMintOnInsertMint_r6(n);
        updateTotalMintOnInsertMint_r68(p,n);
        emit Mint(p,n);
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
  function updateTotalOutOnInsertTransfer_r77(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r3(p,delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r81(address from,address to,address spender,int amount) private   returns (bool) {
      int k_2 = allowance[o][sp].n;
      int m_0 = balanceOf[o].n;
      if(r!=address(0) && sp!=address(0) && n<=k_2 && n<=m_0 && o!=address(0)) {
        updateSpentTotalOnInsertTransferFrom_r70(o,sp,n);
        updateTransferOnInsertTransferFrom_r56(o,r,n);
        emit TransferFrom(o,r,sp,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalBurn_r3(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateDepositOnInsertRecv_deposit_r54(address p,int a) private   returns (bool) {
      address msgSender = msg.sender;
      address s_1 = msg.sender;
      int h_2 = state.a;
      address p_1 = _primary.p;
      int balanceOf_x1 = balanceOf[msgSender].n;
      if(balanceOf_x1<=0 && p!=address(0) && s_1==p_1 && a!=0 && h_2==0) {
        update_depositsOnInsertDeposit_r2(p,a);
        emit Deposit(p,a);
        return true;
      }
      return false;
  }
  function update_depositsOnInsertWithdraw_r62(address p) private    {
      int a = 0;
      _deposits[p] = _depositsTuple(a,true);
  }
  function updateAllMintOnInsertMint_r6(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r37(delta0);
  }
  function updateTotalBurnOnInsertBurn_r14(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r3(p,delta0);
  }
  function updateAllBurnOnInsertBurn_r36(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r37(delta0);
  }
  function updateMintOnInsertBuyToken_r22(address p,int n) private    {
      updateAllMintOnInsertMint_r6(n);
      updateTotalMintOnInsertMint_r68(p,n);
      emit Mint(p,n);
  }
  function update_closingTimeOnInsertConstructor_r64(uint t2) private    {
      _closingTime = _closingTimeTuple(t2,true);
  }
  function update_capOnInsertConstructor_r52(int n,int a) private    {
      if(a<=n) {
        _cap = _capTuple(n,true);
      }
  }
  function update_weiRaisedOnInsertRaised_r57(int n) private    {
      _weiRaised.a += n;
  }
  function updateBurnOnInsertRecv_burn_r38(address p,int amount) private   returns (bool) {
      int m_1 = balanceOf[p].n;
      if(p!=address(0) && n<=m_1) {
        updateAllBurnOnInsertBurn_r36(n);
        updateTotalBurnOnInsertBurn_r14(p,n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateBuyTokenOnInsertRecv_buyToken_r72(address p,int v) private   returns (bool) {
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
      if(p!=address(0) && v!=0 && r_1+v<=c_1 && v>balanceOf_x1 && t_2>=o_2 && b_4!=true && h_3==0 && t_5<=c_5) {
        updateMintOnInsertBuyToken_r22(p,v);
        updateDepositOnInsertBuyToken_r15(p,v);
        updateRaisedOnInsertBuyToken_r31(v);
        emit BuyToken(p,v);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r37(int b) private    {
      totalSupply.n -= b;
  }
  function update_walletOnInsertConstructor_r27(address p) private    {
      // Empty()
  }
  function updateTransferOnInsertTransferFrom_r56(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r10(r,n);
      updateTotalOutOnInsertTransfer_r77(o,n);
      emit Transfer(o,r,n);
  }
  function updateTransferPrimaryOnInsertRecv_transferPrimary_r26(address p) private   returns (bool) {
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
  function updateAllowanceOnIncrementSpentTotal_r74(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTransferOnInsertRecv_transfer_r25(address from,address to,int amount) private   returns (bool) {
      int m_1 = balanceOf[s].n;
      if(r!=address(0) && s!=address(0) && n<=m_1) {
        updateTotalOutOnInsertTransfer_r77(s,n);
        updateTotalInOnInsertTransfer_r10(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function update_goalOnInsertConstructor_r48(int n,int a) private    {
      if(a<=n) {
        _goal = _goalTuple(a,true);
      }
  }
  function updateTotalInOnInsertTransfer_r10(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r3(p,delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r67(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r74(o,s,delta0);
  }
  function update_finalizedOnInsertConstructor_r42() private    {
      _finalized = _finalizedTuple(false,true);
  }
  function update_depositsOnInsertDeposit_r2(address p,int n) private    {
      _deposits[p].a += n;
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
  function updateAllowanceOnIncrementAllowanceTotal_r74(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateWithdrawOnInsertRecv_withdraw_r80(address p) private   returns (bool) {
      address msgSender = msg.sender;
      address s = msg.sender;
      if(p==_primary.p) {
        int balanceOf_x1 = balanceOf[msgSender].n;
        if(s==p && 0==balanceOf_x1) {
          update_depositsOnInsertWithdraw_r62(p);
          emit Withdraw(p);
          return true;
        }
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalMint_r3(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTotalMintOnInsertMint_r68(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r3(p,delta0);
  }
  function updateFinalizeOnInsertRecv_finalize_r28() private   returns (bool) {
      bool b_2 = _finalized.b;
      address p_0 = _primary.p;
      uint c_3 = _closingTime.a;
      int h_1 = state.a;
      uint t_3 = block.timestamp;
      address s_0 = msg.sender;
      if(s_0==p_0 && h_1==0 && b_2!=true && t_3>=c_3) {
        update_finalizedOnInsertFinalize_r40();
        updateStateOnInsertFinalize_r82();
        updateStateOnInsertFinalize_r1();
        emit Finalize();
        return true;
      }
      return false;
  }
  function updateMintOnInsertConstructor_r41() private    {
      address s = msg.sender;
      updateAllMintOnInsertMint_r6(int(1));
      updateTotalMintOnInsertMint_r68(s,int(1));
      emit Mint(s,1);
  }
  function updateSpentTotalOnInsertTransferFrom_r70(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r74(o,s,delta0);
  }
  function update_openingTimeOnInsertConstructor_r11(uint t1) private    {
      _openingTime = _openingTimeTuple(t1,true);
  }
  function updateRaisedOnInsertBuyToken_r31(int a) private    {
      update_weiRaisedOnInsertRaised_r57(a);
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
  function update_primaryOnInsertTransferPrimary_r44(address p) private    {
      _primary = _primaryTuple(p,true);
  }
}