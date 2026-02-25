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
    update_openingTimeOnInsertConstructor_r9(t1);
    update_goalOnInsertConstructor_r49(cap,goal);
    updateTotalSupplyOnInsertConstructor_r26();
    update_primaryOnInsertConstructor_r0();
    update_closingTimeOnInsertConstructor_r62(t2);
    updateStateOnInsertConstructor_r10();
    updateMintOnInsertConstructor_r43();
    update_walletOnInsertConstructor_r23(p);
    update_capOnInsertConstructor_r53(cap,goal);
    update_finalizedOnInsertConstructor_r44();
  }
  function get_deposits(address p) public view  returns (int) {
      int a = _deposits[p].a;
      return a;
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r47 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r47(p,s,n);
      if(r47==false) {
        revert("Rule condition failed");
      }
  }
  function transferPrimary(address p) public    {
      bool r33 = updateTransferPrimaryOnInsertRecv_transferPrimary_r33(p);
      if(r33==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r29 = updateMintOnInsertRecv_mint_r29(p,amount);
      if(r29==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r22 = updateTransferOnInsertRecv_transfer_r22(from,to,amount);
      if(r22==false) {
        revert("Rule condition failed");
      }
  }
  function withdraw(address p) public    {
      bool r39 = updateWithdrawOnInsertRecv_withdraw_r39(p);
      if(r39==false) {
        revert("Rule condition failed");
      }
  }
  function deposit(address p,int a) public    {
      bool r27 = updateDepositOnInsertRecv_deposit_r27(p,a);
      if(r27==false) {
        revert("Rule condition failed");
      }
  }
  function finalize() public    {
      bool r24 = updateFinalizeOnInsertRecv_finalize_r24();
      if(r24==false) {
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
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r34 = updateTransferFromOnInsertRecv_transferFrom_r34(from,to,spender,amount);
      if(r34==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r1 = updateBurnOnInsertRecv_burn_r1(p,amount);
      if(r1==false) {
        revert("Rule condition failed");
      }
  }
  function buyToken(address p,int v) public    {
      bool r41 = updateBuyTokenOnInsertRecv_buyToken_r41(p,v);
      if(r41==false) {
        revert("Rule condition failed");
      }
  }
  function claimRefund(address p) public    {
      bool r64 = updateClaimRefundOnInsertRecv_claimRefund_r64(p);
      if(r64==false) {
        revert("Rule condition failed");
      }
  }
  function updateTotalMintOnInsertMint_r66(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r3(p,delta0);
  }
  function updateStateOnInsertConstructor_r10() private    {
      state = StateTuple(0,true);
  }
  function updateAllMintOnInsertMint_r5(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r36(delta0);
  }
  function update_depositsOnInsertWithdraw_r60(address p) private    {
      int a = 0;
      _deposits[p] = _depositsTuple(a,true);
  }
  function updateDepositOnInsertBuyToken_r15(address p,int n) private    {
      update_depositsOnInsertDeposit_r40(p,n);
      emit Deposit(p,n);
  }
  function updateTransferPrimaryOnInsertRecv_transferPrimary_r33(address p) private   returns (bool) {
      address msgSender = msg.sender;
      int balanceOf_x1 = balanceOf[msgSender].n;
      if(0==balanceOf_x1) {
        update_primaryOnInsertTransferPrimary_r2(p);
        emit TransferPrimary(p);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r3(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r34(address from,address to,address spender,int amount) private   returns (bool) {
      int k_2 = allowance[o][sp].n;
      int m_0 = balanceOf[o].n;
      if(n<=m_0 && r!=address(0) && o!=address(0) && n<=k_2) {
        updateSpentTotalOnInsertTransferFrom_r68(o,sp,n);
        updateTransferOnInsertTransferFrom_r55(o,r,n);
        emit TransferFrom(o,r,sp,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalBurn_r3(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateStateOnInsertFinalize_r76() private    {
      int n = _weiRaised.a;
      int a = _goal.a;
      if(n>=a) {
        state = StateTuple(2,true);
      }
  }
  function update_depositsOnInsertDeposit_r40(address p,int n) private    {
      _deposits[p].a += n;
  }
  function update_openingTimeOnInsertConstructor_r9(uint t1) private    {
      _openingTime = _openingTimeTuple(t1,true);
  }
  function updateTotalBurnOnInsertBurn_r14(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r3(p,delta0);
  }
  function updateClaimRefundOnInsertRecv_claimRefund_r64(address p) private   returns (bool) {
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
  function update_primaryOnInsertTransferPrimary_r2(address p) private    {
      _primary = _primaryTuple(p,true);
  }
  function update_finalizedOnInsertConstructor_r44() private    {
      _finalized = _finalizedTuple(false,true);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r65(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r71(o,s,delta0);
  }
  function updateStateOnInsertFinalize_r37() private    {
      int n = _weiRaised.a;
      int a = _goal.a;
      if(n<a) {
        state = StateTuple(1,true);
      }
  }
  function update_weiRaisedOnInsertRaised_r56(int n) private    {
      _weiRaised.a += n;
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
  function updateTransferOnInsertRecv_transfer_r22(address from,address to,int amount) private   returns (bool) {
      int m_1 = balanceOf[s].n;
      if(r!=address(0) && s!=address(0) && n<=m_1) {
        updateTotalOutOnInsertTransfer_r73(s,n);
        updateTotalInOnInsertTransfer_r13(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateAllBurnOnInsertBurn_r35(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r36(delta0);
  }
  function updateTotalSupplyOnIncrementAllBurn_r36(int b) private    {
      totalSupply.n -= b;
  }
  function updateWithdrawOnInsertRecv_withdraw_r39(address p) private   returns (bool) {
      address msgSender = msg.sender;
      int balanceOf_x1 = balanceOf[msgSender].n;
      if(balanceOf_x1<=0) {
        update_depositsOnInsertWithdraw_r60(p);
        emit Withdraw(p);
        return true;
      }
      return false;
  }
  function update_closingTimeOnInsertConstructor_r62(uint t2) private    {
      _closingTime = _closingTimeTuple(t2,true);
  }
  function update_capOnInsertConstructor_r53(int n,int a) private    {
      if(a<=n) {
        _cap = _capTuple(n,true);
      }
  }
  function updateBalanceOfOnIncrementTotalIn_r3(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateBalanceOfOnIncrementTotalMint_r3(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTotalSupplyOnIncrementAllMint_r36(int m) private    {
      totalSupply.n += m;
  }
  function updateDepositOnInsertRecv_deposit_r27(address p,int a) private   returns (bool) {
      address msgSender = msg.sender;
      int h_1 = state.a;
      int balanceOf_x1 = balanceOf[msgSender].n;
      if(a!=0 && p!=address(0) && h_1==0 && 0==balanceOf_x1) {
        update_depositsOnInsertDeposit_r40(p,a);
        emit Deposit(p,a);
        return true;
      }
      return false;
  }
  function updateBurnOnInsertRecv_burn_r1(address p,int amount) private   returns (bool) {
      int m_1 = balanceOf[p].n;
      if(p!=address(0) && n<=m_1) {
        updateAllBurnOnInsertBurn_r35(n);
        updateTotalBurnOnInsertBurn_r14(p,n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertTransferFrom_r55(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r73(o,n);
      updateTotalInOnInsertTransfer_r13(r,n);
      emit Transfer(o,r,n);
  }
  function updateFinalizeOnInsertRecv_finalize_r24() private   returns (bool) {
      bool b_2 = _finalized.b;
      address p_0 = _primary.p;
      uint c_3 = _closingTime.a;
      int h_1 = state.a;
      uint t_3 = block.timestamp;
      address s_0 = msg.sender;
      if(s_0==p_0 && h_1==0 && b_2!=true && t_3>=c_3) {
        updateStateOnInsertFinalize_r37();
        updateStateOnInsertFinalize_r76();
        update_finalizedOnInsertFinalize_r42();
        emit Finalize();
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementSpentTotal_r71(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTotalOutOnInsertTransfer_r73(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r3(p,delta0);
  }
  function updateTotalInOnInsertTransfer_r13(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r3(p,delta0);
  }
  function updateSpentTotalOnInsertTransferFrom_r68(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r71(o,s,delta0);
  }
  function update_finalizedOnInsertFinalize_r42() private    {
      _finalized = _finalizedTuple(true,true);
  }
  function updateMintOnInsertConstructor_r43() private    {
      address s = msg.sender;
      updateTotalMintOnInsertMint_r66(s,int(1));
      updateAllMintOnInsertMint_r5(int(1));
      emit Mint(s,1);
  }
  function updateMintOnInsertBuyToken_r19(address p,int n) private    {
      updateAllMintOnInsertMint_r5(n);
      updateTotalMintOnInsertMint_r66(p,n);
      emit Mint(p,n);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r47(address p,address s,int n) private   returns (bool) {
      address msgSender = msg.sender;
      int balanceOf_x1 = balanceOf[msgSender].n;
      if(0==balanceOf_x1) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r65(o,s,n);
        emit IncreaseAllowance(o,s,n);
        return true;
      }
      return false;
  }
  function updateBuyTokenOnInsertRecv_buyToken_r41(address p,int v) private   returns (bool) {
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
      if(p!=address(0) && v!=0 && 0==balanceOf_x1 && t_2>=o_2 && r_1<=c_1 && b_4!=true && h_3==0 && t_5<=c_5) {
        updateRaisedOnInsertBuyToken_r28(v);
        updateMintOnInsertBuyToken_r19(p,v);
        updateDepositOnInsertBuyToken_r15(p,v);
        emit BuyToken(p,v);
        return true;
      }
      return false;
  }
  function update_walletOnInsertConstructor_r23(address p) private    {
      // Empty()
  }
  function updateRaisedOnInsertBuyToken_r28(int a) private    {
      update_weiRaisedOnInsertRaised_r56(a);
  }
  function update_goalOnInsertConstructor_r49(int n,int a) private    {
      if(a<=n) {
        _goal = _goalTuple(a,true);
      }
  }
  function updateTotalSupplyOnInsertConstructor_r26() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r71(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateMintOnInsertRecv_mint_r29(address p,int amount) private   returns (bool) {
      address msgSender = msg.sender;
      int balanceOf_x1 = balanceOf[msgSender].n;
      if(p!=address(0) && balanceOf_x1<=0) {
        updateAllMintOnInsertMint_r5(n);
        updateTotalMintOnInsertMint_r66(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
}