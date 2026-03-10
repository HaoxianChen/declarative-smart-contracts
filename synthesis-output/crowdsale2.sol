import "./crowdsale2_udf.sol";
contract Crowdsale2 is CrowdsaleUDF {
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
    update_goalOnInsertConstructor_r48(cap,goal);
    update_closingTimeOnInsertConstructor_r65(t2);
    updateStateOnInsertConstructor_r11();
    update_finalizedOnInsertConstructor_r42();
    update_openingTimeOnInsertConstructor_r10(t1);
    updateTotalSupplyOnInsertConstructor_r27();
    updateMintOnInsertConstructor_r41();
    update_capOnInsertConstructor_r52(cap,goal);
    update_primaryOnInsertConstructor_r35();
    update_walletOnInsertConstructor_r24(p);
  }
  function claimRefund(address p) public    {
      bool r53 = updateClaimRefundOnInsertRecv_claimRefund_r53(p);
      if(r53==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r82 = updateTransferFromOnInsertRecv_transferFrom_r82(from,to,spender,amount);
      if(r82==false) {
        revert("Rule condition failed");
      }
  }
  function finalize() public    {
      bool r25 = updateFinalizeOnInsertRecv_finalize_r25();
      if(r25==false) {
        revert("Rule condition failed");
      }
  }
  function withdraw(address p) public    {
      bool r70 = updateWithdrawOnInsertRecv_withdraw_r70(p);
      if(r70==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r79 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r79(p,s,n);
      if(r79==false) {
        revert("Rule condition failed");
      }
  }
  function deposit(address p,int a) public    {
      bool r59 = updateDepositOnInsertRecv_deposit_r59(p,a);
      if(r59==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r38 = updateBurnOnInsertRecv_burn_r38(p,amount);
      if(r38==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function transfer(address from,address to,int amount) public    {
      bool r23 = updateTransferOnInsertRecv_transfer_r23(from,to,amount);
      if(r23==false) {
        revert("Rule condition failed");
      }
  }
  function buyToken(address p,int v) public    {
      bool r29 = updateBuyTokenOnInsertRecv_buyToken_r29(p,v);
      if(r29==false) {
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
  function transferPrimary(address p) public    {
      bool r34 = updateTransferPrimaryOnInsertRecv_transferPrimary_r34(p);
      if(r34==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r30 = updateMintOnInsertRecv_mint_r30(p,amount);
      if(r30==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function updateTotalSupplyOnIncrementAllBurn_r37(int b) private    {
      totalSupply.n -= b;
  }
  function updateMintOnInsertBuyToken_r3(address p,int v) private    {
      int tokens = getTokenAmount(v);
      updateAllMintOnInsertMint_r5(tokens);
      updateTotalMintOnInsertMint_r68(p,tokens);
      emit Mint(p,tokens);
  }
  function updateBuyTokenOnInsertRecv_buyToken_r29(address p,int v) private   returns (bool) {
      uint t_1 = block.timestamp;
      bool b_3 = _finalized.b;
      int r_6 = _weiRaised.a;
      int h_2 = state.a;
      uint c_4 = _closingTime.a;
      int c_6 = _cap.a;
      uint o_1 = _openingTime.a;
      int balanceOf_x1 = balanceOf[p].n;
      if(balanceOf_x1>0 && p!=address(0) && t_1<=c_4 && v!=0 && t_1>=o_1 && h_2==0 && b_3!=true && r_6+v<=c_6) {
        int tokens_5 = getTokenAmount(v);
        if(tokens_5>0) {
          updateMintOnInsertBuyToken_r3(p,v);
          updateRaisedOnInsertBuyToken_r28(v);
          updateDepositOnInsertBuyToken_r14(p,v);
          emit BuyToken(p,v);
          return true;
        }
      }
      return false;
  }
  function update_finalizedOnInsertFinalize_r40() private    {
      _finalized = _finalizedTuple(true,true);
  }
  function update_finalizedOnInsertConstructor_r42() private    {
      _finalized = _finalizedTuple(false,true);
  }
  function updateBalanceOfOnIncrementTotalMint_r2(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateMintOnInsertRecv_mint_r30(address p,int amount) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(p!=address(0) && amount<balanceOf_x1) {
        updateAllMintOnInsertMint_r5(amount);
        updateTotalMintOnInsertMint_r68(p,amount);
        emit Mint(p,amount);
        return true;
      }
      return false;
  }
  function updateSpentTotalOnInsertTransferFrom_r71(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r74(o,s,delta0);
  }
  function update_depositsOnInsertWithdraw_r63(address p) private    {
      int a = 0;
      _deposits[p] = _depositsTuple(a,true);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r79(address p,address s,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[s].n;
      if(n>=0 && n<=balanceOf_x1) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r67(p,s,n);
        emit IncreaseAllowance(p,s,n);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertRecv_transfer_r23(address from,address to,int amount) private   returns (bool) {
      int m_1 = balanceOf[from].n;
      if(to!=address(0) && from!=address(0) && amount<=m_1) {
        updateTotalInOnInsertTransfer_r9(to,amount);
        updateTotalOutOnInsertTransfer_r77(from,amount);
        emit Transfer(from,to,amount);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r2(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateStateOnInsertConstructor_r11() private    {
      state = StateTuple(0,true);
  }
  function updateAllBurnOnInsertBurn_r36(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r37(delta0);
  }
  function update_openingTimeOnInsertConstructor_r10(uint t1) private    {
      _openingTime = _openingTimeTuple(t1,true);
  }
  function updateTotalMintOnInsertMint_r68(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r2(p,delta0);
  }
  function update_primaryOnInsertConstructor_r35() private    {
      address s = msg.sender;
      _primary = _primaryTuple(s,true);
  }
  function updateAllowanceOnIncrementSpentTotal_r74(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateDepositOnInsertRecv_deposit_r59(address p,int a) private   returns (bool) {
      address s_1 = msg.sender;
      int h_2 = state.a;
      address p_1 = _primary.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(p!=address(0) && s_1==p_1 && a<=balanceOf_x1 && a!=0 && h_2==0) {
        update_depositsOnInsertDeposit_r1(p,a);
        emit Deposit(p,a);
        return true;
      }
      return false;
  }
  function updateAllMintOnInsertMint_r5(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r37(delta0);
  }
  function updateBalanceOfOnIncrementTotalIn_r2(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTotalInOnInsertTransfer_r9(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r2(p,delta0);
  }
  function updateTotalSupplyOnIncrementAllMint_r37(int m) private    {
      totalSupply.n += m;
  }
  function updateRaisedOnInsertBuyToken_r28(int a) private    {
      update_weiRaisedOnInsertRaised_r56(a);
  }
  function updateTransferPrimaryOnInsertRecv_transferPrimary_r34(address p) private   returns (bool) {
      address p_1 = _primary.p;
      address s_1 = msg.sender;
      int balanceOf_x1 = balanceOf[p].n;
      if(p!=address(0) && s_1==p_1 && balanceOf_x1>0) {
        update_primaryOnInsertTransferPrimary_r44(p);
        emit TransferPrimary(p);
        return true;
      }
      return false;
  }
  function updateFinalizeOnInsertRecv_finalize_r25() private   returns (bool) {
      bool b_2 = _finalized.b;
      address p_0 = _primary.p;
      uint c_3 = _closingTime.a;
      int h_1 = state.a;
      uint t_3 = block.timestamp;
      address s_0 = msg.sender;
      if(s_0==p_0 && h_1==0 && b_2!=true && t_3>=c_3) {
        updateStateOnInsertFinalize_r0();
        updateStateOnInsertFinalize_r83();
        update_finalizedOnInsertFinalize_r40();
        emit Finalize();
        return true;
      }
      return false;
  }
  function update_depositsOnInsertDeposit_r1(address p,int n) private    {
      _deposits[p].a += n;
  }
  function updateTotalOutOnInsertTransfer_r77(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r2(p,delta0);
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
  function updateTotalSupplyOnInsertConstructor_r27() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateMintOnInsertConstructor_r41() private    {
      address s = msg.sender;
      updateTotalMintOnInsertMint_r68(s,int(1));
      updateAllMintOnInsertMint_r5(int(1));
      emit Mint(s,1);
  }
  function update_primaryOnInsertTransferPrimary_r44(address p) private    {
      _primary = _primaryTuple(p,true);
  }
  function updateTransferOnInsertTransferFrom_r55(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r9(r,n);
      updateTotalOutOnInsertTransfer_r77(o,n);
      emit Transfer(o,r,n);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r74(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateWithdrawOnInsertRecv_withdraw_r70(address p) private   returns (bool) {
      int totalSupply_n = totalSupply.n;
      if(p==_primary.p) {
        address s = msg.sender;
        if(s==p && totalSupply_n>0) {
          update_depositsOnInsertWithdraw_r63(p);
          emit Withdraw(p);
          return true;
        }
      }
      return false;
  }
  function updateTotalBurnOnInsertBurn_r13(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r2(p,delta0);
  }
  function updateBurnOnInsertRecv_burn_r38(address p,int amount) private   returns (bool) {
      int m_1 = balanceOf[p].n;
      if(p!=address(0) && amount<=m_1) {
        updateAllBurnOnInsertBurn_r36(amount);
        updateTotalBurnOnInsertBurn_r13(p,amount);
        emit Burn(p,amount);
        return true;
      }
      return false;
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
  function updateTransferFromOnInsertRecv_transferFrom_r82(address from,address to,address spender,int amount) private   returns (bool) {
      int k_2 = allowance[from][spender].n;
      int m_0 = balanceOf[from].n;
      if(to!=address(0) && spender!=address(0) && amount<=k_2 && amount<=m_0 && from!=address(0)) {
        updateTransferOnInsertTransferFrom_r55(from,to,amount);
        updateSpentTotalOnInsertTransferFrom_r71(from,spender,amount);
        emit TransferFrom(from,to,spender,amount);
        return true;
      }
      return false;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateStateOnInsertFinalize_r0() private    {
      int n = _weiRaised.a;
      int a = _goal.a;
      if(n<a) {
        state = StateTuple(1,true);
      }
  }
  function update_closingTimeOnInsertConstructor_r65(uint t2) private    {
      _closingTime = _closingTimeTuple(t2,true);
  }
  function updateDepositOnInsertBuyToken_r14(address p,int n) private    {
      update_depositsOnInsertDeposit_r1(p,n);
      emit Deposit(p,n);
  }
  function update_walletOnInsertConstructor_r24(address p) private    {
      // Empty()
  }
  function updateBalanceOfOnIncrementTotalBurn_r2(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function update_goalOnInsertConstructor_r48(int n,int a) private    {
      if(a<=n) {
        _goal = _goalTuple(a,true);
      }
  }
  function update_capOnInsertConstructor_r52(int n,int a) private    {
      if(a<=n) {
        _cap = _capTuple(n,true);
      }
  }
  function updateStateOnInsertFinalize_r83() private    {
      int n = _weiRaised.a;
      int a = _goal.a;
      if(n>=a) {
        state = StateTuple(2,true);
      }
  }
}