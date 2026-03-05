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
    updateTotalSupplyOnInsertConstructor_r32();
    update_goalOnInsertConstructor_r52(cap,goal);
    updateStateOnInsertConstructor_r13();
    update_capOnInsertConstructor_r56(cap,goal);
    update_closingTimeOnInsertConstructor_r68(t2);
    update_walletOnInsertConstructor_r29(p);
    updateMintOnInsertConstructor_r43();
    update_finalizedOnInsertConstructor_r44();
    update_openingTimeOnInsertConstructor_r12(t1);
    update_primaryOnInsertConstructor_r0();
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r82 = updateTransferFromOnInsertRecv_transferFrom_r82(from,to,spender,amount);
      if(r82==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r17 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r17(p,s,n);
      if(r17==false) {
        revert("Rule condition failed");
      }
  }
  function get_deposits(address p) public view  returns (int) {
      int a = _deposits[p].a;
      return a;
  }
  function withdraw(address p) public    {
      bool r50 = updateWithdrawOnInsertRecv_withdraw_r50(p);
      if(r50==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function transfer(address from,address to,int amount) public    {
      bool r27 = updateTransferOnInsertRecv_transfer_r27(from,to,amount);
      if(r27==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r28 = updateMintOnInsertRecv_mint_r28(p,amount);
      if(r28==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r40 = updateBurnOnInsertRecv_burn_r40(p,amount);
      if(r40==false) {
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
  function deposit(address p,int a) public    {
      bool r47 = updateDepositOnInsertRecv_deposit_r47(p,a);
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
  function claimRefund(address p) public    {
      bool r57 = updateClaimRefundOnInsertRecv_claimRefund_r57(p);
      if(r57==false) {
        revert("Rule condition failed");
      }
  }
  function finalize() public    {
      bool r30 = updateFinalizeOnInsertRecv_finalize_r30();
      if(r30==false) {
        revert("Rule condition failed");
      }
  }
  function buyToken(address p,int v) public    {
      bool r26 = updateBuyTokenOnInsertRecv_buyToken_r26(p,v);
      if(r26==false) {
        revert("Rule condition failed");
      }
  }
  function updateBalanceOfOnIncrementTotalBurn_r3(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateStateOnInsertFinalize_r83() private    {
      int n = _weiRaised.a;
      int a = _goal.a;
      if(n>=a) {
        state = StateTuple(2,true);
      }
  }
  function updateMintOnInsertBuyToken_r4(address p,int v) private    {
      int tokens = getTokenAmount(v);
      updateTotalMintOnInsertMint_r71(p,tokens);
      updateAllMintOnInsertMint_r7(tokens);
      emit Mint(p,tokens);
  }
  function updateTotalOutOnInsertTransfer_r79(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r3(p,delta0);
  }
  function updateStateOnInsertConstructor_r13() private    {
      state = StateTuple(0,true);
  }
  function update_weiRaisedOnInsertRaised_r60(int n) private    {
      _weiRaised.a += n;
  }
  function updateTransferOnInsertTransferFrom_r59(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r11(r,n);
      updateTotalOutOnInsertTransfer_r79(o,n);
      emit Transfer(o,r,n);
  }
  function updateTransferPrimaryOnInsertRecv_transferPrimary_r33(address p) private   returns (bool) {
      address msgSender = msg.sender;
      address s_1 = msg.sender;
      address p_1 = _primary.p;
      int balanceOf_x1 = balanceOf[msgSender].n;
      if(p!=address(0) && s_1==p_1 && balanceOf_x1<=0) {
        update_primaryOnInsertTransferPrimary_r46(p);
        emit TransferPrimary(p);
        return true;
      }
      return false;
  }
  function updateMintOnInsertRecv_mint_r28(address p,int amount) private   returns (bool) {
      address msgSender = msg.sender;
      int _goal_a_1 = _goal.a;
      int balanceOf_x1_0 = balanceOf[msgSender].n;
      if(p!=address(0) && n>balanceOf_x1_0 && n<=_goal_a_1) {
        updateTotalMintOnInsertMint_r71(p,n);
        updateAllMintOnInsertMint_r7(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementSpentTotal_r76(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateAllBurnOnInsertBurn_r38(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r39(delta0);
  }
  function updateTotalMintOnInsertMint_r71(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r3(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalOut_r3(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function update_openingTimeOnInsertConstructor_r12(uint t1) private    {
      _openingTime = _openingTimeTuple(t1,true);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r76(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateTotalSupplyOnIncrementAllMint_r39(int m) private    {
      totalSupply.n += m;
  }
  function updateClaimRefundOnInsertRecv_claimRefund_r57(address p) private   returns (bool) {
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
  function updateFinalizeOnInsertRecv_finalize_r30() private   returns (bool) {
      bool b_2 = _finalized.b;
      address p_0 = _primary.p;
      uint c_3 = _closingTime.a;
      int h_1 = state.a;
      uint t_3 = block.timestamp;
      address s_0 = msg.sender;
      if(s_0==p_0 && h_1==0 && b_2!=true && t_3>=c_3) {
        updateStateOnInsertFinalize_r83();
        update_finalizedOnInsertFinalize_r42();
        updateStateOnInsertFinalize_r1();
        emit Finalize();
        return true;
      }
      return false;
  }
  function updateDepositOnInsertBuyToken_r16(address p,int n) private    {
      update_depositsOnInsertDeposit_r2(p,n);
      emit Deposit(p,n);
  }
  function updateBurnOnInsertRecv_burn_r40(address p,int amount) private   returns (bool) {
      int m_1 = balanceOf[p].n;
      if(p!=address(0) && n<=m_1) {
        updateTotalBurnOnInsertBurn_r15(p,n);
        updateAllBurnOnInsertBurn_r38(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function update_depositsOnInsertWithdraw_r66(address p) private    {
      int a = 0;
      _deposits[p] = _depositsTuple(a,true);
  }
  function update_capOnInsertConstructor_r56(int n,int a) private    {
      if(a<=n) {
        _cap = _capTuple(n,true);
      }
  }
  function updateWithdrawOnInsertRecv_withdraw_r50(address p) private   returns (bool) {
      address msgSender = msg.sender;
      address s = msg.sender;
      if(p==_primary.p) {
        int balanceOf_x1 = balanceOf[msgSender].n;
        if(s==p && balanceOf_x1<=0) {
          update_depositsOnInsertWithdraw_r66(p);
          emit Withdraw(p);
          return true;
        }
      }
      return false;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r17(address p,address s,int n) private   returns (bool) {
      address msgSender = msg.sender;
      int _goal_a_1 = _goal.a;
      int balanceOf_x1_0 = balanceOf[msgSender].n;
      if(d>=0 && d>balanceOf_x1_0 && d<=_goal_a_1) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r70(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertRecv_transfer_r27(address from,address to,int amount) private   returns (bool) {
      int m_1 = balanceOf[s].n;
      if(r!=address(0) && s!=address(0) && n<=m_1) {
        updateTotalInOnInsertTransfer_r11(r,n);
        updateTotalOutOnInsertTransfer_r79(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateRaisedOnInsertBuyToken_r34(int a) private    {
      update_weiRaisedOnInsertRaised_r60(a);
  }
  function updateDepositOnInsertRecv_deposit_r47(address p,int a) private   returns (bool) {
      int _goal_a_1 = _goal.a;
      address msgSender = msg.sender;
      address s_1 = msg.sender;
      int h_2 = state.a;
      address p_1 = _primary.p;
      int balanceOf_x1_0 = balanceOf[msgSender].n;
      if(a>balanceOf_x1_0 && p!=address(0) && s_1==p_1 && a<=_goal_a_1 && a!=0 && h_2==0) {
        update_depositsOnInsertDeposit_r2(p,a);
        emit Deposit(p,a);
        return true;
      }
      return false;
  }
  function updateStateOnInsertFinalize_r1() private    {
      int n = _weiRaised.a;
      int a = _goal.a;
      if(n<a) {
        state = StateTuple(1,true);
      }
  }
  function updateTotalSupplyOnIncrementAllBurn_r39(int b) private    {
      totalSupply.n -= b;
  }
  function update_finalizedOnInsertConstructor_r44() private    {
      _finalized = _finalizedTuple(false,true);
  }
  function update_walletOnInsertConstructor_r29(address p) private    {
      // Empty()
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
  function update_closingTimeOnInsertConstructor_r68(uint t2) private    {
      _closingTime = _closingTimeTuple(t2,true);
  }
  function updateSpentTotalOnInsertTransferFrom_r73(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r76(o,s,delta0);
  }
  function update_goalOnInsertConstructor_r52(int n,int a) private    {
      if(a<=n) {
        _goal = _goalTuple(a,true);
      }
  }
  function updateBalanceOfOnIncrementTotalIn_r3(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTotalSupplyOnInsertConstructor_r32() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateBuyTokenOnInsertRecv_buyToken_r26(address p,int v) private   returns (bool) {
      uint t_1 = block.timestamp;
      uint t_4 = block.timestamp;
      bool b_3 = _finalized.b;
      uint o_1 = _openingTime.a;
      address msgSender = msg.sender;
      int r_6 = _weiRaised.a;
      int h_2 = state.a;
      uint c_4 = _closingTime.a;
      int c_6 = _cap.a;
      int balanceOf_x1 = balanceOf[msgSender].n;
      int tokens_5 = getTokenAmount(v);
      if(p!=address(0) && t_4<=c_4 && tokens_5>0 && v!=0 && t_1>=o_1 && h_2==0 && v>balanceOf_x1 && b_3!=true && r_6+v<=c_6) {
        updateDepositOnInsertBuyToken_r16(p,v);
        updateMintOnInsertBuyToken_r4(p,v);
        updateRaisedOnInsertBuyToken_r34(v);
        emit BuyToken(p,v);
        return true;
      }
      return false;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateBalanceOfOnIncrementTotalMint_r3(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function update_primaryOnInsertTransferPrimary_r46(address p) private    {
      _primary = _primaryTuple(p,true);
  }
  function updateTotalBurnOnInsertBurn_r15(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r3(p,delta0);
  }
  function updateAllMintOnInsertMint_r7(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r39(delta0);
  }
  function update_finalizedOnInsertFinalize_r42() private    {
      _finalized = _finalizedTuple(true,true);
  }
  function updateTotalInOnInsertTransfer_r11(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r3(p,delta0);
  }
  function updateMintOnInsertConstructor_r43() private    {
      address s = msg.sender;
      updateAllMintOnInsertMint_r7(int(1));
      updateTotalMintOnInsertMint_r71(s,int(1));
      emit Mint(s,1);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r70(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r76(o,s,delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r82(address from,address to,address spender,int amount) private   returns (bool) {
      int k_2 = allowance[o][sp].n;
      int m_0 = balanceOf[o].n;
      if(r!=address(0) && sp!=address(0) && n<=k_2 && n<=m_0 && o!=address(0)) {
        updateTransferOnInsertTransferFrom_r59(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r73(o,sp,n);
        emit TransferFrom(o,r,sp,n);
        return true;
      }
      return false;
  }
}