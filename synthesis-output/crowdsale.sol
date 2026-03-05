import "./crowdsale_udf.sol";
contract Crowdsale is CrowdsaleUDF {
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
    update_primaryOnInsertConstructor_r1();
    updateTotalSupplyOnInsertConstructor_r25();
    update_walletOnInsertConstructor_r22(p);
    update_finalizedOnInsertConstructor_r42();
    updateStateOnInsertConstructor_r11();
    update_closingTimeOnInsertConstructor_r62(t2);
    update_capOnInsertConstructor_r52(cap,goal);
    update_openingTimeOnInsertConstructor_r10(t1);
  }
  function burn(address p,int amount) public    {
      bool r2 = updateBurnOnInsertRecv_burn_r2(p,amount);
      if(r2==false) {
        revert("Rule condition failed");
      }
  }
  function transferPrimary(address p) public    {
      bool r32 = updateTransferPrimaryOnInsertRecv_transferPrimary_r32(p);
      if(r32==false) {
        revert("Rule condition failed");
      }
  }
  function finalize() public    {
      bool r23 = updateFinalizeOnInsertRecv_finalize_r23();
      if(r23==false) {
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
  function transfer(address from,address to,int amount) public    {
      bool r21 = updateTransferOnInsertRecv_transfer_r21(from,to,amount);
      if(r21==false) {
        revert("Rule condition failed");
      }
  }
  function deposit(address p,int a) public    {
      bool r26 = updateDepositOnInsertRecv_deposit_r26(p,a);
      if(r26==false) {
        revert("Rule condition failed");
      }
  }
  function buyToken(address p,int v) public    {
      bool r43 = updateBuyTokenOnInsertRecv_buyToken_r43(p,v);
      if(r43==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r77 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r77(p,s,n);
      if(r77==false) {
        revert("Rule condition failed");
      }
  }
  function withdraw(address p) public    {
      bool r39 = updateWithdrawOnInsertRecv_withdraw_r39(p);
      if(r39==false) {
        revert("Rule condition failed");
      }
  }
  function claimRefund(address p) public    {
      bool r64 = updateClaimRefundOnInsertRecv_claimRefund_r64(p);
      if(r64==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r28 = updateMintOnInsertRecv_mint_r28(p,amount);
      if(r28==false) {
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
      bool r33 = updateTransferFromOnInsertRecv_transferFrom_r33(from,to,spender,amount);
      if(r33==false) {
        revert("Rule condition failed");
      }
  }
  function updateBalanceOfOnIncrementTotalOut_r3(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateTotalMintOnInsertMint_r66(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r3(p,delta0);
  }
  function updateRaisedOnInsertBuyToken_r27(int a) private    {
      update_weiRaisedOnInsertRaised_r55(a);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r33(address from,address to,address spender,int amount) private   returns (bool) {
      int k_2 = allowance[o][sp].n;
      int m_0 = balanceOf[o].n;
      if(n<=m_0 && r!=address(0) && o!=address(0) && n<=k_2) {
        updateTransferOnInsertTransferFrom_r54(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r68(o,sp,n);
        emit TransferFrom(o,r,sp,n);
        return true;
      }
      return false;
  }
  function update_depositsOnInsertWithdraw_r60(address p) private    {
      int a = 0;
      _deposits[p] = _depositsTuple(a,true);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r77(address p,address s,int n) private   returns (bool) {
      address msgSender = msg.sender;
      int balanceOf_x1 = balanceOf[msgSender].n;
      if(balanceOf_x1<=0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r65(o,s,n);
        emit IncreaseAllowance(o,s,n);
        return true;
      }
      return false;
  }
  function updateTotalInOnInsertTransfer_r14(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r3(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalBurn_r3(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function update_depositsOnInsertDeposit_r38(address p,int n) private    {
      _deposits[p].a += n;
  }
  function update_capOnInsertConstructor_r52(int n,int a) private    {
      if(a<=n) {
        _cap = _capTuple(n,true);
      }
  }
  function updateAllBurnOnInsertBurn_r34(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r35(delta0);
  }
  function updateTotalSupplyOnInsertConstructor_r25() private    {
      totalSupply = TotalSupplyTuple(0,true);
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
  function updateFinalizeOnInsertRecv_finalize_r23() private   returns (bool) {
      bool b_2 = _finalized.b;
      address p_0 = _primary.p;
      uint c_3 = _closingTime.a;
      int h_1 = state.a;
      uint t_3 = block.timestamp;
      address s_0 = msg.sender;
      if(s_0==p_0 && h_1==0 && b_2!=true && t_3>=c_3) {
        update_finalizedOnInsertFinalize_r40();
        updateStateOnInsertFinalize_r75();
        updateStateOnInsertFinalize_r36();
        emit Finalize();
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r35(int b) private    {
      totalSupply.n -= b;
  }
  function update_goalOnInsertConstructor_r48(int n,int a) private    {
      if(a<=n) {
        _goal = _goalTuple(a,true);
      }
  }
  function updateBuyTokenOnInsertRecv_buyToken_r43(address p,int v) private   returns (bool) {
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
      if(p!=address(0) && t_4<=c_4 && tokens_5>0 && v!=0 && t_1>=o_1 && b_3!=true && r_6<=c_6 && h_2==0 && v>balanceOf_x1) {
        updateDepositOnInsertBuyToken_r16(p,v);
        updateRaisedOnInsertBuyToken_r27(v);
        updateMintOnInsertBuyToken_r4(p,v);
        emit BuyToken(p,v);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertRecv_transfer_r21(address from,address to,int amount) private   returns (bool) {
      int m_1 = balanceOf[s].n;
      if(r!=address(0) && s!=address(0) && n<=m_1) {
        updateTotalInOnInsertTransfer_r14(r,n);
        updateTotalOutOnInsertTransfer_r73(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function update_openingTimeOnInsertConstructor_r10(uint t1) private    {
      _openingTime = _openingTimeTuple(t1,true);
  }
  function update_primaryOnInsertConstructor_r1() private    {
      address s = msg.sender;
      _primary = _primaryTuple(s,true);
  }
  function update_finalizedOnInsertFinalize_r40() private    {
      _finalized = _finalizedTuple(true,true);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r65(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r71(o,s,delta0);
  }
  function update_weiRaisedOnInsertRaised_r55(int n) private    {
      _weiRaised.a += n;
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
  function updateMintOnInsertBuyToken_r4(address p,int v) private    {
      int tokens = getTokenAmount(v);
      updateAllMintOnInsertMint_r6(tokens);
      updateTotalMintOnInsertMint_r66(p,tokens);
      emit Mint(p,tokens);
  }
  function updateAllMintOnInsertMint_r6(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r35(delta0);
  }
  function updateStateOnInsertConstructor_r11() private    {
      state = StateTuple(0,true);
  }
  function updateDepositOnInsertRecv_deposit_r26(address p,int a) private   returns (bool) {
      address msgSender = msg.sender;
      int h_1 = state.a;
      int balanceOf_x1 = balanceOf[msgSender].n;
      if(a!=0 && p!=address(0) && h_1==0 && 0==balanceOf_x1) {
        update_depositsOnInsertDeposit_r38(p,a);
        emit Deposit(p,a);
        return true;
      }
      return false;
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
  function updateBurnOnInsertRecv_burn_r2(address p,int amount) private   returns (bool) {
      int m_1 = balanceOf[p].n;
      if(p!=address(0) && n<=m_1) {
        updateTotalBurnOnInsertBurn_r15(p,n);
        updateAllBurnOnInsertBurn_r34(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function update_closingTimeOnInsertConstructor_r62(uint t2) private    {
      _closingTime = _closingTimeTuple(t2,true);
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
  function updateTotalBurnOnInsertBurn_r15(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r3(p,delta0);
  }
  function update_walletOnInsertConstructor_r22(address p) private    {
      // Empty()
  }
  function updateTransferOnInsertTransferFrom_r54(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r73(o,n);
      updateTotalInOnInsertTransfer_r14(r,n);
      emit Transfer(o,r,n);
  }
  function updateAllowanceOnIncrementSpentTotal_r71(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTotalOutOnInsertTransfer_r73(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r3(p,delta0);
  }
  function updateStateOnInsertFinalize_r75() private    {
      int n = _weiRaised.a;
      int a = _goal.a;
      if(n>=a) {
        state = StateTuple(2,true);
      }
  }
  function updateSpentTotalOnInsertTransferFrom_r68(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r71(o,s,delta0);
  }
  function updateMintOnInsertConstructor_r41() private    {
      address s = msg.sender;
      updateAllMintOnInsertMint_r6(int(1));
      updateTotalMintOnInsertMint_r66(s,int(1));
      emit Mint(s,1);
  }
  function updateTransferPrimaryOnInsertRecv_transferPrimary_r32(address p) private   returns (bool) {
      address msgSender = msg.sender;
      int balanceOf_x1 = balanceOf[msgSender].n;
      if(0==balanceOf_x1) {
        update_primaryOnInsertTransferPrimary_r44(p);
        emit TransferPrimary(p);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r35(int m) private    {
      totalSupply.n += m;
  }
  function updateStateOnInsertFinalize_r36() private    {
      int n = _weiRaised.a;
      int a = _goal.a;
      if(n<a) {
        state = StateTuple(1,true);
      }
  }
  function updateMintOnInsertRecv_mint_r28(address p,int amount) private   returns (bool) {
      address msgSender = msg.sender;
      int balanceOf_x1 = balanceOf[msgSender].n;
      if(p!=address(0) && balanceOf_x1<=0) {
        updateAllMintOnInsertMint_r6(n);
        updateTotalMintOnInsertMint_r66(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateDepositOnInsertBuyToken_r16(address p,int n) private    {
      update_depositsOnInsertDeposit_r38(p,n);
      emit Deposit(p,n);
  }
  function update_primaryOnInsertTransferPrimary_r44(address p) private    {
      _primary = _primaryTuple(p,true);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r71(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
}