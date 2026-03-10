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
    update_finalizedOnInsertConstructor_r41();
    update_walletOnInsertConstructor_r25(p);
    update_openingTimeOnInsertConstructor_r13(t1);
    update_capOnInsertConstructor_r50(cap,goal);
    update_goalOnInsertConstructor_r46(cap,goal);
    updateStateOnInsertConstructor_r14();
    updateMintOnInsertConstructor_r40();
    updateTotalSupplyOnInsertConstructor_r28();
    update_primaryOnInsertConstructor_r1();
    update_closingTimeOnInsertConstructor_r62(t2);
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r52 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r52(p,s,n);
      if(r52==false) {
        revert("Rule condition failed");
      }
  }
  function get_deposits(address p) public view  returns (int) {
      int a = _deposits[p].a;
      return a;
  }
  function deposit(address p,int a) public    {
      bool r11 = updateDepositOnInsertRecv_deposit_r11(p,a);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function finalize() public    {
      bool r26 = updateFinalizeOnInsertRecv_finalize_r26();
      if(r26==false) {
        revert("Rule condition failed");
      }
  }
  function transferPrimary(address p) public    {
      bool r6 = updateTransferPrimaryOnInsertRecv_transferPrimary_r6(p);
      if(r6==false) {
        revert("Rule condition failed");
      }
  }
  function withdraw(address p) public    {
      bool r44 = updateWithdrawOnInsertRecv_withdraw_r44(p);
      if(r44==false) {
        revert("Rule condition failed");
      }
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
      bool r2 = updateBurnOnInsertRecv_burn_r2(p,amount);
      if(r2==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r24 = updateTransferOnInsertRecv_transfer_r24(from,to,amount);
      if(r24==false) {
        revert("Rule condition failed");
      }
  }
  function buyToken(address p,int v) public    {
      bool r68 = updateBuyTokenOnInsertRecv_buyToken_r68(p,v);
      if(r68==false) {
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
      bool r30 = updateMintOnInsertRecv_mint_r30(p,amount);
      if(r30==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function updateBuyTokenOnInsertRecv_buyToken_r68(address p,int v) private   returns (bool) {
      uint t_1 = block.timestamp;
      bool b_3 = _finalized.b;
      int r_6 = _weiRaised.a;
      int h_2 = state.a;
      uint c_4 = _closingTime.a;
      int c_6 = _cap.a;
      uint o_1 = _openingTime.a;
      int balanceOf_x1 = balanceOf[p].n;
      if(balanceOf_x1>0 && p!=address(0) && t_1<=c_4 && v!=0 && t_1>=o_1 && h_2==0 && b_3!=true && r_6<=c_6) {
        int tokens_5 = getTokenAmount(v);
        if(tokens_5>0) {
          updateDepositOnInsertBuyToken_r19(p,v);
          updateRaisedOnInsertBuyToken_r29(v);
          updateMintOnInsertBuyToken_r5(p,v);
          emit BuyToken(p,v);
          return true;
        }
      }
      return false;
  }
  function updateTotalInOnInsertTransfer_r17(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r53(p,delta0);
  }
  function update_capOnInsertConstructor_r50(int n,int a) private    {
      if(a<=n) {
        _cap = _capTuple(n,true);
      }
  }
  function update_weiRaisedOnInsertRaised_r55(int n) private    {
      _weiRaised.a += n;
  }
  function updateAllBurnOnInsertBurn_r35(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r36(delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r34(address from,address to,address spender,int amount) private   returns (bool) {
      int k_2 = allowance[from][spender].n;
      int m_0 = balanceOf[from].n;
      if(amount<=m_0 && from!=address(0) && to!=address(0) && amount<=k_2) {
        updateTransferOnInsertTransferFrom_r54(from,to,amount);
        updateSpentTotalOnInsertTransferFrom_r69(from,spender,amount);
        emit TransferFrom(from,to,spender,amount);
        return true;
      }
      return false;
  }
  function update_finalizedOnInsertFinalize_r39() private    {
      _finalized = _finalizedTuple(true,true);
  }
  function updateTotalOutOnInsertTransfer_r74(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r53(p,delta0);
  }
  function updateAllowanceOnIncrementSpentTotal_r72(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function update_finalizedOnInsertConstructor_r41() private    {
      _finalized = _finalizedTuple(false,true);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r72(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateStateOnInsertFinalize_r77() private    {
      int n = _weiRaised.a;
      int a = _goal.a;
      if(n>=a) {
        state = StateTuple(2,true);
      }
  }
  function updateBurnOnInsertRecv_burn_r2(address p,int amount) private   returns (bool) {
      int m_1 = balanceOf[p].n;
      if(p!=address(0) && amount<=m_1) {
        updateTotalBurnOnInsertBurn_r18(p,amount);
        updateAllBurnOnInsertBurn_r35(amount);
        emit Burn(p,amount);
        return true;
      }
      return false;
  }
  function update_walletOnInsertConstructor_r25(address p) private    {
      // Empty()
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r65(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r72(o,s,delta0);
  }
  function updateRaisedOnInsertBuyToken_r29(int a) private    {
      update_weiRaisedOnInsertRaised_r55(a);
  }
  function updateTransferPrimaryOnInsertRecv_transferPrimary_r6(address p) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(balanceOf_x1>0) {
        update_primaryOnInsertTransferPrimary_r4(p);
        emit TransferPrimary(p);
        return true;
      }
      return false;
  }
  function updateTotalBurnOnInsertBurn_r18(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r53(p,delta0);
  }
  function updateAllMintOnInsertMint_r8(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r36(delta0);
  }
  function update_closingTimeOnInsertConstructor_r62(uint t2) private    {
      _closingTime = _closingTimeTuple(t2,true);
  }
  function update_primaryOnInsertTransferPrimary_r4(address p) private    {
      _primary = _primaryTuple(p,true);
  }
  function updateTotalSupplyOnInsertConstructor_r28() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function update_primaryOnInsertConstructor_r1() private    {
      address s = msg.sender;
      _primary = _primaryTuple(s,true);
  }
  function updateTotalMintOnInsertMint_r66(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r53(p,delta0);
  }
  function update_goalOnInsertConstructor_r46(int n,int a) private    {
      if(a<=n) {
        _goal = _goalTuple(a,true);
      }
  }
  function updateStateOnInsertFinalize_r37() private    {
      int n = _weiRaised.a;
      int a = _goal.a;
      if(n<a) {
        state = StateTuple(1,true);
      }
  }
  function updateSpentTotalOnInsertTransferFrom_r69(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r72(o,s,delta0);
  }
  function updateMintOnInsertRecv_mint_r30(address p,int amount) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(p!=address(0) && amount<balanceOf_x1) {
        updateTotalMintOnInsertMint_r66(p,amount);
        updateAllMintOnInsertMint_r8(amount);
        emit Mint(p,amount);
        return true;
      }
      return false;
  }
  function update_depositsOnInsertWithdraw_r60(address p) private    {
      int a = 0;
      _deposits[p] = _depositsTuple(a,true);
  }
  function update_openingTimeOnInsertConstructor_r13(uint t1) private    {
      _openingTime = _openingTimeTuple(t1,true);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateBalanceOfOnIncrementTotalIn_r53(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTransferOnInsertTransferFrom_r54(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r17(r,n);
      updateTotalOutOnInsertTransfer_r74(o,n);
      emit Transfer(o,r,n);
  }
  function updateTransferOnInsertRecv_transfer_r24(address from,address to,int amount) private   returns (bool) {
      int m_1 = balanceOf[from].n;
      if(to!=address(0) && from!=address(0) && amount<=m_1) {
        updateTotalInOnInsertTransfer_r17(to,amount);
        updateTotalOutOnInsertTransfer_r74(from,amount);
        emit Transfer(from,to,amount);
        return true;
      }
      return false;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r52(address p,address s,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[s].n;
      if(n<=balanceOf_x1) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r65(p,s,n);
        emit IncreaseAllowance(p,s,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r36(int b) private    {
      totalSupply.n -= b;
  }
  function updateFinalizeOnInsertRecv_finalize_r26() private   returns (bool) {
      bool b_2 = _finalized.b;
      address p_0 = _primary.p;
      uint c_3 = _closingTime.a;
      int h_1 = state.a;
      uint t_3 = block.timestamp;
      address s_0 = msg.sender;
      if(s_0==p_0 && h_1==0 && b_2!=true && t_3>=c_3) {
        updateStateOnInsertFinalize_r77();
        updateStateOnInsertFinalize_r37();
        update_finalizedOnInsertFinalize_r39();
        emit Finalize();
        return true;
      }
      return false;
  }
  function updateStateOnInsertConstructor_r14() private    {
      state = StateTuple(0,true);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateBalanceOfOnIncrementTotalMint_r53(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateTotalSupplyOnIncrementAllMint_r36(int m) private    {
      totalSupply.n += m;
  }
  function updateBalanceOfOnIncrementTotalBurn_r53(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateDepositOnInsertRecv_deposit_r11(address p,int a) private   returns (bool) {
      int h_1 = state.a;
      int balanceOf_x1 = balanceOf[p].n;
      if(a!=0 && p!=address(0) && h_1==0 && a<=balanceOf_x1) {
        update_depositsOnInsertDeposit_r3(p,a);
        emit Deposit(p,a);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r53(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateWithdrawOnInsertRecv_withdraw_r44(address p) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(balanceOf_x1>0) {
        update_depositsOnInsertWithdraw_r60(p);
        emit Withdraw(p);
        return true;
      }
      return false;
  }
  function updateMintOnInsertConstructor_r40() private    {
      address s = msg.sender;
      updateAllMintOnInsertMint_r8(int(1));
      updateTotalMintOnInsertMint_r66(s,int(1));
      emit Mint(s,1);
  }
  function updateDepositOnInsertBuyToken_r19(address p,int n) private    {
      update_depositsOnInsertDeposit_r3(p,n);
      emit Deposit(p,n);
  }
  function update_depositsOnInsertDeposit_r3(address p,int n) private    {
      _deposits[p].a += n;
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
  function updateMintOnInsertBuyToken_r5(address p,int v) private    {
      int tokens = getTokenAmount(v);
      updateAllMintOnInsertMint_r8(tokens);
      updateTotalMintOnInsertMint_r66(p,tokens);
      emit Mint(p,tokens);
  }
}