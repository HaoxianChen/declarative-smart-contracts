import "./crowdsale_udf.sol";
contract Crowdsale is CrowdsaleUDF {
  struct AllowanceTuple {
    int n;
    bool _valid;
  }
  struct BalanceOfTuple {
    int n;
    bool _valid;
  }
  struct _primaryTuple {
    address p;
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
  _primaryTuple _primary;
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  mapping(address=>_depositsTuple) _deposits;
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalSupplyTuple totalSupply;
  event Withdraw(address p);
  event FinalizeNotByPrimary();
  event Deposit(address p,int a);
  event Burn(address p,int amount);
  event TransferPrimaryNotByPrimary();
  event Transfer(address from,address to,int amount);
  event Finalize();
  event Mint(address p,int amount);
  event BuyToken(address p,int v);
  event TransferFrom(address from,address to,address spender,int amount);
  event ClaimRefund(address p);
  event TransferPrimary(address p);
  event IncreaseAllowance(address p,address s,int n);
  constructor(uint t1,uint t2,address p,int cap,int goal) public {
    update_goalOnInsertConstructor_r35(cap,goal);
    updateTotalSupplyOnInsertConstructor_r17();
    updateOnceWithdrawOutsideSuccessOnInsertConstructor_r21();
    updateOnceClaimRefundOutsideRefundOnInsertConstructor_r6();
    updateMintOnInsertConstructor_r33();
    update_closingTimeOnInsertConstructor_r45(t2);
    update_capOnInsertConstructor_r3(cap,goal);
    updateStateOnInsertConstructor_r12();
    update_walletOnInsertConstructor_r16(p);
    updateOnceBuyAfterFinalizeOnInsertConstructor_r48();
    update_openingTimeOnInsertConstructor_r11(t1);
    update_finalizedOnInsertConstructor_r34();
    update_primaryOnInsertConstructor_r20();
  }
  function deposit(address p,int a) public    {
      bool r32 = updateDepositOnInsertRecv_deposit_r32(p,a);
      if(r32==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r58 = updateBurnOnInsertRecv_burn_r58(p,amount);
      if(r58==false) {
        revert("Rule condition failed");
      }
  }
  function transferPrimary(address p) public    {
      bool r51 = updateTransferPrimaryOnInsertRecv_transferPrimary_r51(p);
      if(r51==false) {
        revert("Rule condition failed");
      }
  }
  function get_deposits(address p) public view  returns (int) {
      int a = _deposits[p].a;
      return a;
  }
  function mint(address p,int amount) public    {
      bool r18 = updateMintOnInsertRecv_mint_r18(p,amount);
      if(r18==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function transfer(address from,address to,int amount) public    {
      bool r50 = updateTransferOnInsertRecv_transfer_r50(from,to,amount);
      if(r50==false) {
        revert("Rule condition failed");
      }
  }
  function finalize() public    {
      bool r28 = updateFinalizeOnInsertRecv_finalize_r28();
      if(r28==false) {
        revert("Rule condition failed");
      }
  }
  function claimRefund(address p) public    {
      bool r38 = updateClaimRefundOnInsertRecv_claimRefund_r38(p);
      if(r38==false) {
        revert("Rule condition failed");
      }
  }
  function buyToken(address p,int v) public    {
      bool r54 = updateBuyTokenOnInsertRecv_buyToken_r54(p,v);
      if(r54==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r15 = updateTransferFromOnInsertRecv_transferFrom_r15(from,to,spender,amount);
      if(r15==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function withdraw(address p) public    {
      bool r57 = updateWithdrawOnInsertRecv_withdraw_r57(p);
      if(r57==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r22 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r22(p,s,n);
      if(r22==false) {
        revert("Rule condition failed");
      }
  }
  function updateTotalBurnOnInsertBurn_r13(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r37(p,delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r47(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r49(o,s,delta0);
  }
  function updateTotalSupplyOnIncrementAllBurn_r24(int b) private    {
      totalSupply.n -= b;
  }
  function updateBuyTokenOnInsertRecv_buyToken_r54(address p,int v) private   returns (bool) {
      updateDepositOnInsertBuyToken_r14(p,v);
      updateMintOnInsertBuyToken_r52(p,v);
      emit BuyToken(p,v);
      return true;
      return false;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateAllMintOnInsertMint_r7(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r24(delta0);
  }
  function updateOnceClaimRefundOutsideRefundOnInsertConstructor_r6() private    {
      // Empty()
  }
  function updateTransferOnInsertTransferFrom_r39(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r55(o,n);
      updateTotalInOnInsertTransfer_r10(r,n);
      emit Transfer(o,r,n);
  }
  function update_depositsOnInsertWithdraw_r42(address p) private    {
      int a = 0;
      _deposits[p] = _depositsTuple(a,true);
  }
  function updateTotalSupplyOnIncrementAllMint_r24(int m) private    {
      totalSupply.n += m;
  }
  function updateMintOnInsertBuyToken_r52(address p,int v) private    {
      int tokens = getTokenAmount(v);
      updateTotalMintOnInsertMint_r4(p,tokens);
      updateAllMintOnInsertMint_r7(tokens);
      emit Mint(p,tokens);
  }
  function update_finalizedOnInsertConstructor_r34() private    {
      // Empty()
  }
  function updateOnceWithdrawOutsideSuccessOnInsertConstructor_r21() private    {
      // Empty()
  }
  function update_depositsOnInsertDeposit_r29(address p,int n) private    {
      _deposits[p].a += n;
  }
  function update_primaryOnInsertTransferPrimary_r1(address p) private    {
      _primary = _primaryTuple(p,true);
  }
  function updateSpentTotalOnInsertTransferFrom_r5(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r49(o,s,delta0);
  }
  function updateDepositOnInsertBuyToken_r14(address p,int n) private    {
      update_depositsOnInsertDeposit_r29(p,n);
      emit Deposit(p,n);
  }
  function updateTransferPrimaryOnInsertRecv_transferPrimary_r51(address p) private   returns (bool) {
      address msgSender = msg.sender;
      if(p==_primary.p) {
        int _deposits_x1 = _deposits[msgSender].a;
        if(msgSender==p && _deposits_x1>0) {
          update_primaryOnInsertTransferPrimary_r1(p);
          emit TransferPrimary(p);
          return true;
        }
      }
      return false;
  }
  function updateOnceBuyAfterFinalizeOnInsertConstructor_r48() private    {
      // Empty()
  }
  function update_closingTimeOnInsertConstructor_r45(uint t2) private    {
      // Empty()
  }
  function updateWithdrawOnInsertRecv_withdraw_r57(address p) private   returns (bool) {
      address msgSender = msg.sender;
      int _deposits_x1 = _deposits[msgSender].a;
      if(0!=_deposits_x1) {
        update_depositsOnInsertWithdraw_r42(p);
        emit Withdraw(p);
        return true;
      }
      return false;
  }
  function update_openingTimeOnInsertConstructor_r11(uint t1) private    {
      // Empty()
  }
  function updateTransferFromOnInsertRecv_transferFrom_r15(address from,address to,address spender,int amount) private   returns (bool) {
      int allowance_x2 = allowance[from][spender].n;
      if(0!=allowance_x2) {
        updateTransferOnInsertTransferFrom_r39(from,to,amount);
        updateSpentTotalOnInsertTransferFrom_r5(from,spender,amount);
        emit TransferFrom(from,to,spender,amount);
        return true;
      }
      return false;
  }
  function updateFinalizeOnInsertRecv_finalize_r28() private   returns (bool) {
      address s = msg.sender;
      address p = _primary.p;
      if(s==p) {
        emit Finalize();
        return true;
      }
      return false;
  }
  function update_primaryOnInsertConstructor_r20() private    {
      address s = msg.sender;
      _primary = _primaryTuple(s,true);
  }
  function updateStateOnInsertConstructor_r12() private    {
      // Empty()
  }
  function updateTotalOutOnInsertTransfer_r55(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r37(p,delta0);
  }
  function updateTransferOnInsertRecv_transfer_r50(address from,address to,int amount) private   returns (bool) {
      int balanceOf_x1 = balanceOf[from].n;
      if(amount<=balanceOf_x1) {
        updateTotalOutOnInsertTransfer_r55(from,amount);
        updateTotalInOnInsertTransfer_r10(to,amount);
        emit Transfer(from,to,amount);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementSpentTotal_r49(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function update_capOnInsertConstructor_r3(int n,int a) private    {
      if(a<=n) {
        // Empty()
      }
  }
  function updateTotalSupplyOnInsertConstructor_r17() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateBurnOnInsertRecv_burn_r58(address p,int amount) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(amount<=balanceOf_x1) {
        updateAllBurnOnInsertBurn_r23(amount);
        updateTotalBurnOnInsertBurn_r13(p,amount);
        emit Burn(p,amount);
        return true;
      }
      return false;
  }
  function updateTotalInOnInsertTransfer_r10(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r37(p,delta0);
  }
  function updateAllBurnOnInsertBurn_r23(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r24(delta0);
  }
  function updateTotalMintOnInsertMint_r4(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r37(p,delta0);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r22(address p,address s,int n) private   returns (bool) {
      if(0!=n) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r47(p,s,n);
        emit IncreaseAllowance(p,s,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalBurn_r37(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateMintOnInsertConstructor_r33() private    {
      address s = msg.sender;
      updateAllMintOnInsertMint_r7(int(1));
      updateTotalMintOnInsertMint_r4(s,int(1));
      emit Mint(s,1);
  }
  function updateBalanceOfOnIncrementTotalIn_r37(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateDepositOnInsertRecv_deposit_r32(address p,int a) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(a<=balanceOf_x1) {
        update_depositsOnInsertDeposit_r29(p,a);
        emit Deposit(p,a);
        return true;
      }
      return false;
  }
  function update_walletOnInsertConstructor_r16(address p) private    {
      // Empty()
  }
  function update_goalOnInsertConstructor_r35(int n,int a) private    {
      if(a<=n) {
        // Empty()
      }
  }
  function updateMintOnInsertRecv_mint_r18(address p,int amount) private   returns (bool) {
      address msgSender = msg.sender;
      int _deposits_x1 = _deposits[msgSender].a;
      if(0!=_deposits_x1) {
        updateTotalMintOnInsertMint_r4(p,amount);
        updateAllMintOnInsertMint_r7(amount);
        emit Mint(p,amount);
        return true;
      }
      return false;
  }
  function updateClaimRefundOnInsertRecv_claimRefund_r38(address p) private   returns (bool) {
      address msgSender = msg.sender;
      int _deposits_x1 = _deposits[msgSender].a;
      if(0!=_deposits_x1) {
        emit ClaimRefund(p);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r49(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateBalanceOfOnIncrementTotalMint_r37(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateBalanceOfOnIncrementTotalOut_r37(address p,int o) private    {
      balanceOf[p].n -= o;
  }
}