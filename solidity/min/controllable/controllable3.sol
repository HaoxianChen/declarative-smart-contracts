contract Controllable {
  struct TotalInTuple {
    uint n;
    bool _valid;
  }
  struct TotalOutTuple {
    uint n;
    bool _valid;
  }
  struct SymbolTuple {
    uint n;
    bool _valid;
  }
  struct AllowanceTuple {
    uint n;
    bool _valid;
  }
  struct TotalBurnTuple {
    uint n;
    bool _valid;
  }
  struct ControllerTuple {
    address p;
    bool _valid;
  }
  struct TotalMintTuple {
    uint n;
    bool _valid;
  }
  struct NameTuple {
    uint n;
    bool _valid;
  }
  struct TotalSupplyTuple {
    uint n;
    bool _valid;
  }
  struct DecimalsTuple {
    uint n;
    bool _valid;
  }
  mapping(address=>TotalInTuple) totalIn;
  mapping(address=>TotalOutTuple) totalOut;
  mapping(address=>TotalBurnTuple) totalBurn;
  ControllerTuple controller;
  mapping(address=>TotalMintTuple) totalMint;
  SymbolTuple symbol;
  DecimalsTuple decimals;
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  NameTuple name;
  TotalSupplyTuple totalSupply;
  event TransferFrom(address from,address to,address spender,uint amount);
  event Burn(address p,uint amount);
  event IncreaseAllowance(address owner,address spender,uint n);
  event Mint(address p,uint amount);
  event ControllerRedeem(address p,uint amount);
  event DecreaseAllowance(address owner,address spender,uint n);
  event ControllerTransfer(address from,address to,uint amount);
  event Transfer(address from,address to,uint amount);
  constructor(address p) public {
    updateNameOnInsertConstructor_r10();
    updateTotalSupplyOnInsertConstructor_r13();
    updateSymbolOnInsertConstructor_r31();
    updateTotalBalancesOnInsertConstructor_r7();
    updateDecimalsOnInsertConstructor_r9();
    updateOwnerOnInsertConstructor_r30();
    updateControllerOnInsertConstructor_r3(p);
  }
  function getSymbol() public view  returns (uint) {
      uint n = symbol.n;
      return n;
  }
  function controllerRedeem(address p,uint amount) public    {
      bool r6 = updateControllerRedeemOnInsertRecv_controllerRedeem_r6(p,amount);
      if(r6==false) {
        revert("Rule condition failed");
      }
  }
  function controllerTransfer(address from,address to,uint amount) public    {
      bool r15 = updateControllerTransferOnInsertRecv_controllerTransfer_r15(from,to,amount);
      if(r15==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (uint) {
      uint n = allowance[p][s].n;
      return n;
  }
  function getBalanceOf(address p) public view  returns (uint) {
      uint n = balanceOf[p].n;
      return n;
  }
  function getDecimals() public view  returns (uint) {
      uint n = decimals.n;
      return n;
  }
  function getName() public view  returns (uint) {
      uint n = name.n;
      return n;
  }
  function mint(address p,uint amount) public    {
      bool r16 = updateMintOnInsertRecv_mint_r16(p,amount);
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address spender,uint n) public    {
      bool r22 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r22(spender,n);
      if(r22==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (uint) {
      uint n = totalSupply.n;
      return n;
  }
  function approve(address s,uint n) public    {
      bool r2 = updateDecreaseAllowanceOnInsertRecv_approve_r2(s,n);
      bool r20 = updateIncreaseAllowanceOnInsertRecv_approve_r20(s,n);
      if(r2==false && r20==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,uint amount) public    {
      bool r29 = updateBurnOnInsertRecv_burn_r29(p,amount);
      if(r29==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address to,uint amount) public    {
      bool r24 = updateTransferOnInsertRecv_transfer_r24(to,amount);
      if(r24==false) {
        revert("Rule condition failed");
      }
  }
  function decreaseAllowance(address spender,uint n) public    {
      bool r17 = updateDecreaseAllowanceOnInsertRecv_decreaseAllowance_r17(spender,n);
      if(r17==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,uint amount) public    {
      bool r32 = updateTransferFromOnInsertRecv_transferFrom_r32(from,to,amount);
      if(r32==false) {
        revert("Rule condition failed");
      }
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r22(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      updateIncreaseAllowanceTotalOnInsertIncreaseAllowance_r5(o,s,n);
      emit IncreaseAllowance(o,s,n);
      return true;
      return false;
  }
  function updateMintOnInsertRecv_mint_r16(address p,uint n) private   returns (bool) {
      address s = msg.sender;
      if(p!=address(0)) {
        updateTotalMintOnInsertMint_r21(p,n);
        updateAllMintOnInsertMint_r14(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateControllerTransferOnInsertRecv_controllerTransfer_r15(address s,address r,uint n) private   returns (bool) {
      address c = controller.p;
      if(c==msg.sender) {
        if(n<=m && balanceOf(s,m)) {
          updateTransferOnInsertControllerTransfer_r18(s,r,n);
          emit ControllerTransfer(s,r,n);
          return true;
        }
      }
      return false;
  }
  function updateTotalOutOnInsertTransfer_r25(address p,uint n) private    {
      totalOut[p].n += n;
  }
  function updateTotalSupplyOnIncrementAllMint_r23(int m) private    {
      int _delta = int(m);
      uint newValue = updateuintByint(totalSupply.n,_delta);
      totalSupply.n = newValue;
  }
  function updateTotalBalancesOnInsertConstructor_r7() private    {
      // Empty()
  }
  function updateIncreaseAllowanceTotalOnInsertIncreaseAllowance_r5(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementIncreaseAllowanceTotal_r12(o,s,delta0);
  }
  function updateNameOnInsertConstructor_r10() private    {
      name = NameTuple(0,true);
  }
  function updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r1(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementDecreaseAllowanceTotal_r12(o,s,delta0);
  }
  function updateTotalSupplyOnIncrementAllBurn_r23(int b) private    {
      int _delta = int(-b);
      uint newValue = updateuintByint(totalSupply.n,_delta);
      totalSupply.n = newValue;
  }
  function updateTotalInOnInsertTransfer_r11(address p,uint n) private    {
      totalIn[p].n += n;
  }
  function updateDecimalsOnInsertConstructor_r9() private    {
      decimals = DecimalsTuple(18,true);
  }
  function updateDecreaseAllowanceOnInsertRecv_decreaseAllowance_r17(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r1(o,s,n);
      emit DecreaseAllowance(o,s,n);
      return true;
      return false;
  }
  function updateBurnOnInsertControllerRedeem_r27(address p,uint n) private    {
      updateAllBurnOnInsertBurn_r33(n);
      updateTotalBurnOnInsertBurn_r19(p,n);
      emit Burn(p,n);
  }
  function updateTransferOnInsertControllerTransfer_r18(address s,address r,uint n) private    {
      updateTotalInOnInsertTransfer_r11(r,n);
      updateTotalOutOnInsertTransfer_r25(s,n);
      emit Transfer(s,r,n);
  }
  function updateAllBurnOnInsertBurn_r33(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r23(delta0);
  }
  function updateControllerRedeemOnInsertRecv_controllerRedeem_r6(address p,uint n) private   returns (bool) {
      address c = controller.p;
      if(c==msg.sender) {
        if(p!=address(0) && n<=m && balanceOf(p,m)) {
          updateBurnOnInsertControllerRedeem_r27(p,n);
          emit ControllerRedeem(p,n);
          return true;
        }
      }
      return false;
  }
  function updateSymbolOnInsertConstructor_r31() private    {
      symbol = SymbolTuple(0,true);
  }
  function updateTransferOnInsertTransferFrom_r0(address o,address r,uint n) private    {
      updateTotalInOnInsertTransfer_r11(r,n);
      updateTotalOutOnInsertTransfer_r25(o,n);
      emit Transfer(o,r,n);
  }
  function updateTransferOnInsertRecv_transfer_r24(address r,uint n) private   returns (bool) {
      address s = msg.sender;
      if(n<=m && balanceOf(s,m)) {
        updateTotalInOnInsertTransfer_r11(r,n);
        updateTotalOutOnInsertTransfer_r25(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateDecreaseAllowanceOnInsertRecv_approve_r2(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      uint m = allowance[o][s].n;
      if(n<m) {
        uint d = m-n;
        updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r1(o,s,d);
        emit DecreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateBurnOnInsertRecv_burn_r29(address p,uint n) private   returns (bool) {
      address s = msg.sender;
      if(p!=address(0) && n<=m && balanceOf(p,m)) {
        updateAllBurnOnInsertBurn_r33(n);
        updateTotalBurnOnInsertBurn_r19(p,n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalMintOnInsertMint_r21(address p,uint n) private    {
      totalMint[p].n += n;
  }
  function updateAllowanceOnIncrementIncreaseAllowanceTotal_r12(address o,address s,int m) private    {
      int _delta = int(m);
      uint newValue = updateuintByint(allowance[o][s].n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateIncreaseAllowanceOnInsertRecv_approve_r20(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      uint m = allowance[o][s].n;
      if(n>=m) {
        uint d = n-m;
        updateIncreaseAllowanceTotalOnInsertIncreaseAllowance_r5(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r13() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateAllMintOnInsertMint_r14(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r23(delta0);
  }
  function updateAllowanceOnIncrementSpentTotal_r12(address o,address s,int l) private    {
      int _delta = int(-l);
      uint newValue = updateuintByint(allowance[o][s].n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateSpentTotalOnInsertTransferFrom_r8(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r12(o,s,delta0);
  }
  function updateAllowanceOnIncrementDecreaseAllowanceTotal_r12(address o,address s,int d) private    {
      int _delta = int(-d);
      uint newValue = updateuintByint(allowance[o][s].n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r32(address o,address r,uint n) private   returns (bool) {
      address s = msg.sender;
      uint k = allowance[o][s].n;
      if(m>=n && k>=n && balanceOf(o,m)) {
        updateSpentTotalOnInsertTransferFrom_r8(o,s,n);
        updateTransferOnInsertTransferFrom_r0(o,r,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateControllerOnInsertConstructor_r3(address p) private    {
      controller = ControllerTuple(p,true);
  }
  function updateTotalBurnOnInsertBurn_r19(address p,uint n) private    {
      totalBurn[p].n += n;
  }
  function balanceOf(address p,uint n) private view  returns (bool) {
      uint i = totalIn[p].n;
      uint o = totalOut[p].n;
      uint m = totalBurn[p].n;
      uint n = totalMint[p].n;
      return true;
      return false;
  }
  function updateOwnerOnInsertConstructor_r30() private    {
      address s = msg.sender;
      // Empty()
  }
}