contract Erc777 {
  struct AllowanceTuple {
    int n;
    bool _valid;
  }
  struct OperatorsTuple {
    bool b;
    bool _valid;
  }
  struct BalanceOfTuple {
    int n;
    bool _valid;
  }
  struct TotalSupplyTuple {
    int n;
    bool _valid;
  }
  struct RevokedDefaultOperatorTuple {
    bool b;
    bool _valid;
  }
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct DefaultOperatorTuple {
    bool b;
    bool _valid;
  }
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  mapping(address=>mapping(address=>OperatorsTuple)) operators;
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalSupplyTuple totalSupply;
  mapping(address=>mapping(address=>RevokedDefaultOperatorTuple)) revokedDefaultOperator;
  mapping(address=>DefaultOperatorTuple) defaultOperator;
  OwnerTuple owner;
  event Transfer(address from,address to,int amount);
  event Mint(address p,int amount);
  event TransferFrom(address from,address to,address spender,int amount);
  event UnauthorizedBurn();
  event RevokeDefaultOperator(address p,address o);
  event Burn(address p,int amount);
  event ApproveOperator(address p,address o);
  event OperatorBurn(address p,address s,int n,int data,int operatorData);
  event IncreaseAllowance(address p,address s,int d);
  event UnauthorizedMint();
  event OperatorSend(address o,address r,address s,int n,int data,int operatorData);
  constructor() public {
    updateTotalSupplyOnInsertConstructor_r15();
    updateOwnerOnInsertConstructor_r27();
    updateOnceUnauthorizedOperatorSendOnInsertConstructor_r16();
    updateOnceUnauthorizedOperatorBurnOnInsertConstructor_r24();
  }
  function getOperators(address p,address o) public view  returns (bool) {
      bool b = operators[p][o].b;
      return b;
  }
  function burn(address p,int amount) public    {
      bool r34 = updateBurnOnInsertRecv_burn_r34(p,amount);
      if(r34==false) {
        revert("Rule condition failed");
      }
  }
  function getRevokedDefaultOperator(address p,address o) public view  returns (bool) {
      bool b = revokedDefaultOperator[p][o].b;
      return b;
  }
  function transfer(address from,address to,int amount) public    {
      bool r10 = updateTransferOnInsertRecv_transfer_r10(from,to,amount);
      if(r10==false) {
        revert("Rule condition failed");
      }
  }
  function getDefaultOperator(address o) public view  returns (bool) {
      bool b = defaultOperator[o].b;
      return b;
  }
  function operatorBurn(address p,address s,int n,int data,int operatorData) public    {
      bool r14 = updateOperatorBurnOnInsertRecv_operatorBurn_r14(p,s,n,data,operatorData);
      if(r14==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function approveOperator(address p,address o) public    {
      bool r3 = updateApproveOperatorOnInsertRecv_approveOperator_r3(p,o);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function revokeDefaultOperator(address p,address o) public    {
      bool r21 = updateRevokeDefaultOperatorOnInsertRecv_revokeDefaultOperator_r21(p,o);
      if(r21==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r35 = updateMintOnInsertRecv_mint_r35(p,amount);
      if(r35==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int d) public    {
      bool r22 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r22(p,s,d);
      if(r22==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function operatorSend(address o,address r,address s,int n,int data,int operatorData) public    {
      bool r37 = updateOperatorSendOnInsertRecv_operatorSend_r37(o,r,s,n,data,operatorData);
      if(r37==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r2 = updateTransferFromOnInsertRecv_transferFrom_r2(from,to,spender,amount);
      if(r2==false) {
        revert("Rule condition failed");
      }
  }
  function updateTransferOnInsertOperatorSend_r6(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r9(o,n);
      updateTotalInOnInsertTransfer_r12(r,n);
      emit Transfer(o,r,n);
  }
  function updateTotalBurnOnInsertBurn_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r7(p,delta0);
  }
  function updateTransferOnInsertRecv_transfer_r10(address from,address to,int amount) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[from].n;
      if(amount>0 && amount<=balanceOf_x1_1) {
        updateTotalInOnInsertTransfer_r12(to,amount);
        updateTotalOutOnInsertTransfer_r9(from,amount);
        emit Transfer(from,to,amount);
        return true;
      }
      return false;
  }
  function updateOnceUnauthorizedOperatorSendOnInsertConstructor_r16() private    {
      // Empty()
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r28(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r36(o,s,delta0);
  }
  function updateOperatorsOnInsertRevokeDefaultOperator_r4(address p,address o) private    {
      operators[p][o] = OperatorsTuple(false,true);
  }
  function updateTotalMintOnInsertMint_r25(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r7(p,delta0);
  }
  function updateTotalOutOnInsertTransfer_r9(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r7(p,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateOperatorSendOnInsertRecv_operatorSend_r37(address o,address r,address s,int n,int data,int operatorData) private   returns (bool) {
      updateTransferOnInsertOperatorSend_r6(o,r,n);
      emit OperatorSend(o,r,s,n,data,operatorData);
      return true;
      return false;
  }
  function updateRevokedDefaultOperatorOnInsertRevokeDefaultOperator_r32(address p,address o) private    {
      revokedDefaultOperator[p][o] = RevokedDefaultOperatorTuple(true,true);
  }
  function updateTransferOnInsertTransferFrom_r8(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r9(o,n);
      updateTotalInOnInsertTransfer_r12(r,n);
      emit Transfer(o,r,n);
  }
  function updateTotalSupplyOnIncrementAllMint_r20(int m) private    {
      totalSupply.n += m;
  }
  function updateTotalInOnInsertTransfer_r12(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r7(p,delta0);
  }
  function updateOperatorsOnInsertApproveOperator_r13(address p,address o) private    {
      operators[p][o] = OperatorsTuple(true,true);
  }
  function updateOnceUnauthorizedOperatorBurnOnInsertConstructor_r24() private    {
      // Empty()
  }
  function updateSpentTotalOnInsertTransferFrom_r31(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r36(o,s,delta0);
  }
  function updateBalanceOfOnIncrementTotalBurn_r7(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateBurnOnInsertOperatorBurn_r26(address p,int n) private    {
      updateTotalBurnOnInsertBurn_r1(p,n);
      updateAllBurnOnInsertBurn_r19(n);
      emit Burn(p,n);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r2(address from,address to,address spender,int amount) private   returns (bool) {
      int balanceOf_x1_2 = balanceOf[from].n;
      int allowance_x2_1 = allowance[from][spender].n;
      if(amount>0 && amount<=allowance_x2_1 && amount<balanceOf_x1_2) {
        updateTransferOnInsertTransferFrom_r8(from,to,amount);
        updateSpentTotalOnInsertTransferFrom_r31(from,spender,amount);
        emit TransferFrom(from,to,spender,amount);
        return true;
      }
      return false;
  }
  function updateOperatorBurnOnInsertRecv_operatorBurn_r14(address p,address s,int n,int data,int operatorData) private   returns (bool) {
      updateBurnOnInsertOperatorBurn_r26(p,n);
      emit OperatorBurn(p,s,n,data,operatorData);
      return true;
      return false;
  }
  function updateApproveOperatorOnInsertRecv_approveOperator_r3(address p,address o) private   returns (bool) {
      updateRevokedDefaultOperatorOnInsertApproveOperator_r30(p,o);
      updateOperatorsOnInsertApproveOperator_r13(p,o);
      emit ApproveOperator(p,o);
      return true;
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r15() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateOwnerOnInsertConstructor_r27() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r36(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateAllBurnOnInsertBurn_r19(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r20(delta0);
  }
  function updateMintOnInsertRecv_mint_r35(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && amount>0) {
        updateAllMintOnInsertMint_r0(amount);
        updateTotalMintOnInsertMint_r25(p,amount);
        emit Mint(p,amount);
        return true;
      }
      return false;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r22(address p,address s,int d) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r28(p,s,d);
        emit IncreaseAllowance(p,s,d);
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
  function updateBalanceOfOnIncrementTotalIn_r7(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateRevokedDefaultOperatorOnInsertApproveOperator_r30(address p,address o) private    {
      revokedDefaultOperator[o][p] = RevokedDefaultOperatorTuple(false,true);
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r20(delta0);
  }
  function updateAllowanceOnIncrementSpentTotal_r36(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTotalSupplyOnIncrementAllBurn_r20(int b) private    {
      totalSupply.n -= b;
  }
  function updateRevokeDefaultOperatorOnInsertRecv_revokeDefaultOperator_r21(address p,address o) private   returns (bool) {
      updateOperatorsOnInsertRevokeDefaultOperator_r4(p,o);
      updateRevokedDefaultOperatorOnInsertRevokeDefaultOperator_r32(p,o);
      emit RevokeDefaultOperator(p,o);
      return true;
      return false;
  }
  function updateBurnOnInsertRecv_burn_r34(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && amount<=balanceOf_x1) {
        updateAllBurnOnInsertBurn_r19(amount);
        updateTotalBurnOnInsertBurn_r1(p,amount);
        emit Burn(p,amount);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r7(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateBalanceOfOnIncrementTotalMint_r7(address p,int n) private    {
      balanceOf[p].n += n;
  }
}