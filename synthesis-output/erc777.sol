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
  struct DefaultOperatorTuple {
    bool b;
    bool _valid;
  }
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  mapping(address=>mapping(address=>OperatorsTuple)) operators;
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalSupplyTuple totalSupply;
  mapping(address=>mapping(address=>RevokedDefaultOperatorTuple)) revokedDefaultOperator;
  mapping(address=>DefaultOperatorTuple) defaultOperator;
  OwnerTuple owner;
  event Mint(address p,int amount);
  event TransferFrom(address from,address to,address spender,int amount);
  event OperatorSend(address o,address r,address s,int n,int data,int operatorData);
  event RevokeDefaultOperator(address p,address o);
  event Burn(address p,int amount);
  event ApproveOperator(address p,address o);
  event OperatorBurn(address p,address s,int n,int data,int operatorData);
  event IncreaseAllowance(address p,address s,int d);
  event Transfer(address from,address to,int amount);
  event InvalidTx();
  constructor() public {
    updateOwnerOnInsertConstructor_r55();
    updateTotalSupplyOnInsertConstructor_r30();
  }
  function getOperators(address p,address o) public view  returns (bool) {
      bool b = operators[p][o].b;
      return b;
  }
  function approveOperator(address p,address o) public    {
      bool r4 = updateApproveOperatorOnInsertRecv_approveOperator_r4(p,o);
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r19 = updateBurnOnInsertRecv_burn_r19(p,amount);
      if(r19==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function increaseAllowance(address p,address s,int d) public    {
      bool r14 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r14(p,s,d);
      if(r14==false) {
        revert("Rule condition failed");
      }
  }
  function getRevokedDefaultOperator(address p,address o) public view  returns (bool) {
      bool b = revokedDefaultOperator[p][o].b;
      return b;
  }
  function getDefaultOperator(address o) public view  returns (bool) {
      bool b = defaultOperator[o].b;
      return b;
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r26 = updateTransferFromOnInsertRecv_transferFrom_r26(from,to,spender,amount);
      if(r26==false) {
        revert("Rule condition failed");
      }
  }
  function operatorBurn(address p,address s,int n,int data,int operatorData) public    {
      bool r57 = updateOperatorBurnOnInsertRecv_operatorBurn_r57(p,s,n,data,operatorData);
      if(r57==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function mint(address p,int amount) public    {
      bool r46 = updateMintOnInsertRecv_mint_r46(p,amount);
      if(r46==false) {
        revert("Rule condition failed");
      }
  }
  function revokeDefaultOperator(address p,address o) public    {
      bool r51 = updateRevokeDefaultOperatorOnInsertRecv_revokeDefaultOperator_r51(p,o);
      if(r51==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r48 = updateTransferOnInsertRecv_transfer_r48(from,to,amount);
      if(r48==false) {
        revert("Rule condition failed");
      }
  }
  function operatorSend(address o,address r,address s,int n,int data,int operatorData) public    {
      bool r49 = updateOperatorSendOnInsertRecv_operatorSend_r49(o,r,s,n,data,operatorData);
      if(r49==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function updateOwnerOnInsertConstructor_r55() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateBurnOnInsertRecv_burn_r19(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      int m_2 = balanceOf[p].n;
      if(p!=address(0) && o_1==s_1 && n<=m_2) {
        updateAllBurnOnInsertBurn_r31(n);
        updateTotalBurnOnInsertBurn_r20(p,n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementSpentTotal_r62(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTotalSupplyOnInsertConstructor_r30() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateBalanceOfOnIncrementTotalIn_r7(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateRevokeDefaultOperatorOnInsertRecv_revokeDefaultOperator_r51(address p,address o) private   returns (bool) {
      if(p!=address(0) && p!=o && o!=address(0)) {
        updateOperatorsOnInsertRevokeDefaultOperator_r24(p,o);
        updateRevokedDefaultOperatorOnInsertRevokeDefaultOperator_r60(p,o);
        emit RevokeDefaultOperator(p,o);
        return true;
      }
      return false;
  }
  function updateTotalMintOnInsertMint_r52(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r7(p,delta0);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r62(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateTransferOnInsertRecv_transfer_r48(address from,address to,int amount) private   returns (bool) {
      int m_1 = balanceOf[s].n;
      if(r!=address(0) && n>0 && s!=address(0) && n<=m_1) {
        updateTotalInOnInsertTransfer_r17(r,n);
        updateTotalOutOnInsertTransfer_r9(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r32(int b) private    {
      totalSupply.n -= b;
  }
  function updateBurnOnInsertOperatorBurn_r54(address p,int n) private    {
      updateAllBurnOnInsertBurn_r31(n);
      updateTotalBurnOnInsertBurn_r20(p,n);
      emit Burn(p,n);
  }
  function updateBalanceOfOnIncrementTotalBurn_r7(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r14(address p,address s,int d) private   returns (bool) {
      if(d>=0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r56(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r26(address from,address to,address spender,int amount) private   returns (bool) {
      int k_2 = allowance[o][sp].n;
      int m_0 = balanceOf[o].n;
      if(r!=address(0) && sp!=address(0) && n<=k_2 && n<=m_0 && o!=address(0) && n>0) {
        updateSpentTotalOnInsertTransferFrom_r59(o,sp,n);
        updateTransferOnInsertTransferFrom_r42(o,r,n);
        emit TransferFrom(o,r,sp,n);
        return true;
      }
      return false;
  }
  function updateTotalInOnInsertTransfer_r17(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r7(p,delta0);
  }
  function updateApproveOperatorOnInsertRecv_approveOperator_r4(address p,address o) private   returns (bool) {
      if(o!=address(0) && p!=address(0) && p!=o) {
        updateOperatorsOnInsertApproveOperator_r15(p,o);
        updateRevokedDefaultOperatorOnInsertApproveOperator_r58(p,o);
        emit ApproveOperator(p,o);
        return true;
      }
      return false;
  }
  function updateTotalBurnOnInsertBurn_r20(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r7(p,delta0);
  }
  function updateAllMintOnInsertMint_r12(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r32(delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateOperatorSendOnInsertRecv_operatorSend_r49(address o,address r,address s,int n,int data,int operatorData) private   returns (bool) {
      int m_1 = balanceOf[o].n;
      if(r!=address(0) && n<=m_1 && operatorData>=0 && o!=address(0) && data>=0 && n>0) {
        updateTransferOnInsertOperatorSend_r6(o,r,n);
        emit OperatorSend(o,r,s,n,data,operatorData);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertTransferFrom_r42(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r9(o,n);
      updateTotalInOnInsertTransfer_r17(r,n);
      emit Transfer(o,r,n);
  }
  function updateTotalOutOnInsertTransfer_r9(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r7(p,delta0);
  }
  function updateOperatorsOnInsertApproveOperator_r15(address p,address o) private    {
      operators[p][o] = OperatorsTuple(true,true);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateSpentTotalOnInsertTransferFrom_r59(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r62(o,s,delta0);
  }
  function updateTransferOnInsertOperatorSend_r6(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r9(o,n);
      updateTotalInOnInsertTransfer_r17(r,n);
      emit Transfer(o,r,n);
  }
  function updateOperatorsOnInsertRevokeDefaultOperator_r24(address p,address o) private    {
      operators[p][o] = OperatorsTuple(false,true);
  }
  function updateAllBurnOnInsertBurn_r31(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r32(delta0);
  }
  function updateRevokedDefaultOperatorOnInsertApproveOperator_r58(address p,address o) private    {
      revokedDefaultOperator[o][p] = RevokedDefaultOperatorTuple(false,true);
  }
  function updateRevokedDefaultOperatorOnInsertRevokeDefaultOperator_r60(address p,address o) private    {
      revokedDefaultOperator[p][o] = RevokedDefaultOperatorTuple(true,true);
  }
  function updateTotalSupplyOnIncrementAllMint_r32(int m) private    {
      totalSupply.n += m;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r56(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r62(o,s,delta0);
  }
  function updateBalanceOfOnIncrementTotalOut_r7(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateBalanceOfOnIncrementTotalMint_r7(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateOperatorBurnOnInsertRecv_operatorBurn_r57(address p,address s,int n,int data,int operatorData) private   returns (bool) {
      int m_1 = balanceOf[p].n;
      if(p!=address(0) && n<=m_1 && operatorData>=0 && data>=0 && n>0) {
        updateBurnOnInsertOperatorBurn_r54(p,n);
        emit OperatorBurn(p,s,n,data,operatorData);
        return true;
      }
      return false;
  }
  function updateMintOnInsertRecv_mint_r46(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(p!=address(0) && o_1==s_1) {
        updateAllMintOnInsertMint_r12(n);
        updateTotalMintOnInsertMint_r52(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
}