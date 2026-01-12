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
    updateOwnerOnInsertConstructor_r56();
    updateTotalSupplyOnInsertConstructor_r29();
  }
  function getOperators(address p,address o) public view  returns (bool) {
      bool b = operators[p][o].b;
      return b;
  }
  function mint(address p,int amount) public    {
      bool r48 = updateMintOnInsertRecv_mint_r48(p,amount);
      if(r48==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r25 = updateTransferFromOnInsertRecv_transferFrom_r25(from,to,spender,amount);
      if(r25==false) {
        revert("Rule condition failed");
      }
  }
  function revokeDefaultOperator(address p,address o) public    {
      bool r52 = updateRevokeDefaultOperatorOnInsertRecv_revokeDefaultOperator_r52(p,o);
      if(r52==false) {
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
  function operatorSend(address o,address r,address s,int n,int data,int operatorData) public    {
      bool r34 = updateOperatorSendOnInsertRecv_operatorSend_r34(o,r,s,n,data,operatorData);
      if(r34==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r50 = updateTransferOnInsertRecv_transfer_r50(from,to,amount);
      if(r50==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int d) public    {
      bool r13 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r13(p,s,d);
      if(r13==false) {
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
  function burn(address p,int amount) public    {
      bool r18 = updateBurnOnInsertRecv_burn_r18(p,amount);
      if(r18==false) {
        revert("Rule condition failed");
      }
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
  function operatorBurn(address p,address s,int n,int data,int operatorData) public    {
      bool r30 = updateOperatorBurnOnInsertRecv_operatorBurn_r30(p,s,n,data,operatorData);
      if(r30==false) {
        revert("Rule condition failed");
      }
  }
  function updateTransferOnInsertTransferFrom_r44(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r16(r,n);
      updateTotalOutOnInsertTransfer_r8(o,n);
      emit Transfer(o,r,n);
  }
  function updateApproveOperatorOnInsertRecv_approveOperator_r3(address p,address o) private   returns (bool) {
      if(o!=address(0) && p!=address(0) && p!=o) {
        updateOperatorsOnInsertApproveOperator_r14(p,o);
        updateRevokedDefaultOperatorOnInsertApproveOperator_r58(p,o);
        emit ApproveOperator(p,o);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementSpentTotal_r62(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r13(address p,address s,int d) private   returns (bool) {
      if(d>=0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r57(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateOwnerOnInsertConstructor_r56() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalMintOnInsertMint_r53(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r6(p,delta0);
  }
  function updateBurnOnInsertOperatorBurn_r55(address p,int n) private    {
      updateAllBurnOnInsertBurn_r31(n);
      updateTotalBurnOnInsertBurn_r19(p,n);
      emit Burn(p,n);
  }
  function updateTransferOnInsertRecv_transfer_r50(address from,address to,int amount) private   returns (bool) {
      int m_1 = balanceOf[s].n;
      if(r!=address(0) && n>0 && s!=address(0) && n<=m_1) {
        updateTotalInOnInsertTransfer_r16(r,n);
        updateTotalOutOnInsertTransfer_r8(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r25(address from,address to,address spender,int amount) private   returns (bool) {
      int k_2 = allowance[o][sp].n;
      int m_0 = balanceOf[o].n;
      if(r!=address(0) && sp!=address(0) && n<=k_2 && n<=m_0 && o!=address(0) && n>0) {
        updateSpentTotalOnInsertTransferFrom_r59(o,sp,n);
        updateTransferOnInsertTransferFrom_r44(o,r,n);
        emit TransferFrom(o,r,sp,n);
        return true;
      }
      return false;
  }
  function updateTotalOutOnInsertTransfer_r8(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r6(p,delta0);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r62(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateOperatorsOnInsertApproveOperator_r14(address p,address o) private    {
      operators[p][o] = OperatorsTuple(true,true);
  }
  function updateOperatorSendOnInsertRecv_operatorSend_r34(address o,address r,address s,int n,int data,int operatorData) private   returns (bool) {
      bool revokedDefaultOperator_x2 = revokedDefaultOperator[o][s].b;
      int m_1 = balanceOf[o].n;
      if(r!=address(0) && n<=m_1 && operatorData>=0 && o!=address(0) && n>0 && revokedDefaultOperator_x2==false && data>=0) {
        updateTransferOnInsertOperatorSend_r5(o,r,n);
        emit OperatorSend(o,r,s,n,data,operatorData);
        return true;
      }
      return false;
  }
  function updateTotalInOnInsertTransfer_r16(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r6(p,delta0);
  }
  function updateTotalSupplyOnIncrementAllBurn_r32(int b) private    {
      totalSupply.n -= b;
  }
  function updateTotalSupplyOnInsertConstructor_r29() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateAllMintOnInsertMint_r11(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r32(delta0);
  }
  function updateBalanceOfOnIncrementTotalIn_r6(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateRevokeDefaultOperatorOnInsertRecv_revokeDefaultOperator_r52(address p,address o) private   returns (bool) {
      if(p!=address(0) && p!=o && o!=address(0)) {
        updateOperatorsOnInsertRevokeDefaultOperator_r23(p,o);
        updateRevokedDefaultOperatorOnInsertRevokeDefaultOperator_r60(p,o);
        emit RevokeDefaultOperator(p,o);
        return true;
      }
      return false;
  }
  function updateTotalBurnOnInsertBurn_r19(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r6(p,delta0);
  }
  function updateMintOnInsertRecv_mint_r48(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(p!=address(0) && o_1==s_1) {
        updateTotalMintOnInsertMint_r53(p,n);
        updateAllMintOnInsertMint_r11(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateSpentTotalOnInsertTransferFrom_r59(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r62(o,s,delta0);
  }
  function updateTransferOnInsertOperatorSend_r5(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r16(r,n);
      updateTotalOutOnInsertTransfer_r8(o,n);
      emit Transfer(o,r,n);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r57(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r62(o,s,delta0);
  }
  function updateBalanceOfOnIncrementTotalMint_r6(address p,int n) private    {
      balanceOf[p].n += n;
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
  function updateBalanceOfOnIncrementTotalBurn_r6(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateBalanceOfOnIncrementTotalOut_r6(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateBurnOnInsertRecv_burn_r18(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      int m_2 = balanceOf[p].n;
      if(p!=address(0) && o_1==s_1 && n<=m_2) {
        updateAllBurnOnInsertBurn_r31(n);
        updateTotalBurnOnInsertBurn_r19(p,n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r32(int m) private    {
      totalSupply.n += m;
  }
  function updateOperatorBurnOnInsertRecv_operatorBurn_r30(address p,address s,int n,int data,int operatorData) private   returns (bool) {
      bool revokedDefaultOperator_x2 = revokedDefaultOperator[p][s].b;
      int m_1 = balanceOf[p].n;
      if(p!=address(0) && n<=m_1 && operatorData>=0 && n>0 && revokedDefaultOperator_x2==false && data>=0) {
        updateBurnOnInsertOperatorBurn_r55(p,n);
        emit OperatorBurn(p,s,n,data,operatorData);
        return true;
      }
      return false;
  }
  function updateOperatorsOnInsertRevokeDefaultOperator_r23(address p,address o) private    {
      operators[p][o] = OperatorsTuple(false,true);
  }
}