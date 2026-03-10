import "./erc777_udf.sol";
contract Erc777 is ERC777UDF {
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
    updateTotalSupplyOnInsertConstructor_r31();
    updateOwnerOnInsertConstructor_r55();
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
  function revokeDefaultOperator(address p,address o) public    {
      bool r51 = updateRevokeDefaultOperatorOnInsertRecv_revokeDefaultOperator_r51(p,o);
      if(r51==false) {
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
  function transfer(address from,address to,int amount) public    {
      bool r22 = updateTransferOnInsertRecv_transfer_r22(from,to,amount);
      if(r22==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r47 = updateMintOnInsertRecv_mint_r47(p,amount);
      if(r47==false) {
        revert("Rule condition failed");
      }
  }
  function operatorBurn(address p,address s,int n,int data,int operatorData) public    {
      bool r57 = updateOperatorBurnOnInsertRecv_operatorBurn_r57(p,s,n,data,operatorData);
      if(r57==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r27 = updateTransferFromOnInsertRecv_transferFrom_r27(from,to,spender,amount);
      if(r27==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
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
  function updateMintOnInsertRecv_mint_r47(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(p!=address(0) && o_1==s_1) {
        updateAllMintOnInsertMint_r12(amount);
        updateTotalMintOnInsertMint_r52(p,amount);
        emit Mint(p,amount);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertTransferFrom_r43(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r9(o,n);
      updateTotalInOnInsertTransfer_r17(r,n);
      emit Transfer(o,r,n);
  }
  function updateBalanceOfOnIncrementTotalBurn_r7(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateAllowanceOnIncrementSpentTotal_r62(address o,address s,int l) private    {
      allowance[o][s].n -= l;
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
  function updateAllMintOnInsertMint_r12(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r33(delta0);
  }
  function updateOwnerOnInsertConstructor_r55() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
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
  function updateTotalOutOnInsertTransfer_r9(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r7(p,delta0);
  }
  function updateTotalSupplyOnIncrementAllMint_r33(int m) private    {
      totalSupply.n += m;
  }
  function updateBurnOnInsertOperatorBurn_r54(address p,int n) private    {
      updateAllBurnOnInsertBurn_r32(n);
      updateTotalBurnOnInsertBurn_r20(p,n);
      emit Burn(p,n);
  }
  function updateRevokedDefaultOperatorOnInsertRevokeDefaultOperator_r60(address p,address o) private    {
      revokedDefaultOperator[p][o] = RevokedDefaultOperatorTuple(true,true);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r27(address from,address to,address spender,int amount) private   returns (bool) {
      int k_2 = allowance[from][spender].n;
      int m_0 = balanceOf[from].n;
      if(to!=address(0) && spender!=address(0) && amount<=k_2 && amount<=m_0 && from!=address(0) && amount>0) {
        updateSpentTotalOnInsertTransferFrom_r59(from,spender,amount);
        updateTransferOnInsertTransferFrom_r43(from,to,amount);
        emit TransferFrom(from,to,spender,amount);
        return true;
      }
      return false;
  }
  function updateOperatorsOnInsertApproveOperator_r15(address p,address o) private    {
      operators[p][o] = OperatorsTuple(true,true);
  }
  function updateTotalSupplyOnIncrementAllBurn_r33(int b) private    {
      totalSupply.n -= b;
  }
  function updateTotalSupplyOnInsertConstructor_r31() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateOperatorsOnInsertRevokeDefaultOperator_r25(address p,address o) private    {
      operators[p][o] = OperatorsTuple(false,true);
  }
  function updateTransferOnInsertRecv_transfer_r22(address from,address to,int amount) private   returns (bool) {
      int m_2 = balanceOf[from].n;
      if(to!=address(0) && from!=address(0) && amount<=m_2 && amount>0) {
        bool success_1 = callTokensReceived(from,to,amount);
        if(success_1!=false) {
          updateTotalInOnInsertTransfer_r17(to,amount);
          updateTotalOutOnInsertTransfer_r9(from,amount);
          emit Transfer(from,to,amount);
          return true;
        }
      }
      return false;
  }
  function updateRevokeDefaultOperatorOnInsertRecv_revokeDefaultOperator_r51(address p,address o) private   returns (bool) {
      if(p!=address(0) && p!=o && o!=address(0)) {
        updateRevokedDefaultOperatorOnInsertRevokeDefaultOperator_r60(p,o);
        updateOperatorsOnInsertRevokeDefaultOperator_r25(p,o);
        emit RevokeDefaultOperator(p,o);
        return true;
      }
      return false;
  }
  function updateRevokedDefaultOperatorOnInsertApproveOperator_r58(address p,address o) private    {
      revokedDefaultOperator[o][p] = RevokedDefaultOperatorTuple(false,true);
  }
  function updateTotalInOnInsertTransfer_r17(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r7(p,delta0);
  }
  function updateTransferOnInsertOperatorSend_r6(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r9(o,n);
      updateTotalInOnInsertTransfer_r17(r,n);
      emit Transfer(o,r,n);
  }
  function updateTotalBurnOnInsertBurn_r20(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r7(p,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateSpentTotalOnInsertTransferFrom_r59(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r62(o,s,delta0);
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
  function updateBurnOnInsertRecv_burn_r19(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      int m_2 = balanceOf[p].n;
      if(p!=address(0) && o_1==s_1 && amount<=m_2) {
        updateTotalBurnOnInsertBurn_r20(p,amount);
        updateAllBurnOnInsertBurn_r32(amount);
        emit Burn(p,amount);
        return true;
      }
      return false;
  }
  function updateAllBurnOnInsertBurn_r32(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r33(delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r56(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r62(o,s,delta0);
  }
  function updateTotalMintOnInsertMint_r52(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r7(p,delta0);
  }
  function updateApproveOperatorOnInsertRecv_approveOperator_r4(address p,address o) private   returns (bool) {
      if(p!=address(0) && o!=address(0) && p!=o) {
        updateOperatorsOnInsertApproveOperator_r15(p,o);
        updateRevokedDefaultOperatorOnInsertApproveOperator_r58(p,o);
        emit ApproveOperator(p,o);
        return true;
      }
      return false;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r14(address p,address s,int d) private   returns (bool) {
      if(d>=0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r56(p,s,d);
        emit IncreaseAllowance(p,s,d);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r62(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateBalanceOfOnIncrementTotalOut_r7(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateBalanceOfOnIncrementTotalMint_r7(address p,int n) private    {
      balanceOf[p].n += n;
  }
}