contract Erc777 {
  struct TotalInTuple {
    uint n;
    bool _valid;
  }
  struct TotalOutTuple {
    uint n;
    bool _valid;
  }
  struct TotalMintTuple {
    uint n;
    bool _valid;
  }
  struct RevokedDefaultOperatorTuple {
    bool b;
    bool _valid;
  }
  struct TotalBurnTuple {
    uint n;
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
  struct OperatorsTuple {
    bool b;
    bool _valid;
  }
  struct TotalSupplyTuple {
    uint n;
    bool _valid;
  }
  struct AllowanceTuple {
    uint n;
    bool _valid;
  }
  mapping(address=>TotalInTuple) totalIn;
  mapping(address=>TotalOutTuple) totalOut;
  mapping(address=>TotalBurnTuple) totalBurn;
  mapping(address=>TotalMintTuple) totalMint;
  mapping(address=>mapping(address=>RevokedDefaultOperatorTuple)) revokedDefaultOperator;
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  mapping(address=>DefaultOperatorTuple) defaultOperator;
  OwnerTuple owner;
  mapping(address=>mapping(address=>OperatorsTuple)) operators;
  TotalSupplyTuple totalSupply;
  event OperatorBurn(address p,address s,uint n,uint data,uint operatorData);
  event TransferFrom(address from,address to,address spender,uint amount);
  event Burn(address p,uint amount);
  event Mint(address p,uint amount);
  event IncreaseAllowance(address p,address s,uint n);
  event OperatorSend(address o,address r,address s,uint n,uint data,uint operatorData);
  event RevokedDefaultOperator(address p,address o,bool b);
  event Operators(address p,address o,bool b);
  event Transfer(address from,address to,uint amount);
  constructor() public {
    updateOwnerOnInsertConstructor_r8();
    updateTotalSupplyOnInsertConstructor_r12();
  }
  function getOperators(address p,address o) public view  returns (bool) {
      bool b = operators[p][o].b;
      return b;
  }
  function approveOperator(address o) public    {
      bool r0 = updateRevokedDefaultOperatorOnInsertRecv_approveOperator_r0(o);
      bool r5 = updateOperatorsOnInsertRecv_approveOperator_r5(o);
      if(r0==false && r5==false) {
        revert("Rule condition failed");
      }
  }
  function approve(address s,uint n) public    {
      bool r29 = updateIncreaseAllowanceOnInsertRecv_approve_r29(s,n);
      if(r29==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,uint amount) public    {
      bool r27 = updateMintOnInsertRecv_mint_r27(p,amount);
      if(r27==false) {
        revert("Rule condition failed");
      }
  }
  function getRevokedDefaultOperator(address p,address o) public view  returns (bool) {
      bool b = revokedDefaultOperator[p][o].b;
      return b;
  }
  function getTotalSupply() public view  returns (uint) {
      uint n = totalSupply.n;
      return n;
  }
  function getAllowance(address p,address s) public view  returns (uint) {
      uint n = allowance[p][s].n;
      return n;
  }
  function transfer(address to,uint amount) public    {
      bool r22 = updateTransferOnInsertRecv_transfer_r22(to,amount);
      if(r22==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,uint amount) public    {
      bool r30 = updateTransferFromOnInsertRecv_transferFrom_r30(from,to,amount);
      if(r30==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,uint amount) public    {
      bool r20 = updateBurnOnInsertRecv_burn_r20(p,amount);
      if(r20==false) {
        revert("Rule condition failed");
      }
  }
  function getDefaultOperator(address p) public view  returns (bool) {
      bool b = defaultOperator[p].b;
      return b;
  }
  function getBalanceOf(address p) public view  returns (uint) {
      uint n = balanceOf(p);
      return n;
  }
  function revokeDefaultOperator(address o) public    {
      bool r14 = updateRevokedDefaultOperatorOnInsertRecv_revokeDefaultOperator_r14(o);
      bool r21 = updateOperatorsOnInsertRecv_revokeDefaultOperator_r21(o);
      if(r14==false && r21==false) {
        revert("Rule condition failed");
      }
  }
  function updateTotalOutOnInsertTransfer_r23(address p,uint n) private    {
      totalOut[p].n += n;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r26(address o,address s,int m) private    {
      int _delta = int(m);
      uint newValue = updateuintByint(allowance[o][s].n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateAllMintOnInsertMint_r3(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r17(delta0);
  }
  function updateTotalBurnOnInsertBurn_r15(address p,uint n) private    {
      totalBurn[p].n += n;
  }
  function updateTotalSupplyOnIncrementAllBurn_r17(int b) private    {
      int _delta = int(-b);
      uint newValue = updateuintByint(totalSupply.n,_delta);
      totalSupply.n = newValue;
  }
  function updateRevokedDefaultOperatorOnInsertRecv_approveOperator_r0(address o) private   returns (bool) {
      address p = msg.sender;
      if(true==defaultOperator[o].b) {
        if(p!=o) {
          revokedDefaultOperator[o][p] = RevokedDefaultOperatorTuple(false,true);
          emit RevokedDefaultOperator(o,p,false);
          return true;
        }
      }
      return false;
  }
  function updateIncreaseAllowanceOnInsertRecv_approve_r29(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      uint m = allowance[o][s].n;
      uint d = n-m;
      updateAllowanceTotalOnInsertIncreaseAllowance_r10(o,s,d);
      emit IncreaseAllowance(o,s,d);
      return true;
      return false;
  }
  function updateMintOnInsertRecv_mint_r27(address p,uint n) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        if(p!=address(0)) {
          updateAllMintOnInsertMint_r3(n);
          updateTotalMintOnInsertMint_r16(p,n);
          emit Mint(p,n);
          return true;
        }
      }
      return false;
  }
  function updateRevokedDefaultOperatorOnInsertRecv_revokeDefaultOperator_r14(address o) private   returns (bool) {
      address p = msg.sender;
      if(true==defaultOperator[o].b) {
        if(p!=o) {
          revokedDefaultOperator[p][o] = RevokedDefaultOperatorTuple(true,true);
          emit RevokedDefaultOperator(p,o,true);
          return true;
        }
      }
      return false;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r30(address o,address r,uint n) private   returns (bool) {
      address s = msg.sender;
      uint k = allowance[o][s].n;
      uint m = balanceOf(o);
      if(m>=n && k>=n) {
        updateTransferOnInsertTransferFrom_r1(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r24(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateBurnOnInsertRecv_burn_r20(address p,uint n) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        uint m = balanceOf(p);
        if(p!=address(0) && n<=m) {
          updateAllBurnOnInsertBurn_r31(n);
          updateTotalBurnOnInsertBurn_r15(p,n);
          emit Burn(p,n);
          return true;
        }
      }
      return false;
  }
  function updateOwnerOnInsertConstructor_r8() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTotalMintOnInsertMint_r16(address p,uint n) private    {
      totalMint[p].n += n;
  }
  function balanceOf(address p) private view  returns (uint) {
      uint i = totalIn[p].n;
      uint o = totalOut[p].n;
      uint m = totalBurn[p].n;
      uint n = totalMint[p].n;
      uint s = ((n+i)-m)-o;
      return s;
  }
  function updateTotalSupplyOnIncrementAllMint_r17(int m) private    {
      int _delta = int(m);
      uint newValue = updateuintByint(totalSupply.n,_delta);
      totalSupply.n = newValue;
  }
  function updateTotalInOnInsertTransfer_r9(address p,uint n) private    {
      totalIn[p].n += n;
  }
  function updateAllowanceOnIncrementSpentTotal_r26(address o,address s,int l) private    {
      int _delta = int(-l);
      uint newValue = updateuintByint(allowance[o][s].n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateSpentTotalOnInsertTransferFrom_r24(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r26(o,s,delta0);
  }
  function updateAllBurnOnInsertBurn_r31(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r17(delta0);
  }
  function updateOperatorsOnInsertRecv_revokeDefaultOperator_r21(address o) private   returns (bool) {
      address p = msg.sender;
      if(false==defaultOperator[o].b) {
        if(p!=o) {
          operators[p][o] = OperatorsTuple(false,true);
          emit Operators(p,o,false);
          return true;
        }
      }
      return false;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r10(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r26(o,s,delta0);
  }
  function updateTransferOnInsertRecv_transfer_r22(address r,uint n) private   returns (bool) {
      address s = msg.sender;
      uint m = balanceOf(s);
      if(n<=m) {
        updateTotalOutOnInsertTransfer_r23(s,n);
        updateTotalInOnInsertTransfer_r9(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertTransferFrom_r1(address o,address r,uint n) private    {
      updateTotalOutOnInsertTransfer_r23(o,n);
      updateTotalInOnInsertTransfer_r9(r,n);
      emit Transfer(o,r,n);
  }
  function updateOperatorsOnInsertRecv_approveOperator_r5(address o) private   returns (bool) {
      address p = msg.sender;
      if(false==defaultOperator[o].b) {
        if(p!=o) {
          operators[p][o] = OperatorsTuple(true,true);
          emit Operators(p,o,true);
          return true;
        }
      }
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r12() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
}