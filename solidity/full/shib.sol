contract Shib {
  struct TotalInTuple {
    uint n;
    bool _valid;
  }
  struct TotalOutTuple {
    uint n;
    bool _valid;
  }
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct TotalMintTuple {
    uint n;
    bool _valid;
  }
  struct TotalSupplyTuple {
    uint n;
    bool _valid;
  }
  struct AllMintTuple {
    uint n;
    bool _valid;
  }
  struct AllowanceTotalTuple {
    uint m;
    bool _valid;
  }
  struct SpentTotalTuple {
    uint m;
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
  struct TransferFromTuple {
    address from;
    address to;
    address spender;
    uint amount;
    bool _valid;
  }
  struct BalanceOfTuple {
    uint n;
    bool _valid;
  }
  struct AllBurnTuple {
    uint n;
    bool _valid;
  }
  mapping(address=>TotalInTuple) totalIn;
  mapping(address=>TotalOutTuple) totalOut;
  mapping(address=>TotalBurnTuple) totalBurn;
  OwnerTuple owner;
  mapping(address=>TotalMintTuple) totalMint;
  TotalSupplyTuple totalSupply;
  AllMintTuple allMint;
  mapping(address=>mapping(address=>AllowanceTotalTuple)) allowanceTotal;
  TransferFromTuple transferFrom;
  mapping(address=>mapping(address=>SpentTotalTuple)) spentTotal;
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  mapping(address=>BalanceOfTuple) balanceOf;
  AllBurnTuple allBurn;
  event TransferFrom(address from,address to,address spender,uint amount);
  event Burn(address p,uint amount);
  event Mint(address p,uint amount);
  event IncreaseAllowance(address p,address s,uint n);
  event BurnFrom(address p,address from,uint n);
  event Transfer(address from,address to,uint amount);
  constructor() public {
    updateTotalSupplyOnInsertConstructor_r1();
    updateOwnerOnInsertConstructor_r8();
    updateTotalBalancesOnInsertConstructor_r22();
  }
  function burn(address p,uint amount) public    {
      bool r5 = updateBurnOnInsertRecv_burn_r5(p,amount);
      if(r5==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address to,uint amount) public    {
      bool r13 = updateTransferOnInsertRecv_transfer_r13(to,amount);
      if(r13==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,uint amount) public    {
      bool r18 = updateMintOnInsertRecv_mint_r18(p,amount);
      if(r18==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (uint) {
      uint n = totalSupply.n;
      return n;
  }
  function getAllowance(address p,address s) public view  returns (uint) {
      uint n = allowance[p][s].n;
      return n;
  }
  function getBalanceOf(address p) public view  returns (uint) {
      uint n = balanceOf[p].n;
      return n;
  }
  function burnFrom(address p,uint n) public    {
      bool r2 = updateBurnFromOnInsertRecv_burnFrom_r2(p,n);
      if(r2==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,uint amount) public    {
      bool r20 = updateTransferFromOnInsertRecv_transferFrom_r20(from,to,amount);
      if(r20==false) {
        revert("Rule condition failed");
      }
  }
  function approve(address s,uint n) public    {
      bool r19 = updateIncreaseAllowanceOnInsertRecv_approve_r19(s,n);
      if(r19==false) {
        revert("Rule condition failed");
      }
  }
  function updateTotalSupplyOnIncrementAllBurn_r12(int b) private    {
      int _delta = int(-b);
      uint newValue = updateuintByint(totalSupply.n,_delta);
      totalSupply.n = newValue;
  }
  function updateTotalOutOnInsertTransfer_r15(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r6(p,delta0);
      totalOut[p].n += n;
  }
  function updateSpentTotalOnInsertTransferFrom_r7(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r17(o,s,delta0);
      spentTotal[o][s].m += n;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r24(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r17(o,s,delta0);
      allowanceTotal[o][s].m += n;
  }
  function updateTotalSupplyOnIncrementAllMint_r12(int m) private    {
      int _delta = int(m);
      uint newValue = updateuintByint(totalSupply.n,_delta);
      totalSupply.n = newValue;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r20(address o,address r,uint n) private   returns (bool) {
      address s = msg.sender;
      uint k = allowance[o][s].n;
      uint m = balanceOf[o].n;
      if(m>=n && k>=n) {
        updateTransferOnInsertTransferFrom_r0(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r7(o,s,n);
        transferFrom = TransferFromTuple(o,r,s,n,true);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r6(address p,int o) private    {
      int _delta = int(-o);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateBurnFromOnInsertRecv_burnFrom_r2(address p,uint n) private   returns (bool) {
      address s = msg.sender;
      uint k = allowance[p][s].n;
      uint m = balanceOf[p].n;
      if(p!=address(0) && m>=n && k>=m) {
        updateBurnOnInsertBurnFrom_r14(p,n);
        updateTransferFromOnInsertBurnFrom_r23(s,p,n);
        emit BurnFrom(s,p,n);
        return true;
      }
      return false;
  }
  function updateOwnerOnInsertConstructor_r8() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateBurnOnInsertBurnFrom_r14(address p,uint n) private    {
      updateTotalBurnOnInsertBurn_r10(p,n);
      updateAllBurnOnInsertBurn_r21(n);
      emit Burn(p,n);
  }
  function updateBurnOnInsertRecv_burn_r5(address p,uint n) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        uint m = balanceOf[p].n;
        if(p!=address(0) && n<=m) {
          updateTotalBurnOnInsertBurn_r10(p,n);
          updateAllBurnOnInsertBurn_r21(n);
          emit Burn(p,n);
          return true;
        }
      }
      return false;
  }
  function updateMintOnInsertRecv_mint_r18(address p,uint n) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        if(p!=address(0)) {
          updateAllMintOnInsertMint_r3(n);
          updateTotalMintOnInsertMint_r11(p,n);
          emit Mint(p,n);
          return true;
        }
      }
      return false;
  }
  function updateTotalBurnOnInsertBurn_r10(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r6(p,delta0);
      totalBurn[p].n += n;
  }
  function updateAllowanceOnIncrementSpentTotal_r17(address o,address s,int l) private    {
      int _delta = int(-l);
      uint newValue = updateuintByint(allowance[o][s].n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r17(address o,address s,int m) private    {
      int _delta = int(m);
      uint newValue = updateuintByint(allowance[o][s].n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateTransferOnInsertTransferFrom_r0(address o,address r,uint n) private    {
      updateTotalOutOnInsertTransfer_r15(o,n);
      updateTotalInOnInsertTransfer_r9(r,n);
      emit Transfer(o,r,n);
  }
  function updateTransferOnInsertRecv_transfer_r13(address r,uint n) private   returns (bool) {
      address s = msg.sender;
      uint m = balanceOf[s].n;
      if(n<=m) {
        updateTotalOutOnInsertTransfer_r15(s,n);
        updateTotalInOnInsertTransfer_r9(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalBurn_r6(address p,int m) private    {
      int _delta = int(-m);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateBalanceOfOnIncrementTotalMint_r6(address p,int n) private    {
      int _delta = int(n);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateTotalInOnInsertTransfer_r9(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r6(p,delta0);
      totalIn[p].n += n;
  }
  function updateTotalMintOnInsertMint_r11(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r6(p,delta0);
      totalMint[p].n += n;
  }
  function updateTotalSupplyOnInsertConstructor_r1() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateTransferFromOnInsertBurnFrom_r23(address s,address p,uint n) private    {
      updateTransferOnInsertTransferFrom_r0(s,p,n);
      updateSpentTotalOnInsertTransferFrom_r7(s,address(0),n);
      transferFrom = TransferFromTuple(s,p,address(0),n,true);
      emit TransferFrom(s,p,address(0),n);
  }
  function updateAllMintOnInsertMint_r3(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r12(delta0);
      allMint.n += n;
  }
  function updateTotalBalancesOnInsertConstructor_r22() private    {
      // Empty()
  }
  function updateIncreaseAllowanceOnInsertRecv_approve_r19(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      uint m = allowance[o][s].n;
      uint d = n-m;
      updateAllowanceTotalOnInsertIncreaseAllowance_r24(o,s,d);
      emit IncreaseAllowance(o,s,d);
      return true;
      return false;
  }
  function updateBalanceOfOnIncrementTotalIn_r6(address p,int i) private    {
      int _delta = int(i);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateAllBurnOnInsertBurn_r21(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r12(delta0);
      allBurn.n += n;
  }
}