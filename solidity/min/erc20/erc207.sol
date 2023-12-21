contract Erc20 {
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
  struct AllowanceTuple {
    uint n;
    bool _valid;
  }
  struct TotalBurnTuple {
    uint n;
    bool _valid;
  }
  struct TotalBalancesTuple {
    uint m;
    bool _valid;
  }
  mapping(address=>TotalInTuple) totalIn;
  mapping(address=>TotalOutTuple) totalOut;
  mapping(address=>TotalBurnTuple) totalBurn;
  mapping(address=>TotalMintTuple) totalMint;
  TotalSupplyTuple totalSupply;
  TotalBalancesTuple totalBalances;
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  OwnerTuple owner;
  event TransferFrom(address from,address to,address spender,uint amount);
  event Burn(address p,uint amount);
  event Mint(address p,uint amount);
  event IncreaseAllowance(address p,address s,uint n);
  event Transfer(address from,address to,uint amount);
  constructor() public {
    updateTotalSupplyOnInsertConstructor_r1();
    updateTotalBalancesOnInsertConstructor_r21();
    updateOwnerOnInsertConstructor_r7();
  }
  function transferFrom(address from,address to,uint amount) public    {
      bool r18 = updateTransferFromOnInsertRecv_transferFrom_r18(from,to,amount);
      if(r18==false) {
        revert("Rule condition failed");
      }
  }
  function approve(address s,uint n) public    {
      bool r17 = updateIncreaseAllowanceOnInsertRecv_approve_r17(s,n);
      if(r17==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,uint amount) public    {
      bool r16 = updateMintOnInsertRecv_mint_r16(p,amount);
      if(r16==false) {
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
  function transfer(address to,uint amount) public    {
      bool r12 = updateTransferOnInsertRecv_transfer_r12(to,amount);
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,uint amount) public    {
      bool r4 = updateBurnOnInsertRecv_burn_r4(p,amount);
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (uint) {
      uint n = balanceOf(p);
      return n;
  }
  function updateTransferOnInsertTransferFrom_r0(address o,address r,uint n) private    {
      updateTotalOutOnInsertTransfer_r13(o,n);
      updateTotalInOnInsertTransfer_r8(r,n);
      emit Transfer(o,r,n);
  }
  function updateTotalMintOnInsertMint_r10(address p,uint n) private    {
      totalMint[p].n += n;
  }
  function updateTotalSupplyOnIncrementAllBurn_r11(int b) private    {
      int _delta = int(-b);
      uint newValue = updateuintByint(totalSupply.n,_delta);
      totalSupply.n = newValue;
  }
  function updateOwnerOnInsertConstructor_r7() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateBurnOnInsertRecv_burn_r4(address p,uint n) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        uint m = balanceOf(p);
        if(p!=address(0) && n<=m) {
          updateTotalBurnOnInsertBurn_r9(p,n);
          updateAllBurnOnInsertBurn_r19(n);
          emit Burn(p,n);
          return true;
        }
      }
      return false;
  }
  function balanceOf(address p) private view  returns (uint) {
      uint i = totalIn[p].n;
      uint o = totalOut[p].n;
      uint m = totalBurn[p].n;
      uint n = totalMint[p].n;
      uint s = ((n+i)-m)-o;
      return s;
  }
  function updateIncreaseAllowanceOnInsertRecv_approve_r17(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      uint m = allowance[o][s].n;
      uint d = n-m;
      updateAllowanceTotalOnInsertIncreaseAllowance_r20(o,s,d);
      emit IncreaseAllowance(o,s,d);
      return true;
      return false;
  }
  function updateAllBurnOnInsertBurn_r19(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r11(delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r20(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r15(o,s,delta0);
  }
  function updateSpentTotalOnInsertTransferFrom_r6(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r15(o,s,delta0);
  }
  function updateAllMintOnInsertMint_r2(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r11(delta0);
  }
  function updateTotalOutOnInsertTransfer_r13(address p,uint n) private    {
      totalOut[p].n += n;
  }
  function updateTotalBalancesOnInsertConstructor_r21() private    {
      totalBalances = TotalBalancesTuple(0,true);
  }
  function updateTransferOnInsertRecv_transfer_r12(address r,uint n) private   returns (bool) {
      address s = msg.sender;
      uint m = balanceOf(s);
      if(n<=m) {
        updateTotalOutOnInsertTransfer_r13(s,n);
        updateTotalInOnInsertTransfer_r8(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementSpentTotal_r15(address o,address s,int l) private    {
      int _delta = int(-l);
      uint newValue = updateuintByint(allowance[o][s].n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r15(address o,address s,int m) private    {
      int _delta = int(m);
      uint newValue = updateuintByint(allowance[o][s].n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateTotalSupplyOnIncrementAllMint_r11(int m) private    {
      int _delta = int(m);
      uint newValue = updateuintByint(totalSupply.n,_delta);
      totalSupply.n = newValue;
  }
  function updateTotalInOnInsertTransfer_r8(address p,uint n) private    {
      totalIn[p].n += n;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalBurnOnInsertBurn_r9(address p,uint n) private    {
      totalBurn[p].n += n;
  }
  function updateTotalSupplyOnInsertConstructor_r1() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r18(address o,address r,uint n) private   returns (bool) {
      address s = msg.sender;
      uint k = allowance[o][s].n;
      uint m = balanceOf(o);
      if(m>=n && k>=n) {
        updateTransferOnInsertTransferFrom_r0(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r6(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateMintOnInsertRecv_mint_r16(address p,uint n) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        if(p!=address(0)) {
          updateTotalMintOnInsertMint_r10(p,n);
          updateAllMintOnInsertMint_r2(n);
          emit Mint(p,n);
          return true;
        }
      }
      return false;
  }
}