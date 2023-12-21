contract Erc20 {
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct AllMintTuple {
    uint n;
    bool _valid;
  }
  struct AllowanceTuple {
    uint n;
    bool _valid;
  }
  struct TotalBalancesTuple {
    uint m;
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
  AllMintTuple allMint;
  TotalBalancesTuple totalBalances;
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  OwnerTuple owner;
  mapping(address=>BalanceOfTuple) balanceOf;
  AllBurnTuple allBurn;
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
  function getAllowance(address p,address s) public view  returns (uint) {
      uint n = allowance[p][s].n;
      return n;
  }
  function getBalanceOf(address p) public view  returns (uint) {
      uint n = balanceOf[p].n;
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
  function getTotalSupply() public view  returns (uint) {
      uint n = totalSupply();
      return n;
  }
  function mint(address p,uint amount) public    {
      bool r16 = updateMintOnInsertRecv_mint_r16(p,amount);
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function updateTotalBurnOnInsertBurn_r9(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r5(p,delta0);
  }
  function totalSupply() private view  returns (uint) {
      uint b = allBurn.n;
      uint m = allMint.n;
      uint n = m-b;
      return n;
  }
  function updateAllowanceOnIncrementSpentTotal_r15(address o,address s,int l) private    {
      int _delta = int(-l);
      uint newValue = updateuintByint(allowance[o][s].n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateOwnerOnInsertConstructor_r7() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTotalOutOnInsertTransfer_r13(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r5(p,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
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
  function updateBalanceOfOnIncrementTotalMint_r5(address p,int n) private    {
      int _delta = int(n);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateSpentTotalOnInsertTransferFrom_r6(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r15(o,s,delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r18(address o,address r,uint n) private   returns (bool) {
      address s = msg.sender;
      uint k = allowance[o][s].n;
      uint m = balanceOf[o].n;
      if(m>=n && k>=n) {
        updateTransferOnInsertTransferFrom_r0(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r6(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertTransferFrom_r0(address o,address r,uint n) private    {
      updateTotalOutOnInsertTransfer_r13(o,n);
      updateTotalInOnInsertTransfer_r8(r,n);
      emit Transfer(o,r,n);
  }
  function updateAllMintOnInsertMint_r2(uint n) private    {
      allMint.n += n;
  }
  function updateAllBurnOnInsertBurn_r19(uint n) private    {
      allBurn.n += n;
  }
  function updateBurnOnInsertRecv_burn_r4(address p,uint n) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        uint m = balanceOf[p].n;
        if(p!=address(0) && n<=m) {
          updateTotalBurnOnInsertBurn_r9(p,n);
          updateAllBurnOnInsertBurn_r19(n);
          emit Burn(p,n);
          return true;
        }
      }
      return false;
  }
  function updateTotalInOnInsertTransfer_r8(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r5(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalBurn_r5(address p,int m) private    {
      int _delta = int(-m);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateTotalSupplyOnInsertConstructor_r1() private    {
      // Empty()
  }
  function updateTransferOnInsertRecv_transfer_r12(address r,uint n) private   returns (bool) {
      address s = msg.sender;
      uint m = balanceOf[s].n;
      if(n<=m) {
        updateTotalOutOnInsertTransfer_r13(s,n);
        updateTotalInOnInsertTransfer_r8(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r15(address o,address s,int m) private    {
      int _delta = int(m);
      uint newValue = updateuintByint(allowance[o][s].n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r20(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r15(o,s,delta0);
  }
  function updateBalanceOfOnIncrementTotalIn_r5(address p,int i) private    {
      int _delta = int(i);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
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
  function updateTotalMintOnInsertMint_r10(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r5(p,delta0);
  }
  function updateTotalBalancesOnInsertConstructor_r21() private    {
      totalBalances = TotalBalancesTuple(0,true);
  }
  function updateBalanceOfOnIncrementTotalOut_r5(address p,int o) private    {
      int _delta = int(-o);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
}