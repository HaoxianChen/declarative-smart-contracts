contract Shib {
  struct AllowanceTuple {
    int n;
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
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalSupplyTuple totalSupply;
  OwnerTuple owner;
  event InvalidTx();
  event Transfer(address s,address r,int n);
  event Mint(address p,int amount);
  event Burn(address p,int amount);
  event TransferFrom(address o,address r,address s,int n);
  event BurnFrom(address p,address from,int n);
  event IncreaseAllowance(address p,address s,int d);
  constructor() public {
    updateOwnerOnInsertConstructor_r22();
    updateTotalSupplyOnInsertConstructor_r7();
  }
  function transfer(address s,address r,int n) public    {
      bool r4 = updateTransferOnInsertRecv_transfer_r4(s,r,n);
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address o,address r,address s,int n) public    {
      bool r3 = updateTransferFromOnInsertRecv_transferFrom_r3(o,r,s,n);
      if(r3==false) {
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
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function burn(address p,int amount) public    {
      bool r10 = updateBurnOnInsertRecv_burn_r10(p,amount);
      if(r10==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r6 = updateMintOnInsertRecv_mint_r6(p,amount);
      if(r6==false) {
        revert("Rule condition failed");
      }
  }
  function burnFrom(address p,address from,int n) public    {
      bool r5 = updateBurnFromOnInsertRecv_burnFrom_r5(p,from,n);
      if(r5==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int d) public    {
      bool r17 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r17(p,s,d);
      if(r17==false) {
        revert("Rule condition failed");
      }
  }
  function updateBalanceOfOnIncrementTotalMint_r20(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r3(address o,address r,address s,int n) private   returns (bool) {
      address msgSender = msg.sender;
      int allowance_x2 = allowance[msgSender][o].n;
      if(allowance_x2>0) {
        updateSpentTotalOnInsertTransferFrom_r16(o,s,n);
        updateTransferOnInsertTransferFrom_r19(o,r,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r17(address p,address s,int d) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r23(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateSpentTotalOnInsertTransferFrom_r16(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r24(o,s,delta0);
  }
  function updateMintOnInsertRecv_mint_r6(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>=0) {
        updateTotalMintOnInsertMint_r21(p,n);
        updateAllMintOnInsertMint_r0(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalIn_r20(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTransferOnInsertTransferFrom_r19(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r9(r,n);
      updateTotalOutOnInsertTransfer_r25(o,n);
      emit Transfer(o,r,n);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateBalanceOfOnIncrementTotalOut_r20(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateTotalSupplyOnIncrementAllMint_r15(int m) private    {
      totalSupply.n += m;
  }
  function updateTotalMintOnInsertMint_r21(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r20(p,delta0);
  }
  function updateAllBurnOnInsertBurn_r14(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r15(delta0);
  }
  function updateTransferFromOnInsertBurnFrom_r12(address s,address p,int n) private    {
      updateTransferOnInsertTransferFrom_r19(s,p,n);
      updateSpentTotalOnInsertTransferFrom_r16(s,address(0),n);
      emit TransferFrom(s,p,address(0),n);
  }
  function updateTotalSupplyOnIncrementAllBurn_r15(int b) private    {
      totalSupply.n -= b;
  }
  function updateBurnFromOnInsertRecv_burnFrom_r5(address p,address from,int n) private   returns (bool) {
      address msgSender = msg.sender;
      int allowance_x2 = allowance[msgSender][p].n;
      if(0!=allowance_x2) {
        updateTransferFromOnInsertBurnFrom_r12(s,p,n);
        updateBurnOnInsertBurnFrom_r8(p,n);
        emit BurnFrom(s,p,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r24(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateTransferOnInsertRecv_transfer_r4(address s,address r,int n) private   returns (bool) {
      address msgSender = msg.sender;
      int allowance_x2 = allowance[msgSender][r].n;
      if(0!=allowance_x2) {
        updateTotalOutOnInsertTransfer_r25(s,n);
        updateTotalInOnInsertTransfer_r9(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r23(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r24(o,s,delta0);
  }
  function updateBalanceOfOnIncrementTotalBurn_r20(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateBurnOnInsertRecv_burn_r10(address p,int amount) private   returns (bool) {
      address o = owner.p;
      address msgSender = msg.sender;
      address s = msg.sender;
      int allowance_x2 = allowance[msgSender][p].n;
      if(o==s && 0!=allowance_x2) {
        updateTotalBurnOnInsertBurn_r2(p,n);
        updateAllBurnOnInsertBurn_r14(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateBurnOnInsertBurnFrom_r8(address p,int n) private    {
      updateTotalBurnOnInsertBurn_r2(p,n);
      updateAllBurnOnInsertBurn_r14(n);
      emit Burn(p,n);
  }
  function updateTotalOutOnInsertTransfer_r25(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r20(p,delta0);
  }
  function updateTotalInOnInsertTransfer_r9(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r20(p,delta0);
  }
  function updateTotalBurnOnInsertBurn_r2(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r20(p,delta0);
  }
  function updateOwnerOnInsertConstructor_r22() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateAllowanceOnIncrementSpentTotal_r24(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTotalSupplyOnInsertConstructor_r7() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r15(delta0);
  }
}