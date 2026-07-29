contract Erc20roles {
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
  struct MinterTuple {
    address p;
    bool _valid;
  }
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalSupplyTuple totalSupply;
  MinterTuple minter;
  OwnerTuple owner;
  event Mint(address p,int amount);
  event TransferFrom(address from,address to,address spender,int amount);
  event SetMinter(address p);
  event Transfer(address from,address to,int amount);
  event InvalidTx();
  event IncreaseAllowance(address o,address s,int n);
  event Burn(address p,int amount);
  constructor() public {
    updateMinterOnInsertConstructor_r12();
    updateOwnerOnInsertConstructor_r22();
    updateTotalSupplyOnInsertConstructor_r17();
  }
  function getMinter() public view  returns (address) {
      address p = minter.p;
      return p;
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function setMinter(address p) public    {
      bool r4 = updateSetMinterOnInsertRecv_setMinter_r4(p);
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r16 = updateTransferFromOnInsertRecv_transferFrom_r16(from,to,spender,amount);
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r6 = updateMintOnInsertRecv_mint_r6(p,amount);
      if(r6==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r26 = updateTransferOnInsertRecv_transfer_r26(from,to,amount);
      if(r26==false) {
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
  function increaseAllowance(address o,address s,int n) public    {
      bool r24 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r24(o,s,n);
      if(r24==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r25 = updateBurnOnInsertRecv_burn_r25(p,amount);
      if(r25==false) {
        revert("Rule condition failed");
      }
  }
  function updateTotalBurnOnInsertBurn_r2(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r10(p,delta0);
  }
  function updateTotalSupplyOnInsertConstructor_r17() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateBalanceOfOnIncrementTotalMint_r10(address p,int m) private    {
      balanceOf[p].n += m;
  }
  function updateAllowanceOnIncrementSpentTotal_r23(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateBurnOnInsertRecv_burn_r25(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && n<=balanceOf_x1) {
        updateTotalBurnOnInsertBurn_r2(p,n);
        updateAllBurnOnInsertBurn_r19(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateAllBurnOnInsertBurn_r19(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r20(delta0);
  }
  function updateSpentTotalOnInsertTransferFrom_r8(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r23(o,s,delta0);
  }
  function updateOwnerOnInsertConstructor_r22() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateBalanceOfOnIncrementTotalIn_r10(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTotalMintOnInsertMint_r21(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r10(p,delta0);
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r20(delta0);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r24(address o,address s,int d) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r7(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r20(int b) private    {
      totalSupply.n -= b;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r23(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateTotalSupplyOnIncrementAllMint_r20(int m) private    {
      totalSupply.n += m;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r16(address o,address r,address s,int n) private   returns (bool) {
      int allowance_x2 = allowance[o][s].n;
      if(n<allowance_x2) {
        updateSpentTotalOnInsertTransferFrom_r8(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateMinterOnInsertSetMinter_r15(address p) private    {
      minter = MinterTuple(p,true);
  }
  function updateTotalInOnInsertTransfer_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r10(p,delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r7(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r23(o,s,delta0);
  }
  function updateTransferOnInsertRecv_transfer_r26(address s,address r,int n) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[s].n;
      if(n>=0 && n<=balanceOf_x1_1) {
        updateTotalOutOnInsertTransfer_r9(s,n);
        updateTotalInOnInsertTransfer_r1(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateMintOnInsertRecv_mint_r6(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address m = minter.p;
      if(m==s && n>=0) {
        updateTotalMintOnInsertMint_r21(p,n);
        updateAllMintOnInsertMint_r0(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateMinterOnInsertConstructor_r12() private    {
      address s = msg.sender;
      minter = MinterTuple(s,true);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateBalanceOfOnIncrementTotalOut_r10(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateBalanceOfOnIncrementTotalBurn_r10(address p,int b) private    {
      balanceOf[p].n -= b;
  }
  function updateTotalOutOnInsertTransfer_r9(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r10(p,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateSetMinterOnInsertRecv_setMinter_r4(address p) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        updateMinterOnInsertSetMinter_r15(p);
        emit SetMinter(p);
        return true;
      }
      return false;
  }
}