contract Erc1155 {
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct TotalSupplyTuple {
    int n;
    bool _valid;
  }
  struct AllowanceTuple {
    int n;
    bool _valid;
  }
  struct BalanceOfTuple {
    int n;
    bool _valid;
  }
  mapping(int=>mapping(address=>mapping(address=>AllowanceTuple))) allowance;
  mapping(int=>TotalSupplyTuple) totalSupply;
  mapping(int=>mapping(address=>BalanceOfTuple)) balanceOf;
  OwnerTuple owner;
  event TransferFrom(int tokenId,address from,address to,address spender,int amount);
  event IncreaseAllowance(int tokenId,address o,address s,int d);
  event Transfer(int tokenId,address from,address to,int amount);
  event Burn(int tokenId,address p,int amount);
  event UnauthorizedMint();
  event UnauthorizedBurn();
  event Mint(int tokenId,address p,int amount);
  constructor() public {
    updateOwnerOnInsertConstructor_r16();
  }
  function mint(int tokenId,address p,int amount) public    {
      bool r5 = updateMintOnInsertRecv_mint_r5(tokenId,p,amount);
      if(r5==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(int tokenId,address from,address to,address spender,int amount) public    {
      bool r1 = updateTransferFromOnInsertRecv_transferFrom_r1(tokenId,from,to,spender,amount);
      if(r1==false) {
        revert("Rule condition failed");
      }
  }
  function burn(int tokenId,address p,int amount) public    {
      bool r18 = updateBurnOnInsertRecv_burn_r18(tokenId,p,amount);
      if(r18==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply(int tokenId) public view  returns (int) {
      int n = totalSupply[tokenId].n;
      return n;
  }
  function getAllowance(int tokenId,address o,address s) public view  returns (int) {
      int n = allowance[tokenId][o][s].n;
      return n;
  }
  function transfer(int tokenId,address from,address to,int amount) public    {
      bool r7 = updateTransferOnInsertRecv_transfer_r7(tokenId,from,to,amount);
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(int tokenId,address p) public view  returns (int) {
      int n = balanceOf[tokenId][p].n;
      return n;
  }
  function increaseAllowance(int tokenId,address o,address s,int d) public    {
      bool r4 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r4(tokenId,o,s,d);
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function updateTotalSupplyOnIncrementAllMint_r20(int t,int m) private    {
      totalSupply[t].n += m;
  }
  function updateAllMintOnInsertMint_r21(int t,int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r20(t,delta0);
  }
  function updateTotalOutOnInsertTransfer_r10(int t,address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r9(t,p,delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r19(int t,address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r8(t,o,s,delta0);
  }
  function updateTotalSupplyOnIncrementAllBurn_r20(int t,int b) private    {
      totalSupply[t].n -= b;
  }
  function updateTransferOnInsertTransferFrom_r17(int t,address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r10(t,o,n);
      updateTotalInOnInsertTransfer_r14(t,r,n);
      emit Transfer(t,o,r,n);
  }
  function updateTransferOnInsertRecv_transfer_r7(int tokenId,address from,address to,int amount) private   returns (bool) {
      int balanceOf_x2_1 = balanceOf[tokenId][from].n;
      if(amount>0 && amount<=balanceOf_x2_1) {
        updateTotalOutOnInsertTransfer_r10(tokenId,from,amount);
        updateTotalInOnInsertTransfer_r14(tokenId,to,amount);
        emit Transfer(tokenId,from,to,amount);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalIn_r9(int t,address p,int i) private    {
      balanceOf[t][p].n += i;
  }
  function updateBalanceOfOnIncrementTotalMint_r9(int t,address p,int n) private    {
      balanceOf[t][p].n += n;
  }
  function updateOwnerOnInsertConstructor_r16() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateBalanceOfOnIncrementTotalBurn_r9(int t,address p,int m) private    {
      balanceOf[t][p].n -= m;
  }
  function updateAllBurnOnInsertBurn_r0(int t,int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r20(t,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalInOnInsertTransfer_r14(int t,address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r9(t,p,delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r1(int tokenId,address from,address to,address spender,int amount) private   returns (bool) {
      address msgSender = msg.sender;
      int allowance_x3 = allowance[tokenId][spender][msgSender].n;
      if(amount<allowance_x3) {
        updateTransferOnInsertTransferFrom_r17(tokenId,from,to,amount);
        updateSpentTotalOnInsertTransferFrom_r22(tokenId,from,spender,amount);
        emit TransferFrom(tokenId,from,to,spender,amount);
        return true;
      }
      return false;
  }
  function updateMintOnInsertRecv_mint_r5(int tokenId,address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && amount>0) {
        updateTotalMintOnInsertMint_r12(tokenId,p,amount);
        updateAllMintOnInsertMint_r21(tokenId,amount);
        emit Mint(tokenId,p,amount);
        return true;
      }
      return false;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r4(int tokenId,address o,address s,int d) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r19(tokenId,o,s,d);
        emit IncreaseAllowance(tokenId,o,s,d);
        return true;
      }
      return false;
  }
  function updateSpentTotalOnInsertTransferFrom_r22(int t,address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r8(t,o,s,delta0);
  }
  function updateBalanceOfOnIncrementTotalOut_r9(int t,address p,int o) private    {
      balanceOf[t][p].n -= o;
  }
  function updateTotalMintOnInsertMint_r12(int t,address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r9(t,p,delta0);
  }
  function updateTotalBurnOnInsertBurn_r23(int t,address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r9(t,p,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateBurnOnInsertRecv_burn_r18(int tokenId,address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x2 = balanceOf[tokenId][p].n;
      if(o==s && amount==balanceOf_x2) {
        updateTotalBurnOnInsertBurn_r23(tokenId,p,amount);
        updateAllBurnOnInsertBurn_r0(tokenId,amount);
        emit Burn(tokenId,p,amount);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r8(int t,address o,address s,int m) private    {
      allowance[t][o][s].n += m;
  }
  function updateAllowanceOnIncrementSpentTotal_r8(int t,address o,address s,int l) private    {
      allowance[t][o][s].n -= l;
  }
}