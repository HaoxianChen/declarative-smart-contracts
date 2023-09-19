contract Erc1155 {
  struct SpentTotalTuple {
    uint m;
    bool _valid;
  }
  struct TotalBurnTuple {
    uint n;
    bool _valid;
  }
  struct BalanceOfTuple {
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
  struct TotalOutTuple {
    uint n;
    bool _valid;
  }
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct AllBurnTuple {
    uint n;
    bool _valid;
  }
  struct TotalInTuple {
    uint n;
    bool _valid;
  }
  struct AllowanceTotalTuple {
    uint m;
    bool _valid;
  }
  struct AllowanceTuple {
    uint n;
    bool _valid;
  }
  struct TotalMintTuple {
    uint n;
    bool _valid;
  }
  mapping(uint=>mapping(address=>TotalOutTuple)) totalOut;
  mapping(uint=>mapping(address=>mapping(address=>SpentTotalTuple))) spentTotal;
  mapping(uint=>mapping(address=>TotalInTuple)) totalIn;
  mapping(uint=>mapping(address=>mapping(address=>AllowanceTotalTuple))) allowanceTotal;
  mapping(uint=>mapping(address=>TotalBurnTuple)) totalBurn;
  mapping(uint=>mapping(address=>BalanceOfTuple)) balanceOf;
  mapping(uint=>TotalSupplyTuple) totalSupply;
  mapping(uint=>AllMintTuple) allMint;
  OwnerTuple owner;
  mapping(uint=>AllBurnTuple) allBurn;
  mapping(uint=>mapping(address=>mapping(address=>AllowanceTuple))) allowance;
  mapping(uint=>mapping(address=>TotalMintTuple)) totalMint;
  event Burn(uint tokenId,address p,uint amount);
  event Transfer(uint tokenId,address from,address to,uint amount);
  event TransferFrom(uint tokenId,address from,address to,address spender,uint amount);
  event Mint(uint tokenId,address p,uint amount);
  event IncreaseAllowance(uint tokenId,address p,address s,uint n);
  constructor() public {
    updateOwnerOnInsertConstructor_r6();
  }
  function approve(uint tokenId,address s,uint n) public    {
      bool r1 = updateIncreaseAllowanceOnInsertRecv_approve_r1(tokenId,s,n);
      if(r1==false) {
        revert("Rule condition failed");
      }
  }
  function mint(uint tokenId,address p,uint amount) public    {
      bool r7 = updateMintOnInsertRecv_mint_r7(tokenId,p,amount);
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(uint tokenId,address to,uint amount) public    {
      bool r9 = updateTransferOnInsertRecv_transfer_r9(tokenId,to,amount);
      if(r9==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(uint tokenId,address from,address to,uint amount) public    {
      bool r18 = updateTransferFromOnInsertRecv_transferFrom_r18(tokenId,from,to,amount);
      if(r18==false) {
        revert("Rule condition failed");
      }
  }
  function burn(uint tokenId,address p,uint amount) public    {
      bool r4 = updateBurnOnInsertRecv_burn_r4(tokenId,p,amount);
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(uint tokenId,address p,address s) public view  returns (uint) {
      uint n = allowance[tokenId][p][s].n;
      return n;
  }
  function getTotalSupply(uint tokenId) public view  returns (uint) {
      uint n = totalSupply[tokenId].n;
      return n;
  }
  function getBalanceOf(uint tokenId,address p) public view  returns (uint) {
      uint n = balanceOf[tokenId][p].n;
      return n;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r16(uint t,address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r0(t,o,s,delta0);
      allowanceTotal[t][o][s].m += n;
  }
  function updateAllBurnOnInsertBurn_r3(uint t,uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r10(t,delta0);
      allBurn[t].n += n;
  }
  function updateBurnOnInsertRecv_burn_r4(uint t,address p,uint n) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        uint m = balanceOf[t][p].n;
        if(p!=address(0) && n<=m) {
          updateAllBurnOnInsertBurn_r3(t,n);
          updateTotalBurnOnInsertBurn_r5(t,p,n);
          emit Burn(t,p,n);
          return true;
        }
      }
      return false;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r0(uint t,address o,address s,int m) private    {
      int _delta = int(m);
      uint newValue = updateuintByint(allowance[t][o][s].n,_delta);
      allowance[t][o][s].n = newValue;
  }
  function updateAllowanceOnIncrementSpentTotal_r0(uint t,address o,address s,int l) private    {
      int _delta = int(-l);
      uint newValue = updateuintByint(allowance[t][o][s].n,_delta);
      allowance[t][o][s].n = newValue;
  }
  function updateOwnerOnInsertConstructor_r6() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateSpentTotalOnInsertTransferFrom_r17(uint t,address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r0(t,o,s,delta0);
      spentTotal[t][o][s].m += n;
  }
  function updateTotalMintOnInsertMint_r13(uint t,address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r2(t,p,delta0);
      totalMint[t][p].n += n;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateAllMintOnInsertMint_r14(uint t,uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r10(t,delta0);
      allMint[t].n += n;
  }
  function updateBalanceOfOnIncrementTotalOut_r2(uint t,address p,int o) private    {
      int _delta = int(-o);
      uint newValue = updateuintByint(balanceOf[t][p].n,_delta);
      balanceOf[t][p].n = newValue;
  }
  function updateTransferOnInsertRecv_transfer_r9(uint t,address r,uint n) private   returns (bool) {
      address s = msg.sender;
      uint m = balanceOf[t][s].n;
      if(n<=m) {
        updateTotalInOnInsertTransfer_r15(t,r,n);
        updateTotalOutOnInsertTransfer_r12(t,s,n);
        emit Transfer(t,s,r,n);
        return true;
      }
      return false;
  }
  function updateMintOnInsertRecv_mint_r7(uint t,address p,uint n) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        if(p!=address(0)) {
          updateTotalMintOnInsertMint_r13(t,p,n);
          updateAllMintOnInsertMint_r14(t,n);
          emit Mint(t,p,n);
          return true;
        }
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalMint_r2(uint t,address p,int n) private    {
      int _delta = int(n);
      uint newValue = updateuintByint(balanceOf[t][p].n,_delta);
      balanceOf[t][p].n = newValue;
  }
  function updateIncreaseAllowanceOnInsertRecv_approve_r1(uint t,address s,uint n) private   returns (bool) {
      address o = msg.sender;
      uint m = allowance[t][o][s].n;
      uint d = n-m;
      updateAllowanceTotalOnInsertIncreaseAllowance_r16(t,o,s,d);
      emit IncreaseAllowance(t,o,s,d);
      return true;
      return false;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r18(uint t,address o,address r,uint n) private   returns (bool) {
      address s = msg.sender;
      uint k = allowance[t][o][s].n;
      uint m = balanceOf[t][o].n;
      if(m>=n && k>=n) {
        updateTransferOnInsertTransferFrom_r8(t,o,r,n);
        updateSpentTotalOnInsertTransferFrom_r17(t,o,s,n);
        emit TransferFrom(t,o,r,s,n);
        return true;
      }
      return false;
  }
  function updateTotalInOnInsertTransfer_r15(uint t,address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r2(t,p,delta0);
      totalIn[t][p].n += n;
  }
  function updateTotalOutOnInsertTransfer_r12(uint t,address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r2(t,p,delta0);
      totalOut[t][p].n += n;
  }
  function updateTotalBurnOnInsertBurn_r5(uint t,address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r2(t,p,delta0);
      totalBurn[t][p].n += n;
  }
  function updateTotalSupplyOnIncrementAllMint_r10(uint t,int m) private    {
      int _delta = int(m);
      uint newValue = updateuintByint(totalSupply[t].n,_delta);
      totalSupply[t].n = newValue;
  }
  function updateTotalSupplyOnIncrementAllBurn_r10(uint t,int b) private    {
      int _delta = int(-b);
      uint newValue = updateuintByint(totalSupply[t].n,_delta);
      totalSupply[t].n = newValue;
  }
  function updateTransferOnInsertTransferFrom_r8(uint t,address o,address r,uint n) private    {
      updateTotalInOnInsertTransfer_r15(t,r,n);
      updateTotalOutOnInsertTransfer_r12(t,o,n);
      emit Transfer(t,o,r,n);
  }
  function updateBalanceOfOnIncrementTotalBurn_r2(uint t,address p,int m) private    {
      int _delta = int(-m);
      uint newValue = updateuintByint(balanceOf[t][p].n,_delta);
      balanceOf[t][p].n = newValue;
  }
  function updateBalanceOfOnIncrementTotalIn_r2(uint t,address p,int i) private    {
      int _delta = int(i);
      uint newValue = updateuintByint(balanceOf[t][p].n,_delta);
      balanceOf[t][p].n = newValue;
  }
}