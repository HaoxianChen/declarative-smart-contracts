contract Theta {
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
  struct AllMintTuple {
    uint n;
    bool _valid;
  }
  struct CanTransferTuple {
    address p;
    address q;
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
  struct AllBurnTuple {
    uint n;
    bool _valid;
  }
  mapping(address=>TotalInTuple) totalIn;
  mapping(address=>TotalOutTuple) totalOut;
  mapping(address=>TotalBurnTuple) totalBurn;
  OwnerTuple owner;
  mapping(address=>TotalMintTuple) totalMint;
  AllMintTuple allMint;
  CanTransferTuple canTransfer;
  AllBurnTuple allBurn;
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  event TransferFrom(address from,address to,address spender,uint amount);
  event Burn(address p,uint amount);
  event Mint(address p,uint amount);
  event Precirculated(address p,bool b);
  event IncreaseAllowance(address p,address s,uint n);
  event Transfer(address from,address to,uint amount);
  constructor(uint t) public {
    updateTotalSupplyOnInsertConstructor_r3();
    updateUnlockTimeOnInsertConstructor_r7(t);
    updateOwnerOnInsertConstructor_r2();
    updateTotalBalancesOnInsertConstructor_r20();
  }
  function transfer(address to,uint amount) public    {
      bool r25 = updateTransferOnInsertRecv_transfer_r25(to,amount);
      if(r25==false) {
        revert("Rule condition failed");
      }
  }
  function disallowPrecirculation(address p) public    {
      bool r26 = updatePrecirculatedOnInsertRecv_disallowPrecirculation_r26(p);
      if(r26==false) {
        revert("Rule condition failed");
      }
  }
  function approve(address s,uint n) public    {
      bool r24 = updateIncreaseAllowanceOnInsertRecv_approve_r24(s,n);
      if(r24==false) {
        revert("Rule condition failed");
      }
  }
  function allowPrecirculation(address p) public    {
      bool r19 = updatePrecirculatedOnInsertRecv_allowPrecirculation_r19(p);
      if(r19==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (uint) {
      uint n = totalSupply.n;
      return n;
  }
  function transferFrom(address from,address to,uint amount) public    {
      bool r10 = updateTransferFromOnInsertRecv_transferFrom_r10(from,to,amount);
      if(r10==false) {
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
  function burn(address p,uint amount) public    {
      bool r8 = updateBurnOnInsertRecv_burn_r8(p,amount);
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,uint amount) public    {
      bool r22 = updateMintOnInsertRecv_mint_r22(p,amount);
      if(r22==false) {
        revert("Rule condition failed");
      }
  }
  function canTransfer(address p,address q) private view  returns (bool) {
      uint t = block.timestamp;
      if(t>=ut && unlockTime(ut)) {
        return true;
      }
      if(true==precirculated[q].b) {
        if(true==precirculated[p].b) {
          return true;
        }
      }
      return false;
  }
  function updateTotalOutOnInsertTransfer_r17(address p,uint n) private    {
      totalOut[p].n += n;
  }
  function updateAllBurnOnInsertBurn_r23(uint n) private    {
      allBurn.n += n;
  }
  function updateOwnerOnInsertConstructor_r2() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateAllMintOnInsertMint_r4(uint n) private    {
      allMint.n += n;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r21(address o,address s,int m) private    {
      int _delta = int(m);
      uint newValue = updateuintByint(allowance[o][s].n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateAllowanceOnIncrementSpentTotal_r21(address o,address s,int l) private    {
      int _delta = int(-l);
      uint newValue = updateuintByint(allowance[o][s].n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r13(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r21(o,s,delta0);
  }
  function updatePrecirculatedOnInsertRecv_allowPrecirculation_r19(address p) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        emit Precirculated(p,true);
        return true;
      }
      return false;
  }
  function updateSpentTotalOnInsertTransferFrom_r11(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r21(o,s,delta0);
  }
  function updateTotalInOnInsertTransfer_r12(address p,uint n) private    {
      totalIn[p].n += n;
  }
  function unlockTime(uint t) private view  returns (bool) {
      if(t==constructor.t) {
        return true;
      }
      return false;
  }
  function updateMintOnInsertRecv_mint_r22(address p,uint n) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        if(p!=address(0)) {
          updateTotalMintOnInsertMint_r15(p,n);
          updateAllMintOnInsertMint_r4(n);
          emit Mint(p,n);
          return true;
        }
      }
      return false;
  }
  function updateTotalMintOnInsertMint_r15(address p,uint n) private    {
      totalMint[p].n += n;
  }
  function updateTotalBurnOnInsertBurn_r14(address p,uint n) private    {
      totalBurn[p].n += n;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTransferOnInsertRecv_transfer_r25(address r,uint n) private   returns (bool) {
      address s = msg.sender;
      if(n<=m && balanceOf(s,m) && canTransfer(s,r)) {
        updateTotalOutOnInsertTransfer_r17(s,n);
        updateTotalInOnInsertTransfer_r12(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateUnlockTimeOnInsertConstructor_r7(uint t) private    {
      // Empty()
  }
  function updateBurnOnInsertRecv_burn_r8(address p,uint n) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        if(p!=address(0) && n<=m && balanceOf(p,m)) {
          updateTotalBurnOnInsertBurn_r14(p,n);
          updateAllBurnOnInsertBurn_r23(n);
          emit Burn(p,n);
          return true;
        }
      }
      return false;
  }
  function updateTotalBalancesOnInsertConstructor_r20() private    {
      // Empty()
  }
  function updateTransferOnInsertTransferFrom_r0(address o,address r,uint n) private    {
      updateTotalOutOnInsertTransfer_r17(o,n);
      updateTotalInOnInsertTransfer_r12(r,n);
      emit Transfer(o,r,n);
  }
  function balanceOf(address p,uint n) private view  returns (bool) {
      uint i = totalIn[p].n;
      uint o = totalOut[p].n;
      uint m = totalBurn[p].n;
      uint n = totalMint[p].n;
      return true;
      return false;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r10(address o,address r,uint n) private   returns (bool) {
      address s = msg.sender;
      uint k = allowance[o][s].n;
      if(m>=n && k>=n && canTransfer(o,r) && balanceOf(o,m)) {
        updateSpentTotalOnInsertTransferFrom_r11(o,s,n);
        updateTransferOnInsertTransferFrom_r0(o,r,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function totalSupply(uint n) private view  returns (bool) {
      return true;
      uint b = allBurn.n;
      uint m = allMint.n;
      return true;
      return false;
  }
  function updateIncreaseAllowanceOnInsertRecv_approve_r24(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      uint m = allowance[o][s].n;
      uint d = n-m;
      updateAllowanceTotalOnInsertIncreaseAllowance_r13(o,s,d);
      emit IncreaseAllowance(o,s,d);
      return true;
      return false;
  }
  function updatePrecirculatedOnInsertRecv_disallowPrecirculation_r26(address p) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        emit Precirculated(p,false);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r3() private    {
      // Empty()
  }
}