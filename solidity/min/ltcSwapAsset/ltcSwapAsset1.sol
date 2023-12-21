contract LtcSwapAsset {
  struct AllMintTuple {
    uint n;
    bool _valid;
  }
  struct AllowanceTotalTuple {
    uint m;
    bool _valid;
  }
  struct NewOwnerTuple {
    address p;
    bool _valid;
  }
  struct SpentTotalTuple {
    uint m;
    bool _valid;
  }
  struct OldOwnerTuple {
    address p;
    bool _valid;
  }
  struct EffectiveTimeTuple {
    uint t;
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
  EffectiveTimeTuple effectiveTime;
  AllMintTuple allMint;
  mapping(address=>mapping(address=>AllowanceTotalTuple)) allowanceTotal;
  NewOwnerTuple newOwner;
  mapping(address=>mapping(address=>SpentTotalTuple)) spentTotal;
  OldOwnerTuple oldOwner;
  TotalBalancesTuple totalBalances;
  mapping(address=>BalanceOfTuple) balanceOf;
  AllBurnTuple allBurn;
  event TransferFrom(address from,address to,address spender,uint amount);
  event Burn(address p,uint amount);
  event Mint(address p,uint amount);
  event SwapOwner(address p,address q,uint t);
  event IncreaseAllowance(address p,address s,uint n);
  event Transfer(address from,address to,uint amount);
  constructor() public {
    updateTotalSupplyOnInsertConstructor_r13();
    updateNewOwnerOnInsertConstructor_r9();
    updateTotalBalancesOnInsertConstructor_r28();
    updateEffectiveTimeOnInsertConstructor_r1();
  }
  function approve(address s,uint n) public    {
      bool r26 = updateIncreaseAllowanceOnInsertRecv_approve_r26(s,n);
      if(r26==false) {
        revert("Rule condition failed");
      }
  }
  function swapOwner(address p,address q,uint d) public    {
      bool r22 = updateSwapOwnerOnInsertRecv_swapOwner_r22(p,q,d);
      if(r22==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (uint) {
      uint n = totalSupply();
      return n;
  }
  function mint(address p,uint amount) public    {
      bool r21 = updateMintOnInsertRecv_mint_r21(p,amount);
      if(r21==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,uint amount) public    {
      bool r23 = updateTransferFromOnInsertRecv_transferFrom_r23(from,to,amount);
      if(r23==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,uint amount) public    {
      bool r6 = updateBurnOnInsertRecv_burn_r6(p,amount);
      if(r6==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (uint) {
      uint n = allowance(p,s);
      return n;
  }
  function transfer(address to,uint amount) public    {
      bool r17 = updateTransferOnInsertRecv_transfer_r17(to,amount);
      if(r17==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (uint) {
      uint n = balanceOf[p].n;
      return n;
  }
  function updateBurnOnInsertRecv_burn_r6(address p,uint n) private   returns (bool) {
      address s = msg.sender;
      uint m = balanceOf[p].n;
      if(p!=address(0) && n<=m && owner(s)) {
        updateTotalBurnOnInsertBurn_r14(p,n);
        updateAllBurnOnInsertBurn_r24(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertTransferFrom_r0(address o,address r,uint n) private    {
      updateTotalOutOnInsertTransfer_r18(o,n);
      updateTotalInOnInsertTransfer_r11(r,n);
      emit Transfer(o,r,n);
  }
  function totalSupply() private view  returns (uint) {
      uint b = allBurn.n;
      uint m = allMint.n;
      uint n = m-b;
      return n;
  }
  function updateNewOwnerOnInsertSwapOwner_r25(address q) private    {
      newOwner = NewOwnerTuple(q,true);
  }
  function updateTransferOnInsertRecv_transfer_r17(address r,uint n) private   returns (bool) {
      address s = msg.sender;
      uint m = balanceOf[s].n;
      if(n<=m) {
        updateTotalInOnInsertTransfer_r11(r,n);
        updateTotalOutOnInsertTransfer_r18(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateSwapOwnerOnInsertRecv_swapOwner_r22(address p,address q,uint d) private   returns (bool) {
      address s = msg.sender;
      uint t0 = block.timestamp;
      if(owner(s)) {
        uint t = t0+d;
        updateEffectiveTimeOnInsertSwapOwner_r2(t);
        updateOldOwnerOnInsertSwapOwner_r12(p);
        updateNewOwnerOnInsertSwapOwner_r25(q);
        emit SwapOwner(p,q,t);
        return true;
      }
      return false;
  }
  function updateTotalOutOnInsertTransfer_r18(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r7(p,delta0);
  }
  function updateOldOwnerOnInsertSwapOwner_r12(address p) private    {
      oldOwner = OldOwnerTuple(p,true);
  }
  function updateNewOwnerOnInsertConstructor_r9() private    {
      address s = msg.sender;
      newOwner = NewOwnerTuple(s,true);
  }
  function updateMintOnInsertRecv_mint_r21(address p,uint n) private   returns (bool) {
      address s = msg.sender;
      if(p!=address(0) && owner(s)) {
        updateAllMintOnInsertMint_r3(n);
        updateTotalMintOnInsertMint_r15(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateTotalBalancesOnInsertConstructor_r28() private    {
      totalBalances = TotalBalancesTuple(0,true);
  }
  function updateBalanceOfOnIncrementTotalOut_r7(address p,int o) private    {
      int _delta = int(-o);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateTotalInOnInsertTransfer_r11(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r7(p,delta0);
  }
  function updateAllBurnOnInsertBurn_r24(uint n) private    {
      allBurn.n += n;
  }
  function updateTotalMintOnInsertMint_r15(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r7(p,delta0);
  }
  function updateTotalBurnOnInsertBurn_r14(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r7(p,delta0);
  }
  function updateSpentTotalOnInsertTransferFrom_r10(address o,address s,uint n) private    {
      spentTotal[o][s].m += n;
  }
  function allowance(address o,address s) private view  returns (uint) {
      uint l = spentTotal[o][s].m;
      uint m = allowanceTotal[o][s].m;
      uint n = m-l;
      return n;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateEffectiveTimeOnInsertSwapOwner_r2(uint t) private    {
      effectiveTime = EffectiveTimeTuple(t,true);
  }
  function updateAllMintOnInsertMint_r3(uint n) private    {
      allMint.n += n;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r23(address o,address r,uint n) private   returns (bool) {
      address s = msg.sender;
      uint m = balanceOf[o].n;
      uint k = allowance(o,s);
      if(m>=n && k>=n) {
        updateTransferOnInsertTransferFrom_r0(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r10(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r13() private    {
      // Empty()
  }
  function owner(address p) private view  returns (bool) {
      if(p==newOwner.p) {
        uint t2 = effectiveTime.t;
        uint t = block.timestamp;
        if(t>=t2) {
          return true;
        }
      }
      if(p==oldOwner.p) {
        uint t2 = effectiveTime.t;
        uint t = block.timestamp;
        if(t<t2) {
          return true;
        }
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalBurn_r7(address p,int m) private    {
      int _delta = int(-m);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r27(address o,address s,uint n) private    {
      allowanceTotal[o][s].m += n;
  }
  function updateIncreaseAllowanceOnInsertRecv_approve_r26(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      uint m = allowance(o,s);
      uint d = n-m;
      updateAllowanceTotalOnInsertIncreaseAllowance_r27(o,s,d);
      emit IncreaseAllowance(o,s,d);
      return true;
      return false;
  }
  function updateEffectiveTimeOnInsertConstructor_r1() private    {
      uint t = block.timestamp;
      effectiveTime = EffectiveTimeTuple(t,true);
  }
  function updateBalanceOfOnIncrementTotalIn_r7(address p,int i) private    {
      int _delta = int(i);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateBalanceOfOnIncrementTotalMint_r7(address p,int n) private    {
      int _delta = int(n);
      uint newValue = updateuintByint(balanceOf[p].n,_delta);
      balanceOf[p].n = newValue;
  }
}