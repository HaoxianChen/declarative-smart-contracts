contract Bnb {
  struct TotalOutTuple {
    uint n;
    bool _valid;
  }
  struct TotalFreezeTuple {
    uint n;
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
  struct SpentTotalTuple {
    uint m;
    bool _valid;
  }
  struct TotalInTuple {
    uint n;
    bool _valid;
  }
  struct TotalUnfreezeTuple {
    uint n;
    bool _valid;
  }
  struct TotalBurnTuple {
    uint n;
    bool _valid;
  }
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct AllowanceTotalTuple {
    uint m;
    bool _valid;
  }
  struct AllBurnTuple {
    uint n;
    bool _valid;
  }
  mapping(address=>TotalInTuple) totalIn;
  mapping(address=>TotalOutTuple) totalOut;
  mapping(address=>TotalFreezeTuple) totalFreeze;
  OwnerTuple owner;
  mapping(address=>TotalMintTuple) totalMint;
  AllMintTuple allMint;
  mapping(address=>mapping(address=>AllowanceTotalTuple)) allowanceTotal;
  mapping(address=>mapping(address=>SpentTotalTuple)) spentTotal;
  AllBurnTuple allBurn;
  mapping(address=>TotalUnfreezeTuple) totalUnfreeze;
  mapping(address=>TotalBurnTuple) totalBurn;
  event TransferFrom(address from,address to,address spender,uint amount);
  event Burn(address p,uint amount);
  event Mint(address p,uint amount);
  event WithdrawEther(address p,uint amount);
  event IncreaseAllowance(address p,address s,uint n);
  event Unfreeze(address p,uint n);
  event Freeze(address p,uint n);
  event Transfer(address from,address to,uint amount);
  constructor(uint initialSupply) public {
    updateTotalSupplyOnInsertConstructor_r5(initialSupply);
    updateOwnerOnInsertConstructor_r7();
    updateTotalInOnInsertConstructor_r30(initialSupply);
    updateAllMintOnInsertConstructor_r9(initialSupply);
    updateTotalMintOnInsertConstructor_r32(initialSupply);
    updateBalanceOfOnInsertConstructor_r4(initialSupply);
    updateTotalBalancesOnInsertConstructor_r25(initialSupply);
  }
  function approve(address s,uint n) public    {
      bool r29 = updateIncreaseAllowanceOnInsertRecv_approve_r29(s,n);
      if(r29==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address to,uint amount) public    {
      bool r8 = updateTransferOnInsertRecv_transfer_r8(to,amount);
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function freeze(uint n) public    {
      bool r24 = updateFreezeOnInsertRecv_freeze_r24(n);
      if(r24==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (uint) {
      uint n = allowance[p][s].n;
      return n;
  }
  function getTotalSupply() public view  returns (uint) {
      uint n = totalSupply.n;
      return n;
  }
  function transferFrom(address from,address to,uint amount) public    {
      bool r14 = updateTransferFromOnInsertRecv_transferFrom_r14(from,to,amount);
      if(r14==false) {
        revert("Rule condition failed");
      }
  }
  function unfreeze(uint n) public    {
      bool r6 = updateUnfreezeOnInsertRecv_unfreeze_r6(n);
      if(r6==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (uint) {
      uint n = balanceOf[p].n;
      return n;
  }
  function withdrawEther(uint amount) public    {
      bool r23 = updateWithdrawEtherOnInsertRecv_withdrawEther_r23(amount);
      if(r23==false) {
        revert("Rule condition failed");
      }
  }
  function burn(uint amount) public    {
      bool r16 = updateBurnOnInsertRecv_burn_r16(amount);
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function updateFreezeOnInsertRecv_freeze_r24(uint n) private   returns (bool) {
      address p = msg.sender;
      if(n<=m && n>0 && balanceOf(p,m)) {
        updateTotalFreezeOnInsertFreeze_r0(p,n);
        emit Freeze(p,n);
        return true;
      }
      return false;
  }
  function allowance(address p,address s,uint n) private view  returns (bool) {
      uint l = spentTotal[o][s].m;
      uint m = allowanceTotal[o][s].m;
      return true;
      return false;
  }
  function updateTotalInOnInsertConstructor_r30(uint n) private    {
      address s = msg.sender;
      totalIn[s] = TotalInTuple(n,true);
  }
  function updateOwnerOnInsertConstructor_r7() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function freezeOf(address p,uint n) private view  returns (bool) {
      uint u = totalUnfreeze[p].n;
      uint f = totalFreeze[p].n;
      return true;
      return false;
  }
  function updateSendOnInsertWithdrawEther_r3(address p,uint n) private    {
      payable(p).send(n);
  }
  function updateTransferOnInsertRecv_transfer_r8(address r,uint n) private   returns (bool) {
      address s = msg.sender;
      if(n<=m && n>0 && r!=address(0) && n+m>=m && balanceOf(s,m)) {
        updateTotalOutOnInsertTransfer_r20(s,n);
        updateTotalInOnInsertTransfer_r31(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateTotalBurnOnInsertBurn_r17(address p,uint n) private    {
      totalBurn[p].n += n;
  }
  function totalSupply(uint n) private view  returns (bool) {
      if(n==constructor.initialSupply) {
        return true;
      }
      uint b = allBurn.n;
      uint m = allMint.n;
      return true;
      return false;
  }
  function updateUnfreezeOnInsertRecv_unfreeze_r6(uint n) private   returns (bool) {
      address p = msg.sender;
      if(n<=m && n>0 && freezeOf(p,m)) {
        updateTotalUnfreezeOnInsertUnfreeze_r13(p,n);
        emit Unfreeze(p,n);
        return true;
      }
      return false;
  }
  function updateBurnOnInsertRecv_burn_r16(uint n) private   returns (bool) {
      address p = msg.sender;
      if(p!=address(0) && n<=m && balanceOf(p,m)) {
        updateAllBurnOnInsertBurn_r28(n);
        updateTotalBurnOnInsertBurn_r17(p,n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function balanceOf(address p,uint n) private view  returns (bool) {
      uint i = totalIn[p].n;
      uint m = totalBurn[p].n;
      uint o = totalOut[p].n;
      uint n = totalMint[p].n;
      if(freezeOf(p,f)) {
        return true;
      }
      if(s==msg.sender) {
        if(n==constructor.initialSupply) {
          return true;
        }
      }
      return false;
  }
  function updateTotalOutOnInsertTransfer_r20(address p,uint n) private    {
      totalOut[p].n += n;
  }
  function updateTotalInOnInsertTransfer_r31(address p,uint n) private    {
      totalIn[p].n += n;
  }
  function updateTotalBalancesOnInsertConstructor_r25(uint n) private    {
      // Empty()
  }
  function updateTransferOnInsertTransferFrom_r1(address o,address r,uint n) private    {
      updateTotalInOnInsertTransfer_r31(r,n);
      updateTotalOutOnInsertTransfer_r20(o,n);
      emit Transfer(o,r,n);
  }
  function updateAllBurnOnInsertBurn_r28(uint n) private    {
      allBurn.n += n;
  }
  function updateSpentTotalOnInsertTransferFrom_r22(address o,address s,uint n) private    {
      spentTotal[o][s].m += n;
  }
  function updateTotalSupplyOnInsertConstructor_r5(uint n) private    {
      // Empty()
  }
  function updateWithdrawEtherOnInsertRecv_withdrawEther_r23(uint n) private   returns (bool) {
      address p = owner.p;
      if(p==msg.sender) {
        updateSendOnInsertWithdrawEther_r3(p,n);
        emit WithdrawEther(p,n);
        return true;
      }
      return false;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalFreezeOnInsertFreeze_r0(address p,uint n) private    {
      totalFreeze[p].n += n;
  }
  function updateTotalUnfreezeOnInsertUnfreeze_r13(address p,uint n) private    {
      totalUnfreeze[p].n += n;
  }
  function updateIncreaseAllowanceOnInsertRecv_approve_r29(address s,uint n) private   returns (bool) {
      address o = msg.sender;
      if(allowance(o,s,m)) {
        uint d = n-m;
        updateAllowanceTotalOnInsertIncreaseAllowance_r33(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r33(address o,address s,uint n) private    {
      allowanceTotal[o][s].m += n;
  }
  function updateAllMintOnInsertConstructor_r9(uint n) private    {
      allMint = AllMintTuple(n,true);
  }
  function updateBalanceOfOnInsertConstructor_r4(uint n) private    {
      address s = msg.sender;
      // Empty()
  }
  function updateTransferFromOnInsertRecv_transferFrom_r14(address o,address r,uint n) private   returns (bool) {
      address s = msg.sender;
      if(m>=n && r!=address(0) && n+m>=m && n>0 && k>=n && balanceOf(o,m) && allowance(o,s,k)) {
        updateSpentTotalOnInsertTransferFrom_r22(o,s,n);
        updateTransferOnInsertTransferFrom_r1(o,r,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateTotalMintOnInsertConstructor_r32(uint n) private    {
      address s = msg.sender;
      totalMint[s] = TotalMintTuple(n,true);
  }
}