contract Controllable {
  struct AllowanceTuple {
    int n;
    bool _valid;
  }
  struct ControllerTuple {
    address p;
    bool _valid;
  }
  struct OwnerTuple {
    address p;
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
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  ControllerTuple controller;
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalSupplyTuple totalSupply;
  OwnerTuple owner;
  event Mint(address p,int amount);
  event TransferFrom(address from,address to,address spender,int amount);
  event ControllerRedeem(address p,int amount);
  event Burn(address p,int amount);
  event IncreaseAllowance(address p,address s,int n);
  event Transfer(address from,address to,int amount);
  event InvalidTx();
  event IllegalRedeem(address p,int n);
  event ControllerTransfer(address from,address to,int amount);
  constructor(address p) public {
    updateTotalBalancesOnInsertConstructor_r4();
    updateTotalSupplyOnInsertConstructor_r10();
    updateControllerOnInsertConstructor_r18(p);
    updateOwnerOnInsertConstructor_r23();
  }
  function controllerRedeem(address p,int amount) public    {
      bool r3 = updateControllerRedeemOnInsertRecv_controllerRedeem_r3(p,amount);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r12 = updateTransferFromOnInsertRecv_transferFrom_r12(from,to,spender,amount);
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function getOwner() public view  returns (address) {
      address p = owner.p;
      return p;
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r22 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r22(p,s,n);
      if(r22==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function mint(address p,int amount) public    {
      bool r13 = updateMintOnInsertRecv_mint_r13(p,amount);
      if(r13==false) {
        revert("Rule condition failed");
      }
  }
  function getController() public view  returns (address) {
      address p = controller.p;
      return p;
  }
  function transfer(address from,address to,int amount) public    {
      bool r24 = updateTransferOnInsertRecv_transfer_r24(from,to,amount);
      if(r24==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r8 = updateBurnOnInsertRecv_burn_r8(p,amount);
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function updateTotalBurnOnInsertBurn_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r5(p,delta0);
  }
  function updateTotalBalancesOnInsertConstructor_r4() private    {
      // Empty()
  }
  function updateTransferOnInsertRecv_transfer_r24(address from,address to,int amount) private   returns (bool) {
      address owner_p = owner.p;
      if(r==owner_p && s==owner_p) {
        updateTotalInOnInsertTransfer_r11(r,n);
        updateTotalOutOnInsertTransfer_r30(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertTransferFrom_r20(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r30(o,n);
      updateTotalInOnInsertTransfer_r11(r,n);
      emit Transfer(o,r,n);
  }
  function updateTotalSupplyOnIncrementAllBurn_r16(int b) private    {
      totalSupply.n -= b;
  }
  function updateTotalSupplyOnInsertConstructor_r10() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateTotalMintOnInsertMint_r25(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r5(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalMint_r5(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateOwnerOnInsertConstructor_r23() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r6(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r7(o,s,delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r12(address from,address to,address spender,int amount) private   returns (bool) {
      if(0==n) {
        updateTransferOnInsertTransferFrom_r20(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r27(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateMintOnInsertRecv_mint_r13(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>=0) {
        updateAllMintOnInsertMint_r0(n);
        updateTotalMintOnInsertMint_r25(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateTotalInOnInsertTransfer_r11(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r5(p,delta0);
  }
  function updateBurnOnInsertControllerRedeem_r29(address p,int n) private    {
      updateTotalBurnOnInsertBurn_r1(p,n);
      updateAllBurnOnInsertBurn_r15(n);
      emit Burn(p,n);
  }
  function updateAllBurnOnInsertBurn_r15(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r16(delta0);
  }
  function updateBurnOnInsertRecv_burn_r8(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && 0==n) {
        updateTotalBurnOnInsertBurn_r1(p,n);
        updateAllBurnOnInsertBurn_r15(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateSpentTotalOnInsertTransferFrom_r27(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r7(o,s,delta0);
  }
  function updateBalanceOfOnIncrementTotalIn_r5(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r7(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r16(delta0);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r22(address p,address s,int n) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r6(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r16(int m) private    {
      totalSupply.n += m;
  }
  function updateBalanceOfOnIncrementTotalBurn_r5(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalOutOnInsertTransfer_r30(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r5(p,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateAllowanceOnIncrementSpentTotal_r7(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateControllerOnInsertConstructor_r18(address p) private    {
      controller = ControllerTuple(p,true);
  }
  function updateControllerRedeemOnInsertRecv_controllerRedeem_r3(address p,int amount) private   returns (bool) {
      address c = controller.p;
      if(p==c && 0==n) {
        updateBurnOnInsertControllerRedeem_r29(p,n);
        emit ControllerRedeem(p,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r5(address p,int o) private    {
      balanceOf[p].n -= o;
  }
}