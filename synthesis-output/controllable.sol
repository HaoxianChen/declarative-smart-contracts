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
    updateControllerOnInsertConstructor_r17(p);
    updateOwnerOnInsertConstructor_r22();
    updateTotalSupplyOnInsertConstructor_r9();
  }
  function controllerRedeem(address p,int amount) public    {
      bool r3 = updateControllerRedeemOnInsertRecv_controllerRedeem_r3(p,amount);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function getController() public view  returns (address) {
      address p = controller.p;
      return p;
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
      bool r21 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r21(p,s,n);
      if(r21==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r11 = updateTransferFromOnInsertRecv_transferFrom_r11(from,to,spender,amount);
      if(r11==false) {
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
  function burn(address p,int amount) public    {
      bool r25 = updateBurnOnInsertRecv_burn_r25(p,amount);
      if(r25==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r23 = updateTransferOnInsertRecv_transfer_r23(from,to,amount);
      if(r23==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r12 = updateMintOnInsertRecv_mint_r12(p,amount);
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function updateBalanceOfOnIncrementTotalBurn_r5(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateTotalBurnOnInsertBurn_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r5(p,delta0);
  }
  function updateTotalBalancesOnInsertConstructor_r4() private    {
      // Empty()
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r21(address p,address s,int n) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r6(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateTotalInOnInsertTransfer_r10(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r5(p,delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r6(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r7(o,s,delta0);
  }
  function updateBurnOnInsertControllerRedeem_r29(address p,int n) private    {
      updateTotalBurnOnInsertBurn_r1(p,n);
      updateAllBurnOnInsertBurn_r14(n);
      emit Burn(p,n);
  }
  function updateBurnOnInsertRecv_burn_r25(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n<=0) {
        updateTotalBurnOnInsertBurn_r1(p,n);
        updateAllBurnOnInsertBurn_r14(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r15(int m) private    {
      totalSupply.n += m;
  }
  function updateControllerOnInsertConstructor_r17(address p) private    {
      controller = ControllerTuple(p,true);
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
  function updateTotalSupplyOnInsertConstructor_r9() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateTotalSupplyOnIncrementAllBurn_r15(int b) private    {
      totalSupply.n -= b;
  }
  function updateTransferOnInsertTransferFrom_r19(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r30(o,n);
      updateTotalInOnInsertTransfer_r10(r,n);
      emit Transfer(o,r,n);
  }
  function updateMintOnInsertRecv_mint_r12(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>=0) {
        updateAllMintOnInsertMint_r0(n);
        updateTotalMintOnInsertMint_r24(p,n);
        emit Mint(p,n);
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
  function updateTotalOutOnInsertTransfer_r30(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r5(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalMint_r5(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateOwnerOnInsertConstructor_r22() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateAllowanceOnIncrementSpentTotal_r7(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTransferOnInsertRecv_transfer_r23(address from,address to,int amount) private   returns (bool) {
      address owner_p = owner.p;
      if(r==owner_p && s==owner_p) {
        updateTotalOutOnInsertTransfer_r30(s,n);
        updateTotalInOnInsertTransfer_r10(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateTotalMintOnInsertMint_r24(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r5(p,delta0);
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r15(delta0);
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
  function updateTransferFromOnInsertRecv_transferFrom_r11(address from,address to,address spender,int amount) private   returns (bool) {
      if(0==n) {
        updateTransferOnInsertTransferFrom_r19(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r27(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateAllBurnOnInsertBurn_r14(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r15(delta0);
  }
}