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
    updateOwnerOnInsertConstructor_r4();
    updateControllerOnInsertConstructor_r17(p);
    updateTotalBalancesOnInsertConstructor_r2();
    updateTotalSupplyOnInsertConstructor_r10();
  }
  function burn(address p,int amount) public    {
      bool r19 = updateBurnOnInsertRecv_burn_r19(p,amount);
      if(r19==false) {
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
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r20 = updateTransferFromOnInsertRecv_transferFrom_r20(from,to,spender,amount);
      if(r20==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r25 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r25(p,s,n);
      if(r25==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r3 = updateTransferOnInsertRecv_transfer_r3(from,to,amount);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function mint(address p,int amount) public    {
      bool r7 = updateMintOnInsertRecv_mint_r7(p,amount);
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function getController() public view  returns (address) {
      address p = controller.p;
      return p;
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function controllerRedeem(address p,int amount) public    {
      bool r13 = updateControllerRedeemOnInsertRecv_controllerRedeem_r13(p,amount);
      if(r13==false) {
        revert("Rule condition failed");
      }
  }
  function updateBalanceOfOnIncrementTotalOut_r21(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateTotalSupplyOnIncrementAllBurn_r15(int b) private    {
      totalSupply.n -= b;
  }
  function updateControllerRedeemOnInsertRecv_controllerRedeem_r13(address p,int amount) private   returns (bool) {
      address c = controller.p;
      if(p==c) {
        updateBurnOnInsertControllerRedeem_r29(p,n);
        emit ControllerRedeem(p,n);
        return true;
      }
      return false;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r20(address from,address to,address spender,int amount) private   returns (bool) {
      if(n>=0) {
        updateTransferOnInsertTransferFrom_r22(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r27(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r10() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateTotalOutOnInsertTransfer_r30(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r21(p,delta0);
  }
  function updateTotalMintOnInsertMint_r24(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r21(p,delta0);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r25(address p,address s,int n) private   returns (bool) {
      updateAllowanceTotalOnInsertIncreaseAllowance_r5(o,s,d);
      emit IncreaseAllowance(o,s,d);
      return true;
      return false;
  }
  function updateAllowanceOnIncrementSpentTotal_r6(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r5(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r6(o,s,delta0);
  }
  function updateTotalBurnOnInsertBurn_r0(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r21(p,delta0);
  }
  function updateAllMintOnInsertMint_r8(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r15(delta0);
  }
  function updateOwnerOnInsertConstructor_r4() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateAllBurnOnInsertBurn_r14(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r15(delta0);
  }
  function updateTotalBalancesOnInsertConstructor_r2() private    {
      // Empty()
  }
  function updateBalanceOfOnIncrementTotalMint_r21(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateBalanceOfOnIncrementTotalBurn_r21(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateTotalInOnInsertTransfer_r11(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r21(p,delta0);
  }
  function updateTransferOnInsertTransferFrom_r22(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r30(o,n);
      updateTotalInOnInsertTransfer_r11(r,n);
      emit Transfer(o,r,n);
  }
  function updateTransferOnInsertRecv_transfer_r3(address from,address to,int amount) private   returns (bool) {
      address controller_p_1 = controller.p;
      if(n>0 && s==controller_p_1) {
        updateTotalInOnInsertTransfer_r11(r,n);
        updateTotalOutOnInsertTransfer_r30(s,n);
        emit Transfer(s,r,n);
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
  function updateAllowanceOnIncrementAllowanceTotal_r6(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateSpentTotalOnInsertTransferFrom_r27(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r6(o,s,delta0);
  }
  function updateTotalSupplyOnIncrementAllMint_r15(int m) private    {
      totalSupply.n += m;
  }
  function updateMintOnInsertRecv_mint_r7(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        updateAllMintOnInsertMint_r8(n);
        updateTotalMintOnInsertMint_r24(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateBurnOnInsertControllerRedeem_r29(address p,int n) private    {
      updateTotalBurnOnInsertBurn_r0(p,n);
      updateAllBurnOnInsertBurn_r14(n);
      emit Burn(p,n);
  }
  function updateBalanceOfOnIncrementTotalIn_r21(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateControllerOnInsertConstructor_r17(address p) private    {
      controller = ControllerTuple(p,true);
  }
  function updateBurnOnInsertRecv_burn_r19(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address owner_x = owner.p;
      address msgSender_x = msg.sender;
      address o = owner.p;
      if(o==s && owner_x==msgSender_x) {
        updateTotalBurnOnInsertBurn_r0(p,n);
        updateAllBurnOnInsertBurn_r14(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
}