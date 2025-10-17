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
    updateTotalBalancesOnInsertConstructor_r5();
    updateTotalSupplyOnInsertConstructor_r10();
    updateOwnerOnInsertConstructor_r21();
    updateControllerOnInsertConstructor_r16(p);
  }
  function transfer(address from,address to,int amount) public    {
      bool r8 = updateTransferOnInsertRecv_transfer_r8(from,to,amount);
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function getController() public view  returns (address) {
      address p = controller.p;
      return p;
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r23 = updateTransferFromOnInsertRecv_transferFrom_r23(from,to,spender,amount);
      if(r23==false) {
        revert("Rule condition failed");
      }
  }
  function controllerRedeem(address p,int amount) public    {
      bool r4 = updateControllerRedeemOnInsertRecv_controllerRedeem_r4(p,amount);
      if(r4==false) {
        revert("Rule condition failed");
      }
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
      bool r26 = updateBurnOnInsertRecv_burn_r26(p,amount);
      if(r26==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r20 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r20(p,s,n);
      if(r20==false) {
        revert("Rule condition failed");
      }
  }
  function getOwner() public view  returns (address) {
      address p = owner.p;
      return p;
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function mint(address p,int amount) public    {
      bool r3 = updateMintOnInsertRecv_mint_r3(p,amount);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r7(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r27(o,s,delta0);
  }
  function updateControllerRedeemOnInsertRecv_controllerRedeem_r4(address p,int n) private   returns (bool) {
      address c = controller.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(p==c && n<balanceOf_x1) {
        updateBurnOnInsertControllerRedeem_r29(p,n);
        emit ControllerRedeem(p,n);
        return true;
      }
      return false;
  }
  function updateBurnOnInsertControllerRedeem_r29(address p,int n) private    {
      updateTotalBurnOnInsertBurn_r1(p,n);
      updateAllBurnOnInsertBurn_r13(n);
      emit Burn(p,n);
  }
  function updateBalanceOfOnIncrementTotalIn_r6(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalMintOnInsertMint_r22(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r6(p,delta0);
  }
  function updateTotalInOnInsertTransfer_r11(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r6(p,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateBalanceOfOnIncrementTotalMint_r6(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateTotalBalancesOnInsertConstructor_r5() private    {
      // Empty()
  }
  function updateTransferOnInsertTransferFrom_r18(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r30(o,n);
      updateTotalInOnInsertTransfer_r11(r,n);
      emit Transfer(o,r,n);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r20(address o,address s,int d) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r7(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertRecv_transfer_r8(address s,address r,int n) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[s].n;
      if(n>=0 && n<=balanceOf_x1_1) {
        updateTotalInOnInsertTransfer_r11(r,n);
        updateTotalOutOnInsertTransfer_r30(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r14(int b) private    {
      totalSupply.n -= b;
  }
  function updateAllowanceOnIncrementSpentTotal_r27(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTotalOutOnInsertTransfer_r30(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r6(p,delta0);
  }
  function updateMintOnInsertRecv_mint_r3(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>=0) {
        updateTotalMintOnInsertMint_r22(p,n);
        updateAllMintOnInsertMint_r0(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r14(delta0);
  }
  function updateBurnOnInsertRecv_burn_r26(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && n<=balanceOf_x1) {
        updateTotalBurnOnInsertBurn_r1(p,n);
        updateAllBurnOnInsertBurn_r13(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r27(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateTotalSupplyOnIncrementAllMint_r14(int m) private    {
      totalSupply.n += m;
  }
  function updateSpentTotalOnInsertTransferFrom_r25(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r27(o,s,delta0);
  }
  function updateTotalSupplyOnInsertConstructor_r10() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateAllBurnOnInsertBurn_r13(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r14(delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r23(address o,address r,address s,int n) private   returns (bool) {
      int balanceOf_x1_2 = balanceOf[o].n;
      int allowance_x2_1 = allowance[o][s].n;
      if(n>=0 && n<=allowance_x2_1 && n<=balanceOf_x1_2) {
        updateSpentTotalOnInsertTransferFrom_r25(o,s,n);
        updateTransferOnInsertTransferFrom_r18(o,r,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateTotalBurnOnInsertBurn_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r6(p,delta0);
  }
  function updateControllerOnInsertConstructor_r16(address p) private    {
      controller = ControllerTuple(p,true);
  }
  function updateOwnerOnInsertConstructor_r21() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateBalanceOfOnIncrementTotalBurn_r6(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateBalanceOfOnIncrementTotalOut_r6(address p,int o) private    {
      balanceOf[p].n -= o;
  }
}