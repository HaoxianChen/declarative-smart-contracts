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
  event Transfer(address from,address to,int amount);
  event Mint(address p,int amount);
  event TransferFrom(address from,address to,address spender,int amount);
  event ControllerRedeem(address p,int amount);
  event Burn(address p,int amount);
  event IncreaseAllowance(address p,address s,int n);
  event IllegalRedeem(address p,int n);
  event IllegalControllerTransfer();
  event ControllerTransfer(address from,address to,int amount);
  constructor(address p) public {
    updateControllerOnInsertConstructor_r21(p);
    updateTotalSupplyOnInsertConstructor_r15();
    updateOwnerOnInsertConstructor_r25();
    updateTotalBalancesOnInsertConstructor_r2();
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function controllerTransfer(address from,address to,int amount) public    {
      bool r4 = updateControllerTransferOnInsertRecv_controllerTransfer_r4(from,to,amount);
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function getOwner() public view  returns (address) {
      address p = owner.p;
      return p;
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r24 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r24(p,s,n);
      if(r24==false) {
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
  function transfer(address from,address to,int amount) public    {
      bool r12 = updateTransferOnInsertRecv_transfer_r12(from,to,amount);
      if(r12==false) {
        revert("Rule condition failed");
      }
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
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r7 = updateTransferFromOnInsertRecv_transferFrom_r7(from,to,spender,amount);
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r11 = updateMintOnInsertRecv_mint_r11(p,amount);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r13 = updateBurnOnInsertRecv_burn_r13(p,amount);
      if(r13==false) {
        revert("Rule condition failed");
      }
  }
  function updateControllerRedeemOnInsertRecv_controllerRedeem_r3(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address c = controller.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(s==c && amount<=balanceOf_x1) {
        updateBurnOnInsertControllerRedeem_r29(p,amount);
        emit ControllerRedeem(p,amount);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalBurn_r5(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateTotalBurnOnInsertBurn_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r5(p,delta0);
  }
  function updateTotalSupplyOnInsertConstructor_r15() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateOwnerOnInsertConstructor_r25() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r19(delta0);
  }
  function updateTotalMintOnInsertMint_r9(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r5(p,delta0);
  }
  function updateBurnOnInsertRecv_burn_r13(address p,int amount) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(amount<=balanceOf_x1) {
        updateAllBurnOnInsertBurn_r18(amount);
        updateTotalBurnOnInsertBurn_r1(p,amount);
        emit Burn(p,amount);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r19(int b) private    {
      totalSupply.n -= b;
  }
  function updateControllerOnInsertConstructor_r21(address p) private    {
      controller = ControllerTuple(p,true);
  }
  function updateMintOnInsertRecv_mint_r11(address p,int amount) private   returns (bool) {
      if(amount>0) {
        updateTotalMintOnInsertMint_r9(p,amount);
        updateAllMintOnInsertMint_r0(amount);
        emit Mint(p,amount);
        return true;
      }
      return false;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r8(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r10(o,s,delta0);
  }
  function updateBurnOnInsertControllerRedeem_r29(address p,int n) private    {
      updateTotalBurnOnInsertBurn_r1(p,n);
      updateAllBurnOnInsertBurn_r18(n);
      emit Burn(p,n);
  }
  function updateTotalInOnInsertTransfer_r16(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r5(p,delta0);
  }
  function updateTransferOnInsertTransferFrom_r6(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r30(o,n);
      updateTotalInOnInsertTransfer_r16(r,n);
      emit Transfer(o,r,n);
  }
  function updateBalanceOfOnIncrementTotalIn_r5(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r24(address p,address s,int n) private   returns (bool) {
      if(n>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r8(p,s,n);
        emit IncreaseAllowance(p,s,n);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertControllerTransfer_r22(address s,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r30(s,n);
      updateTotalInOnInsertTransfer_r16(r,n);
      emit Transfer(s,r,n);
  }
  function updateTotalSupplyOnIncrementAllMint_r19(int m) private    {
      totalSupply.n += m;
  }
  function updateTotalBalancesOnInsertConstructor_r2() private    {
      // Empty()
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
  function updateTransferOnInsertRecv_transfer_r12(address from,address to,int amount) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[from].n;
      if(amount>0 && amount<=balanceOf_x1_1) {
        updateTotalInOnInsertTransfer_r16(to,amount);
        updateTotalOutOnInsertTransfer_r30(from,amount);
        emit Transfer(from,to,amount);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalMint_r5(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateAllBurnOnInsertBurn_r18(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r19(delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r7(address from,address to,address spender,int amount) private   returns (bool) {
      int balanceOf_x1_2 = balanceOf[from].n;
      int allowance_x2_1 = allowance[from][spender].n;
      if(amount>0 && amount<allowance_x2_1 && amount<balanceOf_x1_2) {
        updateTransferOnInsertTransferFrom_r6(from,to,amount);
        updateSpentTotalOnInsertTransferFrom_r27(from,spender,amount);
        emit TransferFrom(from,to,spender,amount);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r10(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateControllerTransferOnInsertRecv_controllerTransfer_r4(address from,address to,int amount) private   returns (bool) {
      address controller_p = controller.p;
      if(from==msg.sender) {
        if(from==controller_p && to==controller_p) {
          updateTransferOnInsertControllerTransfer_r22(from,to,amount);
          emit ControllerTransfer(from,to,amount);
          return true;
        }
      }
      return false;
  }
  function updateAllowanceOnIncrementSpentTotal_r10(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateBalanceOfOnIncrementTotalOut_r5(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateSpentTotalOnInsertTransferFrom_r27(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r10(o,s,delta0);
  }
}