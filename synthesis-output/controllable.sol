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
  event ControllerTransfer(address from,address to,int amount);
  constructor(address p) public {
    updateOwnerOnInsertConstructor_r5();
    updateTotalBalancesOnInsertConstructor_r2();
    updateTotalSupplyOnInsertConstructor_r9();
    updateControllerOnInsertConstructor_r16(p);
  }
  function getController() public view  returns (address) {
      address p = controller.p;
      return p;
  }
  function controllerRedeem(address p,int amount) public    {
      bool r11 = updateControllerRedeemOnInsertRecv_controllerRedeem_r11(p,amount);
      if(r11==false) {
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
      bool r13 = updateTransferFromOnInsertRecv_transferFrom_r13(from,to,spender,amount);
      if(r13==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r19 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r19(p,s,n);
      if(r19==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function transfer(address from,address to,int amount) public    {
      bool r23 = updateTransferOnInsertRecv_transfer_r23(from,to,amount);
      if(r23==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r25 = updateMintOnInsertRecv_mint_r25(p,amount);
      if(r25==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r7 = updateBurnOnInsertRecv_burn_r7(p,amount);
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function updateTotalBalancesOnInsertConstructor_r2() private    {
      // Empty()
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r6(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r22(o,s,delta0);
  }
  function updateTotalInOnInsertTransfer_r10(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r3(p,delta0);
  }
  function updateTotalBurnOnInsertBurn_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r3(p,delta0);
  }
  function updateOwnerOnInsertConstructor_r5() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalOutOnInsertTransfer_r27(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r3(p,delta0);
  }
  function updateBurnOnInsertControllerRedeem_r26(address p,int n) private    {
      updateTotalBurnOnInsertBurn_r1(p,n);
      updateAllBurnOnInsertBurn_r14(n);
      emit Burn(p,n);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r22(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r13(address o,address r,address s,int n) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[o].n;
      if(n>=0 && n<balanceOf_x1_1) {
        updateTransferOnInsertTransferFrom_r4(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r21(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateAllBurnOnInsertBurn_r14(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r15(delta0);
  }
  function updateBalanceOfOnIncrementTotalOut_r3(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateSpentTotalOnInsertTransferFrom_r21(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r22(o,s,delta0);
  }
  function updateTotalMintOnInsertMint_r18(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r3(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalBurn_r3(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateMintOnInsertRecv_mint_r25(address p,int n) private   returns (bool) {
      if(n>=0) {
        updateAllMintOnInsertMint_r0(n);
        updateTotalMintOnInsertMint_r18(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r9() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateTotalSupplyOnIncrementAllBurn_r15(int b) private    {
      totalSupply.n -= b;
  }
  function updateBurnOnInsertRecv_burn_r7(address p,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(n<=balanceOf_x1) {
        updateTotalBurnOnInsertBurn_r1(p,n);
        updateAllBurnOnInsertBurn_r14(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementSpentTotal_r22(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateControllerOnInsertConstructor_r16(address p) private    {
      controller = ControllerTuple(p,true);
  }
  function updateBalanceOfOnIncrementTotalIn_r3(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateBalanceOfOnIncrementTotalMint_r3(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateControllerRedeemOnInsertRecv_controllerRedeem_r11(address p,int n) private   returns (bool) {
      address controller_p_0 = controller.p;
      int balanceOf_x1_1 = balanceOf[p].n;
      if(p==controller_p_0 && n<=balanceOf_x1_1) {
        updateBurnOnInsertControllerRedeem_r26(p,n);
        emit ControllerRedeem(p,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r15(int m) private    {
      totalSupply.n += m;
  }
  function updateTransferOnInsertRecv_transfer_r23(address s,address r,int n) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[s].n;
      if(n>=0 && n<balanceOf_x1_1) {
        updateTotalOutOnInsertTransfer_r27(s,n);
        updateTotalInOnInsertTransfer_r10(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r15(delta0);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r19(address o,address s,int d) private   returns (bool) {
      updateAllowanceTotalOnInsertIncreaseAllowance_r6(o,s,d);
      emit IncreaseAllowance(o,s,d);
      return true;
      return false;
  }
  function updateTransferOnInsertTransferFrom_r4(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r27(o,n);
      updateTotalInOnInsertTransfer_r10(r,n);
      emit Transfer(o,r,n);
  }
}