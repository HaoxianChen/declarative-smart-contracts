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
    updateOwnerOnInsertConstructor_r22();
    updateTotalSupplyOnInsertConstructor_r13();
    updateControllerOnInsertConstructor_r19(p);
    updateTotalBalancesOnInsertConstructor_r3();
  }
  function getController() public view  returns (address) {
      address p = controller.p;
      return p;
  }
  function mint(address p,int amount) public    {
      bool r26 = updateMintOnInsertRecv_mint_r26(p,amount);
      if(r26==false) {
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
      bool r10 = updateTransferFromOnInsertRecv_transferFrom_r10(from,to,spender,amount);
      if(r10==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r21 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r21(p,s,n);
      if(r21==false) {
        revert("Rule condition failed");
      }
  }
  function controllerRedeem(address p,int amount) public    {
      bool r15 = updateControllerRedeemOnInsertRecv_controllerRedeem_r15(p,amount);
      if(r15==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function burn(address p,int amount) public    {
      bool r11 = updateBurnOnInsertRecv_burn_r11(p,amount);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r9 = updateTransferOnInsertRecv_transfer_r9(from,to,amount);
      if(r9==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function updateTotalInOnInsertTransfer_r14(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r4(p,delta0);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r8(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateBalanceOfOnIncrementTotalMint_r4(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateBalanceOfOnIncrementTotalOut_r4(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateBurnOnInsertRecv_burn_r11(address p,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(n<=balanceOf_x1) {
        updateTotalBurnOnInsertBurn_r1(p,n);
        updateAllBurnOnInsertBurn_r17(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementSpentTotal_r8(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r21(address o,address s,int d) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r6(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r18(int b) private    {
      totalSupply.n -= b;
  }
  function updateTransferOnInsertRecv_transfer_r9(address s,address r,int n) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[s].n;
      if(n>0 && n<=balanceOf_x1_1) {
        updateTotalInOnInsertTransfer_r14(r,n);
        updateTotalOutOnInsertTransfer_r28(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r13() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateSpentTotalOnInsertTransferFrom_r24(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r8(o,s,delta0);
  }
  function updateBalanceOfOnIncrementTotalBurn_r4(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateTotalMintOnInsertMint_r7(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r4(p,delta0);
  }
  function updateControllerOnInsertConstructor_r19(address p) private    {
      controller = ControllerTuple(p,true);
  }
  function updateTotalBalancesOnInsertConstructor_r3() private    {
      // Empty()
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r18(delta0);
  }
  function updateTotalBurnOnInsertBurn_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r4(p,delta0);
  }
  function updateBurnOnInsertControllerRedeem_r27(address p,int n) private    {
      updateTotalBurnOnInsertBurn_r1(p,n);
      updateAllBurnOnInsertBurn_r17(n);
      emit Burn(p,n);
  }
  function updateTotalOutOnInsertTransfer_r28(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r4(p,delta0);
  }
  function updateControllerRedeemOnInsertRecv_controllerRedeem_r15(address p,int n) private   returns (bool) {
      address controller_p_0 = controller.p;
      int balanceOf_x1_1 = balanceOf[p].n;
      if(p==controller_p_0 && n<=balanceOf_x1_1) {
        updateBurnOnInsertControllerRedeem_r27(p,n);
        emit ControllerRedeem(p,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r18(int m) private    {
      totalSupply.n += m;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r10(address o,address r,address s,int n) private   returns (bool) {
      int balanceOf_x1_2 = balanceOf[o].n;
      int allowance_x2_1 = allowance[o][s].n;
      if(n>0 && n<=allowance_x2_1 && n<=balanceOf_x1_2) {
        updateSpentTotalOnInsertTransferFrom_r24(o,s,n);
        updateTransferOnInsertTransferFrom_r5(o,r,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateAllBurnOnInsertBurn_r17(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r18(delta0);
  }
  function updateOwnerOnInsertConstructor_r22() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTransferOnInsertTransferFrom_r5(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r14(r,n);
      updateTotalOutOnInsertTransfer_r28(o,n);
      emit Transfer(o,r,n);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r6(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r8(o,s,delta0);
  }
  function updateBalanceOfOnIncrementTotalIn_r4(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateMintOnInsertRecv_mint_r26(address p,int n) private   returns (bool) {
      if(n>=0) {
        updateTotalMintOnInsertMint_r7(p,n);
        updateAllMintOnInsertMint_r0(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
}