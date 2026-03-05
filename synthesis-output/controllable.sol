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
    updateOwnerOnInsertConstructor_r41();
    updateTotalSupplyOnInsertConstructor_r13();
    updateControllerOnInsertConstructor_r30(p);
    updateTotalBalancesOnInsertConstructor_r3();
  }
  function getController() public view  returns (address) {
      address p = controller.p;
      return p;
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function burn(address p,int amount) public    {
      bool r32 = updateBurnOnInsertRecv_burn_r32(p,amount);
      if(r32==false) {
        revert("Rule condition failed");
      }
  }
  function controllerRedeem(address p,int amount) public    {
      bool r19 = updateControllerRedeemOnInsertRecv_controllerRedeem_r19(p,amount);
      if(r19==false) {
        revert("Rule condition failed");
      }
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
  function controllerTransfer(address from,address to,int amount) public    {
      bool r54 = updateControllerTransferOnInsertRecv_controllerTransfer_r54(from,to,amount);
      if(r54==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function mint(address p,int amount) public    {
      bool r44 = updateMintOnInsertRecv_mint_r44(p,amount);
      if(r44==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r26 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r26(p,s,n);
      if(r26==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r38 = updateTransferOnInsertRecv_transfer_r38(from,to,amount);
      if(r38==false) {
        revert("Rule condition failed");
      }
  }
  function updateTotalOutOnInsertTransfer_r52(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r33(p,delta0);
  }
  function updateTotalBalancesOnInsertConstructor_r3() private    {
      // Empty()
  }
  function updateTotalMintOnInsertMint_r46(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r33(p,delta0);
  }
  function updateAllBurnOnInsertBurn_r23(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r25(delta0);
  }
  function updateControllerTransferOnInsertRecv_controllerTransfer_r54(address from,address to,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address c_1 = controller.p;
      int m_2 = balanceOf[s].n;
      if(s!=address(0) && n<=m_2 && n>0 && r!=address(0) && s_1==c_1) {
        updateTransferOnInsertControllerTransfer_r29(s,r,n);
        emit ControllerTransfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalMint_r33(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateBalanceOfOnIncrementTotalBurn_r33(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateOwnerOnInsertConstructor_r41() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTotalSupplyOnIncrementAllBurn_r25(int b) private    {
      totalSupply.n -= b;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r20(address from,address to,address spender,int amount) private   returns (bool) {
      int m_1 = balanceOf[o].n;
      if(r!=address(0) && n<=m_1 && s!=address(0) && o!=address(0) && n>0) {
        updateSpentTotalOnInsertTransferFrom_r49(o,s,n);
        updateTransferOnInsertTransferFrom_r34(o,r,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateBurnOnInsertRecv_burn_r32(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      int m_2 = balanceOf[p].n;
      if(n>0 && p!=address(0) && o_1==s_1 && n<=m_2) {
        updateTotalBurnOnInsertBurn_r0(p,n);
        updateAllBurnOnInsertBurn_r23(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertTransferFrom_r34(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r15(r,n);
      updateTotalOutOnInsertTransfer_r52(o,n);
      emit Transfer(o,r,n);
  }
  function updateSpentTotalOnInsertTransferFrom_r49(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r7(o,s,delta0);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r7(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateTotalSupplyOnIncrementAllMint_r25(int m) private    {
      totalSupply.n += m;
  }
  function updateControllerRedeemOnInsertRecv_controllerRedeem_r19(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address c_1 = controller.p;
      int m_2 = balanceOf[p].n;
      if(s_1==c_1 && p!=address(0) && n<=m_2 && n>0) {
        updateBurnOnInsertControllerRedeem_r51(p,n);
        emit ControllerRedeem(p,n);
        return true;
      }
      return false;
  }
  function updateAllMintOnInsertMint_r11(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r25(delta0);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r26(address p,address s,int n) private   returns (bool) {
      if(d>=0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r45(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertControllerTransfer_r29(address s,address r,int n) private    {
      updateTotalInOnInsertTransfer_r15(r,n);
      updateTotalOutOnInsertTransfer_r52(s,n);
      emit Transfer(s,r,n);
  }
  function updateBurnOnInsertControllerRedeem_r51(address p,int n) private    {
      updateTotalBurnOnInsertBurn_r0(p,n);
      updateAllBurnOnInsertBurn_r23(n);
      emit Burn(p,n);
  }
  function updateMintOnInsertRecv_mint_r44(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(n>0 && p!=address(0) && o_1==s_1) {
        updateTotalMintOnInsertMint_r46(p,n);
        updateAllMintOnInsertMint_r11(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateControllerOnInsertConstructor_r30(address p) private    {
      controller = ControllerTuple(p,true);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTotalInOnInsertTransfer_r15(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r33(p,delta0);
  }
  function updateAllowanceOnIncrementSpentTotal_r7(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTotalBurnOnInsertBurn_r0(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r33(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalOut_r33(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateTotalSupplyOnInsertConstructor_r13() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateTransferOnInsertRecv_transfer_r38(address from,address to,int amount) private   returns (bool) {
      int m_1 = balanceOf[s].n;
      if(r!=address(0) && n>0 && s!=address(0) && n<=m_1) {
        updateTotalInOnInsertTransfer_r15(r,n);
        updateTotalOutOnInsertTransfer_r52(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalIn_r33(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r45(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r7(o,s,delta0);
  }
}