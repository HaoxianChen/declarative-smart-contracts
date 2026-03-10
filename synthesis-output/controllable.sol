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
    updateControllerOnInsertConstructor_r29(p);
    updateOwnerOnInsertConstructor_r41();
    updateTotalBalancesOnInsertConstructor_r4();
    updateTotalSupplyOnInsertConstructor_r16();
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
      bool r12 = updateTransferFromOnInsertRecv_transferFrom_r12(from,to,spender,amount);
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r37 = updateTransferOnInsertRecv_transfer_r37(from,to,amount);
      if(r37==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r40 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r40(p,s,n);
      if(r40==false) {
        revert("Rule condition failed");
      }
  }
  function getOwner() public view  returns (address) {
      address p = owner.p;
      return p;
  }
  function controllerTransfer(address from,address to,int amount) public    {
      bool r54 = updateControllerTransferOnInsertRecv_controllerTransfer_r54(from,to,amount);
      if(r54==false) {
        revert("Rule condition failed");
      }
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
  function burn(address p,int amount) public    {
      bool r31 = updateBurnOnInsertRecv_burn_r31(p,amount);
      if(r31==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function updateTotalBurnOnInsertBurn_r0(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r32(p,delta0);
  }
  function updateTotalInOnInsertTransfer_r18(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r32(p,delta0);
  }
  function updateControllerRedeemOnInsertRecv_controllerRedeem_r3(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address c_1 = controller.p;
      int m_2 = balanceOf[p].n;
      if(s_1==c_1 && p!=address(0) && amount<=m_2 && amount>0) {
        updateBurnOnInsertControllerRedeem_r51(p,amount);
        emit ControllerRedeem(p,amount);
        return true;
      }
      return false;
  }
  function updateControllerOnInsertConstructor_r29(address p) private    {
      controller = ControllerTuple(p,true);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r9(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateTotalSupplyOnInsertConstructor_r16() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateTotalSupplyOnIncrementAllMint_r26(int m) private    {
      totalSupply.n += m;
  }
  function updateBalanceOfOnIncrementTotalMint_r32(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateBalanceOfOnIncrementTotalBurn_r32(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateControllerTransferOnInsertRecv_controllerTransfer_r54(address from,address to,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address c_1 = controller.p;
      int m_2 = balanceOf[from].n;
      if(from!=address(0) && amount<=m_2 && amount>0 && to!=address(0) && s_1==c_1) {
        updateTransferOnInsertControllerTransfer_r28(from,to,amount);
        emit ControllerTransfer(from,to,amount);
        return true;
      }
      return false;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r40(address p,address s,int n) private   returns (bool) {
      if(n>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r45(p,s,n);
        emit IncreaseAllowance(p,s,n);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertTransferFrom_r33(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r52(o,n);
      updateTotalInOnInsertTransfer_r18(r,n);
      emit Transfer(o,r,n);
  }
  function updateSpentTotalOnInsertTransferFrom_r49(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r9(o,s,delta0);
  }
  function updateTransferOnInsertRecv_transfer_r37(address from,address to,int amount) private   returns (bool) {
      int m_1 = balanceOf[from].n;
      if(to!=address(0) && from!=address(0) && amount>0 && amount<=m_1) {
        updateTotalOutOnInsertTransfer_r52(from,amount);
        updateTotalInOnInsertTransfer_r18(to,amount);
        emit Transfer(from,to,amount);
        return true;
      }
      return false;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r12(address from,address to,address spender,int amount) private   returns (bool) {
      address msgSender = msg.sender;
      address controller_p_1 = controller.p;
      address owner_p_2 = owner.p;
      int balanceOf_x1_3 = balanceOf[spender].n;
      int m_1 = balanceOf[from].n;
      int allowance_x2_0 = allowance[msgSender][spender].n;
      if(to!=address(0) && to==owner_p_2 && amount<=m_1 && amount<balanceOf_x1_3 && spender!=address(0) && to==controller_p_1 && amount<allowance_x2_0 && from!=address(0) && amount>0) {
        updateTransferOnInsertTransferFrom_r33(from,to,amount);
        updateSpentTotalOnInsertTransferFrom_r49(from,spender,amount);
        emit TransferFrom(from,to,spender,amount);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r32(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateTotalOutOnInsertTransfer_r52(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r32(p,delta0);
  }
  function updateAllowanceOnIncrementSpentTotal_r9(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateBurnOnInsertRecv_burn_r31(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      int m_2 = balanceOf[p].n;
      if(amount>0 && p!=address(0) && o_1==s_1 && amount<=m_2) {
        updateAllBurnOnInsertBurn_r24(amount);
        updateTotalBurnOnInsertBurn_r0(p,amount);
        emit Burn(p,amount);
        return true;
      }
      return false;
  }
  function updateBurnOnInsertControllerRedeem_r51(address p,int n) private    {
      updateTotalBurnOnInsertBurn_r0(p,n);
      updateAllBurnOnInsertBurn_r24(n);
      emit Burn(p,n);
  }
  function updateTotalSupplyOnIncrementAllBurn_r26(int b) private    {
      totalSupply.n -= b;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r45(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r9(o,s,delta0);
  }
  function updateTotalMintOnInsertMint_r46(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r32(p,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalBalancesOnInsertConstructor_r4() private    {
      // Empty()
  }
  function updateMintOnInsertRecv_mint_r44(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(amount>0 && p!=address(0) && o_1==s_1) {
        updateAllMintOnInsertMint_r14(amount);
        updateTotalMintOnInsertMint_r46(p,amount);
        emit Mint(p,amount);
        return true;
      }
      return false;
  }
  function updateAllBurnOnInsertBurn_r24(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r26(delta0);
  }
  function updateTransferOnInsertControllerTransfer_r28(address s,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r52(s,n);
      updateTotalInOnInsertTransfer_r18(r,n);
      emit Transfer(s,r,n);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateAllMintOnInsertMint_r14(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r26(delta0);
  }
  function updateBalanceOfOnIncrementTotalIn_r32(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateOwnerOnInsertConstructor_r41() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
}