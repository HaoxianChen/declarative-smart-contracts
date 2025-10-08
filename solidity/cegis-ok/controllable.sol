contract Controllable {
  struct TotalSupplyTuple {
    uint n;
    bool _valid;
  }
  struct AllowanceTuple {
    uint n;
    bool _valid;
  }
  struct BalanceOfTuple {
    uint n;
    bool _valid;
  }
  TotalSupplyTuple totalSupply;
  mapping(address=>BalanceOfTuple) balanceOf;
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  event TransferFrom(address from,address to,address spender,uint amount);
  event Burn(address p,uint amount);
  event Mint(address p,uint amount);
  event ControllerRedeem(address p,uint amount);
  event IncreaseAllowance(address p,address s,uint n);
  event ControllerTransfer(address from,address to,uint amount);
  event Transfer(address from,address to,uint amount);
  constructor(address p) public {
    updateOwnerOnInsertConstructor_r8();
    updateTotalBalancesOnInsertConstructor_r7();
    updateTotalSupplyOnInsertConstructor_r13();
    updateControllerOnInsertConstructor_r5(p);
  }
  function transferFrom(address from,address to,uint amount) public    {
      bool r25 = updateTransferFromOnInsertRecv_transferFrom_r25(from,to,amount);
      if(r25==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (uint) {
      uint n = totalSupply.n;
      return n;
  }
  function controllerRedeem(address p,uint amount) public    {
      bool r0 = updateControllerRedeemOnInsertRecv_controllerRedeem_r0(p,amount);
      if(r0==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (uint) {
      uint n = balanceOf[p].n;
      return n;
  }
  function burn(address p,uint amount) public    {
      bool r24 = updateBurnOnInsertRecv_burn_r24(p,amount);
      if(r24==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,uint amount) public    {
      bool r12 = updateMintOnInsertRecv_mint_r12(p,amount);
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address to,uint amount) public    {
      bool r2 = updateTransferOnInsertRecv_transfer_r2(to,amount);
      if(r2==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (uint) {
      uint n = allowance[p][s].n;
      return n;
  }
  function approve(address s,uint n) public    {
      bool r14 = updateIncreaseAllowanceOnInsertRecv_approve_r14(s,n);
      if(r14==false) {
        revert("Rule condition failed");
      }
  }
  function updateTransferFromOnInsertRecv_transferFrom_r25(address o,address r,uint n) private   returns (bool) {
      updateSpentTotalOnInsertTransferFrom_r23(o,s,n);
      updateTransferOnInsertTransferFrom_r1(o,r,n);
      emit TransferFrom(o,r,s,n);
      return true;
      return false;
  }
  function updateControllerOnInsertConstructor_r5(address p) private    {
      // Empty()
  }
  function updateSpentTotalOnInsertTransferFrom_r23(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r26(o,s,delta0);
  }
  function updateControllerRedeemOnInsertRecv_controllerRedeem_r0(address p,uint n) private   returns (bool) {
      updateBurnOnInsertControllerRedeem_r20(p,n);
      emit ControllerRedeem(p,n);
      return true;
      return false;
  }
  function updateTransferOnInsertTransferFrom_r1(address o,address r,uint n) private    {
      updateTotalOutOnInsertTransfer_r18(o,n);
      updateTotalInOnInsertTransfer_r10(r,n);
      emit Transfer(o,r,n);
  }
  function updateBalanceOfOnIncrementTotalBurn_r22(address p,int m) private    {
      int _delta = int(-m);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateTotalSupplyOnIncrementAllBurn_r17(int b) private    {
      int _delta = int(-b);
      uint x_totalSupply__n = totalSupply.n;
      uint newValue = updateuintByint(x_totalSupply__n,_delta);
      totalSupply.n = newValue;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalBalancesOnInsertConstructor_r7() private    {
      // Empty()
  }
  function updateBalanceOfOnIncrementTotalIn_r22(address p,int i) private    {
      int _delta = int(i);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateTotalSupplyOnInsertConstructor_r13() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateTotalInOnInsertTransfer_r10(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r22(p,delta0);
  }
  function updateIncreaseAllowanceOnInsertRecv_approve_r14(address s,uint n) private   returns (bool) {
      updateAllowanceTotalOnInsertIncreaseAllowance_r11(o,s,d);
      emit IncreaseAllowance(o,s,d);
      return true;
      return false;
  }
  function updateBurnOnInsertRecv_burn_r24(address p,uint n) private   returns (bool) {
      updateTotalBurnOnInsertBurn_r15(p,n);
      updateAllBurnOnInsertBurn_r9(n);
      emit Burn(p,n);
      return true;
      return false;
  }
  function updateTotalOutOnInsertTransfer_r18(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r22(p,delta0);
  }
  function updateAllowanceOnIncrementSpentTotal_r26(address o,address s,int l) private    {
      int _delta = int(-l);
      uint x_allowance_o_s_n = allowance[o][s].n;
      uint newValue = updateuintByint(x_allowance_o_s_n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateBalanceOfOnIncrementTotalMint_r22(address p,int n) private    {
      int _delta = int(n);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r11(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r26(o,s,delta0);
  }
  function updateAllMintOnInsertMint_r3(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r17(delta0);
  }
  function updateTotalSupplyOnIncrementAllMint_r17(int m) private    {
      int _delta = int(m);
      uint x_totalSupply__n = totalSupply.n;
      uint newValue = updateuintByint(x_totalSupply__n,_delta);
      totalSupply.n = newValue;
  }
  function updateBalanceOfOnIncrementTotalOut_r22(address p,int o) private    {
      int _delta = int(-o);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateTotalBurnOnInsertBurn_r15(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r22(p,delta0);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r26(address o,address s,int m) private    {
      int _delta = int(m);
      uint x_allowance_o_s_n = allowance[o][s].n;
      uint newValue = updateuintByint(x_allowance_o_s_n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateMintOnInsertRecv_mint_r12(address p,uint n) private   returns (bool) {
      updateAllMintOnInsertMint_r3(n);
      updateTotalMintOnInsertMint_r16(p,n);
      emit Mint(p,n);
      return true;
      return false;
  }
  function updateOwnerOnInsertConstructor_r8() private    {
      address s = msg.sender;
      // Empty()
  }
  function updateTotalMintOnInsertMint_r16(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r22(p,delta0);
  }
  function updateAllBurnOnInsertBurn_r9(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r17(delta0);
  }
  function updateTransferOnInsertRecv_transfer_r2(address r,uint n) private   returns (bool) {
      updateTotalOutOnInsertTransfer_r18(s,n);
      updateTotalInOnInsertTransfer_r10(r,n);
      emit Transfer(s,r,n);
      return true;
      return false;
  }
  function updateBurnOnInsertControllerRedeem_r20(address p,uint n) private    {
      updateTotalBurnOnInsertBurn_r15(p,n);
      updateAllBurnOnInsertBurn_r9(n);
      emit Burn(p,n);
  }
}