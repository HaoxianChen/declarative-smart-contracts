contract Shib {
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
  event IncreaseAllowance(address p,address s,uint n);
  event BurnFrom(address p,address from,uint n);
  event Transfer(address from,address to,uint amount);
  constructor() public {
    updateOwnerOnInsertConstructor_r8();
    updateTotalBalancesOnInsertConstructor_r22();
    updateTotalSupplyOnInsertConstructor_r3();
  }
  function burnFrom(address p,uint n) public    {
      bool r4 = updateBurnFromOnInsertRecv_burnFrom_r4(p,n);
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (uint) {
      uint n = totalSupply.n;
      return n;
  }
  function burn(address p,uint amount) public    {
      bool r19 = updateBurnOnInsertRecv_burn_r19(p,amount);
      if(r19==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,uint amount) public    {
      bool r20 = updateTransferFromOnInsertRecv_transferFrom_r20(from,to,amount);
      if(r20==false) {
        revert("Rule condition failed");
      }
  }
  function approve(address s,uint n) public    {
      bool r11 = updateIncreaseAllowanceOnInsertRecv_approve_r11(s,n);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (uint) {
      uint n = allowance[p][s].n;
      return n;
  }
  function transfer(address to,uint amount) public    {
      bool r1 = updateTransferOnInsertRecv_transfer_r1(to,amount);
      if(r1==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,uint amount) public    {
      bool r2 = updateMintOnInsertRecv_mint_r2(p,amount);
      if(r2==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (uint) {
      uint n = balanceOf[p].n;
      return n;
  }
  function updateTotalMintOnInsertMint_r13(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r7(p,delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r20(address o,address r,uint n) private   returns (bool) {
      updateSpentTotalOnInsertTransferFrom_r18(o,s,n);
      updateTransferOnInsertTransferFrom_r0(o,r,n);
      emit TransferFrom(o,r,s,n);
      return true;
      return false;
  }
  function updateBalanceOfOnIncrementTotalMint_r7(address p,int n) private    {
      int _delta = int(n);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateSpentTotalOnInsertTransferFrom_r18(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r21(o,s,delta0);
  }
  function updateIncreaseAllowanceOnInsertRecv_approve_r11(address s,uint n) private   returns (bool) {
      updateAllowanceTotalOnInsertIncreaseAllowance_r24(o,s,d);
      emit IncreaseAllowance(o,s,d);
      return true;
      return false;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r21(address o,address s,int m) private    {
      int _delta = int(m);
      uint x_allowance_o_s_n = allowance[o][s].n;
      uint newValue = updateuintByint(x_allowance_o_s_n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateTotalSupplyOnIncrementAllBurn_r14(int b) private    {
      int _delta = int(-b);
      uint x_totalSupply__n = totalSupply.n;
      uint newValue = updateuintByint(x_totalSupply__n,_delta);
      totalSupply.n = newValue;
  }
  function updateOwnerOnInsertConstructor_r8() private    {
      address s = msg.sender;
      // Empty()
  }
  function updateTransferOnInsertRecv_transfer_r1(address r,uint n) private   returns (bool) {
      updateTotalOutOnInsertTransfer_r16(s,n);
      updateTotalInOnInsertTransfer_r10(r,n);
      emit Transfer(s,r,n);
      return true;
      return false;
  }
  function updateTotalInOnInsertTransfer_r10(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r7(p,delta0);
  }
  function updateAllBurnOnInsertBurn_r9(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r14(delta0);
  }
  function updateTotalSupplyOnInsertConstructor_r3() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateBurnOnInsertBurnFrom_r15(address p,uint n) private    {
      updateTotalBurnOnInsertBurn_r12(p,n);
      updateAllBurnOnInsertBurn_r9(n);
      emit Burn(p,n);
  }
  function updateTotalBalancesOnInsertConstructor_r22() private    {
      // Empty()
  }
  function updateMintOnInsertRecv_mint_r2(address p,uint n) private   returns (bool) {
      updateTotalMintOnInsertMint_r13(p,n);
      updateAllMintOnInsertMint_r5(n);
      emit Mint(p,n);
      return true;
      return false;
  }
  function updateBalanceOfOnIncrementTotalIn_r7(address p,int i) private    {
      int _delta = int(i);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateBalanceOfOnIncrementTotalOut_r7(address p,int o) private    {
      int _delta = int(-o);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r24(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r21(o,s,delta0);
  }
  function updateAllMintOnInsertMint_r5(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r14(delta0);
  }
  function updateTotalSupplyOnIncrementAllMint_r14(int m) private    {
      int _delta = int(m);
      uint x_totalSupply__n = totalSupply.n;
      uint newValue = updateuintByint(x_totalSupply__n,_delta);
      totalSupply.n = newValue;
  }
  function updateTransferFromOnInsertBurnFrom_r23(address s,address p,uint n) private    {
      updateSpentTotalOnInsertTransferFrom_r18(s,address(0),n);
      updateTransferOnInsertTransferFrom_r0(s,p,n);
      emit TransferFrom(s,p,address(0),n);
  }
  function updateBurnFromOnInsertRecv_burnFrom_r4(address p,uint n) private   returns (bool) {
      updateTransferFromOnInsertBurnFrom_r23(s,p,n);
      updateBurnOnInsertBurnFrom_r15(p,n);
      emit BurnFrom(s,p,n);
      return true;
      return false;
  }
  function updateTotalOutOnInsertTransfer_r16(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r7(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalBurn_r7(address p,int m) private    {
      int _delta = int(-m);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateTransferOnInsertTransferFrom_r0(address o,address r,uint n) private    {
      updateTotalOutOnInsertTransfer_r16(o,n);
      updateTotalInOnInsertTransfer_r10(r,n);
      emit Transfer(o,r,n);
  }
  function updateTotalBurnOnInsertBurn_r12(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r7(p,delta0);
  }
  function updateAllowanceOnIncrementSpentTotal_r21(address o,address s,int l) private    {
      int _delta = int(-l);
      uint x_allowance_o_s_n = allowance[o][s].n;
      uint newValue = updateuintByint(x_allowance_o_s_n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateBurnOnInsertRecv_burn_r19(address p,uint n) private   returns (bool) {
      updateTotalBurnOnInsertBurn_r12(p,n);
      updateAllBurnOnInsertBurn_r9(n);
      emit Burn(p,n);
      return true;
      return false;
  }
}