contract Erc20 {
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
  event Transfer(address from,address to,uint amount);
  constructor() public {
    updateTotalBalancesOnInsertConstructor_r21();
    updateOwnerOnInsertConstructor_r7();
    updateTotalSupplyOnInsertConstructor_r3();
  }
  function transferFrom(address from,address to,uint amount) public    {
      bool r18 = updateTransferFromOnInsertRecv_transferFrom_r18(from,to,amount);
      if(r18==false) {
        revert("Rule condition failed");
      }
  }
  function approve(address s,uint n) public    {
      bool r10 = updateIncreaseAllowanceOnInsertRecv_approve_r10(s,n);
      if(r10==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (uint) {
      uint n = totalSupply.n;
      return n;
  }
  function burn(address p,uint amount) public    {
      bool r17 = updateBurnOnInsertRecv_burn_r17(p,amount);
      if(r17==false) {
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
  function updateMintOnInsertRecv_mint_r2(address p,uint n) private   returns (bool) {
      updateAllMintOnInsertMint_r4(n);
      updateTotalMintOnInsertMint_r12(p,n);
      emit Mint(p,n);
      return true;
      return false;
  }
  function updateAllBurnOnInsertBurn_r8(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r13(delta0);
  }
  function updateTotalBalancesOnInsertConstructor_r21() private    {
      // Empty()
  }
  function updateOwnerOnInsertConstructor_r7() private    {
      address s = msg.sender;
      // Empty()
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r20(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r19(o,s,delta0);
  }
  function updateSpentTotalOnInsertTransferFrom_r16(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r19(o,s,delta0);
  }
  function updateAllowanceOnIncrementSpentTotal_r19(address o,address s,int l) private    {
      int _delta = int(-l);
      uint x_allowance_o_s_n = allowance[o][s].n;
      uint newValue = updateuintByint(x_allowance_o_s_n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateTotalInOnInsertTransfer_r9(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r6(p,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTransferOnInsertRecv_transfer_r1(address r,uint n) private   returns (bool) {
      updateTotalOutOnInsertTransfer_r14(s,n);
      updateTotalInOnInsertTransfer_r9(r,n);
      emit Transfer(s,r,n);
      return true;
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r6(address p,int o) private    {
      int _delta = int(-o);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateTotalOutOnInsertTransfer_r14(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r6(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalIn_r6(address p,int i) private    {
      int _delta = int(i);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateTotalMintOnInsertMint_r12(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r6(p,delta0);
  }
  function updateBurnOnInsertRecv_burn_r17(address p,uint n) private   returns (bool) {
      updateAllBurnOnInsertBurn_r8(n);
      updateTotalBurnOnInsertBurn_r11(p,n);
      emit Burn(p,n);
      return true;
      return false;
  }
  function updateIncreaseAllowanceOnInsertRecv_approve_r10(address s,uint n) private   returns (bool) {
      updateAllowanceTotalOnInsertIncreaseAllowance_r20(o,s,d);
      emit IncreaseAllowance(o,s,d);
      return true;
      return false;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r19(address o,address s,int m) private    {
      int _delta = int(m);
      uint x_allowance_o_s_n = allowance[o][s].n;
      uint newValue = updateuintByint(x_allowance_o_s_n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateAllMintOnInsertMint_r4(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r13(delta0);
  }
  function updateTransferOnInsertTransferFrom_r0(address o,address r,uint n) private    {
      updateTotalOutOnInsertTransfer_r14(o,n);
      updateTotalInOnInsertTransfer_r9(r,n);
      emit Transfer(o,r,n);
  }
  function updateTotalSupplyOnIncrementAllBurn_r13(int b) private    {
      int _delta = int(-b);
      uint x_totalSupply__n = totalSupply.n;
      uint newValue = updateuintByint(x_totalSupply__n,_delta);
      totalSupply.n = newValue;
  }
  function updateTotalSupplyOnIncrementAllMint_r13(int m) private    {
      int _delta = int(m);
      uint x_totalSupply__n = totalSupply.n;
      uint newValue = updateuintByint(x_totalSupply__n,_delta);
      totalSupply.n = newValue;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r18(address o,address r,uint n) private   returns (bool) {
      updateSpentTotalOnInsertTransferFrom_r16(o,s,n);
      updateTransferOnInsertTransferFrom_r0(o,r,n);
      emit TransferFrom(o,r,s,n);
      return true;
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r3() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateTotalBurnOnInsertBurn_r11(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r6(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalBurn_r6(address p,int m) private    {
      int _delta = int(-m);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateBalanceOfOnIncrementTotalMint_r6(address p,int n) private    {
      int _delta = int(n);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
}