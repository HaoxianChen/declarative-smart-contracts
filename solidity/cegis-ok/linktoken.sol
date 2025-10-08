contract Linktoken {
  struct TotalSupplyTuple {
    uint n;
    bool _valid;
  }
  struct BalanceOfTuple {
    uint n;
    bool _valid;
  }
  struct AllowanceTuple {
    uint n;
    bool _valid;
  }
  TotalSupplyTuple totalSupply;
  mapping(address=>BalanceOfTuple) balanceOf;
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  event TransferFrom(address from,address to,address spender,uint amount);
  event Burn(address p,uint amount);
  event Mint(address p,uint amount);
  event DecreaseAllowance(address p,address s,uint n);
  event IncreaseAllowance(address p,address s,uint n);
  event Transfer(address from,address to,uint amount);
  constructor(uint n) public {
    updateBalanceOfOnInsertConstructor_r6(n);
    updateTotalInOnInsertConstructor_r12(n);
    updateTotalSupplyOnInsertConstructor_r25(n);
    updateOwnerOnInsertConstructor_r8();
    updateTotalMintOnInsertConstructor_r19(n);
    updateTotalBalancesOnInsertConstructor_r27(n);
  }
  function mint(address p,uint amount) public    {
      bool r1 = updateMintOnInsertRecv_mint_r1(p,amount);
      if(r1==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address to,uint amount) public    {
      bool r0 = updateTransferOnInsertRecv_transfer_r0(to,amount);
      if(r0==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,uint amount) public    {
      bool r24 = updateBurnOnInsertRecv_burn_r24(p,amount);
      if(r24==false) {
        revert("Rule condition failed");
      }
  }
  function decreaseApproval(address p,uint n) public    {
      bool r21 = updateDecreaseAllowanceOnInsertRecv_decreaseApproval_r21(p,n);
      if(r21==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (uint) {
      uint n = totalSupply.n;
      return n;
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
  function increaseApproval(address p,uint n) public    {
      bool r2 = updateIncreaseAllowanceOnInsertRecv_increaseApproval_r2(p,n);
      if(r2==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,uint amount) public    {
      bool r9 = updateTransferFromOnInsertRecv_transferFrom_r9(from,to,amount);
      if(r9==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (uint) {
      uint n = balanceOf[p].n;
      return n;
  }
  function updateBalanceOfOnIncrementTotalIn_r5(address p,int i) private    {
      int _delta = int(i);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateMintOnInsertRecv_mint_r1(address p,uint n) private   returns (bool) {
      updateAllMintOnInsertMint_r3(n);
      updateTotalMintOnInsertMint_r16(p,n);
      emit Mint(p,n);
      return true;
      return false;
  }
  function updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r7(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementDecreaseAllowanceTotal_r26(o,s,delta0);
  }
  function updateBalanceOfOnInsertConstructor_r6(uint n) private    {
      address p = msg.sender;
      balanceOf[p] = BalanceOfTuple(n,true);
  }
  function updateSpentTotalOnInsertTransferFrom_r23(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r26(o,s,delta0);
  }
  function validRecipient(address p) private view  returns (bool) {
      address t = address(this);
      if(p!=t && p!=address(0)) {
        return true;
      }
      return false;
  }
  function updateTransferOnInsertRecv_transfer_r0(address r,uint n) private   returns (bool) {
      updateTotalOutOnInsertTransfer_r20(s,n);
      updateTotalInOnInsertTransfer_r11(r,n);
      emit Transfer(s,r,n);
      return true;
      return false;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseApproval_r2(address s,uint n) private   returns (bool) {
      updateAllowanceTotalOnInsertIncreaseAllowance_r28(o,s,n);
      emit IncreaseAllowance(o,s,n);
      return true;
      return false;
  }
  function updateTotalBalancesOnInsertConstructor_r27(uint n) private    {
      // Empty()
  }
  function updateAllBurnOnInsertBurn_r10(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r17(delta0);
  }
  function updateTotalMintOnInsertMint_r16(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r5(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalBurn_r5(address p,int m) private    {
      int _delta = int(-m);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateTotalMintOnInsertConstructor_r19(uint n) private    {
      address s = msg.sender;
      // Empty()
  }
  function updateDecreaseAllowanceOnInsertRecv_decreaseApproval_r21(address s,uint n) private   returns (bool) {
      updateDecreaseAllowanceTotalOnInsertDecreaseAllowance_r7(o,s,n);
      emit DecreaseAllowance(o,s,n);
      return true;
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r25(uint n) private    {
      totalSupply = TotalSupplyTuple(n,true);
  }
  function updateTotalOutOnInsertTransfer_r20(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r5(p,delta0);
  }
  function updateBurnOnInsertRecv_burn_r24(address p,uint n) private   returns (bool) {
      updateAllBurnOnInsertBurn_r10(n);
      updateTotalBurnOnInsertBurn_r15(p,n);
      emit Burn(p,n);
      return true;
      return false;
  }
  function updateTotalInOnInsertConstructor_r12(uint n) private    {
      address s = msg.sender;
      // Empty()
  }
  function updateTotalBurnOnInsertBurn_r15(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r5(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalMint_r5(address p,int n) private    {
      int _delta = int(n);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
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
  function updateTotalSupplyOnIncrementAllBurn_r17(int b) private    {
      int _delta = int(-b);
      uint x_totalSupply__n = totalSupply.n;
      uint newValue = updateuintByint(x_totalSupply__n,_delta);
      totalSupply.n = newValue;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r9(address o,address r,uint n) private   returns (bool) {
      updateTransferOnInsertTransferFrom_r13(o,r,n);
      updateSpentTotalOnInsertTransferFrom_r23(o,s,n);
      emit TransferFrom(o,r,s,n);
      return true;
      return false;
  }
  function updateIncreaseAllowanceOnInsertRecv_approve_r14(address s,uint n) private   returns (bool) {
      updateAllowanceTotalOnInsertIncreaseAllowance_r28(o,s,d);
      emit IncreaseAllowance(o,s,d);
      return true;
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r5(address p,int o) private    {
      int _delta = int(-o);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r26(address o,address s,int m) private    {
      int _delta = int(m);
      uint x_allowance_o_s_n = allowance[o][s].n;
      uint newValue = updateuintByint(x_allowance_o_s_n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateTransferOnInsertTransferFrom_r13(address o,address r,uint n) private    {
      updateTotalInOnInsertTransfer_r11(r,n);
      updateTotalOutOnInsertTransfer_r20(o,n);
      emit Transfer(o,r,n);
  }
  function updateOwnerOnInsertConstructor_r8() private    {
      address s = msg.sender;
      // Empty()
  }
  function updateTotalInOnInsertTransfer_r11(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r5(p,delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r28(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r26(o,s,delta0);
  }
  function updateAllowanceOnIncrementSpentTotal_r26(address o,address s,int l) private    {
      int _delta = int(-l);
      uint x_allowance_o_s_n = allowance[o][s].n;
      uint newValue = updateuintByint(x_allowance_o_s_n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateAllowanceOnIncrementDecreaseAllowanceTotal_r26(address o,address s,int d) private    {
      int _delta = int(-d);
      uint x_allowance_o_s_n = allowance[o][s].n;
      uint newValue = updateuintByint(x_allowance_o_s_n,_delta);
      allowance[o][s].n = newValue;
  }
}