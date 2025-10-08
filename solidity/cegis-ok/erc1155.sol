contract Erc1155 {
  struct BalanceOfTuple {
    uint n;
    bool _valid;
  }
  struct TotalSupplyTuple {
    uint n;
    bool _valid;
  }
  struct AllowanceTuple {
    uint n;
    bool _valid;
  }
  mapping(uint=>mapping(address=>mapping(address=>AllowanceTuple))) allowance;
  mapping(uint=>mapping(address=>BalanceOfTuple)) balanceOf;
  mapping(uint=>TotalSupplyTuple) totalSupply;
  event Burn(uint tokenId,address p,uint amount);
  event Transfer(uint tokenId,address from,address to,uint amount);
  event TransferFrom(uint tokenId,address from,address to,address spender,uint amount);
  event Mint(uint tokenId,address p,uint amount);
  event IncreaseAllowance(uint tokenId,address p,address s,uint n);
  constructor() public {
    updateOwnerOnInsertConstructor_r8();
  }
  function getAllowance(uint tokenId,address p,address s) public view  returns (uint) {
      uint n = allowance[tokenId][p][s].n;
      return n;
  }
  function transfer(uint tokenId,address to,uint amount) public    {
      bool r10 = updateTransferOnInsertRecv_transfer_r10(tokenId,to,amount);
      if(r10==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(uint tokenId,address from,address to,uint amount) public    {
      bool r2 = updateTransferFromOnInsertRecv_transferFrom_r2(tokenId,from,to,amount);
      if(r2==false) {
        revert("Rule condition failed");
      }
  }
  function mint(uint tokenId,address p,uint amount) public    {
      bool r9 = updateMintOnInsertRecv_mint_r9(tokenId,p,amount);
      if(r9==false) {
        revert("Rule condition failed");
      }
  }
  function burn(uint tokenId,address p,uint amount) public    {
      bool r11 = updateBurnOnInsertRecv_burn_r11(tokenId,p,amount);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function approve(uint tokenId,address s,uint n) public    {
      bool r5 = updateIncreaseAllowanceOnInsertRecv_approve_r5(tokenId,s,n);
      if(r5==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply(uint tokenId) public view  returns (uint) {
      uint n = totalSupply[tokenId].n;
      return n;
  }
  function getBalanceOf(uint tokenId,address p) public view  returns (uint) {
      uint n = balanceOf[tokenId][p].n;
      return n;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r2(uint t,address o,address r,uint n) private   returns (bool) {
      updateTransferOnInsertTransferFrom_r1(t,o,r,n);
      updateSpentTotalOnInsertTransferFrom_r18(t,o,s,n);
      emit TransferFrom(t,o,r,s,n);
      return true;
      return false;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r17(uint t,address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r0(t,o,s,delta0);
  }
  function updateAllowanceOnIncrementSpentTotal_r0(uint t,address o,address s,int l) private    {
      int _delta = int(-l);
      uint x_allowance_t_o_s_n = allowance[t][o][s].n;
      uint newValue = updateuintByint(x_allowance_t_o_s_n,_delta);
      allowance[t][o][s].n = newValue;
  }
  function updateTransferOnInsertTransferFrom_r1(uint t,address o,address r,uint n) private    {
      updateTotalInOnInsertTransfer_r16(t,r,n);
      updateTotalOutOnInsertTransfer_r13(t,o,n);
      emit Transfer(t,o,r,n);
  }
  function updateBalanceOfOnIncrementTotalOut_r4(uint t,address p,int o) private    {
      int _delta = int(-o);
      uint x_balanceOf_t_p_n = balanceOf[t][p].n;
      uint newValue = updateuintByint(x_balanceOf_t_p_n,_delta);
      balanceOf[t][p].n = newValue;
  }
  function updateTransferOnInsertRecv_transfer_r10(uint t,address r,uint n) private   returns (bool) {
      updateTotalInOnInsertTransfer_r16(t,r,n);
      updateTotalOutOnInsertTransfer_r13(t,s,n);
      emit Transfer(t,s,r,n);
      return true;
      return false;
  }
  function updateAllBurnOnInsertBurn_r6(uint t,uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r3(t,delta0);
  }
  function updateMintOnInsertRecv_mint_r9(uint t,address p,uint n) private   returns (bool) {
      updateAllMintOnInsertMint_r15(t,n);
      updateTotalMintOnInsertMint_r14(t,p,n);
      emit Mint(t,p,n);
      return true;
      return false;
  }
  function updateAllMintOnInsertMint_r15(uint t,uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r3(t,delta0);
  }
  function updateTotalInOnInsertTransfer_r16(uint t,address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r4(t,p,delta0);
  }
  function updateSpentTotalOnInsertTransferFrom_r18(uint t,address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r0(t,o,s,delta0);
  }
  function updateTotalSupplyOnIncrementAllBurn_r3(uint t,int b) private    {
      int _delta = int(-b);
      uint x_totalSupply_t_n = totalSupply[t].n;
      uint newValue = updateuintByint(x_totalSupply_t_n,_delta);
      totalSupply[t].n = newValue;
  }
  function updateTotalMintOnInsertMint_r14(uint t,address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r4(t,p,delta0);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r0(uint t,address o,address s,int m) private    {
      int _delta = int(m);
      uint x_allowance_t_o_s_n = allowance[t][o][s].n;
      uint newValue = updateuintByint(x_allowance_t_o_s_n,_delta);
      allowance[t][o][s].n = newValue;
  }
  function updateTotalOutOnInsertTransfer_r13(uint t,address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r4(t,p,delta0);
  }
  function updateIncreaseAllowanceOnInsertRecv_approve_r5(uint t,address s,uint n) private   returns (bool) {
      updateAllowanceTotalOnInsertIncreaseAllowance_r17(t,o,s,d);
      emit IncreaseAllowance(t,o,s,d);
      return true;
      return false;
  }
  function updateBalanceOfOnIncrementTotalBurn_r4(uint t,address p,int m) private    {
      int _delta = int(-m);
      uint x_balanceOf_t_p_n = balanceOf[t][p].n;
      uint newValue = updateuintByint(x_balanceOf_t_p_n,_delta);
      balanceOf[t][p].n = newValue;
  }
  function updateBalanceOfOnIncrementTotalIn_r4(uint t,address p,int i) private    {
      int _delta = int(i);
      uint x_balanceOf_t_p_n = balanceOf[t][p].n;
      uint newValue = updateuintByint(x_balanceOf_t_p_n,_delta);
      balanceOf[t][p].n = newValue;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateBurnOnInsertRecv_burn_r11(uint t,address p,uint n) private   returns (bool) {
      updateTotalBurnOnInsertBurn_r7(t,p,n);
      updateAllBurnOnInsertBurn_r6(t,n);
      emit Burn(t,p,n);
      return true;
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r3(uint t,int m) private    {
      int _delta = int(m);
      uint x_totalSupply_t_n = totalSupply[t].n;
      uint newValue = updateuintByint(x_totalSupply_t_n,_delta);
      totalSupply[t].n = newValue;
  }
  function updateBalanceOfOnIncrementTotalMint_r4(uint t,address p,int n) private    {
      int _delta = int(n);
      uint x_balanceOf_t_p_n = balanceOf[t][p].n;
      uint newValue = updateuintByint(x_balanceOf_t_p_n,_delta);
      balanceOf[t][p].n = newValue;
  }
  function updateTotalBurnOnInsertBurn_r7(uint t,address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r4(t,p,delta0);
  }
  function updateOwnerOnInsertConstructor_r8() private    {
      address s = msg.sender;
      // Empty()
  }
}