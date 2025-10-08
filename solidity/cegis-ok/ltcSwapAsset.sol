contract LtcSwapAsset {
  struct TotalSupplyTuple {
    uint n;
    bool _valid;
  }
  struct NewOwnerTuple {
    address p;
    bool _valid;
  }
  struct OldOwnerTuple {
    address p;
    bool _valid;
  }
  struct AllowanceTuple {
    uint n;
    bool _valid;
  }
  struct EffectiveTimeTuple {
    uint t;
    bool _valid;
  }
  struct BalanceOfTuple {
    uint n;
    bool _valid;
  }
  EffectiveTimeTuple effectiveTime;
  TotalSupplyTuple totalSupply;
  NewOwnerTuple newOwner;
  OldOwnerTuple oldOwner;
  mapping(address=>BalanceOfTuple) balanceOf;
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  event TransferFrom(address from,address to,address spender,uint amount);
  event Burn(address p,uint amount);
  event Mint(address p,uint amount);
  event SwapOwner(address p,address q,uint t);
  event IncreaseAllowance(address p,address s,uint n);
  event Transfer(address from,address to,uint amount);
  constructor() public {
    updateTotalBalancesOnInsertConstructor_r28();
    updateNewOwnerOnInsertConstructor_r8();
    updateEffectiveTimeOnInsertConstructor_r11();
    updateTotalSupplyOnInsertConstructor_r14();
  }
  function mint(address p,uint amount) public    {
      bool r1 = updateMintOnInsertRecv_mint_r1(p,amount);
      if(r1==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (uint) {
      uint n = allowance[p][s].n;
      return n;
  }
  function getBalanceOf(address p) public view  returns (uint) {
      uint n = balanceOf[p].n;
      return n;
  }
  function approve(address s,uint n) public    {
      bool r15 = updateIncreaseAllowanceOnInsertRecv_approve_r15(s,n);
      if(r15==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (uint) {
      uint n = totalSupply.n;
      return n;
  }
  function transferFrom(address from,address to,uint amount) public    {
      bool r23 = updateTransferFromOnInsertRecv_transferFrom_r23(from,to,amount);
      if(r23==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address to,uint amount) public    {
      bool r12 = updateTransferOnInsertRecv_transfer_r12(to,amount);
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,uint amount) public    {
      bool r22 = updateBurnOnInsertRecv_burn_r22(p,amount);
      if(r22==false) {
        revert("Rule condition failed");
      }
  }
  function swapOwner(address p,address q,uint d) public    {
      bool r9 = updateSwapOwnerOnInsertRecv_swapOwner_r9(p,q,d);
      if(r9==false) {
        revert("Rule condition failed");
      }
  }
  function updateIncreaseAllowanceOnInsertRecv_approve_r15(address s,uint n) private   returns (bool) {
      updateAllowanceTotalOnInsertIncreaseAllowance_r27(o,s,d);
      emit IncreaseAllowance(o,s,d);
      return true;
      return false;
  }
  function updateSpentTotalOnInsertTransferFrom_r21(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r24(o,s,delta0);
  }
  function updateTotalInOnInsertTransfer_r10(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r6(p,delta0);
  }
  function updateTotalBalancesOnInsertConstructor_r28() private    {
      // Empty()
  }
  function updateTransferOnInsertRecv_transfer_r12(address r,uint n) private   returns (bool) {
      updateTotalOutOnInsertTransfer_r19(s,n);
      updateTotalInOnInsertTransfer_r10(r,n);
      emit Transfer(s,r,n);
      return true;
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r18(int b) private    {
      int _delta = int(-b);
      uint x_totalSupply__n = totalSupply.n;
      uint newValue = updateuintByint(x_totalSupply__n,_delta);
      totalSupply.n = newValue;
  }
  function updateBurnOnInsertRecv_burn_r22(address p,uint n) private   returns (bool) {
      updateTotalBurnOnInsertBurn_r16(p,n);
      updateAllBurnOnInsertBurn_r25(n);
      emit Burn(p,n);
      return true;
      return false;
  }
  function updateTotalOutOnInsertTransfer_r19(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r6(p,delta0);
  }
  function owner(address p) private view  returns (bool) {
      if(p==oldOwner.p) {
        uint t2 = effectiveTime.t;
        uint t = block.timestamp;
        if(t<t2) {
          return true;
        }
      }
      if(p==newOwner.p) {
        uint t2 = effectiveTime.t;
        uint t = block.timestamp;
        if(t>=t2) {
          return true;
        }
      }
      return false;
  }
  function updateAllBurnOnInsertBurn_r25(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r18(delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r23(address o,address r,uint n) private   returns (bool) {
      updateTransferOnInsertTransferFrom_r0(o,r,n);
      updateSpentTotalOnInsertTransferFrom_r21(o,s,n);
      emit TransferFrom(o,r,s,n);
      return true;
      return false;
  }
  function updateEffectiveTimeOnInsertConstructor_r11() private    {
      uint t = block.timestamp;
      effectiveTime = EffectiveTimeTuple(t,true);
  }
  function updateSwapOwnerOnInsertRecv_swapOwner_r9(address p,address q,uint d) private   returns (bool) {
      emit SwapOwner(p,q,t);
      return true;
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r18(int m) private    {
      int _delta = int(m);
      uint x_totalSupply__n = totalSupply.n;
      uint newValue = updateuintByint(x_totalSupply__n,_delta);
      totalSupply.n = newValue;
  }
  function updateTotalBurnOnInsertBurn_r16(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r6(p,delta0);
  }
  function updateMintOnInsertRecv_mint_r1(address p,uint n) private   returns (bool) {
      updateAllMintOnInsertMint_r3(n);
      updateTotalMintOnInsertMint_r17(p,n);
      emit Mint(p,n);
      return true;
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r6(address p,int o) private    {
      int _delta = int(-o);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateTransferOnInsertTransferFrom_r0(address o,address r,uint n) private    {
      updateTotalOutOnInsertTransfer_r19(o,n);
      updateTotalInOnInsertTransfer_r10(r,n);
      emit Transfer(o,r,n);
  }
  function updateTotalSupplyOnInsertConstructor_r14() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateNewOwnerOnInsertConstructor_r8() private    {
      address s = msg.sender;
      newOwner = NewOwnerTuple(s,true);
  }
  function updateBalanceOfOnIncrementTotalIn_r6(address p,int i) private    {
      int _delta = int(i);
      uint x_balanceOf_p_n = balanceOf[p].n;
      uint newValue = updateuintByint(x_balanceOf_p_n,_delta);
      balanceOf[p].n = newValue;
  }
  function updateAllMintOnInsertMint_r3(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r18(delta0);
  }
  function updateTotalMintOnInsertMint_r17(address p,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r6(p,delta0);
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
  function updateAllowanceOnIncrementAllowanceTotal_r24(address o,address s,int m) private    {
      int _delta = int(m);
      uint x_allowance_o_s_n = allowance[o][s].n;
      uint newValue = updateuintByint(x_allowance_o_s_n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateAllowanceOnIncrementSpentTotal_r24(address o,address s,int l) private    {
      int _delta = int(-l);
      uint x_allowance_o_s_n = allowance[o][s].n;
      uint newValue = updateuintByint(x_allowance_o_s_n,_delta);
      allowance[o][s].n = newValue;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r27(address o,address s,uint n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r24(o,s,delta0);
  }
}