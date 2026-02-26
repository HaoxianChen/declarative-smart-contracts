contract Theta {
  struct AllowanceTuple {
    int n;
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
  struct UnlockTimeTuple {
    uint t;
    bool _valid;
  }
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct PrecirculatedTuple {
    bool b;
    bool _valid;
  }
  UnlockTimeTuple unlockTime;
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  mapping(address=>PrecirculatedTuple) precirculated;
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalSupplyTuple totalSupply;
  OwnerTuple owner;
  event Mint(address p,int amount);
  event Burn(address p,int amount);
  event AllowPrecirculation(address p);
  event IncreaseAllowance(address p,address s,int d);
  event Transfer(address from,address to,int amount);
  event InvalidTx();
  event DisallowPrecirculation(address p);
  event TransferFrom(address from,address to,address spender,int amount);
  constructor(uint t) public {
    updateUnlockTimeOnInsertConstructor_r27(t);
    updateTotalBalancesOnInsertConstructor_r2();
    updateOwnerOnInsertConstructor_r19();
    updateTotalSupplyOnInsertConstructor_r11();
  }
  function mint(address p,int amount) public    {
      bool r32 = updateMintOnInsertRecv_mint_r32(p,amount);
      if(r32==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r44 = updateTransferFromOnInsertRecv_transferFrom_r44(from,to,spender,amount);
      if(r44==false) {
        revert("Rule condition failed");
      }
  }
  function allowPrecirculation(address p) public    {
      bool r17 = updateAllowPrecirculationOnInsertRecv_allowPrecirculation_r17(p);
      if(r17==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function transfer(address from,address to,int amount) public    {
      bool r20 = updateTransferOnInsertRecv_transfer_r20(from,to,amount);
      if(r20==false) {
        revert("Rule condition failed");
      }
  }
  function disallowPrecirculation(address p) public    {
      bool r29 = updateDisallowPrecirculationOnInsertRecv_disallowPrecirculation_r29(p);
      if(r29==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int d) public    {
      bool r12 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r12(p,s,d);
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r15 = updateBurnOnInsertRecv_burn_r15(p,amount);
      if(r15==false) {
        revert("Rule condition failed");
      }
  }
  function updateTotalInOnInsertTransfer_r14(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r5(p,delta0);
  }
  function updateOwnerOnInsertConstructor_r19() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTotalBalancesOnInsertConstructor_r2() private    {
      // Empty()
  }
  function updateTransferFromOnInsertRecv_transferFrom_r44(address from,address to,address spender,int amount) private   returns (bool) {
      int k_2 = allowance[o][sp].n;
      int m_0 = balanceOf[o].n;
      if(r!=address(0) && sp!=address(0) && n<=k_2 && n<=m_0 && o!=address(0)) {
        updateSpentTotalOnInsertTransferFrom_r38(o,sp,n);
        updateTransferOnInsertTransferFrom_r30(o,r,n);
        emit TransferFrom(o,r,sp,n);
        return true;
      }
      return false;
  }
  function updateUnlockTimeOnInsertConstructor_r27(uint t) private    {
      unlockTime = UnlockTimeTuple(t,true);
  }
  function updateTotalMintOnInsertMint_r34(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r5(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalOut_r5(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateTransferOnInsertRecv_transfer_r20(address from,address to,int amount) private   returns (bool) {
      int m_1 = balanceOf[s].n;
      if(r!=address(0) && s!=address(0) && n<=m_1) {
        updateTotalOutOnInsertTransfer_r40(s,n);
        updateTotalInOnInsertTransfer_r14(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateDisallowPrecirculationOnInsertRecv_disallowPrecirculation_r29(address p) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        emit DisallowPrecirculation(p);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalIn_r5(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r7(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateTotalSupplyOnIncrementAllMint_r25(int m) private    {
      totalSupply.n += m;
  }
  function updateTransferOnInsertTransferFrom_r30(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r40(o,n);
      updateTotalInOnInsertTransfer_r14(r,n);
      emit Transfer(o,r,n);
  }
  function updateBalanceOfOnIncrementTotalBurn_r5(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateTotalSupplyOnInsertConstructor_r11() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateSpentTotalOnInsertTransferFrom_r38(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r7(o,s,delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r36(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r7(o,s,delta0);
  }
  function canTransfer(address p,address q) private view  returns (bool) {
      uint ut = unlockTime.t;
      uint t = block.timestamp;
      if(t>=ut) {
        return true;
      }
      if(true==precirculated[q].b) {
        if(true==precirculated[p].b) {
          return true;
        }
      }
      return false;
  }
  function updateAllMintOnInsertMint_r10(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r25(delta0);
  }
  function updateAllBurnOnInsertBurn_r24(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r25(delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateBalanceOfOnIncrementTotalMint_r5(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateTotalBurnOnInsertBurn_r16(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r5(p,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTotalOutOnInsertTransfer_r40(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r5(p,delta0);
  }
  function updateAllowanceOnIncrementSpentTotal_r7(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r12(address p,address s,int d) private   returns (bool) {
      if(d>=0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r36(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateAllowPrecirculationOnInsertRecv_allowPrecirculation_r17(address p) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        emit AllowPrecirculation(p);
        return true;
      }
      return false;
  }
  function updateMintOnInsertRecv_mint_r32(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(p!=address(0) && o_1==s_1) {
        updateAllMintOnInsertMint_r10(n);
        updateTotalMintOnInsertMint_r34(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateBurnOnInsertRecv_burn_r15(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      int m_2 = balanceOf[p].n;
      if(p!=address(0) && o_1==s_1 && n<=m_2) {
        updateAllBurnOnInsertBurn_r24(n);
        updateTotalBurnOnInsertBurn_r16(p,n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r25(int b) private    {
      totalSupply.n -= b;
  }
}