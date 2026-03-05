import "./theta_udf.sol";
contract Theta is ThetaUDF {
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
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
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
    updateTotalBalancesOnInsertConstructor_r22();
    updateOwnerOnInsertConstructor_r1();
    updateTotalSupplyOnInsertConstructor_r14();
    updateUnlockTimeOnInsertConstructor_r28(t);
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function allowPrecirculation(address p) public    {
      bool r0 = updateAllowPrecirculationOnInsertRecv_allowPrecirculation_r0(p);
      if(r0==false) {
        revert("Rule condition failed");
      }
  }
  function disallowPrecirculation(address p) public    {
      bool r31 = updateDisallowPrecirculationOnInsertRecv_disallowPrecirculation_r31(p);
      if(r31==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r30 = updateTransferFromOnInsertRecv_transferFrom_r30(from,to,spender,amount);
      if(r30==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function transfer(address from,address to,int amount) public    {
      bool r13 = updateTransferOnInsertRecv_transfer_r13(from,to,amount);
      if(r13==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function increaseAllowance(address p,address s,int d) public    {
      bool r15 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r15(p,s,d);
      if(r15==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r34 = updateMintOnInsertRecv_mint_r34(p,amount);
      if(r34==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r18 = updateBurnOnInsertRecv_burn_r18(p,amount);
      if(r18==false) {
        revert("Rule condition failed");
      }
  }
  function updateTransferOnInsertRecv_transfer_r13(address from,address to,int amount) private   returns (bool) {
      int m_1 = balanceOf[s].n;
      bool ok_2 = canTransfer(s,r);
      if(r!=address(0) && n<=m_1 && s!=address(0) && ok_2!=false && n>0) {
        updateTotalInOnInsertTransfer_r17(r,n);
        updateTotalOutOnInsertTransfer_r43(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r8(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateTotalInOnInsertTransfer_r17(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r6(p,delta0);
  }
  function updateTransferOnInsertTransferFrom_r32(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r17(r,n);
      updateTotalOutOnInsertTransfer_r43(o,n);
      emit Transfer(o,r,n);
  }
  function updateTotalSupplyOnIncrementAllMint_r26(int m) private    {
      totalSupply.n += m;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalBurnOnInsertBurn_r19(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r6(p,delta0);
  }
  function updateUnlockTimeOnInsertConstructor_r28(uint t) private    {
      // Empty()
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateBalanceOfOnIncrementTotalMint_r6(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateTotalMintOnInsertMint_r36(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r6(p,delta0);
  }
  function updateDisallowPrecirculationOnInsertRecv_disallowPrecirculation_r31(address p) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        emit DisallowPrecirculation(p);
        return true;
      }
      return false;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r38(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r8(o,s,delta0);
  }
  function updateSpentTotalOnInsertTransferFrom_r40(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r8(o,s,delta0);
  }
  function updateTotalBalancesOnInsertConstructor_r22() private    {
      // Empty()
  }
  function updateAllBurnOnInsertBurn_r25(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r26(delta0);
  }
  function updateOwnerOnInsertConstructor_r1() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateAllowPrecirculationOnInsertRecv_allowPrecirculation_r0(address p) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        emit AllowPrecirculation(p);
        return true;
      }
      return false;
  }
  function updateTotalOutOnInsertTransfer_r43(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r6(p,delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r30(address from,address to,address spender,int amount) private   returns (bool) {
      int k_3 = allowance[o][sp].n;
      int m_1 = balanceOf[o].n;
      bool ok_0 = canTransfer(o,r);
      if(r!=address(0) && sp!=address(0) && n<=m_1 && ok_0!=false && n<=k_3 && o!=address(0)) {
        updateTransferOnInsertTransferFrom_r32(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r40(o,sp,n);
        emit TransferFrom(o,r,sp,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r14() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateBurnOnInsertRecv_burn_r18(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      int m_2 = balanceOf[p].n;
      if(p!=address(0) && o_1==s_1 && n<=m_2) {
        updateAllBurnOnInsertBurn_r25(n);
        updateTotalBurnOnInsertBurn_r19(p,n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r15(address p,address s,int d) private   returns (bool) {
      if(d>=0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r38(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateMintOnInsertRecv_mint_r34(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(p!=address(0) && o_1==s_1) {
        updateTotalMintOnInsertMint_r36(p,n);
        updateAllMintOnInsertMint_r12(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateAllMintOnInsertMint_r12(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r26(delta0);
  }
  function updateTotalSupplyOnIncrementAllBurn_r26(int b) private    {
      totalSupply.n -= b;
  }
  function updateBalanceOfOnIncrementTotalIn_r6(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateAllowanceOnIncrementSpentTotal_r8(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateBalanceOfOnIncrementTotalBurn_r6(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateBalanceOfOnIncrementTotalOut_r6(address p,int o) private    {
      balanceOf[p].n -= o;
  }
}