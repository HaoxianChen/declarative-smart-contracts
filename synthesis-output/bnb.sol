import "./bnb_udf.sol";
contract Bnb is BnbUDF {
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
  struct AllowanceTuple {
    int n;
    bool _valid;
  }
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalSupplyTuple totalSupply;
  OwnerTuple owner;
  event Unfreeze(address p,int n);
  event Mint(address p,int amount);
  event TransferFrom(address from,address to,address spender,int amount);
  event WithdrawEther(address p,int amount);
  event Freeze(address p,int n);
  event Transfer(address from,address to,int amount);
  event InvalidTx();
  event IncreaseAllowance(address o,address s,int n);
  event Burn(address p,int amount);
  constructor(int n) public {
    updateOwnerOnInsertConstructor_r23();
  }
  function freeze(address p,int n) public    {
      bool r18 = updateFreezeOnInsertRecv_freeze_r18(p,n);
      if(r18==false) {
        revert("Rule condition failed");
      }
  }
  function withdrawEther(address p,int amount) public    {
      bool r7 = updateWithdrawEtherOnInsertRecv_withdrawEther_r7(p,amount);
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function transfer(address from,address to,int amount) public    {
      bool r31 = updateTransferOnInsertRecv_transfer_r31(from,to,amount);
      if(r31==false) {
        revert("Rule condition failed");
      }
  }
  function unfreeze(address p,int n) public    {
      bool r32 = updateUnfreezeOnInsertRecv_unfreeze_r32(p,n);
      if(r32==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r22 = updateMintOnInsertRecv_mint_r22(p,amount);
      if(r22==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function increaseAllowance(address o,address s,int n) public    {
      bool r1 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r1(o,s,n);
      if(r1==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r3 = updateBurnOnInsertRecv_burn_r3(p,amount);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r36 = updateTransferFromOnInsertRecv_transferFrom_r36(from,to,spender,amount);
      if(r36==false) {
        revert("Rule condition failed");
      }
  }
  function updateTotalInOnInsertTransfer_r15(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r0(p,delta0);
  }
  function updateTotalBurnOnInsertBurn_r16(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r0(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalMint_r0(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateFreezeOfOnIncrementTotalFreeze_r19(address p,int f) private    {
      int delta0 = int(f);
      updateBalanceOfOnIncrementFreezeOf_r0(p,delta0);
  }
  function updateTransferOnInsertTransferFrom_r27(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r15(r,n);
      updateTotalOutOnInsertTransfer_r10(o,n);
      emit Transfer(o,r,n);
  }
  function updateTotalSupplyOnIncrementAllBurn_r26(int b) private    {
      totalSupply.n -= b;
  }
  function updateOwnerOnInsertConstructor_r23() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateBalanceOfOnIncrementTotalIn_r0(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r8(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r9(o,s,delta0);
  }
  function updateBurnOnInsertRecv_burn_r3(address p,int amount) private   returns (bool) {
      address s_0 = msg.sender;
      address o_0 = owner.p;
      int m_1 = balanceOf[p].n;
      if(o_0==s_0 && amount<=m_1) {
        updateTotalBurnOnInsertBurn_r16(p,amount);
        updateAllBurnOnInsertBurn_r25(amount);
        emit Burn(p,amount);
        return true;
      }
      return false;
  }
  function updateAllMintOnInsertMint_r13(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r26(delta0);
  }
  function updateBalanceOfOnIncrementTotalBurn_r0(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r9(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r36(address from,address to,address spender,int amount) private   returns (bool) {
      int allowance_x2 = allowance[from][spender].n;
      int m_1 = balanceOf[from].n;
      if(amount>0 && amount<=m_1 && amount<=allowance_x2) {
        updateTransferOnInsertTransferFrom_r27(from,to,amount);
        updateSpentTotalOnInsertTransferFrom_r38(from,spender,amount);
        emit TransferFrom(from,to,spender,amount);
        return true;
      }
      return false;
  }
  function updateTotalOutOnInsertTransfer_r10(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r0(p,delta0);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r1(address o,address s,int n) private   returns (bool) {
      if(n>=0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r8(o,s,n);
        emit IncreaseAllowance(o,s,n);
        return true;
      }
      return false;
  }
  function updateUnfreezeOnInsertRecv_unfreeze_r32(address p,int n) private   returns (bool) {
      if(n>=0) {
        updateTotalUnfreezeOnInsertUnfreeze_r17(p,n);
        emit Unfreeze(p,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementSpentTotal_r9(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateMintOnInsertRecv_mint_r22(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && amount>=0) {
        updateTotalMintOnInsertMint_r34(p,amount);
        updateAllMintOnInsertMint_r13(amount);
        emit Mint(p,amount);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r26(int m) private    {
      totalSupply.n += m;
  }
  function updateWithdrawEtherOnInsertRecv_withdrawEther_r7(address p,int amount) private   returns (bool) {
      bool success = sendEther(p,amount);
      if(success!=false) {
        emit WithdrawEther(p,amount);
        return true;
      }
      return false;
  }
  function updateTotalMintOnInsertMint_r34(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r0(p,delta0);
  }
  function updateBalanceOfOnIncrementFreezeOf_r0(address p,int f) private    {
      balanceOf[p].n -= f;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateBalanceOfOnIncrementTotalOut_r0(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateTotalUnfreezeOnInsertUnfreeze_r17(address p,int n) private    {
      int delta0 = int(n);
      updateFreezeOfOnIncrementTotalUnfreeze_r19(p,delta0);
  }
  function updateSpentTotalOnInsertTransferFrom_r38(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r9(o,s,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateFreezeOfOnIncrementTotalUnfreeze_r19(address p,int u) private    {
      int delta0 = int(-u);
      updateBalanceOfOnIncrementFreezeOf_r0(p,delta0);
  }
  function updateFreezeOnInsertRecv_freeze_r18(address p,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(n<=balanceOf_x1) {
        updateTotalFreezeOnInsertFreeze_r6(p,n);
        emit Freeze(p,n);
        return true;
      }
      return false;
  }
  function updateAllBurnOnInsertBurn_r25(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r26(delta0);
  }
  function updateTotalFreezeOnInsertFreeze_r6(address p,int n) private    {
      int delta0 = int(n);
      updateFreezeOfOnIncrementTotalFreeze_r19(p,delta0);
  }
  function updateTransferOnInsertRecv_transfer_r31(address from,address to,int amount) private   returns (bool) {
      int m_1 = balanceOf[from].n;
      if(to!=address(0) && from!=address(0) && amount>0 && amount<=m_1) {
        updateTotalInOnInsertTransfer_r15(to,amount);
        updateTotalOutOnInsertTransfer_r10(from,amount);
        emit Transfer(from,to,amount);
        return true;
      }
      return false;
  }
}