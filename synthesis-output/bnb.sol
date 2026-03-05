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
    updateOwnerOnInsertConstructor_r24();
  }
  function withdrawEther(address p,int amount) public    {
      bool r7 = updateWithdrawEtherOnInsertRecv_withdrawEther_r7(p,amount);
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function freeze(address p,int n) public    {
      bool r19 = updateFreezeOnInsertRecv_freeze_r19(p,n);
      if(r19==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function mint(address p,int amount) public    {
      bool r23 = updateMintOnInsertRecv_mint_r23(p,amount);
      if(r23==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r10 = updateTransferFromOnInsertRecv_transferFrom_r10(from,to,spender,amount);
      if(r10==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function burn(address p,int amount) public    {
      bool r3 = updateBurnOnInsertRecv_burn_r3(p,amount);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r32 = updateTransferOnInsertRecv_transfer_r32(from,to,amount);
      if(r32==false) {
        revert("Rule condition failed");
      }
  }
  function unfreeze(address p,int n) public    {
      bool r33 = updateUnfreezeOnInsertRecv_unfreeze_r33(p,n);
      if(r33==false) {
        revert("Rule condition failed");
      }
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
  function updateBalanceOfOnIncrementTotalMint_r0(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateAllowanceOnIncrementSpentTotal_r9(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTotalOutOnInsertTransfer_r11(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r0(p,delta0);
  }
  function updateTotalFreezeOnInsertFreeze_r6(address p,int n) private    {
      int delta0 = int(n);
      updateFreezeOfOnIncrementTotalFreeze_r20(p,delta0);
  }
  function updateTotalBurnOnInsertBurn_r17(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r0(p,delta0);
  }
  function updateTotalSupplyOnIncrementAllMint_r27(int m) private    {
      totalSupply.n += m;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r1(address o,address s,int n) private   returns (bool) {
      if(d>=0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r8(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r8(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r9(o,s,delta0);
  }
  function updateTransferOnInsertTransferFrom_r28(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r16(r,n);
      updateTotalOutOnInsertTransfer_r11(o,n);
      emit Transfer(o,r,n);
  }
  function updateTotalSupplyOnIncrementAllBurn_r27(int b) private    {
      totalSupply.n -= b;
  }
  function updateBalanceOfOnIncrementTotalBurn_r0(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateUnfreezeOnInsertRecv_unfreeze_r33(address p,int n) private   returns (bool) {
      if(n>=0) {
        updateTotalUnfreezeOnInsertUnfreeze_r18(p,n);
        emit Unfreeze(p,n);
        return true;
      }
      return false;
  }
  function updateMintOnInsertRecv_mint_r23(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>=0) {
        updateAllMintOnInsertMint_r14(n);
        updateTotalMintOnInsertMint_r35(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateTotalUnfreezeOnInsertUnfreeze_r18(address p,int n) private    {
      int delta0 = int(n);
      updateFreezeOfOnIncrementTotalUnfreeze_r20(p,delta0);
  }
  function updateFreezeOfOnIncrementTotalUnfreeze_r20(address p,int u) private    {
      int delta0 = int(-u);
      updateBalanceOfOnIncrementFreezeOf_r0(p,delta0);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r9(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateBurnOnInsertRecv_burn_r3(address p,int amount) private   returns (bool) {
      address s_0 = msg.sender;
      address o_0 = owner.p;
      int m_1 = balanceOf[p].n;
      if(o_0==s_0 && n<=m_1) {
        updateTotalBurnOnInsertBurn_r17(p,n);
        updateAllBurnOnInsertBurn_r26(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r10(address from,address to,address spender,int amount) private   returns (bool) {
      int balanceOf_x1 = balanceOf[r].n;
      int m_1 = balanceOf[o].n;
      if(n>0 && n<=m_1 && n<=balanceOf_x1) {
        updateTransferOnInsertTransferFrom_r28(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r38(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateAllBurnOnInsertBurn_r26(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r27(delta0);
  }
  function updateTotalInOnInsertTransfer_r16(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r0(p,delta0);
  }
  function updateFreezeOnInsertRecv_freeze_r19(address p,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(n<=balanceOf_x1) {
        updateTotalFreezeOnInsertFreeze_r6(p,n);
        emit Freeze(p,n);
        return true;
      }
      return false;
  }
  function updateOwnerOnInsertConstructor_r24() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
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
  function updateFreezeOfOnIncrementTotalFreeze_r20(address p,int f) private    {
      int delta0 = int(f);
      updateBalanceOfOnIncrementFreezeOf_r0(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalOut_r0(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateWithdrawEtherOnInsertRecv_withdrawEther_r7(address p,int amount) private   returns (bool) {
      bool success = sendEther(p,n);
      if(success!=false) {
        emit WithdrawEther(p,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalIn_r0(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateAllMintOnInsertMint_r14(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r27(delta0);
  }
  function updateTransferOnInsertRecv_transfer_r32(address from,address to,int amount) private   returns (bool) {
      int m_1 = balanceOf[s].n;
      if(r!=address(0) && n>0 && s!=address(0) && n<=m_1) {
        updateTotalOutOnInsertTransfer_r11(s,n);
        updateTotalInOnInsertTransfer_r16(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateTotalMintOnInsertMint_r35(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r0(p,delta0);
  }
  function updateSpentTotalOnInsertTransferFrom_r38(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r9(o,s,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
}