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
  event Transfer(address from,address to,int amount);
  event Mint(address p,int amount);
  event TransferFrom(address from,address to,address spender,int amount);
  event UnauthorizedMint();
  event UnauthorizedBurn();
  event WithdrawEther(address p,int amount);
  event UnauthorizedWithdrawEther();
  event Freeze(address p,int n);
  event IncreaseAllowance(address o,address s,int n);
  event Burn(address p,int amount);
  constructor(int n) public {
    updateOwnerOnInsertConstructor_r21();
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function transfer(address from,address to,int amount) public    {
      bool r10 = updateTransferOnInsertRecv_transfer_r10(from,to,amount);
      if(r10==false) {
        revert("Rule condition failed");
      }
  }
  function unfreeze(address p,int n) public    {
      bool r26 = updateUnfreezeOnInsertRecv_unfreeze_r26(p,n);
      if(r26==false) {
        revert("Rule condition failed");
      }
  }
  function freeze(address p,int n) public    {
      bool r17 = updateFreezeOnInsertRecv_freeze_r17(p,n);
      if(r17==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r11 = updateTransferFromOnInsertRecv_transferFrom_r11(from,to,spender,amount);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function withdrawEther(address p,int amount) public    {
      bool r20 = updateWithdrawEtherOnInsertRecv_withdrawEther_r20(p,amount);
      if(r20==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function burn(address p,int amount) public    {
      bool r25 = updateBurnOnInsertRecv_burn_r25(p,amount);
      if(r25==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address o,address s,int n) public    {
      bool r29 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r29(o,s,n);
      if(r29==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r30 = updateMintOnInsertRecv_mint_r30(p,amount);
      if(r30==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function updateTotalSupplyOnIncrementAllMint_r24(int m) private    {
      totalSupply.n += m;
  }
  function updateTransferOnInsertTransferFrom_r4(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r9(o,n);
      updateTotalInOnInsertTransfer_r13(r,n);
      emit Transfer(o,r,n);
  }
  function updateTotalSupplyOnIncrementAllBurn_r24(int b) private    {
      totalSupply.n -= b;
  }
  function updateBalanceOfOnIncrementTotalMint_r19(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateTransferOnInsertRecv_transfer_r10(address from,address to,int amount) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[from].n;
      if(amount>0 && amount<=balanceOf_x1_1) {
        updateTotalInOnInsertTransfer_r13(to,amount);
        updateTotalOutOnInsertTransfer_r9(from,amount);
        emit Transfer(from,to,amount);
        return true;
      }
      return false;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalMintOnInsertMint_r8(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r19(p,delta0);
  }
  function updateAllBurnOnInsertBurn_r23(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r24(delta0);
  }
  function updateTotalOutOnInsertTransfer_r9(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r19(p,delta0);
  }
  function updateTotalBurnOnInsertBurn_r14(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r19(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalIn_r19(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r24(delta0);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r31(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateBalanceOfOnIncrementTotalOut_r19(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r11(address from,address to,address spender,int amount) private   returns (bool) {
      int balanceOf_x1_2 = balanceOf[from].n;
      int allowance_x2_1 = allowance[from][spender].n;
      if(amount>0 && amount<allowance_x2_1 && amount<=balanceOf_x1_2) {
        updateSpentTotalOnInsertTransferFrom_r28(from,spender,amount);
        updateTransferOnInsertTransferFrom_r4(from,to,amount);
        emit TransferFrom(from,to,spender,amount);
        return true;
      }
      return false;
  }
  function updateOwnerOnInsertConstructor_r21() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r7(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r31(o,s,delta0);
  }
  function updateBurnOnInsertRecv_burn_r25(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(s==o && amount<=balanceOf_x1) {
        updateTotalBurnOnInsertBurn_r14(p,amount);
        updateAllBurnOnInsertBurn_r23(amount);
        emit Burn(p,amount);
        return true;
      }
      return false;
  }
  function updateWithdrawEtherOnInsertRecv_withdrawEther_r20(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(s==o) {
        emit WithdrawEther(p,amount);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalBurn_r19(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateUnfreezeOnInsertRecv_unfreeze_r26(address p,int n) private   returns (bool) {
      if(n>0) {
        updateTotalUnfreezeOnInsertUnfreeze_r15(p,n);
        emit Unfreeze(p,n);
        return true;
      }
      return false;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateAllowanceOnIncrementSpentTotal_r31(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateMintOnInsertRecv_mint_r30(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(s==o && amount>0) {
        updateAllMintOnInsertMint_r0(amount);
        updateTotalMintOnInsertMint_r8(p,amount);
        emit Mint(p,amount);
        return true;
      }
      return false;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r29(address o,address s,int n) private   returns (bool) {
      if(n>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r7(o,s,n);
        emit IncreaseAllowance(o,s,n);
        return true;
      }
      return false;
  }
  function updateFreezeOfOnIncrementTotalUnfreeze_r18(address p,int u) private    {
      int delta0 = int(-u);
      updateBalanceOfOnIncrementFreezeOf_r19(p,delta0);
  }
  function updateTotalUnfreezeOnInsertUnfreeze_r15(address p,int n) private    {
      int delta0 = int(n);
      updateFreezeOfOnIncrementTotalUnfreeze_r18(p,delta0);
  }
  function updateTotalFreezeOnInsertFreeze_r3(address p,int n) private    {
      int delta0 = int(n);
      updateFreezeOfOnIncrementTotalFreeze_r18(p,delta0);
  }
  function updateBalanceOfOnIncrementFreezeOf_r19(address p,int f) private    {
      balanceOf[p].n -= f;
  }
  function updateFreezeOnInsertRecv_freeze_r17(address p,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(n<=balanceOf_x1) {
        updateTotalFreezeOnInsertFreeze_r3(p,n);
        emit Freeze(p,n);
        return true;
      }
      return false;
  }
  function updateSpentTotalOnInsertTransferFrom_r28(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r31(o,s,delta0);
  }
  function updateTotalInOnInsertTransfer_r13(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r19(p,delta0);
  }
  function updateFreezeOfOnIncrementTotalFreeze_r18(address p,int f) private    {
      int delta0 = int(f);
      updateBalanceOfOnIncrementFreezeOf_r19(p,delta0);
  }
}