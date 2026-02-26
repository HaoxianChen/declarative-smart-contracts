contract FinalizableCrowdSale {
  struct AllowanceTuple {
    int n;
    bool _valid;
  }
  struct StartTuple {
    uint time;
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
  struct EndTuple {
    uint time;
    bool _valid;
  }
  struct RateTuple {
    int r;
    bool _valid;
  }
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  EndTuple end;
  StartTuple start;
  RateTuple rate;
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalSupplyTuple totalSupply;
  OwnerTuple owner;
  event Mint(address p,int amount);
  event TransferFrom(address from,address to,address spender,int amount);
  event BuyToken(address p,int amount);
  event Burn(address p,int amount);
  event IncreaseAllowance(address p,address s,int n);
  event Transfer(address from,address to,int amount);
  event InvalidTx();
  event Finalize();
  constructor() public {
    updateTotalSupplyOnInsertConstructor_r4();
    updateOwnerOnInsertConstructor_r26();
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r3 = updateTransferFromOnInsertRecv_transferFrom_r3(from,to,spender,amount);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r24 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r24(p,s,n);
      if(r24==false) {
        revert("Rule condition failed");
      }
  }
  function finalize() public    {
      bool r22 = updateFinalizeOnInsertRecv_finalize_r22();
      if(r22==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r29 = updateTransferOnInsertRecv_transfer_r29(from,to,amount);
      if(r29==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r27 = updateMintOnInsertRecv_mint_r27(p,amount);
      if(r27==false) {
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
  function buyToken(address p,int amount) public    {
      bool r15 = updateBuyTokenOnInsertRecv_buyToken_r15(p,amount);
      if(r15==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function burn(address p,int amount) public    {
      bool r10 = updateBurnOnInsertRecv_burn_r10(p,amount);
      if(r10==false) {
        revert("Rule condition failed");
      }
  }
  function updateTotalInOnInsertTransfer_r12(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r7(p,delta0);
  }
  function updateFinalizeOnInsertRecv_finalize_r22() private   returns (bool) {
      address s_0 = msg.sender;
      address o_0 = owner.p;
      uint t_1 = block.timestamp;
      uint e_1 = end.time;
      if(o_0==s_0 && t_1>=e_1) {
        emit Finalize();
        return true;
      }
      return false;
  }
  function updateBurnOnInsertRecv_burn_r10(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n<0) {
        updateAllBurnOnInsertBurn_r18(n);
        updateTotalBurnOnInsertBurn_r14(p,n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r3(address from,address to,address spender,int amount) private   returns (bool) {
      if(0==n) {
        updateTransferOnInsertTransferFrom_r20(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r9(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r4() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateSpentTotalOnInsertTransferFrom_r9(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r28(o,s,delta0);
  }
  function updateTotalSupplyOnIncrementAllBurn_r19(int b) private    {
      totalSupply.n -= b;
  }
  function updateBalanceOfOnIncrementTotalIn_r7(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateMintOnInsertBuyToken_r1(address p,int m) private    {
      int r = rate.r;
      int n = m*r;
      updateAllMintOnInsertMint_r0(n);
      updateTotalMintOnInsertMint_r25(p,n);
      emit Mint(p,n);
  }
  function updateTransferOnInsertRecv_transfer_r29(address from,address to,int amount) private   returns (bool) {
      if(0==n) {
        updateTotalOutOnInsertTransfer_r30(s,n);
        updateTotalInOnInsertTransfer_r12(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r28(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateOwnerOnInsertConstructor_r26() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateAllowanceOnIncrementSpentTotal_r28(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateBuyTokenOnInsertRecv_buyToken_r15(address p,int amount) private   returns (bool) {
      uint s_0 = start.time;
      uint t_0 = block.timestamp;
      uint t_1 = block.timestamp;
      uint e_1 = end.time;
      if(t_0>=s_0 && t_1<=e_1) {
        updateMintOnInsertBuyToken_r1(p,n);
        emit BuyToken(p,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalBurn_r7(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r19(delta0);
  }
  function updateMintOnInsertRecv_mint_r27(address p,int amount) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>0) {
        updateAllMintOnInsertMint_r0(n);
        updateTotalMintOnInsertMint_r25(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r19(int m) private    {
      totalSupply.n += m;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r24(address p,address s,int n) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r8(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateTotalMintOnInsertMint_r25(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r7(p,delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r8(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r28(o,s,delta0);
  }
  function updateTotalBurnOnInsertBurn_r14(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r7(p,delta0);
  }
  function updateAllBurnOnInsertBurn_r18(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r19(delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTotalOutOnInsertTransfer_r30(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r7(p,delta0);
  }
  function updateTransferOnInsertTransferFrom_r20(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r30(o,n);
      updateTotalInOnInsertTransfer_r12(r,n);
      emit Transfer(o,r,n);
  }
  function updateBalanceOfOnIncrementTotalOut_r7(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateBalanceOfOnIncrementTotalMint_r7(address p,int n) private    {
      balanceOf[p].n += n;
  }
}