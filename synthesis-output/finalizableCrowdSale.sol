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
    updateTotalSupplyOnInsertConstructor_r3();
    updateOwnerOnInsertConstructor_r27();
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
  function mint(address p,int amount) public    {
      bool r29 = updateMintOnInsertRecv_mint_r29(p,amount);
      if(r29==false) {
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
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r26 = updateTransferFromOnInsertRecv_transferFrom_r26(from,to,spender,amount);
      if(r26==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r28 = updateBurnOnInsertRecv_burn_r28(p,amount);
      if(r28==false) {
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
  function updateTransferOnInsertTransferFrom_r20(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r9(o,n);
      updateTotalInOnInsertTransfer_r12(r,n);
      emit Transfer(o,r,n);
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
  function updateAllowanceTotalOnInsertIncreaseAllowance_r7(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r30(o,s,delta0);
  }
  function updateTotalOutOnInsertTransfer_r9(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r6(p,delta0);
  }
  function updateOwnerOnInsertConstructor_r27() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r19(delta0);
  }
  function updateTotalSupplyOnIncrementAllBurn_r19(int b) private    {
      totalSupply.n -= b;
  }
  function updateMintOnInsertBuyToken_r1(address p,int m) private    {
      int r = rate.r;
      int n = m*r;
      updateAllMintOnInsertMint_r0(n);
      updateTotalMintOnInsertMint_r25(p,n);
      emit Mint(p,n);
  }
  function updateTotalMintOnInsertMint_r25(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r6(p,delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r26(address o,address r,address s,int n) private   returns (bool) {
      int balanceOf_x1_2 = balanceOf[o].n;
      int allowance_x2_1 = allowance[o][s].n;
      if(n>=0 && n<=allowance_x2_1 && n<=balanceOf_x1_2) {
        updateTransferOnInsertTransferFrom_r20(o,r,n);
        updateSpentTotalOnInsertTransferFrom_r8(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalIn_r6(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTotalBurnOnInsertBurn_r14(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r6(p,delta0);
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
  function updateAllBurnOnInsertBurn_r18(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r19(delta0);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r30(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateBalanceOfOnIncrementTotalMint_r6(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateTransferOnInsertRecv_transfer_r10(address s,address r,int n) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[s].n;
      if(n>=0 && n<=balanceOf_x1_1) {
        updateTotalOutOnInsertTransfer_r9(s,n);
        updateTotalInOnInsertTransfer_r12(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r3() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateSpentTotalOnInsertTransferFrom_r8(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r30(o,s,delta0);
  }
  function updateBalanceOfOnIncrementTotalOut_r6(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateBalanceOfOnIncrementTotalBurn_r6(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r24(address o,address s,int d) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r7(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementSpentTotal_r30(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateBuyTokenOnInsertRecv_buyToken_r15(address p,int m) private   returns (bool) {
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
  function updateMintOnInsertRecv_mint_r29(address p,int n) private   returns (bool) {
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
  function updateBurnOnInsertRecv_burn_r28(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && n<=balanceOf_x1) {
        updateAllBurnOnInsertBurn_r18(n);
        updateTotalBurnOnInsertBurn_r14(p,n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateTotalInOnInsertTransfer_r12(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r6(p,delta0);
  }
}