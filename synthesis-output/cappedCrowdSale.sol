import "./cappedCrowdSale_udf.sol";
contract CappedCrowdSale is CappedCrowdsaleUDF {
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
  struct CapTuple {
    int n;
    bool _valid;
  }
  struct FinalizedTuple {
    bool b;
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
  struct EndTuple {
    uint time;
    bool _valid;
  }
  struct RaisedTuple {
    int n;
    bool _valid;
  }
  CapTuple cap;
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  EndTuple end;
  StartTuple start;
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalSupplyTuple totalSupply;
  RaisedTuple raised;
  FinalizedTuple finalized;
  OwnerTuple owner;
  event Mint(address p,int amount);
  event TransferFrom(address from,address to,address spender,int amount);
  event BuyToken(address p,int amount);
  event FinalizeNotByOwner();
  event Transfer(address from,address to,int amount);
  event Finalize();
  event IncreaseAllowance(address o,address s,int n);
  event Burn(address p,int amount);
  constructor(int c,int r) public {
    updateRateOnInsertConstructor_r28(r);
    updateCapOnInsertConstructor_r8(c);
    updateOnceFinalizeOnInsertConstructor_r7();
    updateOwnerOnInsertConstructor_r2();
    updateTotalSupplyOnInsertConstructor_r25();
    updateOnceBuyAfterFinalizeOnInsertConstructor_r18();
  }
  function burn(address p,int amount) public    {
      bool r14 = updateBurnOnInsertRecv_burn_r14(p,amount);
      if(r14==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function getEnd() public view  returns (uint) {
      uint time = end.time;
      return time;
  }
  function transfer(address from,address to,int amount) public    {
      bool r16 = updateTransferOnInsertRecv_transfer_r16(from,to,amount);
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function finalize() public    {
      bool r22 = updateFinalizeOnInsertRecv_finalize_r22();
      if(r22==false) {
        revert("Rule condition failed");
      }
  }
  function getRaised() public view  returns (int) {
      int n = raised.n;
      return n;
  }
  function increaseAllowance(address o,address s,int n) public    {
      bool r33 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r33(o,s,n);
      if(r33==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r13 = updateTransferFromOnInsertRecv_transferFrom_r13(from,to,spender,amount);
      if(r13==false) {
        revert("Rule condition failed");
      }
  }
  function buyToken(address p,int amount) public    {
      bool r32 = updateBuyTokenOnInsertRecv_buyToken_r32(p,amount);
      if(r32==false) {
        revert("Rule condition failed");
      }
  }
  function getStart() public view  returns (uint) {
      uint time = start.time;
      return time;
  }
  function mint(address p,int amount) public    {
      bool r11 = updateMintOnInsertRecv_mint_r11(p,amount);
      if(r11==false) {
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
  function getCap() public view  returns (int) {
      int n = cap.n;
      return n;
  }
  function getFinalized() public view  returns (bool) {
      bool b = finalized.b;
      return b;
  }
  function updateMintOnInsertBuyToken_r20(address p,int v) private    {
      int tokens = getTokenAmount(v);
      updateAllMintOnInsertMint_r0(tokens);
      updateTotalMintOnInsertMint_r9(p,tokens);
      emit Mint(p,tokens);
  }
  function updateTotalInOnInsertTransfer_r17(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r6(p,delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r31(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r34(o,s,delta0);
  }
  function updateTransferOnInsertRecv_transfer_r16(address from,address to,int amount) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[from].n;
      if(amount>0 && amount<balanceOf_x1_1) {
        updateTotalInOnInsertTransfer_r17(to,amount);
        updateTotalOutOnInsertTransfer_r12(from,amount);
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
  function updateOwnerOnInsertConstructor_r2() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateMintOnInsertRecv_mint_r11(address p,int amount) private   returns (bool) {
      if(amount>0) {
        updateTotalMintOnInsertMint_r9(p,amount);
        updateAllMintOnInsertMint_r0(amount);
        emit Mint(p,amount);
        return true;
      }
      return false;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateBalanceOfOnIncrementTotalMint_r6(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r34(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateSpentTotalOnInsertTransferFrom_r10(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r34(o,s,delta0);
  }
  function updateFinalizeOnInsertRecv_finalize_r22() private   returns (bool) {
      uint end_time = end.time;
      address s = msg.sender;
      address o = owner.p;
      if(s==o && 0!=end_time) {
        updateFinalizedOnInsertFinalize_r5();
        emit Finalize();
        return true;
      }
      return false;
  }
  function updateTotalOutOnInsertTransfer_r12(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r6(p,delta0);
  }
  function updateTotalSupplyOnIncrementAllMint_r24(int m) private    {
      totalSupply.n += m;
  }
  function updateRaisedOnInsertBuyToken_r35(int n) private    {
      raised.n += n;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r33(address o,address s,int n) private   returns (bool) {
      if(n>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r31(o,s,n);
        emit IncreaseAllowance(o,s,n);
        return true;
      }
      return false;
  }
  function updateOnceFinalizeOnInsertConstructor_r7() private    {
      // Empty()
  }
  function updateCapOnInsertConstructor_r8(int c) private    {
      if(c>0) {
        cap = CapTuple(c,true);
      }
  }
  function updateBuyTokenOnInsertRecv_buyToken_r32(address p,int amount) private   returns (bool) {
      updateRaisedOnInsertBuyToken_r35(amount);
      updateMintOnInsertBuyToken_r20(p,amount);
      emit BuyToken(p,amount);
      return true;
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r24(int b) private    {
      totalSupply.n -= b;
  }
  function updateBalanceOfOnIncrementTotalIn_r6(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateOnceBuyAfterFinalizeOnInsertConstructor_r18() private    {
      // Empty()
  }
  function updateAllowanceOnIncrementSpentTotal_r34(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTotalMintOnInsertMint_r9(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r6(p,delta0);
  }
  function updateTotalBurnOnInsertBurn_r19(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r6(p,delta0);
  }
  function updateAllBurnOnInsertBurn_r23(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r24(delta0);
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r24(delta0);
  }
  function updateBalanceOfOnIncrementTotalBurn_r6(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateBalanceOfOnIncrementTotalOut_r6(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateBurnOnInsertRecv_burn_r14(address p,int amount) private   returns (bool) {
      int balanceOf_x1 = balanceOf[p].n;
      if(amount<=balanceOf_x1) {
        updateAllBurnOnInsertBurn_r23(amount);
        updateTotalBurnOnInsertBurn_r19(p,amount);
        emit Burn(p,amount);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r25() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateFinalizedOnInsertFinalize_r5() private    {
      finalized = FinalizedTuple(true,true);
  }
  function updateRateOnInsertConstructor_r28(int r) private    {
      if(r>0) {
        // Empty()
      }
  }
  function updateTransferOnInsertTransferFrom_r27(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r17(r,n);
      updateTotalOutOnInsertTransfer_r12(o,n);
      emit Transfer(o,r,n);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r13(address from,address to,address spender,int amount) private   returns (bool) {
      int balanceOf_x1_2 = balanceOf[from].n;
      int allowance_x2_1 = allowance[from][spender].n;
      if(amount>0 && amount<=allowance_x2_1 && amount<=balanceOf_x1_2) {
        updateTransferOnInsertTransferFrom_r27(from,to,amount);
        updateSpentTotalOnInsertTransferFrom_r10(from,spender,amount);
        emit TransferFrom(from,to,spender,amount);
        return true;
      }
      return false;
  }
}