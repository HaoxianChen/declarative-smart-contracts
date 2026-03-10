import "./cappedCrowdSale_udf.sol";
contract CappedCrowdSale is CappedCrowdsaleUDF {
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
  event BuyAfterFinalize();
  event Transfer(address from,address to,int amount);
  event InvalidTx();
  event Finalize();
  event IncreaseAllowance(address o,address s,int n);
  event Burn(address p,int amount);
  constructor(int c,int r) public {
    updateOwnerOnInsertConstructor_r22();
    updateRateOnInsertConstructor_r36(r);
    updateOnceFinalizeOnInsertConstructor_r42();
    updateCapOnInsertConstructor_r44(c);
    updateTotalSupplyOnInsertConstructor_r33();
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function getEnd() public view  returns (uint) {
      uint time = end.time;
      return time;
  }
  function getRaised() public view  returns (int) {
      int n = raised.n;
      return n;
  }
  function transfer(address from,address to,int amount) public    {
      bool r43 = updateTransferOnInsertRecv_transfer_r43(from,to,amount);
      if(r43==false) {
        revert("Rule condition failed");
      }
  }
  function getStart() public view  returns (uint) {
      uint time = start.time;
      return time;
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function increaseAllowance(address o,address s,int n) public    {
      bool r20 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r20(o,s,n);
      if(r20==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r48 = updateMintOnInsertRecv_mint_r48(p,amount);
      if(r48==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r34 = updateBurnOnInsertRecv_burn_r34(p,amount);
      if(r34==false) {
        revert("Rule condition failed");
      }
  }
  function getCap() public view  returns (int) {
      int n = cap.n;
      return n;
  }
  function getFinalized() public view  returns (bool) {
      bool b = finalized.b;
      return b;
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r3 = updateTransferFromOnInsertRecv_transferFrom_r3(from,to,spender,amount);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function buyToken(address p,int amount) public    {
      bool r17 = updateBuyTokenOnInsertRecv_buyToken_r17(p,amount);
      if(r17==false) {
        revert("Rule condition failed");
      }
  }
  function finalize() public    {
      bool r5 = updateFinalizeOnInsertRecv_finalize_r5();
      if(r5==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function updateBuyTokenOnInsertRecv_buyToken_r17(address p,int amount) private   returns (bool) {
      uint s_0 = start.time;
      uint t_0 = block.timestamp;
      uint e_1 = end.time;
      if(p!=address(0) && amount>0 && t_0>=s_0 && t_0<=e_1) {
        int tokens_2 = getTokenAmount(amount);
        if(tokens_2>0) {
          updateRaisedOnInsertBuyToken_r53(amount);
          updateMintOnInsertBuyToken_r26(p,amount);
          emit BuyToken(p,amount);
          return true;
        }
      }
      return false;
  }
  function updateTotalMintOnInsertMint_r50(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r35(p,delta0);
  }
  function updateAllBurnOnInsertBurn_r28(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r29(delta0);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r20(address o,address s,int n) private   returns (bool) {
      if(n>=0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r49(o,s,n);
        emit IncreaseAllowance(o,s,n);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertRecv_transfer_r43(address from,address to,int amount) private   returns (bool) {
      int m_1 = balanceOf[from].n;
      if(to!=address(0) && from!=address(0) && amount>0 && amount<=m_1) {
        updateTotalInOnInsertTransfer_r16(to,amount);
        updateTotalOutOnInsertTransfer_r9(from,amount);
        emit Transfer(from,to,amount);
        return true;
      }
      return false;
  }
  function updateMintOnInsertBuyToken_r26(address p,int v) private    {
      int tokens = getTokenAmount(v);
      updateTotalMintOnInsertMint_r50(p,tokens);
      updateAllMintOnInsertMint_r13(tokens);
      emit Mint(p,tokens);
  }
  function updateRateOnInsertConstructor_r36(int r) private    {
      if(r>0) {
        // Empty()
      }
  }
  function updateOwnerOnInsertConstructor_r22() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r3(address from,address to,address spender,int amount) private   returns (bool) {
      int m_1 = balanceOf[from].n;
      if(to!=address(0) && amount<=m_1 && spender!=address(0) && from!=address(0) && amount>0) {
        updateTransferOnInsertTransferFrom_r39(from,to,amount);
        updateSpentTotalOnInsertTransferFrom_r8(from,spender,amount);
        emit TransferFrom(from,to,spender,amount);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r35(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateBalanceOfOnIncrementTotalIn_r35(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateOnceFinalizeOnInsertConstructor_r42() private    {
      // Empty()
  }
  function updateAllowanceOnIncrementSpentTotal_r52(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateBalanceOfOnIncrementTotalBurn_r35(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r49(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r52(o,s,delta0);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r52(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateTotalSupplyOnIncrementAllBurn_r29(int b) private    {
      totalSupply.n -= b;
  }
  function updateAllMintOnInsertMint_r13(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r29(delta0);
  }
  function updateFinalizedOnInsertFinalize_r7() private    {
      finalized = FinalizedTuple(true,true);
  }
  function updateFinalizeOnInsertRecv_finalize_r5() private   returns (bool) {
      address msgSender = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[msgSender].n;
      if(o==msgSender && balanceOf_x1>0) {
        updateFinalizedOnInsertFinalize_r7();
        updateOnceFinalizeOnInsertFinalize_r6();
        emit Finalize();
        return true;
      }
      return false;
  }
  function updateMintOnInsertRecv_mint_r48(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(amount>0 && p!=address(0) && o_1==s_1) {
        updateTotalMintOnInsertMint_r50(p,amount);
        updateAllMintOnInsertMint_r13(amount);
        emit Mint(p,amount);
        return true;
      }
      return false;
  }
  function updateRaisedOnInsertBuyToken_r53(int n) private    {
      raised.n += n;
  }
  function updateOnceFinalizeOnInsertFinalize_r6() private    {
      // Empty()
  }
  function updateBalanceOfOnIncrementTotalMint_r35(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateTotalOutOnInsertTransfer_r9(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r35(p,delta0);
  }
  function updateCapOnInsertConstructor_r44(int c) private    {
      if(c>0) {
        cap = CapTuple(c,true);
      }
  }
  function updateTotalInOnInsertTransfer_r16(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r35(p,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalSupplyOnInsertConstructor_r33() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateBurnOnInsertRecv_burn_r34(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      int m_2 = balanceOf[p].n;
      if(amount>0 && p!=address(0) && o_1==s_1 && amount<=m_2) {
        updateAllBurnOnInsertBurn_r28(amount);
        updateTotalBurnOnInsertBurn_r18(p,amount);
        emit Burn(p,amount);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r29(int m) private    {
      totalSupply.n += m;
  }
  function updateSpentTotalOnInsertTransferFrom_r8(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r52(o,s,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTotalBurnOnInsertBurn_r18(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r35(p,delta0);
  }
  function updateTransferOnInsertTransferFrom_r39(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r9(o,n);
      updateTotalInOnInsertTransfer_r16(r,n);
      emit Transfer(o,r,n);
  }
}