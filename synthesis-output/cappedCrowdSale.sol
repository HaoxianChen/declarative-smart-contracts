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
    updateOwnerOnInsertConstructor_r19();
    updateOnceFinalizeOnInsertConstructor_r41();
    updateCapOnInsertConstructor_r43(c);
    updateTotalSupplyOnInsertConstructor_r30();
    updateRateOnInsertConstructor_r35(r);
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function getEnd() public view  returns (uint) {
      uint time = end.time;
      return time;
  }
  function burn(address p,int amount) public    {
      bool r33 = updateBurnOnInsertRecv_burn_r33(p,amount);
      if(r33==false) {
        revert("Rule condition failed");
      }
  }
  function getRaised() public view  returns (int) {
      int n = raised.n;
      return n;
  }
  function getStart() public view  returns (uint) {
      uint time = start.time;
      return time;
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
  function increaseAllowance(address o,address s,int n) public    {
      bool r49 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r49(o,s,n);
      if(r49==false) {
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
  function transfer(address from,address to,int amount) public    {
      bool r42 = updateTransferOnInsertRecv_transfer_r42(from,to,amount);
      if(r42==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r47 = updateMintOnInsertRecv_mint_r47(p,amount);
      if(r47==false) {
        revert("Rule condition failed");
      }
  }
  function finalize() public    {
      bool r32 = updateFinalizeOnInsertRecv_finalize_r32();
      if(r32==false) {
        revert("Rule condition failed");
      }
  }
  function updateOnceFinalizeOnInsertConstructor_r41() private    {
      // Empty()
  }
  function updateTotalOutOnInsertTransfer_r7(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r34(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalBurn_r34(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r49(address o,address s,int n) private   returns (bool) {
      updateAllowanceTotalOnInsertIncreaseAllowance_r48(o,s,d);
      emit IncreaseAllowance(o,s,d);
      return true;
      return false;
  }
  function updateFinalizeOnInsertRecv_finalize_r32() private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        updateFinalizedOnInsertFinalize_r5();
        updateOnceFinalizeOnInsertFinalize_r31();
        emit Finalize();
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalMint_r34(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateTotalInOnInsertTransfer_r14(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r34(p,delta0);
  }
  function updateCapOnInsertConstructor_r43(int c) private    {
      if(c>0) {
        cap = CapTuple(c,true);
      }
  }
  function updateMintOnInsertBuyToken_r23(address p,int v) private    {
      int tokens = getTokenAmount(v);
      updateAllMintOnInsertMint_r11(tokens);
      updateTotalMintOnInsertMint_r50(p,tokens);
      emit Mint(p,tokens);
  }
  function updateAllMintOnInsertMint_r11(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r26(delta0);
  }
  function updateBalanceOfOnIncrementTotalIn_r34(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTotalBurnOnInsertBurn_r16(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r34(p,delta0);
  }
  function updateAllowanceOnIncrementSpentTotal_r52(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r48(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r52(o,s,delta0);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r52(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateOwnerOnInsertConstructor_r19() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateRaisedOnInsertBuyToken_r53(int n) private    {
      raised.n += n;
  }
  function updateOnceFinalizeOnInsertFinalize_r31() private    {
      // Empty()
  }
  function updateTransferOnInsertRecv_transfer_r42(address from,address to,int amount) private   returns (bool) {
      int m_1 = balanceOf[s].n;
      if(r!=address(0) && n>0 && s!=address(0) && n<=m_1) {
        updateTotalOutOnInsertTransfer_r7(s,n);
        updateTotalInOnInsertTransfer_r14(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r26(int b) private    {
      totalSupply.n -= b;
  }
  function updateBurnOnInsertRecv_burn_r33(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      int m_2 = balanceOf[p].n;
      if(n>0 && p!=address(0) && o_1==s_1 && n<=m_2) {
        updateTotalBurnOnInsertBurn_r16(p,n);
        updateAllBurnOnInsertBurn_r25(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateBuyTokenOnInsertRecv_buyToken_r15(address p,int amount) private   returns (bool) {
      uint s_0 = start.time;
      uint t_0 = block.timestamp;
      uint t_1 = block.timestamp;
      uint e_1 = end.time;
      int tokens_2 = getTokenAmount(m);
      if(tokens_2>0 && p!=address(0) && m>0 && t_0>=s_0 && t_1<=e_1) {
        updateMintOnInsertBuyToken_r23(p,m);
        updateRaisedOnInsertBuyToken_r53(m);
        emit BuyToken(p,m);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r26(int m) private    {
      totalSupply.n += m;
  }
  function updateRateOnInsertConstructor_r35(int r) private    {
      if(r>0) {
        // Empty()
      }
  }
  function updateTransferOnInsertTransferFrom_r38(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r14(r,n);
      updateTotalOutOnInsertTransfer_r7(o,n);
      emit Transfer(o,r,n);
  }
  function updateFinalizedOnInsertFinalize_r5() private    {
      finalized = FinalizedTuple(true,true);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r3(address from,address to,address spender,int amount) private   returns (bool) {
      int m_1 = balanceOf[o].n;
      if(r!=address(0) && n<=m_1 && s!=address(0) && o!=address(0) && n>0) {
        updateSpentTotalOnInsertTransferFrom_r6(o,s,n);
        updateTransferOnInsertTransferFrom_r38(o,r,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateSpentTotalOnInsertTransferFrom_r6(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r52(o,s,delta0);
  }
  function updateMintOnInsertRecv_mint_r47(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(n>0 && p!=address(0) && o_1==s_1) {
        updateTotalMintOnInsertMint_r50(p,n);
        updateAllMintOnInsertMint_r11(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r30() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateBalanceOfOnIncrementTotalOut_r34(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateAllBurnOnInsertBurn_r25(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r26(delta0);
  }
  function updateTotalMintOnInsertMint_r50(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r34(p,delta0);
  }
}