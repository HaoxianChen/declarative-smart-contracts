import "./finalizableCrowdSale_udf.sol";
contract FinalizableCrowdSale is FinalizableCrowdsaleUDF {
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
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  EndTuple end;
  StartTuple start;
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
    updateOwnerOnInsertConstructor_r44();
    updateTotalSupplyOnInsertConstructor_r21();
  }
  function burn(address p,int amount) public    {
      bool r29 = updateBurnOnInsertRecv_burn_r29(p,amount);
      if(r29==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r43 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r43(p,s,n);
      if(r43==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r39 = updateMintOnInsertRecv_mint_r39(p,amount);
      if(r39==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r22 = updateTransferFromOnInsertRecv_transferFrom_r22(from,to,spender,amount);
      if(r22==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r34 = updateTransferOnInsertRecv_transfer_r34(from,to,amount);
      if(r34==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function buyToken(address p,int amount) public    {
      bool r45 = updateBuyTokenOnInsertRecv_buyToken_r45(p,amount);
      if(r45==false) {
        revert("Rule condition failed");
      }
  }
  function finalize() public    {
      bool r35 = updateFinalizeOnInsertRecv_finalize_r35();
      if(r35==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function updateAllMintOnInsertMint_r11(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r26(delta0);
  }
  function updateTotalInOnInsertTransfer_r14(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r30(p,delta0);
  }
  function updateTransferOnInsertTransferFrom_r31(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r14(r,n);
      updateTotalOutOnInsertTransfer_r7(o,n);
      emit Transfer(o,r,n);
  }
  function updateTotalSupplyOnIncrementAllMint_r26(int m) private    {
      totalSupply.n += m;
  }
  function updateSpentTotalOnInsertTransferFrom_r6(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r46(o,s,delta0);
  }
  function updateMintOnInsertBuyToken_r23(address p,int v) private    {
      int tokens = getTokenAmount(v);
      updateTotalMintOnInsertMint_r41(p,tokens);
      updateAllMintOnInsertMint_r11(tokens);
      emit Mint(p,tokens);
  }
  function updateTransferOnInsertRecv_transfer_r34(address from,address to,int amount) private   returns (bool) {
      int m_1 = balanceOf[s].n;
      if(r!=address(0) && n>0 && s!=address(0) && n<=m_1) {
        updateTotalOutOnInsertTransfer_r7(s,n);
        updateTotalInOnInsertTransfer_r14(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementSpentTotal_r46(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateMintOnInsertRecv_mint_r39(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(n>0 && p!=address(0) && o_1==s_1) {
        updateTotalMintOnInsertMint_r41(p,n);
        updateAllMintOnInsertMint_r11(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateTotalBurnOnInsertBurn_r16(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r30(p,delta0);
  }
  function updateTotalSupplyOnInsertConstructor_r21() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateBalanceOfOnIncrementTotalIn_r30(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r43(address p,address s,int n) private   returns (bool) {
      updateAllowanceTotalOnInsertIncreaseAllowance_r40(o,s,d);
      emit IncreaseAllowance(o,s,d);
      return true;
      return false;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r22(address from,address to,address spender,int amount) private   returns (bool) {
      int m_1 = balanceOf[o].n;
      if(r!=address(0) && n<=m_1 && s!=address(0) && o!=address(0) && n>0) {
        updateSpentTotalOnInsertTransferFrom_r6(o,s,n);
        updateTransferOnInsertTransferFrom_r31(o,r,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateOwnerOnInsertConstructor_r44() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTotalSupplyOnIncrementAllBurn_r26(int b) private    {
      totalSupply.n -= b;
  }
  function updateBalanceOfOnIncrementTotalMint_r30(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateBuyTokenOnInsertRecv_buyToken_r45(address p,int amount) private   returns (bool) {
      uint e_3 = end.time;
      uint t_1 = block.timestamp;
      uint t_3 = block.timestamp;
      uint s_1 = start.time;
      int tokens_2 = getTokenAmount(n);
      if(n>0 && t_1>=s_1 && tokens_2>0 && t_3<=e_3) {
        updateMintOnInsertBuyToken_r23(p,n);
        emit BuyToken(p,n);
        return true;
      }
      return false;
  }
  function updateTotalMintOnInsertMint_r41(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r30(p,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateBurnOnInsertRecv_burn_r29(address p,int amount) private   returns (bool) {
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
  function updateAllowanceTotalOnInsertIncreaseAllowance_r40(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r46(o,s,delta0);
  }
  function updateTotalOutOnInsertTransfer_r7(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r30(p,delta0);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r46(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateAllBurnOnInsertBurn_r25(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r26(delta0);
  }
  function updateBalanceOfOnIncrementTotalOut_r30(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateFinalizeOnInsertRecv_finalize_r35() private   returns (bool) {
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
  function updateBalanceOfOnIncrementTotalBurn_r30(address p,int m) private    {
      balanceOf[p].n -= m;
  }
}