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
      bool r30 = updateBurnOnInsertRecv_burn_r30(p,amount);
      if(r30==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r40 = updateMintOnInsertRecv_mint_r40(p,amount);
      if(r40==false) {
        revert("Rule condition failed");
      }
  }
  function finalize() public    {
      bool r36 = updateFinalizeOnInsertRecv_finalize_r36();
      if(r36==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r28 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r28(p,s,n);
      if(r28==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r22 = updateTransferFromOnInsertRecv_transferFrom_r22(from,to,spender,amount);
      if(r22==false) {
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
  function transfer(address from,address to,int amount) public    {
      bool r35 = updateTransferOnInsertRecv_transfer_r35(from,to,amount);
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
  function updateBalanceOfOnIncrementTotalIn_r31(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTotalSupplyOnIncrementAllBurn_r26(int b) private    {
      totalSupply.n -= b;
  }
  function updateTotalSupplyOnIncrementAllMint_r26(int m) private    {
      totalSupply.n += m;
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r41(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r46(o,s,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalOutOnInsertTransfer_r7(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r31(p,delta0);
  }
  function updateSpentTotalOnInsertTransferFrom_r6(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r46(o,s,delta0);
  }
  function updateOwnerOnInsertConstructor_r44() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateBuyTokenOnInsertRecv_buyToken_r45(address p,int amount) private   returns (bool) {
      uint e_3 = end.time;
      uint t_1 = block.timestamp;
      uint s_1 = start.time;
      if(amount>0 && t_1>=s_1 && t_1<=e_3) {
        int tokens_2 = getTokenAmount(amount);
        if(tokens_2>0) {
          updateMintOnInsertBuyToken_r23(p,amount);
          emit BuyToken(p,amount);
          return true;
        }
      }
      return false;
  }
  function updateBurnOnInsertRecv_burn_r30(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      int m_2 = balanceOf[p].n;
      if(amount>0 && p!=address(0) && o_1==s_1 && amount<=m_2) {
        updateTotalBurnOnInsertBurn_r16(p,amount);
        updateAllBurnOnInsertBurn_r25(amount);
        emit Burn(p,amount);
        return true;
      }
      return false;
  }
  function updateMintOnInsertRecv_mint_r40(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(amount>0 && p!=address(0) && o_1==s_1) {
        updateAllMintOnInsertMint_r11(amount);
        updateTotalMintOnInsertMint_r42(p,amount);
        emit Mint(p,amount);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementSpentTotal_r46(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTotalInOnInsertTransfer_r14(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r31(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalOut_r31(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateBalanceOfOnIncrementTotalMint_r31(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateTransferOnInsertRecv_transfer_r35(address from,address to,int amount) private   returns (bool) {
      int m_1 = balanceOf[from].n;
      if(to!=address(0) && from!=address(0) && amount>0 && amount<=m_1) {
        updateTotalOutOnInsertTransfer_r7(from,amount);
        updateTotalInOnInsertTransfer_r14(to,amount);
        emit Transfer(from,to,amount);
        return true;
      }
      return false;
  }
  function updateMintOnInsertBuyToken_r23(address p,int v) private    {
      int tokens = getTokenAmount(v);
      updateAllMintOnInsertMint_r11(tokens);
      updateTotalMintOnInsertMint_r42(p,tokens);
      emit Mint(p,tokens);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r22(address from,address to,address spender,int amount) private   returns (bool) {
      int m_1 = balanceOf[from].n;
      if(to!=address(0) && amount<=m_1 && spender!=address(0) && from!=address(0) && amount>0) {
        updateSpentTotalOnInsertTransferFrom_r6(from,spender,amount);
        updateTransferOnInsertTransferFrom_r32(from,to,amount);
        emit TransferFrom(from,to,spender,amount);
        return true;
      }
      return false;
  }
  function updateTotalBurnOnInsertBurn_r16(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r31(p,delta0);
  }
  function updateTotalMintOnInsertMint_r42(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r31(p,delta0);
  }
  function updateTotalSupplyOnInsertConstructor_r21() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateFinalizeOnInsertRecv_finalize_r36() private   returns (bool) {
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
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r28(address p,address s,int n) private   returns (bool) {
      if(n>=0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r41(p,s,n);
        emit IncreaseAllowance(p,s,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalBurn_r31(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r46(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateTransferOnInsertTransferFrom_r32(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r14(r,n);
      updateTotalOutOnInsertTransfer_r7(o,n);
      emit Transfer(o,r,n);
  }
  function updateAllBurnOnInsertBurn_r25(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r26(delta0);
  }
}