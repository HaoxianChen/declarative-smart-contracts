import "./erc1155_udf.sol";
contract Erc1155 is ERC1155UDF {
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct TotalSupplyTuple {
    int n;
    bool _valid;
  }
  struct AllowanceTuple {
    int n;
    bool _valid;
  }
  struct BalanceOfTuple {
    int n;
    bool _valid;
  }
  mapping(int=>mapping(address=>mapping(address=>AllowanceTuple))) allowance;
  mapping(int=>TotalSupplyTuple) totalSupply;
  mapping(int=>mapping(address=>BalanceOfTuple)) balanceOf;
  OwnerTuple owner;
  event InvalidTx();
  event TransferFrom(int tokenId,address from,address to,address spender,int amount);
  event IncreaseAllowance(int tokenId,address o,address s,int d);
  event Transfer(int tokenId,address from,address to,int amount);
  event Burn(int tokenId,address p,int amount);
  event Mint(int tokenId,address p,int amount);
  constructor() public {
    updateOwnerOnInsertConstructor_r25();
  }
  function burn(int tokenId,address p,int amount) public    {
      bool r27 = updateBurnOnInsertRecv_burn_r27(tokenId,p,amount);
      if(r27==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(int tokenId,address from,address to,int amount) public    {
      bool r5 = updateTransferOnInsertRecv_transfer_r5(tokenId,from,to,amount);
      if(r5==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(int tokenId,address from,address to,address spender,int amount) public    {
      bool r1 = updateTransferFromOnInsertRecv_transferFrom_r1(tokenId,from,to,spender,amount);
      if(r1==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply(int tokenId) public view  returns (int) {
      int n = totalSupply[tokenId].n;
      return n;
  }
  function getBalanceOf(int tokenId,address p) public view  returns (int) {
      int n = balanceOf[tokenId][p].n;
      return n;
  }
  function mint(int tokenId,address p,int amount) public    {
      bool r13 = updateMintOnInsertRecv_mint_r13(tokenId,p,amount);
      if(r13==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(int tokenId,address o,address s) public view  returns (int) {
      int n = allowance[tokenId][o][s].n;
      return n;
  }
  function increaseAllowance(int tokenId,address o,address s,int d) public    {
      bool r24 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r24(tokenId,o,s,d);
      if(r24==false) {
        revert("Rule condition failed");
      }
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r10(int t,address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r6(t,o,s,delta0);
  }
  function updateBalanceOfOnIncrementTotalIn_r7(int t,address p,int i) private    {
      balanceOf[t][p].n += i;
  }
  function updateTransferOnInsertTransferFrom_r26(int t,address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r14(t,o,n);
      updateTotalInOnInsertTransfer_r18(t,r,n);
      emit Transfer(t,o,r,n);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r24(int tokenId,address o,address s,int d) private   returns (bool) {
      updateAllowanceTotalOnInsertIncreaseAllowance_r10(tokenId,o,s,d);
      emit IncreaseAllowance(tokenId,o,s,d);
      return true;
      return false;
  }
  function updateTotalBurnOnInsertBurn_r31(int t,address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r7(t,p,delta0);
  }
  function updateTransferOnInsertRecv_transfer_r5(int tokenId,address from,address to,int amount) private   returns (bool) {
      int m_1 = balanceOf[tokenId][from].n;
      if(to!=address(0) && from!=address(0) && amount<=m_1) {
        updateTotalOutOnInsertTransfer_r14(tokenId,from,amount);
        updateTotalInOnInsertTransfer_r18(tokenId,to,amount);
        emit Transfer(tokenId,from,to,amount);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllBurn_r28(int t,int b) private    {
      totalSupply[t].n -= b;
  }
  function updateAllMintOnInsertMint_r29(int t,int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r28(t,delta0);
  }
  function updateTotalInOnInsertTransfer_r18(int t,address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r7(t,p,delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r1(int tokenId,address from,address to,address spender,int amount) private   returns (bool) {
      int k_1 = allowance[tokenId][from][spender].n;
      int m_2 = balanceOf[tokenId][from].n;
      if(amount<=k_1 && to!=address(0) && from!=address(0) && amount<=m_2) {
        bool success_0 = onERC1155Received(to,tokenId,amount);
        if(success_0!=false) {
          updateTransferOnInsertTransferFrom_r26(tokenId,from,to,amount);
          updateSpentTotalOnInsertTransferFrom_r30(tokenId,from,spender,amount);
          emit TransferFrom(tokenId,from,to,spender,amount);
          return true;
        }
      }
      return false;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r6(int t,address o,address s,int m) private    {
      allowance[t][o][s].n += m;
  }
  function updateAllowanceOnIncrementSpentTotal_r6(int t,address o,address s,int l) private    {
      allowance[t][o][s].n -= l;
  }
  function updateOwnerOnInsertConstructor_r25() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateAllBurnOnInsertBurn_r0(int t,int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r28(t,delta0);
  }
  function updateMintOnInsertRecv_mint_r13(int tokenId,address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(p!=address(0) && o_1==s_1) {
        updateTotalMintOnInsertMint_r17(tokenId,p,amount);
        updateAllMintOnInsertMint_r29(tokenId,amount);
        emit Mint(tokenId,p,amount);
        return true;
      }
      return false;
  }
  function updateTotalMintOnInsertMint_r17(int t,address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r7(t,p,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateBalanceOfOnIncrementTotalOut_r7(int t,address p,int o) private    {
      balanceOf[t][p].n -= o;
  }
  function updateBurnOnInsertRecv_burn_r27(int tokenId,address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      int m_2 = balanceOf[tokenId][p].n;
      if(p!=address(0) && o_1==s_1 && amount<=m_2) {
        updateAllBurnOnInsertBurn_r0(tokenId,amount);
        updateTotalBurnOnInsertBurn_r31(tokenId,p,amount);
        emit Burn(tokenId,p,amount);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalMint_r7(int t,address p,int n) private    {
      balanceOf[t][p].n += n;
  }
  function updateTotalOutOnInsertTransfer_r14(int t,address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r7(t,p,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTotalSupplyOnIncrementAllMint_r28(int t,int m) private    {
      totalSupply[t].n += m;
  }
  function updateSpentTotalOnInsertTransferFrom_r30(int t,address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r6(t,o,s,delta0);
  }
  function updateBalanceOfOnIncrementTotalBurn_r7(int t,address p,int m) private    {
      balanceOf[t][p].n -= m;
  }
}