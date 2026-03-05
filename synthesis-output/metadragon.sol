import "./metadragon_udf.sol";
contract Metadragon is MetaDragonUDF {
  struct BalanceOfTuple {
    int n;
    bool _valid;
  }
  struct TotalNFTsTuple {
    int n;
    bool _valid;
  }
  struct TotalSupplyTuple {
    int n;
    bool _valid;
  }
  struct NftBalanceTuple {
    int n;
    bool _valid;
  }
  mapping(address=>NftBalanceTuple) nftBalance;
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalNFTsTuple totalNFTs;
  TotalSupplyTuple totalSupply;
  event NftToErc20(address user,uint tokenId,int tokens);
  event Update(address user,address to,uint value);
  event Erc20ToNft(address user,int amount,int nfts);
  event Transfer(address from,address to,int amount);
  event InvalidTx();
  constructor() public {
    updateTotalSupplyOnInsertConstructor_r0();
    updateTotalNFTsOnInsertConstructor_r12();
  }
  function getNftBalance(address user) public view  returns (int) {
      int n = nftBalance[user].n;
      return n;
  }
  function getBalanceOf(address user) public view  returns (int) {
      int n = balanceOf[user].n;
      return n;
  }
  function transfer(address to,int amount) public    {
      bool r24 = updateTransferOnInsertRecv_transfer_r24(to,amount);
      if(r24==false) {
        revert("Rule condition failed");
      }
  }
  function erc20ToNft(int amount) public    {
      bool r15 = updateErc20ToNftOnInsertRecv_erc20ToNft_r15(amount);
      if(r15==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalNFTs() public view  returns (int) {
      int n = totalNFTs.n;
      return n;
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function nftToErc20(uint tokenId) public    {
      bool r18 = updateNftToErc20OnInsertRecv_nftToErc20_r18(tokenId);
      if(r18==false) {
        revert("Rule condition failed");
      }
  }
  function update(address to,uint value) public    {
      bool r25 = updateUpdateOnInsertRecv_update_r25(to,value);
      if(r25==false) {
        revert("Rule condition failed");
      }
  }
  function updateBalanceOfOnIncrementTokensIn_r10(address user,int i) private    {
      balanceOf[user].n += i;
  }
  function updateTokensOutOnInsertTokenExpense_r1(address user,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTokensOut_r10(user,delta0);
  }
  function updateNftsOutOnInsertNftExpense_r13(address user,int n) private    {
      int delta0 = int(n);
      updateNftBalanceOnIncrementNftsOut_r3(user,delta0);
  }
  function updateUpdateOnInsertRecv_update_r25(address to,uint value) private   returns (bool) {
      address msgSender = msg.sender;
      address t_0 = address(this);
      address user = msg.sender;
      int balanceOf_x1 = balanceOf[msgSender].n;
      bool ok_1 = isValidTokenId(value);
      if(to==t_0 && ok_1!=true && 0!=balanceOf_x1) {
        emit Update(user,to,value);
        return true;
      }
      return false;
  }
  function updateNftsInOnInsertNftIncome_r5(address user,int n) private    {
      int delta0 = int(n);
      updateNftBalanceOnIncrementNftsIn_r3(user,delta0);
  }
  function updateTokenExpenseOnInsertErc20ToNft_r16(address user,int n) private    {
      updateTokensOutOnInsertTokenExpense_r1(user,n);
  }
  function updateErc20ToNftOnInsertRecv_erc20ToNft_r15(int amount) private   returns (bool) {
      address user = msg.sender;
      int n = nftsForAmount(amt);
      if(amt>0) {
        updateNftIncomeOnInsertErc20ToNft_r23(user,n);
        updateTotalNFTsOnInsertErc20ToNft_r17(n);
        updateTokenExpenseOnInsertErc20ToNft_r16(user,amt);
        emit Erc20ToNft(user,amt,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r0() private    {
      totalSupply = TotalSupplyTuple(260000000,true);
  }
  function updateTokenExpenseOnInsertTransfer_r9(address from,int n) private    {
      updateTokensOutOnInsertTokenExpense_r1(from,n);
  }
  function updateNftBalanceOnIncrementNftsIn_r3(address user,int i) private    {
      nftBalance[user].n += i;
  }
  function updateNftExpenseOnInsertNftToErc20_r11(address user) private    {
      updateNftsOutOnInsertNftExpense_r13(user,int(1));
  }
  function updateNftIncomeOnInsertErc20ToNft_r23(address user,int n) private    {
      updateNftsInOnInsertNftIncome_r5(user,n);
  }
  function updateTotalNFTsOnInsertErc20ToNft_r17(int k) private    {
      totalNFTs.n += k;
  }
  function updateTokensInOnInsertTokenIncome_r19(address user,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTokensIn_r10(user,delta0);
  }
  function updateBalanceOfOnIncrementTokensOut_r10(address user,int o) private    {
      balanceOf[user].n -= o;
  }
  function updateNftToErc20OnInsertRecv_nftToErc20_r18(uint tokenId) private   returns (bool) {
      address msgSender = msg.sender;
      address user = msg.sender;
      int nftBalance_x1 = nftBalance[msgSender].n;
      bool ok = isValidTokenId(tid);
      if(ok!=false && nftBalance_x1>0) {
        int tok = 9800;
        updateTokenIncomeOnInsertNftToErc20_r21(user,tok);
        updateNftExpenseOnInsertNftToErc20_r11(user);
        emit NftToErc20(user,tid,tok);
        return true;
      }
      return false;
  }
  function updateTotalNFTsOnInsertConstructor_r12() private    {
      totalNFTs = TotalNFTsTuple(0,true);
  }
  function updateNftBalanceOnIncrementNftsOut_r3(address user,int o) private    {
      nftBalance[user].n -= o;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTransferOnInsertRecv_transfer_r24(address to,int amount) private   returns (bool) {
      address from = msg.sender;
      if(amt>0 && to!=address(0)) {
        updateTokenExpenseOnInsertTransfer_r9(from,amt);
        emit Transfer(from,to,amt);
        return true;
      }
      return false;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTokenIncomeOnInsertNftToErc20_r21(address user,int n) private    {
      updateTokensInOnInsertTokenIncome_r19(user,n);
  }
}