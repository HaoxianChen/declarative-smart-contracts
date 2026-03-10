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
    updateTotalNFTsOnInsertConstructor_r13();
    updateTotalSupplyOnInsertConstructor_r0();
  }
  function getNftBalance(address user) public view  returns (int) {
      int n = nftBalance[user].n;
      return n;
  }
  function getBalanceOf(address user) public view  returns (int) {
      int n = balanceOf[user].n;
      return n;
  }
  function getTotalNFTs() public view  returns (int) {
      int n = totalNFTs.n;
      return n;
  }
  function nftToErc20(uint tokenId) public    {
      bool r23 = updateNftToErc20OnInsertRecv_nftToErc20_r23(tokenId);
      if(r23==false) {
        revert("Rule condition failed");
      }
  }
  function update(address to,uint value) public    {
      bool r19 = updateUpdateOnInsertRecv_update_r19(to,value);
      if(r19==false) {
        revert("Rule condition failed");
      }
  }
  function erc20ToNft(int amount) public    {
      bool r16 = updateErc20ToNftOnInsertRecv_erc20ToNft_r16(amount);
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function transfer(address to,int amount) public    {
      bool r25 = updateTransferOnInsertRecv_transfer_r25(to,amount);
      if(r25==false) {
        revert("Rule condition failed");
      }
  }
  function updateNftsOutOnInsertNftExpense_r14(address user,int n) private    {
      int delta0 = int(n);
      updateNftBalanceOnIncrementNftsOut_r3(user,delta0);
  }
  function updateTransferOnInsertRecv_transfer_r25(address to,int amount) private   returns (bool) {
      address from = msg.sender;
      if(amount>0 && to!=address(0)) {
        updateTokenExpenseOnInsertTransfer_r10(from,amount);
        emit Transfer(from,to,amount);
        return true;
      }
      return false;
  }
  function updateErc20ToNftOnInsertRecv_erc20ToNft_r16(int amount) private   returns (bool) {
      address user = msg.sender;
      if(amount>0) {
        int n = this.nftsForAmount(amount);
        updateTokenExpenseOnInsertErc20ToNft_r17(user,amount);
        updateTotalNFTsOnInsertErc20ToNft_r5(n);
        updateNftIncomeOnInsertErc20ToNft_r24(user,n);
        emit Erc20ToNft(user,amount,n);
        return true;
      }
      return false;
  }
  function updateNftToErc20OnInsertRecv_nftToErc20_r23(uint tokenId) private   returns (bool) {
      address msgSender = msg.sender;
      int nftBalance_x1 = nftBalance[msgSender].n;
      if(nftBalance_x1<0) {
        int tok = 9800;
        bool ok = this.isValidTokenId(tokenId);
        if(ok!=false) {
          updateNftExpenseOnInsertNftToErc20_r12(msgSender);
          updateTokenIncomeOnInsertNftToErc20_r21(msgSender,tok);
          emit NftToErc20(msgSender,tokenId,tok);
          return true;
        }
      }
      return false;
  }
  function updateTokenExpenseOnInsertErc20ToNft_r17(address user,int n) private    {
      updateTokensOutOnInsertTokenExpense_r1(user,n);
  }
  function updateBalanceOfOnIncrementTokensOut_r11(address user,int o) private    {
      balanceOf[user].n -= o;
  }
  function updateTotalSupplyOnInsertConstructor_r0() private    {
      totalSupply = TotalSupplyTuple(260000000,true);
  }
  function updateNftExpenseOnInsertNftToErc20_r12(address user) private    {
      updateNftsOutOnInsertNftExpense_r14(user,int(1));
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTokensInOnInsertTokenIncome_r18(address user,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTokensIn_r11(user,delta0);
  }
  function updateNftsInOnInsertNftIncome_r6(address user,int n) private    {
      int delta0 = int(n);
      updateNftBalanceOnIncrementNftsIn_r3(user,delta0);
  }
  function updateTotalNFTsOnInsertConstructor_r13() private    {
      totalNFTs = TotalNFTsTuple(0,true);
  }
  function updateTokensOutOnInsertTokenExpense_r1(address user,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTokensOut_r11(user,delta0);
  }
  function updateTokenIncomeOnInsertNftToErc20_r21(address user,int n) private    {
      updateTokensInOnInsertTokenIncome_r18(user,n);
  }
  function updateUpdateOnInsertRecv_update_r19(address to,uint value) private   returns (bool) {
      int totalSupply_n = totalSupply.n;
      address t_0 = address(this);
      address user = msg.sender;
      if(to==t_0 && totalSupply_n<0) {
        bool ok_1 = this.isValidTokenId(value);
        if(ok_1!=true) {
          emit Update(user,to,value);
          return true;
        }
      }
      return false;
  }
  function updateBalanceOfOnIncrementTokensIn_r11(address user,int i) private    {
      balanceOf[user].n += i;
  }
  function updateNftIncomeOnInsertErc20ToNft_r24(address user,int n) private    {
      updateNftsInOnInsertNftIncome_r6(user,n);
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
  function updateTokenExpenseOnInsertTransfer_r10(address from,int n) private    {
      updateTokensOutOnInsertTokenExpense_r1(from,n);
  }
  function updateTotalNFTsOnInsertErc20ToNft_r5(int k) private    {
      totalNFTs.n += k;
  }
  function updateNftBalanceOnIncrementNftsIn_r3(address user,int i) private    {
      nftBalance[user].n += i;
  }
}