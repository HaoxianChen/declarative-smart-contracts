contract TokenPartition {
  struct BalanceOfByPartitionTuple {
    int n;
    bool _valid;
  }
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct TotalSupplyByPartitionTuple {
    int n;
    bool _valid;
  }
  struct TotalSupplyTuple {
    int n;
    bool _valid;
  }
  mapping(address=>mapping(int=>BalanceOfByPartitionTuple)) balanceOfByPartition;
  TotalSupplyTuple totalSupply;
  OwnerTuple owner;
  mapping(int=>TotalSupplyByPartitionTuple) totalSupplyByPartition;
  event InvalidTx();
  event TransferByPartition(address s,address r,int q,int n);
  event RedeemByPartition(address p,int q,int n);
  event IssueByPartition(address p,int q,int n);
  constructor() public {
    updateTotalSupplyOnInsertConstructor_r2();
    updateOwnerOnInsertConstructor_r23();
  }
  function issueByPartition(address p,int q,int n) public    {
      bool r14 = updateIssueByPartitionOnInsertRecv_issueByPartition_r14(p,q,n);
      if(r14==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupplyByPartition(int q) public view  returns (int) {
      int n = totalSupplyByPartition[q].n;
      return n;
  }
  function getBalanceOfByPartition(address p,int q) public view  returns (int) {
      int n = balanceOfByPartition[p][q].n;
      return n;
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function redeemByPartition(address p,int q,int n) public    {
      bool r24 = updateRedeemByPartitionOnInsertRecv_redeemByPartition_r24(p,q,n);
      if(r24==false) {
        revert("Rule condition failed");
      }
  }
  function transferByPartition(address s,address r,int q,int n) public    {
      bool r16 = updateTransferByPartitionOnInsertRecv_transferByPartition_r16(s,r,q,n);
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function updateRedeemByPartitionOnInsertRecv_redeemByPartition_r24(address p,int q,int n) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      int m_2 = balanceOfByPartition[p][q].n;
      if(p!=address(0) && n>0 && o_1==s_1 && n<=m_2) {
        updateTotalBurnOnInsertRedeemByPartition_r20(p,q,n);
        updateRedeemTotalByPartitionOnInsertRedeemByPartition_r13(q,n);
        updateAllBurnOnInsertRedeemByPartition_r4(n);
        emit RedeemByPartition(p,q,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyByPartitionOnIncrementRedeemTotalByPartition_r15(int q,int r) private    {
      totalSupplyByPartition[q].n -= r;
  }
  function updateTotalInOnInsertTransferByPartition_r10(address p,int q,int n) private    {
      int delta0 = int(n);
      updateBalanceOfByPartitionOnIncrementTotalIn_r21(p,q,delta0);
  }
  function updateTotalSupplyOnIncrementAllMint_r3(int m) private    {
      totalSupply.n += m;
  }
  function updateIssueTotalByPartitionOnInsertIssueByPartition_r8(int q,int n) private    {
      int delta0 = int(n);
      updateTotalSupplyByPartitionOnIncrementIssueTotalByPartition_r15(q,delta0);
  }
  function updateOwnerOnInsertConstructor_r23() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateBalanceOfByPartitionOnIncrementTotalBurn_r21(address p,int q,int m) private    {
      balanceOfByPartition[p][q].n -= m;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalMintOnInsertIssueByPartition_r18(address p,int q,int n) private    {
      int delta0 = int(n);
      updateBalanceOfByPartitionOnIncrementTotalMint_r21(p,q,delta0);
  }
  function updateTotalSupplyOnIncrementAllBurn_r3(int b) private    {
      totalSupply.n -= b;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTransferByPartitionOnInsertRecv_transferByPartition_r16(address s,address r,int q,int n) private   returns (bool) {
      int m_1 = balanceOfByPartition[s][q].n;
      if(n>0 && r!=address(0) && s!=address(0) && n<=m_1) {
        updateTotalOutOnInsertTransferByPartition_r25(s,q,n);
        updateTotalInOnInsertTransferByPartition_r10(r,q,n);
        emit TransferByPartition(s,r,q,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfByPartitionOnIncrementTotalIn_r21(address p,int q,int i) private    {
      balanceOfByPartition[p][q].n += i;
  }
  function updateTotalOutOnInsertTransferByPartition_r25(address p,int q,int n) private    {
      int delta0 = int(n);
      updateBalanceOfByPartitionOnIncrementTotalOut_r21(p,q,delta0);
  }
  function updateBalanceOfByPartitionOnIncrementTotalMint_r21(address p,int q,int n) private    {
      balanceOfByPartition[p][q].n += n;
  }
  function updateIssueByPartitionOnInsertRecv_issueByPartition_r14(address p,int q,int n) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(n>0 && p!=address(0) && o_1==s_1) {
        updateAllMintOnInsertIssueByPartition_r19(n);
        updateIssueTotalByPartitionOnInsertIssueByPartition_r8(q,n);
        updateTotalMintOnInsertIssueByPartition_r18(p,q,n);
        emit IssueByPartition(p,q,n);
        return true;
      }
      return false;
  }
  function updateAllMintOnInsertIssueByPartition_r19(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r3(delta0);
  }
  function updateRedeemTotalByPartitionOnInsertRedeemByPartition_r13(int q,int n) private    {
      int delta0 = int(n);
      updateTotalSupplyByPartitionOnIncrementRedeemTotalByPartition_r15(q,delta0);
  }
  function updateBalanceOfByPartitionOnIncrementTotalOut_r21(address p,int q,int o) private    {
      balanceOfByPartition[p][q].n -= o;
  }
  function updateTotalBurnOnInsertRedeemByPartition_r20(address p,int q,int n) private    {
      int delta0 = int(n);
      updateBalanceOfByPartitionOnIncrementTotalBurn_r21(p,q,delta0);
  }
  function updateAllBurnOnInsertRedeemByPartition_r4(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r3(delta0);
  }
  function updateTotalSupplyByPartitionOnIncrementIssueTotalByPartition_r15(int q,int i) private    {
      totalSupplyByPartition[q].n += i;
  }
  function updateTotalSupplyOnInsertConstructor_r2() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
}