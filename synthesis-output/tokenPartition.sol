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
    updateOwnerOnInsertConstructor_r10();
    updateTotalSupplyOnInsertConstructor_r2();
  }
  function transferByPartition(address s,address r,int q,int n) public    {
      bool r24 = updateTransferByPartitionOnInsertRecv_transferByPartition_r24(s,r,q,n);
      if(r24==false) {
        revert("Rule condition failed");
      }
  }
  function issueByPartition(address p,int q,int n) public    {
      bool r5 = updateIssueByPartitionOnInsertRecv_issueByPartition_r5(p,q,n);
      if(r5==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupplyByPartition(int q) public view  returns (int) {
      int n = totalSupplyByPartition[q].n;
      return n;
  }
  function redeemByPartition(address p,int q,int n) public    {
      bool r16 = updateRedeemByPartitionOnInsertRecv_redeemByPartition_r16(p,q,n);
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOfByPartition(address p,int q) public view  returns (int) {
      int n = balanceOfByPartition[p][q].n;
      return n;
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function updateRedeemByPartitionOnInsertRecv_redeemByPartition_r16(address p,int q,int n) private   returns (bool) {
      address s_2 = msg.sender;
      address o_2 = owner.p;
      int b_1 = balanceOfByPartition[p][q].n;
      if(n>0 && n<=b_1 && o_2==s_2) {
        updateTotalBurnOnInsertRedeemByPartition_r21(p,q,n);
        updateRedeemTotalByPartitionOnInsertRedeemByPartition_r0(q,n);
        updateAllBurnOnInsertRedeemByPartition_r4(n);
        emit RedeemByPartition(p,q,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyByPartitionOnIncrementRedeemTotalByPartition_r15(int q,int r) private    {
      totalSupplyByPartition[q].n -= r;
  }
  function updateIssueTotalByPartitionOnInsertIssueByPartition_r8(int q,int n) private    {
      int delta0 = int(n);
      updateTotalSupplyByPartitionOnIncrementIssueTotalByPartition_r15(q,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTransferByPartitionOnInsertRecv_transferByPartition_r24(address s,address r,int q,int n) private   returns (bool) {
      address m_2 = msg.sender;
      int b_1 = balanceOfByPartition[s][q].n;
      if(n>0 && n<=b_1 && s==m_2) {
        updateTotalInOnInsertTransferByPartition_r9(r,q,n);
        updateTotalOutOnInsertTransferByPartition_r13(s,q,n);
        emit TransferByPartition(s,r,q,n);
        return true;
      }
      return false;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateAllBurnOnInsertRedeemByPartition_r4(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r3(delta0);
  }
  function updateBalanceOfByPartitionOnIncrementTotalIn_r22(address p,int q,int i) private    {
      balanceOfByPartition[p][q].n += i;
  }
  function updateTotalSupplyByPartitionOnIncrementIssueTotalByPartition_r15(int q,int i) private    {
      totalSupplyByPartition[q].n += i;
  }
  function updateIssueByPartitionOnInsertRecv_issueByPartition_r5(address p,int q,int n) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(n>0 && o_1==s_1) {
        updateTotalMintOnInsertIssueByPartition_r19(p,q,n);
        updateAllMintOnInsertIssueByPartition_r20(n);
        updateIssueTotalByPartitionOnInsertIssueByPartition_r8(q,n);
        emit IssueByPartition(p,q,n);
        return true;
      }
      return false;
  }
  function updateTotalInOnInsertTransferByPartition_r9(address p,int q,int n) private    {
      int delta0 = int(n);
      updateBalanceOfByPartitionOnIncrementTotalIn_r22(p,q,delta0);
  }
  function updateBalanceOfByPartitionOnIncrementTotalOut_r22(address p,int q,int o) private    {
      balanceOfByPartition[p][q].n -= o;
  }
  function updateBalanceOfByPartitionOnIncrementTotalBurn_r22(address p,int q,int m) private    {
      balanceOfByPartition[p][q].n -= m;
  }
  function updateRedeemTotalByPartitionOnInsertRedeemByPartition_r0(int q,int n) private    {
      int delta0 = int(n);
      updateTotalSupplyByPartitionOnIncrementRedeemTotalByPartition_r15(q,delta0);
  }
  function updateTotalOutOnInsertTransferByPartition_r13(address p,int q,int n) private    {
      int delta0 = int(n);
      updateBalanceOfByPartitionOnIncrementTotalOut_r22(p,q,delta0);
  }
  function updateOwnerOnInsertConstructor_r10() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTotalSupplyOnIncrementAllMint_r3(int m) private    {
      totalSupply.n += m;
  }
  function updateAllMintOnInsertIssueByPartition_r20(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r3(delta0);
  }
  function updateTotalMintOnInsertIssueByPartition_r19(address p,int q,int n) private    {
      int delta0 = int(n);
      updateBalanceOfByPartitionOnIncrementTotalMint_r22(p,q,delta0);
  }
  function updateBalanceOfByPartitionOnIncrementTotalMint_r22(address p,int q,int n) private    {
      balanceOfByPartition[p][q].n += n;
  }
  function updateTotalBurnOnInsertRedeemByPartition_r21(address p,int q,int n) private    {
      int delta0 = int(n);
      updateBalanceOfByPartitionOnIncrementTotalBurn_r22(p,q,delta0);
  }
  function updateTotalSupplyOnIncrementAllBurn_r3(int b) private    {
      totalSupply.n -= b;
  }
  function updateTotalSupplyOnInsertConstructor_r2() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
}