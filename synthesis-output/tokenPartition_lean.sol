contract TokenPartition_lean {
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
    updateTotalSupplyOnInsertConstructor_r3();
    updateOwnerOnInsertConstructor_r8();
  }
  function issueByPartition(address p,int q,int n) public    {
      bool r16 = updateIssueByPartitionOnInsertRecv_issueByPartition_r16(p,q,n);
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function transferByPartition(address s,address r,int q,int n) public    {
      bool r14 = updateTransferByPartitionOnInsertRecv_transferByPartition_r14(s,r,q,n);
      if(r14==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupplyByPartition(int q) public view  returns (int) {
      int n = totalSupplyByPartition[q].n;
      return n;
  }
  function redeemByPartition(address p,int q,int n) public    {
      bool r17 = updateRedeemByPartitionOnInsertRecv_redeemByPartition_r17(p,q,n);
      if(r17==false) {
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
  function updateAllBurnOnInsertRedeemByPartition_r4(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r15(delta0);
  }
  function updateAllMintOnInsertIssueByPartition_r11(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r15(delta0);
  }
  function updateTotalOutOnInsertTransferByPartition_r18(address p,int q,int n) private    {
      int delta0 = int(n);
      updateBalanceOfByPartitionOnIncrementTotalOut_r13(p,q,delta0);
  }
  function updateRedeemTotalByPartitionOnInsertRedeemByPartition_r0(int q,int n) private    {
      int delta0 = int(n);
      updateTotalSupplyByPartitionOnIncrementRedeemTotalByPartition_r1(q,delta0);
  }
  function updateTotalMintOnInsertIssueByPartition_r10(address p,int q,int n) private    {
      int delta0 = int(n);
      updateBalanceOfByPartitionOnIncrementTotalMint_r13(p,q,delta0);
  }
  function updateBalanceOfByPartitionOnIncrementTotalIn_r13(address p,int q,int i) private    {
      balanceOfByPartition[p][q].n += i;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateBalanceOfByPartitionOnIncrementTotalBurn_r13(address p,int q,int m) private    {
      balanceOfByPartition[p][q].n -= m;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateBalanceOfByPartitionOnIncrementTotalOut_r13(address p,int q,int o) private    {
      balanceOfByPartition[p][q].n -= o;
  }
  function updateTotalInOnInsertTransferByPartition_r7(address p,int q,int n) private    {
      int delta0 = int(n);
      updateBalanceOfByPartitionOnIncrementTotalIn_r13(p,q,delta0);
  }
  function updateIssueTotalByPartitionOnInsertIssueByPartition_r6(int q,int n) private    {
      int delta0 = int(n);
      updateTotalSupplyByPartitionOnIncrementIssueTotalByPartition_r1(q,delta0);
  }
  function updateOwnerOnInsertConstructor_r8() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTotalSupplyOnIncrementAllBurn_r15(int b) private    {
      totalSupply.n -= b;
  }
  function updateTransferByPartitionOnInsertRecv_transferByPartition_r14(address s,address r,int q,int n) private   returns (bool) {
      if(0==n) {
        updateTotalOutOnInsertTransferByPartition_r18(s,q,n);
        updateTotalInOnInsertTransferByPartition_r7(r,q,n);
        emit TransferByPartition(s,r,q,n);
        return true;
      }
      return false;
  }
  function updateRedeemByPartitionOnInsertRecv_redeemByPartition_r17(address p,int q,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && 0==n) {
        updateTotalBurnOnInsertRedeemByPartition_r12(p,q,n);
        updateRedeemTotalByPartitionOnInsertRedeemByPartition_r0(q,n);
        updateAllBurnOnInsertRedeemByPartition_r4(n);
        emit RedeemByPartition(p,q,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyByPartitionOnIncrementIssueTotalByPartition_r1(int q,int i) private    {
      totalSupplyByPartition[q].n += i;
  }
  function updateTotalSupplyByPartitionOnIncrementRedeemTotalByPartition_r1(int q,int r) private    {
      totalSupplyByPartition[q].n -= r;
  }
  function updateIssueByPartitionOnInsertRecv_issueByPartition_r16(address p,int q,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>0) {
        updateTotalMintOnInsertIssueByPartition_r10(p,q,n);
        updateAllMintOnInsertIssueByPartition_r11(n);
        updateIssueTotalByPartitionOnInsertIssueByPartition_r6(q,n);
        emit IssueByPartition(p,q,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfByPartitionOnIncrementTotalMint_r13(address p,int q,int n) private    {
      balanceOfByPartition[p][q].n += n;
  }
  function updateTotalSupplyOnInsertConstructor_r3() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateTotalBurnOnInsertRedeemByPartition_r12(address p,int q,int n) private    {
      int delta0 = int(n);
      updateBalanceOfByPartitionOnIncrementTotalBurn_r13(p,q,delta0);
  }
  function updateTotalSupplyOnIncrementAllMint_r15(int m) private    {
      totalSupply.n += m;
  }
}