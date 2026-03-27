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
  event RedeemByPartition(address p,int q,int n);
  event IssueByPartition(address p,int q,int n);
  event UnauthorizedIssueByPartition();
  event TransferByPartition(address s,address r,int q,int n);
  event UnauthorizedRedeemByPartition();
  constructor() public {
    updateTotalSupplyOnInsertConstructor_r1();
    updateOwnerOnInsertConstructor_r9();
  }
  function redeemByPartition(address p,int q,int n) public    {
      bool r13 = updateRedeemByPartitionOnInsertRecv_redeemByPartition_r13(p,q,n);
      if(r13==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupplyByPartition(int q) public view  returns (int) {
      int n = totalSupplyByPartition[q].n;
      return n;
  }
  function issueByPartition(address p,int q,int n) public    {
      bool r6 = updateIssueByPartitionOnInsertRecv_issueByPartition_r6(p,q,n);
      if(r6==false) {
        revert("Rule condition failed");
      }
  }
  function transferByPartition(address s,address r,int q,int n) public    {
      bool r7 = updateTransferByPartitionOnInsertRecv_transferByPartition_r7(s,r,q,n);
      if(r7==false) {
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
  function updateBalanceOfByPartitionOnIncrementTotalBurn_r18(address p,int q,int m) private    {
      balanceOfByPartition[p][q].n -= m;
  }
  function updateTotalSupplyByPartitionOnIncrementRedeemTotalByPartition_r0(int q,int r) private    {
      totalSupplyByPartition[q].n -= r;
  }
  function updateTotalBurnOnInsertRedeemByPartition_r17(address p,int q,int n) private    {
      int delta0 = int(n);
      updateBalanceOfByPartitionOnIncrementTotalBurn_r18(p,q,delta0);
  }
  function updateTotalMintOnInsertIssueByPartition_r14(address p,int q,int n) private    {
      int delta0 = int(n);
      updateBalanceOfByPartitionOnIncrementTotalMint_r18(p,q,delta0);
  }
  function updateTransferByPartitionOnInsertRecv_transferByPartition_r7(address s,address r,int q,int n) private   returns (bool) {
      int balanceOfByPartition_x2_1 = balanceOfByPartition[s][q].n;
      if(n>0 && n<balanceOfByPartition_x2_1) {
        updateTotalOutOnInsertTransferByPartition_r11(s,q,n);
        updateTotalInOnInsertTransferByPartition_r8(r,q,n);
        emit TransferByPartition(s,r,q,n);
        return true;
      }
      return false;
  }
  function updateIssueTotalByPartitionOnInsertIssueByPartition_r5(int q,int n) private    {
      int delta0 = int(n);
      updateTotalSupplyByPartitionOnIncrementIssueTotalByPartition_r0(q,delta0);
  }
  function updateRedeemTotalByPartitionOnInsertRedeemByPartition_r12(int q,int n) private    {
      int delta0 = int(n);
      updateTotalSupplyByPartitionOnIncrementRedeemTotalByPartition_r0(q,delta0);
  }
  function updateAllBurnOnInsertRedeemByPartition_r3(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r2(delta0);
  }
  function updateTotalSupplyOnIncrementAllBurn_r2(int b) private    {
      totalSupply.n -= b;
  }
  function updateTotalSupplyByPartitionOnIncrementIssueTotalByPartition_r0(int q,int i) private    {
      totalSupplyByPartition[q].n += i;
  }
  function updateIssueByPartitionOnInsertRecv_issueByPartition_r6(address p,int q,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(s==o && n>0) {
        updateAllMintOnInsertIssueByPartition_r15(n);
        updateTotalMintOnInsertIssueByPartition_r14(p,q,n);
        updateIssueTotalByPartitionOnInsertIssueByPartition_r5(q,n);
        emit IssueByPartition(p,q,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r2(int m) private    {
      totalSupply.n += m;
  }
  function updateOwnerOnInsertConstructor_r9() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateBalanceOfByPartitionOnIncrementTotalIn_r18(address p,int q,int i) private    {
      balanceOfByPartition[p][q].n += i;
  }
  function updateRedeemByPartitionOnInsertRecv_redeemByPartition_r13(address p,int q,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOfByPartition_x2 = balanceOfByPartition[p][q].n;
      if(s==o && n<=balanceOfByPartition_x2) {
        updateRedeemTotalByPartitionOnInsertRedeemByPartition_r12(q,n);
        updateTotalBurnOnInsertRedeemByPartition_r17(p,q,n);
        updateAllBurnOnInsertRedeemByPartition_r3(n);
        emit RedeemByPartition(p,q,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfByPartitionOnIncrementTotalOut_r18(address p,int q,int o) private    {
      balanceOfByPartition[p][q].n -= o;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalOutOnInsertTransferByPartition_r11(address p,int q,int n) private    {
      int delta0 = int(n);
      updateBalanceOfByPartitionOnIncrementTotalOut_r18(p,q,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTotalSupplyOnInsertConstructor_r1() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateBalanceOfByPartitionOnIncrementTotalMint_r18(address p,int q,int n) private    {
      balanceOfByPartition[p][q].n += n;
  }
  function updateAllMintOnInsertIssueByPartition_r15(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r2(delta0);
  }
  function updateTotalInOnInsertTransferByPartition_r8(address p,int q,int n) private    {
      int delta0 = int(n);
      updateBalanceOfByPartitionOnIncrementTotalIn_r18(p,q,delta0);
  }
}