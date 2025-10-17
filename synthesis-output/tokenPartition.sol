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
    updateOwnerOnInsertConstructor_r8();
    updateTotalSupplyOnInsertConstructor_r2();
  }
  function getTotalSupplyByPartition(int q) public view  returns (int) {
      int n = totalSupplyByPartition[q].n;
      return n;
  }
  function redeemByPartition(address p,int q,int n) public    {
      bool r4 = updateRedeemByPartitionOnInsertRecv_redeemByPartition_r4(p,q,n);
      if(r4==false) {
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
  function issueByPartition(address p,int q,int n) public    {
      bool r17 = updateIssueByPartitionOnInsertRecv_issueByPartition_r17(p,q,n);
      if(r17==false) {
        revert("Rule condition failed");
      }
  }
  function transferByPartition(address s,address r,int q,int n) public    {
      bool r16 = updateTransferByPartitionOnInsertRecv_transferByPartition_r16(s,r,q,n);
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function updateAllMintOnInsertIssueByPartition_r12(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r3(delta0);
  }
  function updateOwnerOnInsertConstructor_r8() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateIssueByPartitionOnInsertRecv_issueByPartition_r17(address p,int q,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s && n>=0) {
        updateAllMintOnInsertIssueByPartition_r12(n);
        updateTotalMintOnInsertIssueByPartition_r11(p,q,n);
        updateIssueTotalByPartitionOnInsertIssueByPartition_r6(q,n);
        emit IssueByPartition(p,q,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r3(int m) private    {
      totalSupply.n += m;
  }
  function updateBalanceOfByPartitionOnIncrementTotalMint_r14(address p,int q,int n) private    {
      balanceOfByPartition[p][q].n += n;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateRedeemByPartitionOnInsertRecv_redeemByPartition_r4(address p,int q,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOfByPartition_x2 = balanceOfByPartition[p][q].n;
      if(o==s && n<=balanceOfByPartition_x2) {
        updateTotalBurnOnInsertRedeemByPartition_r13(p,q,n);
        updateAllBurnOnInsertRedeemByPartition_r18(n);
        updateRedeemTotalByPartitionOnInsertRedeemByPartition_r0(q,n);
        emit RedeemByPartition(p,q,n);
        return true;
      }
      return false;
  }
  function updateTotalInOnInsertTransferByPartition_r7(address p,int q,int n) private    {
      int delta0 = int(n);
      updateBalanceOfByPartitionOnIncrementTotalIn_r14(p,q,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTotalMintOnInsertIssueByPartition_r11(address p,int q,int n) private    {
      int delta0 = int(n);
      updateBalanceOfByPartitionOnIncrementTotalMint_r14(p,q,delta0);
  }
  function updateTotalBurnOnInsertRedeemByPartition_r13(address p,int q,int n) private    {
      int delta0 = int(n);
      updateBalanceOfByPartitionOnIncrementTotalBurn_r14(p,q,delta0);
  }
  function updateAllBurnOnInsertRedeemByPartition_r18(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r3(delta0);
  }
  function updateRedeemTotalByPartitionOnInsertRedeemByPartition_r0(int q,int n) private    {
      int delta0 = int(n);
      updateTotalSupplyByPartitionOnIncrementRedeemTotalByPartition_r1(q,delta0);
  }
  function updateBalanceOfByPartitionOnIncrementTotalBurn_r14(address p,int q,int m) private    {
      balanceOfByPartition[p][q].n -= m;
  }
  function updateBalanceOfByPartitionOnIncrementTotalIn_r14(address p,int q,int i) private    {
      balanceOfByPartition[p][q].n += i;
  }
  function updateTotalOutOnInsertTransferByPartition_r10(address p,int q,int n) private    {
      int delta0 = int(n);
      updateBalanceOfByPartitionOnIncrementTotalOut_r14(p,q,delta0);
  }
  function updateTotalSupplyByPartitionOnIncrementIssueTotalByPartition_r1(int q,int i) private    {
      totalSupplyByPartition[q].n += i;
  }
  function updateBalanceOfByPartitionOnIncrementTotalOut_r14(address p,int q,int o) private    {
      balanceOfByPartition[p][q].n -= o;
  }
  function updateTotalSupplyByPartitionOnIncrementRedeemTotalByPartition_r1(int q,int r) private    {
      totalSupplyByPartition[q].n -= r;
  }
  function updateTotalSupplyOnIncrementAllBurn_r3(int b) private    {
      totalSupply.n -= b;
  }
  function updateTotalSupplyOnInsertConstructor_r2() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateTransferByPartitionOnInsertRecv_transferByPartition_r16(address s,address r,int q,int n) private   returns (bool) {
      address msgSender = msg.sender;
      int totalSupplyByPartition_x1_1 = totalSupplyByPartition[n].n;
      int balanceOfByPartition_x2_3 = balanceOfByPartition[msgSender][q].n;
      int balanceOfByPartition_x2_0 = balanceOfByPartition[r][n].n;
      int balanceOfByPartition_x2_2 = balanceOfByPartition[s][q].n;
      int balanceOfByPartition_x2_4 = balanceOfByPartition[msgSender][n].n;
      if(0==balanceOfByPartition_x2_2 && 0!=balanceOfByPartition_x2_4 && n<totalSupplyByPartition_x1_1 && balanceOfByPartition_x2_0>0 && q>=balanceOfByPartition_x2_3) {
        updateTotalOutOnInsertTransferByPartition_r10(s,q,n);
        updateTotalInOnInsertTransferByPartition_r7(r,q,n);
        emit TransferByPartition(s,r,q,n);
        return true;
      }
      return false;
  }
  function updateIssueTotalByPartitionOnInsertIssueByPartition_r6(int q,int n) private    {
      int delta0 = int(n);
      updateTotalSupplyByPartitionOnIncrementIssueTotalByPartition_r1(q,delta0);
  }
}