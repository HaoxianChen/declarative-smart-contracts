contract TokenPartition {
  struct TotalSupplyByPartitionTuple {
    uint n;
    bool _valid;
  }
  struct TotalSupplyTuple {
    uint n;
    bool _valid;
  }
  struct BalanceOfByPartitionTuple {
    uint n;
    bool _valid;
  }
  mapping(uint=>TotalSupplyByPartitionTuple) totalSupplyByPartition;
  TotalSupplyTuple totalSupply;
  mapping(address=>mapping(uint=>BalanceOfByPartitionTuple)) balanceOfByPartition;
  event TransferByPartition(address from,address to,uint q,uint amount);
  event IssueByPartition(address p,uint q,uint amount);
  event RedeemByPartition(address p,uint q,uint amount);
  constructor() public {
    updateOwnerOnInsertConstructor_r10();
    updateTotalSupplyOnInsertConstructor_r1();
  }
  function getTotalSupply() public view  returns (uint) {
      uint n = totalSupply.n;
      return n;
  }
  function transferByPartition(address to,uint q,uint amount) public    {
      bool r9 = updateTransferByPartitionOnInsertRecv_transferByPartition_r9(to,q,amount);
      if(r9==false) {
        revert("Rule condition failed");
      }
  }
  function redeemByPartition(address p,uint q,uint amount) public    {
      bool r7 = updateRedeemByPartitionOnInsertRecv_redeemByPartition_r7(p,q,amount);
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function issueByPartition(address p,uint q,uint amount) public    {
      bool r14 = updateIssueByPartitionOnInsertRecv_issueByPartition_r14(p,q,amount);
      if(r14==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupplyByPartition(uint q) public view  returns (uint) {
      uint n = totalSupplyByPartition[q].n;
      return n;
  }
  function getBalanceOfByPartition(address p,uint q) public view  returns (uint) {
      uint n = balanceOfByPartition[p][q].n;
      return n;
  }
  function updateBalanceOfByPartitionOnIncrementTotalOut_r12(address p,uint q,int o) private    {
      int _delta = int(-o);
      uint x_balanceOfByPartition_p_q_n = balanceOfByPartition[p][q].n;
      uint newValue = updateuintByint(x_balanceOfByPartition_p_q_n,_delta);
      balanceOfByPartition[p][q].n = newValue;
  }
  function updateBalanceOfByPartitionOnIncrementTotalBurn_r12(address p,uint q,int m) private    {
      int _delta = int(-m);
      uint x_balanceOfByPartition_p_q_n = balanceOfByPartition[p][q].n;
      uint newValue = updateuintByint(x_balanceOfByPartition_p_q_n,_delta);
      balanceOfByPartition[p][q].n = newValue;
  }
  function updateAllMintOnInsertIssueByPartition_r0(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r5(delta0);
  }
  function updateIssueTotalByPartitionOnInsertIssueByPartition_r3(uint q,uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyByPartitionOnIncrementIssueTotalByPartition_r2(q,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateOwnerOnInsertConstructor_r10() private    {
      address s = msg.sender;
      // Empty()
  }
  function updateBalanceOfByPartitionOnIncrementTotalIn_r12(address p,uint q,int i) private    {
      int _delta = int(i);
      uint x_balanceOfByPartition_p_q_n = balanceOfByPartition[p][q].n;
      uint newValue = updateuintByint(x_balanceOfByPartition_p_q_n,_delta);
      balanceOfByPartition[p][q].n = newValue;
  }
  function updateTotalSupplyByPartitionOnIncrementIssueTotalByPartition_r2(uint q,int i) private    {
      int _delta = int(i);
      uint x_totalSupplyByPartition_q_n = totalSupplyByPartition[q].n;
      uint newValue = updateuintByint(x_totalSupplyByPartition_q_n,_delta);
      totalSupplyByPartition[q].n = newValue;
  }
  function updateIssueByPartitionOnInsertRecv_issueByPartition_r14(address p,uint q,uint n) private   returns (bool) {
      updateIssueTotalByPartitionOnInsertIssueByPartition_r3(q,n);
      updateTotalMintOnInsertIssueByPartition_r6(p,q,n);
      updateAllMintOnInsertIssueByPartition_r0(n);
      emit IssueByPartition(p,q,n);
      return true;
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r1() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateTotalBurnOnInsertRedeemByPartition_r16(address p,uint q,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfByPartitionOnIncrementTotalBurn_r12(p,q,delta0);
  }
  function updateTotalSupplyOnIncrementAllMint_r5(int m) private    {
      int _delta = int(m);
      uint x_totalSupply__n = totalSupply.n;
      uint newValue = updateuintByint(x_totalSupply__n,_delta);
      totalSupply.n = newValue;
  }
  function updateTotalInOnInsertTransferByPartition_r17(address p,uint q,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfByPartitionOnIncrementTotalIn_r12(p,q,delta0);
  }
  function updateTotalMintOnInsertIssueByPartition_r6(address p,uint q,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfByPartitionOnIncrementTotalMint_r12(p,q,delta0);
  }
  function updateTotalSupplyByPartitionOnIncrementRedeemTotalByPartition_r2(uint q,int r) private    {
      int _delta = int(-r);
      uint x_totalSupplyByPartition_q_n = totalSupplyByPartition[q].n;
      uint newValue = updateuintByint(x_totalSupplyByPartition_q_n,_delta);
      totalSupplyByPartition[q].n = newValue;
  }
  function updateAllBurnOnInsertRedeemByPartition_r8(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r5(delta0);
  }
  function updateTotalSupplyOnIncrementAllBurn_r5(int b) private    {
      int _delta = int(-b);
      uint x_totalSupply__n = totalSupply.n;
      uint newValue = updateuintByint(x_totalSupply__n,_delta);
      totalSupply.n = newValue;
  }
  function updateRedeemTotalByPartitionOnInsertRedeemByPartition_r13(uint q,uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyByPartitionOnIncrementRedeemTotalByPartition_r2(q,delta0);
  }
  function updateTransferByPartitionOnInsertRecv_transferByPartition_r9(address r,uint q,uint n) private   returns (bool) {
      updateTotalInOnInsertTransferByPartition_r17(r,q,n);
      updateTotalOutOnInsertTransferByPartition_r4(s,q,n);
      emit TransferByPartition(s,r,q,n);
      return true;
      return false;
  }
  function updateBalanceOfByPartitionOnIncrementTotalMint_r12(address p,uint q,int n) private    {
      int _delta = int(n);
      uint x_balanceOfByPartition_p_q_n = balanceOfByPartition[p][q].n;
      uint newValue = updateuintByint(x_balanceOfByPartition_p_q_n,_delta);
      balanceOfByPartition[p][q].n = newValue;
  }
  function updateTotalOutOnInsertTransferByPartition_r4(address p,uint q,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfByPartitionOnIncrementTotalOut_r12(p,q,delta0);
  }
  function updateRedeemByPartitionOnInsertRecv_redeemByPartition_r7(address p,uint q,uint n) private   returns (bool) {
      updateRedeemTotalByPartitionOnInsertRedeemByPartition_r13(q,n);
      updateTotalBurnOnInsertRedeemByPartition_r16(p,q,n);
      updateAllBurnOnInsertRedeemByPartition_r8(n);
      emit RedeemByPartition(p,q,n);
      return true;
      return false;
  }
}