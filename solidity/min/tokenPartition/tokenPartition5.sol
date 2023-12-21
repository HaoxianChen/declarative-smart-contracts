contract TokenPartition {
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct IssueTotalByPartitionTuple {
    uint n;
    bool _valid;
  }
  struct TotalBalancesByPartitionTuple {
    uint m;
    bool _valid;
  }
  struct AllBurnTuple {
    uint n;
    bool _valid;
  }
  struct RedeemTotalByPartitionTuple {
    uint n;
    bool _valid;
  }
  struct AllMintTuple {
    uint n;
    bool _valid;
  }
  struct BalanceOfByPartitionTuple {
    uint n;
    bool _valid;
  }
  mapping(uint=>IssueTotalByPartitionTuple) issueTotalByPartition;
  mapping(uint=>TotalBalancesByPartitionTuple) totalBalancesByPartition;
  AllBurnTuple allBurn;
  mapping(uint=>RedeemTotalByPartitionTuple) redeemTotalByPartition;
  OwnerTuple owner;
  AllMintTuple allMint;
  mapping(address=>mapping(uint=>BalanceOfByPartitionTuple)) balanceOfByPartition;
  event TransferByPartition(address from,address to,uint q,uint amount);
  event IssueByPartition(address p,uint q,uint amount);
  event RedeemByPartition(address p,uint q,uint amount);
  constructor() public {
    updateOwnerOnInsertConstructor_r11();
    updateTotalSupplyOnInsertConstructor_r3();
  }
  function redeemByPartition(address p,uint q,uint amount) public    {
      bool r9 = updateRedeemByPartitionOnInsertRecv_redeemByPartition_r9(p,q,amount);
      if(r9==false) {
        revert("Rule condition failed");
      }
  }
  function issueByPartition(address p,uint q,uint amount) public    {
      bool r1 = updateIssueByPartitionOnInsertRecv_issueByPartition_r1(p,q,amount);
      if(r1==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (uint) {
      uint n = totalSupply();
      return n;
  }
  function transferByPartition(address to,uint q,uint amount) public    {
      bool r17 = updateTransferByPartitionOnInsertRecv_transferByPartition_r17(to,q,amount);
      if(r17==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOfByPartition(address p,uint q) public view  returns (uint) {
      uint n = balanceOfByPartition[p][q].n;
      return n;
  }
  function getTotalSupplyByPartition(uint q) public view  returns (uint) {
      uint n = totalSupplyByPartition(q);
      return n;
  }
  function updateRedeemByPartitionOnInsertRecv_redeemByPartition_r9(address p,uint q,uint n) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        uint m = balanceOfByPartition[p][q].n;
        if(p!=address(0) && n<=m) {
          updateTotalBurnOnInsertRedeemByPartition_r14(p,q,n);
          updateRedeemTotalByPartitionOnInsertRedeemByPartition_r13(q,n);
          updateAllBurnOnInsertRedeemByPartition_r10(n);
          emit RedeemByPartition(p,q,n);
          return true;
        }
      }
      return false;
  }
  function totalSupply() private view  returns (uint) {
      uint b = allBurn.n;
      uint m = allMint.n;
      uint n = m-b;
      return n;
  }
  function updateIssueTotalByPartitionOnInsertIssueByPartition_r5(uint q,uint n) private    {
      issueTotalByPartition[q].n += n;
  }
  function updateRedeemTotalByPartitionOnInsertRedeemByPartition_r13(uint q,uint n) private    {
      redeemTotalByPartition[q].n += n;
  }
  function updateBalanceOfByPartitionOnIncrementTotalMint_r16(address p,uint q,int n) private    {
      int _delta = int(n);
      uint newValue = updateuintByint(balanceOfByPartition[p][q].n,_delta);
      balanceOfByPartition[p][q].n = newValue;
  }
  function totalSupplyByPartition(uint q) private view  returns (uint) {
      uint r = redeemTotalByPartition[q].n;
      uint i = issueTotalByPartition[q].n;
      uint n = i-r;
      return n;
  }
  function updateTotalInOnInsertTransferByPartition_r15(address p,uint q,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfByPartitionOnIncrementTotalIn_r16(p,q,delta0);
  }
  function updateTotalOutOnInsertTransferByPartition_r6(address p,uint q,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfByPartitionOnIncrementTotalOut_r16(p,q,delta0);
  }
  function updateTotalMintOnInsertIssueByPartition_r8(address p,uint q,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfByPartitionOnIncrementTotalMint_r16(p,q,delta0);
  }
  function updateIssueByPartitionOnInsertRecv_issueByPartition_r1(address p,uint q,uint n) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        if(p!=address(0)) {
          updateIssueTotalByPartitionOnInsertIssueByPartition_r5(q,n);
          updateTotalMintOnInsertIssueByPartition_r8(p,q,n);
          updateAllMintOnInsertIssueByPartition_r0(n);
          emit IssueByPartition(p,q,n);
          return true;
        }
      }
      return false;
  }
  function updateTotalBurnOnInsertRedeemByPartition_r14(address p,uint q,uint n) private    {
      int delta0 = int(n);
      updateBalanceOfByPartitionOnIncrementTotalBurn_r16(p,q,delta0);
  }
  function updateTotalSupplyOnInsertConstructor_r3() private    {
      // Empty()
  }
  function updateBalanceOfByPartitionOnIncrementTotalIn_r16(address p,uint q,int i) private    {
      int _delta = int(i);
      uint newValue = updateuintByint(balanceOfByPartition[p][q].n,_delta);
      balanceOfByPartition[p][q].n = newValue;
  }
  function updateTransferByPartitionOnInsertRecv_transferByPartition_r17(address r,uint q,uint n) private   returns (bool) {
      address s = msg.sender;
      uint m = balanceOfByPartition[s][q].n;
      if(n<=m) {
        updateTotalOutOnInsertTransferByPartition_r6(s,q,n);
        updateTotalInOnInsertTransferByPartition_r15(r,q,n);
        emit TransferByPartition(s,r,q,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfByPartitionOnIncrementTotalOut_r16(address p,uint q,int o) private    {
      int _delta = int(-o);
      uint newValue = updateuintByint(balanceOfByPartition[p][q].n,_delta);
      balanceOfByPartition[p][q].n = newValue;
  }
  function updateOwnerOnInsertConstructor_r11() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateAllMintOnInsertIssueByPartition_r0(uint n) private    {
      allMint.n += n;
  }
  function updateAllBurnOnInsertRedeemByPartition_r10(uint n) private    {
      allBurn.n += n;
  }
  function updateBalanceOfByPartitionOnIncrementTotalBurn_r16(address p,uint q,int m) private    {
      int _delta = int(-m);
      uint newValue = updateuintByint(balanceOfByPartition[p][q].n,_delta);
      balanceOfByPartition[p][q].n = newValue;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
}