contract TokenPartition {
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct TotalBurnTuple {
    uint n;
    bool _valid;
  }
  struct IssueTotalByPartitionTuple {
    uint n;
    bool _valid;
  }
  struct TotalSupplyTuple {
    uint n;
    bool _valid;
  }
  struct TotalBalancesByPartitionTuple {
    uint m;
    bool _valid;
  }
  struct TotalInTuple {
    uint n;
    bool _valid;
  }
  struct TotalMintTuple {
    uint n;
    bool _valid;
  }
  struct RedeemTotalByPartitionTuple {
    uint n;
    bool _valid;
  }
  struct TotalOutTuple {
    uint n;
    bool _valid;
  }
  mapping(address=>mapping(uint=>TotalOutTuple)) totalOut;
  mapping(address=>mapping(uint=>TotalBurnTuple)) totalBurn;
  mapping(uint=>IssueTotalByPartitionTuple) issueTotalByPartition;
  TotalSupplyTuple totalSupply;
  mapping(uint=>TotalBalancesByPartitionTuple) totalBalancesByPartition;
  mapping(address=>mapping(uint=>TotalInTuple)) totalIn;
  mapping(address=>mapping(uint=>TotalMintTuple)) totalMint;
  mapping(uint=>RedeemTotalByPartitionTuple) redeemTotalByPartition;
  OwnerTuple owner;
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
      uint n = totalSupply.n;
      return n;
  }
  function getBalanceOfByPartition(address p,uint q) public view  returns (uint) {
      uint n = balanceOfByPartition(p,q);
      return n;
  }
  function transferByPartition(address to,uint q,uint amount) public    {
      bool r17 = updateTransferByPartitionOnInsertRecv_transferByPartition_r17(to,q,amount);
      if(r17==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupplyByPartition(uint q) public view  returns (uint) {
      uint n = totalSupplyByPartition(q);
      return n;
  }
  function updateTotalSupplyOnIncrementAllMint_r7(int m) private    {
      int _delta = int(m);
      uint newValue = updateuintByint(totalSupply.n,_delta);
      totalSupply.n = newValue;
  }
  function updateOwnerOnInsertConstructor_r11() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateIssueTotalByPartitionOnInsertIssueByPartition_r5(uint q,uint n) private    {
      issueTotalByPartition[q].n += n;
  }
  function updateRedeemTotalByPartitionOnInsertRedeemByPartition_r13(uint q,uint n) private    {
      redeemTotalByPartition[q].n += n;
  }
  function updateRedeemByPartitionOnInsertRecv_redeemByPartition_r9(address p,uint q,uint n) private   returns (bool) {
      address s = owner.p;
      if(s==msg.sender) {
        uint m = balanceOfByPartition(p,q);
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
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalBurnOnInsertRedeemByPartition_r14(address p,uint q,uint n) private    {
      totalBurn[p][q].n += n;
  }
  function updateTotalInOnInsertTransferByPartition_r15(address p,uint q,uint n) private    {
      totalIn[p][q].n += n;
  }
  function balanceOfByPartition(address p,uint q) private view  returns (uint) {
      uint i = totalIn[p][q].n;
      uint o = totalOut[p][q].n;
      uint m = totalBurn[p][q].n;
      uint n = totalMint[p][q].n;
      uint s = ((n+i)-m)-o;
      return s;
  }
  function updateTotalSupplyOnInsertConstructor_r3() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateAllBurnOnInsertRedeemByPartition_r10(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r7(delta0);
  }
  function updateTransferByPartitionOnInsertRecv_transferByPartition_r17(address r,uint q,uint n) private   returns (bool) {
      address s = msg.sender;
      uint m = balanceOfByPartition(s,q);
      if(n<=m) {
        updateTotalOutOnInsertTransferByPartition_r6(s,q,n);
        updateTotalInOnInsertTransferByPartition_r15(r,q,n);
        emit TransferByPartition(s,r,q,n);
        return true;
      }
      return false;
  }
  function updateTotalMintOnInsertIssueByPartition_r8(address p,uint q,uint n) private    {
      totalMint[p][q].n += n;
  }
  function updateTotalOutOnInsertTransferByPartition_r6(address p,uint q,uint n) private    {
      totalOut[p][q].n += n;
  }
  function totalSupplyByPartition(uint q) private view  returns (uint) {
      uint r = redeemTotalByPartition[q].n;
      uint i = issueTotalByPartition[q].n;
      uint n = i-r;
      return n;
  }
  function updateTotalSupplyOnIncrementAllBurn_r7(int b) private    {
      int _delta = int(-b);
      uint newValue = updateuintByint(totalSupply.n,_delta);
      totalSupply.n = newValue;
  }
  function updateAllMintOnInsertIssueByPartition_r0(uint n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r7(delta0);
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
}