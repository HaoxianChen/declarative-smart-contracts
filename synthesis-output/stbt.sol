import "./stbt_udf.sol";
contract Stbt is StbtUDF {
  struct AllowanceTuple {
    int n;
    bool _valid;
  }
  struct TotalSupplyTuple {
    int n;
    bool _valid;
  }
  struct LastDistributeTimeTuple {
    uint time;
    bool _valid;
  }
  struct MinIntervalTuple {
    uint time;
    bool _valid;
  }
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct BalanceOfTuple {
    int n;
    bool _valid;
  }
  LastDistributeTimeTuple lastDistributeTime;
  MinIntervalTuple minInterval;
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalSupplyTuple totalSupply;
  OwnerTuple owner;
  event Mint(address p,int amount);
  event TransferFrom(address from,address to,address spender,int amount);
  event Burn(address p,int amount);
  event IncreaseAllowance(address p,address s,int d);
  event DistributeInterests(int n,uint t);
  event Transfer(address from,address to,int amount);
  event InvalidTx();
  constructor() public {
    updateTotalSupplyOnInsertConstructor_r22();
    updateTotalBalancesOnInsertConstructor_r32();
    updateOwnerOnInsertConstructor_r40();
    updateDistributeTotalOnInsertConstructor_r42();
    updateMinIntervalOnInsertConstructor_r46();
    updateLastDistributeTimeOnInsertConstructor_r6();
  }
  function mint(address p,int amount) public    {
      bool r36 = updateMintOnInsertRecv_mint_r36(p,amount);
      if(r36==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function distributeInterests(int n,uint t) public    {
      bool r1 = updateDistributeInterestsOnInsertRecv_distributeInterests_r1(n,t);
      if(r1==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address p,address s,int d) public    {
      bool r13 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r13(p,s,d);
      if(r13==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function transfer(address from,address to,int amount) public    {
      bool r23 = updateTransferOnInsertRecv_transfer_r23(from,to,amount);
      if(r23==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r28 = updateBurnOnInsertRecv_burn_r28(p,amount);
      if(r28==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r27 = updateTransferFromOnInsertRecv_transferFrom_r27(from,to,spender,amount);
      if(r27==false) {
        revert("Rule condition failed");
      }
  }
  function updateAllMintOnInsertMint_r12(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r16(delta0);
  }
  function updateOwnerOnInsertConstructor_r40() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTotalSupplyOnIncrementAllMint_r16(int m) private    {
      totalSupply.n += m;
  }
  function updateTotalInOnInsertTransfer_r15(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r29(p,delta0);
  }
  function updateTransferOnInsertTransferFrom_r30(address o,address r,int n) private    {
      updateTotalInOnInsertTransfer_r15(r,n);
      updateTotalOutOnInsertTransfer_r50(o,n);
      emit Transfer(o,r,n);
  }
  function updateTotalSupplyOnIncrementAllBurn_r16(int b) private    {
      totalSupply.n -= b;
  }
  function updateLastDistributeTimeOnInsertDistributeInterests_r33(uint t) private    {
      uint _max = lastDistributeTime.time;
      if(t>_max) {
        lastDistributeTime = LastDistributeTimeTuple(m,true);
      }
  }
  function updateTransferFromOnInsertRecv_transferFrom_r27(address from,address to,address spender,int amount) private   returns (bool) {
      int m_1 = balanceOf[o].n;
      bool ok_3 = canRecv(r);
      bool ok_2 = canSend(o);
      if(sp!=address(0) && n<=m_1 && n>0 && ok_3!=false && r!=address(0) && ok_2!=false && o!=address(0)) {
        updateSpentTotalOnInsertTransferFrom_r44(o,sp,n);
        updateTransferOnInsertTransferFrom_r30(o,r,n);
        emit TransferFrom(o,r,sp,n);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertRecv_transfer_r23(address from,address to,int amount) private   returns (bool) {
      int m_2 = balanceOf[s].n;
      bool ok_3 = canRecv(r);
      bool ok_1 = canSend(s);
      if(ok_1!=false && s!=address(0) && n<=m_2 && n>0 && ok_3!=false && r!=address(0)) {
        updateTotalInOnInsertTransfer_r15(r,n);
        updateTotalOutOnInsertTransfer_r50(s,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateTotalMintOnInsertMint_r37(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r29(p,delta0);
  }
  function updateTotalSupplyOnInsertConstructor_r22() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r41(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r48(o,s,delta0);
  }
  function updateBurnOnInsertRecv_burn_r28(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      int m_2 = balanceOf[p].n;
      if(n>0 && p!=address(0) && o_1==s_1 && n<=m_2) {
        updateAllBurnOnInsertBurn_r3(n);
        updateTotalBurnOnInsertBurn_r0(p,n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateTotalBalancesOnInsertConstructor_r32() private    {
      // Empty()
  }
  function updateBalanceOfOnIncrementTotalBurn_r29(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateLastDistributeTimeOnInsertConstructor_r6() private    {
      lastDistributeTime = LastDistributeTimeTuple(0,true);
  }
  function updateAllowanceOnIncrementSpentTotal_r48(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateBalanceOfOnIncrementTotalIn_r29(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateBalanceOfOnIncrementTotalMint_r29(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateTotalBurnOnInsertBurn_r0(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r29(p,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateDistributeTotalOnInsertDistributeInterests_r45(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementDistributeTotal_r16(delta0);
  }
  function updateMintOnInsertRecv_mint_r36(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(n>0 && p!=address(0) && o_1==s_1) {
        updateTotalMintOnInsertMint_r37(p,n);
        updateAllMintOnInsertMint_r12(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r13(address p,address s,int d) private   returns (bool) {
      if(d>=0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r41(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateDistributeTotalOnInsertConstructor_r42() private    {
      // Empty()
  }
  function updateSpentTotalOnInsertTransferFrom_r44(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r48(o,s,delta0);
  }
  function updateMinIntervalOnInsertConstructor_r46() private    {
      minInterval = MinIntervalTuple(0,true);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateBalanceOfOnIncrementTotalOut_r29(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateDistributeInterestsOnInsertRecv_distributeInterests_r1(int n,uint t) private   returns (bool) {
      uint t1_1 = lastDistributeTime.time;
      uint i_1 = minInterval.time;
      if(n>0 && t-t1_1>i_1) {
        updateLastDistributeTimeOnInsertDistributeInterests_r33(t);
        updateDistributeTotalOnInsertDistributeInterests_r45(n);
        emit DistributeInterests(n,t);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementDistributeTotal_r16(int d) private    {
      totalSupply.n -= d;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r48(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateAllBurnOnInsertBurn_r3(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r16(delta0);
  }
  function updateTotalOutOnInsertTransfer_r50(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r29(p,delta0);
  }
}