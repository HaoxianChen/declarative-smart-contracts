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
    updateOwnerOnInsertConstructor_r40();
    updateLastDistributeTimeOnInsertConstructor_r5();
    updateMinIntervalOnInsertConstructor_r46();
    updateDistributeTotalOnInsertConstructor_r42();
    updateTotalBalancesOnInsertConstructor_r32();
    updateTotalSupplyOnInsertConstructor_r22();
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
  function increaseAllowance(address p,address s,int d) public    {
      bool r13 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r13(p,s,d);
      if(r13==false) {
        revert("Rule condition failed");
      }
  }
  function distributeInterests(int n,uint t) public    {
      bool r12 = updateDistributeInterestsOnInsertRecv_distributeInterests_r12(n,t);
      if(r12==false) {
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
  function updateLastDistributeTimeOnInsertConstructor_r5() private    {
      lastDistributeTime = LastDistributeTimeTuple(0,true);
  }
  function updateTransferOnInsertRecv_transfer_r23(address from,address to,int amount) private   returns (bool) {
      int m_2 = balanceOf[from].n;
      if(from!=address(0) && amount<=m_2 && amount>0 && to!=address(0)) {
        bool ok_3 = this.canRecv(to);
        bool ok_1 = this.canSend(from);
        if(ok_1!=false && ok_3!=false) {
          updateTotalOutOnInsertTransfer_r50(from,amount);
          updateTotalInOnInsertTransfer_r15(to,amount);
          emit Transfer(from,to,amount);
          return true;
        }
      }
      return false;
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
  function updateTotalBalancesOnInsertConstructor_r32() private    {
      // Empty()
  }
  function updateMinIntervalOnInsertConstructor_r46() private    {
      minInterval = MinIntervalTuple(0,true);
  }
  function updateDistributeInterestsOnInsertRecv_distributeInterests_r12(int n,uint t) private   returns (bool) {
      address msgSender = msg.sender;
      uint t1_1 = lastDistributeTime.time;
      uint i_1 = minInterval.time;
      int balanceOf_x1 = balanceOf[msgSender].n;
      if(n>0 && t-t1_1>i_1 && n<balanceOf_x1) {
        updateLastDistributeTimeOnInsertDistributeInterests_r33(n,t);
        updateDistributeTotalOnInsertDistributeInterests_r45(n);
        emit DistributeInterests(n,t);
        return true;
      }
      return false;
  }
  function updateOwnerOnInsertConstructor_r40() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateTotalOutOnInsertTransfer_r50(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r29(p,delta0);
  }
  function updateDistributeTotalOnInsertConstructor_r42() private    {
      // Empty()
  }
  function updateTotalMintOnInsertMint_r37(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r29(p,delta0);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r41(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r48(o,s,delta0);
  }
  function updateMintOnInsertRecv_mint_r36(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(amount>0 && p!=address(0) && o_1==s_1) {
        updateTotalMintOnInsertMint_r37(p,amount);
        updateAllMintOnInsertMint_r11(amount);
        emit Mint(p,amount);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r16(int m) private    {
      totalSupply.n += m;
  }
  function updateLastDistributeTimeOnInsertDistributeInterests_r33(int _n0,uint t) private    {
      uint _max = lastDistributeTime.time;
      if(t>_max) {
        lastDistributeTime = LastDistributeTimeTuple(t,true);
      }
  }
  function updateBalanceOfOnIncrementTotalBurn_r29(address p,int m) private    {
      balanceOf[p].n -= m;
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
  function updateBurnOnInsertRecv_burn_r28(address p,int amount) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      int m_2 = balanceOf[p].n;
      if(amount>0 && p!=address(0) && o_1==s_1 && amount<=m_2) {
        updateAllBurnOnInsertBurn_r2(amount);
        updateTotalBurnOnInsertBurn_r0(p,amount);
        emit Burn(p,amount);
        return true;
      }
      return false;
  }
  function updateAllBurnOnInsertBurn_r2(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r16(delta0);
  }
  function updateDistributeTotalOnInsertDistributeInterests_r45(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementDistributeTotal_r16(delta0);
  }
  function updateSpentTotalOnInsertTransferFrom_r44(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r48(o,s,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r13(address p,address s,int d) private   returns (bool) {
      if(d>=0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r41(p,s,d);
        emit IncreaseAllowance(p,s,d);
        return true;
      }
      return false;
  }
  function updateAllMintOnInsertMint_r11(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r16(delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r27(address from,address to,address spender,int amount) private   returns (bool) {
      int m_1 = balanceOf[from].n;
      if(spender!=address(0) && amount<=m_1 && amount>0 && to!=address(0) && from!=address(0)) {
        bool ok_3 = this.canRecv(to);
        bool ok_2 = this.canSend(from);
        if(ok_3!=false && ok_2!=false) {
          updateSpentTotalOnInsertTransferFrom_r44(from,spender,amount);
          updateTransferOnInsertTransferFrom_r30(from,to,amount);
          emit TransferFrom(from,to,spender,amount);
          return true;
        }
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r29(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateTotalSupplyOnInsertConstructor_r22() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateTotalSupplyOnIncrementDistributeTotal_r16(int d) private    {
      totalSupply.n -= d;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r48(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
}