contract Stbt {
  struct MinIntervalTuple {
    uint time;
    bool _valid;
  }
  struct AllowanceTuple {
    int n;
    bool _valid;
  }
  struct BalanceOfTuple {
    int n;
    bool _valid;
  }
  struct TotalSupplyTuple {
    int n;
    bool _valid;
  }
  struct PermissionExpiryTimeTuple {
    uint t;
    bool _valid;
  }
  struct LastDistributeTimeTuple {
    uint time;
    bool _valid;
  }
  struct PermissionReceiveAllowedTuple {
    bool b;
    bool _valid;
  }
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct PermissionSendAllowedTuple {
    bool b;
    bool _valid;
  }
  mapping(address=>PermissionExpiryTimeTuple) permissionExpiryTime;
  LastDistributeTimeTuple lastDistributeTime;
  mapping(address=>PermissionReceiveAllowedTuple) permissionReceiveAllowed;
  MinIntervalTuple minInterval;
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  mapping(address=>PermissionSendAllowedTuple) permissionSendAllowed;
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
    updateDistributeTotalOnInsertConstructor_r41();
    updateTotalSupplyOnInsertConstructor_r20();
    updateLastDistributeTimeOnInsertConstructor_r4();
    updateMinIntervalOnInsertConstructor_r46();
    updateOwnerOnInsertConstructor_r39();
    updateTotalBalancesOnInsertConstructor_r33();
  }
  function burn(address p,int amount) public    {
      bool r14 = updateBurnOnInsertRecv_burn_r14(p,amount);
      if(r14==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function transfer(address from,address to,int amount) public    {
      bool r16 = updateTransferOnInsertRecv_transfer_r16(from,to,amount);
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function distributeInterests(int n,uint t) public    {
      bool r1 = updateDistributeInterestsOnInsertRecv_distributeInterests_r1(n,t);
      if(r1==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r26 = updateTransferFromOnInsertRecv_transferFrom_r26(from,to,spender,amount);
      if(r26==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r30 = updateMintOnInsertRecv_mint_r30(p,amount);
      if(r30==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function increaseAllowance(address p,address s,int d) public    {
      bool r11 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r11(p,s,d);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function updateTransferOnInsertTransferFrom_r28(address o,address r,int n) private    {
      updateTotalOutOnInsertTransfer_r50(o,n);
      updateTotalInOnInsertTransfer_r13(r,n);
      emit Transfer(o,r,n);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r40(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r47(o,s,delta0);
  }
  function updateAllBurnOnInsertBurn_r23(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r17(delta0);
  }
  function updateBalanceOfOnIncrementTotalMint_r27(address p,int n) private    {
      balanceOf[p].n += n;
  }
  function updateTotalBurnOnInsertBurn_r15(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r27(p,delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r26(address o,address r,address sp,int n) private   returns (bool) {
      bool recvOk_0 = permissionReceiveAllowed[r].b;
      int k_4 = allowance[o][sp].n;
      int m_2 = balanceOf[o].n;
      bool sendOk_1 = permissionSendAllowed[o].b;
      if(r!=address(0) && sp!=address(0) && n<=k_4 && recvOk_0!=false && o!=address(0) && n<=m_2 && sendOk_1!=false) {
        updateSpentTotalOnInsertTransferFrom_r44(o,sp,n);
        updateTransferOnInsertTransferFrom_r28(o,r,n);
        emit TransferFrom(o,r,sp,n);
        return true;
      }
      return false;
  }
  function updateAllowanceOnIncrementAllowanceTotal_r47(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateAllMintOnInsertMint_r10(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r17(delta0);
  }
  function updateBalanceOfOnIncrementTotalOut_r27(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateBalanceOfOnIncrementTotalBurn_r27(address p,int m) private    {
      balanceOf[p].n -= m;
  }
  function updateDistributeTotalOnInsertDistributeInterests_r45(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementDistributeTotal_r17(delta0);
  }
  function updateTotalSupplyOnInsertConstructor_r20() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateTotalInOnInsertTransfer_r13(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r27(p,delta0);
  }
  function updateTotalSupplyOnIncrementAllMint_r17(int m) private    {
      totalSupply.n += m;
  }
  function updateOwnerOnInsertConstructor_r39() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r11(address o,address s,int d) private   returns (bool) {
      if(d>=0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r40(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateLastDistributeTimeOnInsertConstructor_r4() private    {
      lastDistributeTime = LastDistributeTimeTuple(0,true);
  }
  function updateTotalSupplyOnIncrementAllBurn_r17(int b) private    {
      totalSupply.n -= b;
  }
  function updateTotalBalancesOnInsertConstructor_r33() private    {
      // Empty()
  }
  function updateTransferOnInsertRecv_transfer_r16(address s,address r,int n) private   returns (bool) {
      int m_3 = balanceOf[s].n;
      bool sendOk_2 = permissionSendAllowed[s].b;
      bool recvOk_1 = permissionReceiveAllowed[r].b;
      if(sendOk_2!=false && r!=address(0) && recvOk_1!=false && s!=address(0) && n<=m_3) {
        updateTotalOutOnInsertTransfer_r50(s,n);
        updateTotalInOnInsertTransfer_r13(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateMintOnInsertRecv_mint_r30(address p,int n) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(p!=address(0) && o_1==s_1) {
        updateAllMintOnInsertMint_r10(n);
        updateTotalMintOnInsertMint_r37(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateDistributeInterestsOnInsertRecv_distributeInterests_r1(int n,uint t) private   returns (bool) {
      uint t1 = lastDistributeTime.time;
      uint i = minInterval.time;
      if(t1-t>i) {
        updateDistributeTotalOnInsertDistributeInterests_r45(n);
        updateLastDistributeTimeOnInsertDistributeInterests_r34(t);
        emit DistributeInterests(n,t);
        return true;
      }
      return false;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function canRecv(address s) private view  returns (bool) {
      uint t = block.timestamp;
      uint expire = permissionExpiryTime[s].t;
      if(true==permissionReceiveAllowed[s].b) {
        if(t<expire) {
          return true;
        }
      }
      if(0==permissionExpiryTime[s].t) {
        if(true==permissionReceiveAllowed[s].b) {
          return true;
        }
      }
      return false;
  }
  function updateTotalSupplyOnIncrementDistributeTotal_r17(int d) private    {
      totalSupply.n -= d;
  }
  function updateBalanceOfOnIncrementTotalIn_r27(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateAllowanceOnIncrementSpentTotal_r47(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTotalOutOnInsertTransfer_r50(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r27(p,delta0);
  }
  function canSend(address s) private view  returns (bool) {
      uint t = block.timestamp;
      uint expire = permissionExpiryTime[s].t;
      if(true==permissionSendAllowed[s].b) {
        if(t<expire) {
          return true;
        }
      }
      if(0==permissionExpiryTime[s].t) {
        if(true==permissionSendAllowed[s].b) {
          return true;
        }
      }
      return false;
  }
  function updateDistributeTotalOnInsertConstructor_r41() private    {
      // Empty()
  }
  function updateBurnOnInsertRecv_burn_r14(address p,int n) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      int m_2 = balanceOf[p].n;
      if(p!=address(0) && o_1==s_1 && n<=m_2) {
        updateTotalBurnOnInsertBurn_r15(p,n);
        updateAllBurnOnInsertBurn_r23(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateTotalMintOnInsertMint_r37(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r27(p,delta0);
  }
  function updateMinIntervalOnInsertConstructor_r46() private    {
      minInterval = MinIntervalTuple(0,true);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateLastDistributeTimeOnInsertDistributeInterests_r34(uint t) private    {
      uint _max = lastDistributeTime.time;
      if(t>_max) {
        lastDistributeTime = LastDistributeTimeTuple(m,true);
      }
  }
  function updateSpentTotalOnInsertTransferFrom_r44(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r47(o,s,delta0);
  }
}