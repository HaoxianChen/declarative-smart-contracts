contract Dai {
  struct AllowanceTuple {
    int n;
    bool _valid;
  }
  struct WardTuple {
    address p;
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
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  WardTuple ward;
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalSupplyTuple totalSupply;
  OwnerTuple owner;
  event Mint(address p,int amount);
  event TransferFrom(address from,address to,address spender,int amount);
  event Rely(address p);
  event Transfer(address from,address to,int amount);
  event InvalidTx();
  event IncreaseAllowance(address o,address s,int n);
  event Burn(address p,int amount);
  constructor() public {
    updateOwnerOnInsertConstructor_r24();
    updateTotalSupplyOnInsertConstructor_r18();
    updateWardOnInsertConstructor_r20();
  }
  function increaseAllowance(address o,address s,int n) public    {
      bool r26 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r26(o,s,n);
      if(r26==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r7 = updateMintOnInsertRecv_mint_r7(p,amount);
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function transfer(address from,address to,int amount) public    {
      bool r11 = updateTransferOnInsertRecv_transfer_r11(from,to,amount);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r16 = updateTransferFromOnInsertRecv_transferFrom_r16(from,to,spender,amount);
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function rely(address p) public    {
      bool r4 = updateRelyOnInsertRecv_rely_r4(p);
      if(r4==false) {
        revert("Rule condition failed");
      }
  }
  function getWard() public view  returns (address) {
      address p = ward.p;
      return p;
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function burn(address p,int amount) public    {
      bool r25 = updateBurnOnInsertRecv_burn_r25(p,amount);
      if(r25==false) {
        revert("Rule condition failed");
      }
  }
  function updateBalanceOfOnIncrementTotalBurn_r12(address p,int b) private    {
      balanceOf[p].n -= b;
  }
  function updateTotalSupplyOnIncrementAllMint_r22(int m) private    {
      totalSupply.n += m;
  }
  function updateAllBurnOnInsertBurn_r21(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r22(delta0);
  }
  function updateBalanceOfOnIncrementTotalOut_r12(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateRelyOnInsertRecv_rely_r4(address p) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        updateWardOnInsertRely_r17(p);
        emit Rely(p);
        return true;
      }
      return false;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r16(address o,address r,address s,int n) private   returns (bool) {
      int allowance_x2 = allowance[o][s].n;
      if(n<allowance_x2) {
        updateSpentTotalOnInsertTransferFrom_r9(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertRecv_transfer_r11(address s,address r,int n) private   returns (bool) {
      int balanceOf_x1_1 = balanceOf[s].n;
      if(n>0 && n<=balanceOf_x1_1) {
        updateTotalOutOnInsertTransfer_r10(s,n);
        updateTotalInOnInsertTransfer_r1(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalMint_r12(address p,int m) private    {
      balanceOf[p].n += m;
  }
  function updateMintOnInsertRecv_mint_r7(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address w = ward.p;
      if(w==s && n>0) {
        updateAllMintOnInsertMint_r0(n);
        updateTotalMintOnInsertMint_r23(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateSpentTotalOnInsertTransferFrom_r9(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r27(o,s,delta0);
  }
  function updateTotalSupplyOnInsertConstructor_r18() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTotalMintOnInsertMint_r23(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r12(p,delta0);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r27(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateWardOnInsertRely_r17(address p) private    {
      ward = WardTuple(p,true);
  }
  function updateAllowanceTotalOnInsertIncreaseAllowance_r8(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r27(o,s,delta0);
  }
  function updateOwnerOnInsertConstructor_r24() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r26(address o,address s,int d) private   returns (bool) {
      if(d>0) {
        updateAllowanceTotalOnInsertIncreaseAllowance_r8(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateBurnOnInsertRecv_burn_r25(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address w = ward.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(w==s && n<=balanceOf_x1) {
        updateTotalBurnOnInsertBurn_r2(p,n);
        updateAllBurnOnInsertBurn_r21(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateWardOnInsertConstructor_r20() private    {
      address s = msg.sender;
      ward = WardTuple(s,true);
  }
  function updateTotalSupplyOnIncrementAllBurn_r22(int b) private    {
      totalSupply.n -= b;
  }
  function updateTotalBurnOnInsertBurn_r2(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r12(p,delta0);
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r22(delta0);
  }
  function updateBalanceOfOnIncrementTotalIn_r12(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateTotalOutOnInsertTransfer_r10(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r12(p,delta0);
  }
  function updateAllowanceOnIncrementSpentTotal_r27(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTotalInOnInsertTransfer_r1(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r12(p,delta0);
  }
}