contract Aave {
  struct AllowanceTuple {
    int n;
    bool _valid;
  }
  struct LendingPoolTuple {
    address p;
    bool _valid;
  }
  struct AllBurnTuple {
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
  struct AllMintTuple {
    int n;
    bool _valid;
  }
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  LendingPoolTuple lendingPool;
  AllBurnTuple allBurn;
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalSupplyTuple totalSupply;
  AllMintTuple allMint;
  event Mint(address p,int amount);
  event TransferFrom(address from,address to,address spender,int amount);
  event Transfer(address from,address to,int amount);
  event InvalidTx();
  event IncreaseAllowance(address o,address s,int n);
  event Burn(address p,int amount);
  constructor(address lendingPool,int initialSupply) public {
    updateTotalBalancesOnInsertConstructor_r1(initialSupply);
    updateTotalSupplyOnInsertConstructor_r19(initialSupply);
    updateLendingPoolOnInsertConstructor_r14(lendingPool);
    updateTotalSupplyOnInsertConstructor_r9(initialSupply);
  }
  function burn(address p,int amount) public    {
      bool r21 = updateBurnOnInsertRecv_burn_r21(p,amount);
      if(r21==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function increaseAllowance(address o,address s,int n) public    {
      bool r2 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r2(o,s,n);
      if(r2==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r29 = updateMintOnInsertRecv_mint_r29(p,amount);
      if(r29==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r13 = updateTransferFromOnInsertRecv_transferFrom_r13(from,to,spender,amount);
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
  function getLendingPool() public view  returns (address) {
      address p = lendingPool.p;
      return p;
  }
  function transfer(address from,address to,int amount) public    {
      bool r38 = updateTransferOnInsertRecv_transfer_r38(from,to,amount);
      if(r38==false) {
        revert("Rule condition failed");
      }
  }
  function updateAllowanceOnIncrementAllowanceTotal_r40(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateAllowanceEntryOnInsertIncreaseAllowance_r7(address o,address s,int n) private    {
      updateAllowanceTotalOnInsertAllowanceEntry_r42(o,s,n);
  }
  function updateInEntryOnInsertParticipant_r6(address p) private    {
      updateTotalInOnInsertInEntry_r16(p,int(0));
  }
  function updateTotalInOnInsertInEntry_r16(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r11(p,delta0);
  }
  function updateAllowanceEntryOnInsertAllowanceParticipant_r32(address o,address s) private    {
      updateAllowanceTotalOnInsertAllowanceEntry_r42(o,s,int(0));
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r2(address o,address s,int d) private   returns (bool) {
      if(d>=0) {
        updateAllowanceEntryOnInsertIncreaseAllowance_r7(o,s,d);
        updateAllowanceParticipantOnInsertIncreaseAllowance_r44(o,s);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r11(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateTransferOnInsertRecv_transfer_r38(address s,address r,int n) private   returns (bool) {
      if(n>0) {
        updateParticipantOnInsertTransfer_r36(r);
        updateParticipantOnInsertTransfer_r4(s);
        updateOutEntryOnInsertTransfer_r31(s,n);
        updateInEntryOnInsertTransfer_r25(r,n);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateParticipantOnInsertMint_r35(address p) private    {
      updateMintEntryOnInsertParticipant_r12(p);
      updateInEntryOnInsertParticipant_r6(p);
      updateBurnEntryOnInsertParticipant_r17(p);
      updateOutEntryOnInsertParticipant_r3(p);
  }
  function updateTotalOutOnInsertOutEntry_r27(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r11(p,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTotalBurnOnInsertBurnEntry_r8(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r11(p,delta0);
  }
  function updateBurnEntryOnInsertBurn_r15(address p,int n) private    {
      updateTotalBurnOnInsertBurnEntry_r8(p,n);
  }
  function updateMintEntryOnInsertParticipant_r12(address p) private    {
      updateTotalMintOnInsertMintEntry_r28(p,int(0));
  }
  function updateAllowanceTotalOnInsertAllowanceEntry_r42(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r40(o,s,delta0);
  }
  function updateAllowanceParticipantOnInsertTransferFrom_r34(address o,address s) private    {
      updateAllowanceEntryOnInsertAllowanceParticipant_r32(o,s);
      updateSpentEntryOnInsertAllowanceParticipant_r33(o,s);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r13(address o,address r,address s,int n) private   returns (bool) {
      int totalSupply_n = totalSupply.n;
      if(n>0 && 0!=totalSupply_n) {
        updateAllowanceParticipantOnInsertTransferFrom_r34(o,s);
        updateSpentEntryOnInsertTransferFrom_r41(o,s,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnIncrementAllMint_r9(int m) private    {
      totalSupply.n += m;
  }
  function updateOutEntryOnInsertTransfer_r31(address p,int n) private    {
      updateTotalOutOnInsertOutEntry_r27(p,n);
  }
  function updateSpentTotalOnInsertSpentEntry_r10(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r40(o,s,delta0);
  }
  function updateMintOnInsertRecv_mint_r29(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address lp = lendingPool.p;
      if(lp==s) {
        updateMintEntryOnInsertMint_r5(p,n);
        updateParticipantOnInsertMint_r35(p);
        updateAllMintOnInsertMint_r0(n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateInEntryOnInsertTransfer_r25(address p,int n) private    {
      updateTotalInOnInsertInEntry_r16(p,n);
  }
  function updateBurnEntryOnInsertParticipant_r17(address p) private    {
      updateTotalBurnOnInsertBurnEntry_r8(p,int(0));
  }
  function updateLendingPoolOnInsertConstructor_r14(address lp) private    {
      lendingPool = LendingPoolTuple(lp,true);
  }
  function updateParticipantOnInsertBurn_r30(address p) private    {
      updateMintEntryOnInsertParticipant_r12(p);
      updateInEntryOnInsertParticipant_r6(p);
      updateBurnEntryOnInsertParticipant_r17(p);
      updateOutEntryOnInsertParticipant_r3(p);
  }
  function updateParticipantOnInsertTransfer_r36(address p) private    {
      updateMintEntryOnInsertParticipant_r12(p);
      updateInEntryOnInsertParticipant_r6(p);
      updateBurnEntryOnInsertParticipant_r17(p);
      updateOutEntryOnInsertParticipant_r3(p);
  }
  function updateOutEntryOnInsertParticipant_r3(address p) private    {
      updateTotalOutOnInsertOutEntry_r27(p,int(0));
  }
  function updateSpentEntryOnInsertAllowanceParticipant_r33(address o,address s) private    {
      updateSpentTotalOnInsertSpentEntry_r10(o,s,int(0));
  }
  function updateTotalSupplyOnInsertConstructor_r9(int init) private    {
      int b = allBurn.n;
      int m = allMint.n;
      int n = (init+m)-b;
      totalSupply = TotalSupplyTuple(n,true);
  }
  function updateBalanceOfOnIncrementTotalMint_r11(address p,int m) private    {
      balanceOf[p].n += m;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateTotalBalancesOnInsertConstructor_r1(int s) private    {
      // Empty()
  }
  function updateTotalSupplyOnInsertConstructor_r19(int s) private    {
      totalSupply = TotalSupplyTuple(s,true);
  }
  function updateSpentEntryOnInsertTransferFrom_r41(address o,address s,int n) private    {
      updateSpentTotalOnInsertSpentEntry_r10(o,s,n);
  }
  function updateParticipantOnInsertTransfer_r4(address p) private    {
      updateMintEntryOnInsertParticipant_r12(p);
      updateInEntryOnInsertParticipant_r6(p);
      updateBurnEntryOnInsertParticipant_r17(p);
      updateOutEntryOnInsertParticipant_r3(p);
  }
  function updateBalanceOfOnIncrementTotalIn_r11(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateAllBurnOnInsertBurn_r26(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r9(delta0);
      allBurn.n += n;
  }
  function updateTotalMintOnInsertMintEntry_r28(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r11(p,delta0);
  }
  function updateTotalSupplyOnIncrementAllBurn_r9(int b) private    {
      totalSupply.n -= b;
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r9(delta0);
      allMint.n += n;
  }
  function updateAllowanceOnIncrementSpentTotal_r40(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateBurnOnInsertRecv_burn_r21(address p,int n) private   returns (bool) {
      address s = msg.sender;
      if(p==s) {
        updateParticipantOnInsertBurn_r30(p);
        updateBurnEntryOnInsertBurn_r15(p,n);
        updateAllBurnOnInsertBurn_r26(n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalBurn_r11(address p,int b) private    {
      balanceOf[p].n -= b;
  }
  function updateMintEntryOnInsertMint_r5(address p,int n) private    {
      updateTotalMintOnInsertMintEntry_r28(p,n);
  }
  function updateAllowanceParticipantOnInsertIncreaseAllowance_r44(address o,address s) private    {
      updateAllowanceEntryOnInsertAllowanceParticipant_r32(o,s);
      updateSpentEntryOnInsertAllowanceParticipant_r33(o,s);
  }
}