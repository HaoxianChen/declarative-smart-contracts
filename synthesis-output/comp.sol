contract Comp {
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
  struct CapTuple {
    int n;
    bool _valid;
  }
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  CapTuple cap;
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalSupplyTuple totalSupply;
  OwnerTuple owner;
  event Mint(address p,int amount);
  event TransferFrom(address from,address to,address spender,int amount);
  event IncreaseAllowance(address o,address s,int n);
  event Transfer(address from,address to,int amount);
  event InvalidTx();
  constructor(int cap) public {
    updateTotalBalancesOnInsertConstructor_r10();
    updateTotalSupplyOnInsertConstructor_r12();
    updateCapOnInsertConstructor_r34(cap);
    updateOwnerOnInsertConstructor_r22();
  }
  function getCap() public view  returns (int) {
      int n = cap.n;
      return n;
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r6 = updateTransferFromOnInsertRecv_transferFrom_r6(from,to,spender,amount);
      if(r6==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function mint(address p,int amount) public    {
      bool r18 = updateMintOnInsertRecv_mint_r18(p,amount);
      if(r18==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r37 = updateTransferOnInsertRecv_transfer_r37(from,to,amount);
      if(r37==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function increaseAllowance(address o,address s,int n) public    {
      bool r1 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r1(o,s,n);
      if(r1==false) {
        revert("Rule condition failed");
      }
  }
  function updateAllowanceOnIncrementAllowanceTotal_r4(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateBalanceOfOnIncrementTotalIn_r8(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateAllowanceParticipantOnInsertIncreaseAllowance_r39(address o,address s) private    {
      updateSpentEntryOnInsertAllowanceParticipant_r27(o,s);
      updateAllowanceEntryOnInsertAllowanceParticipant_r26(o,s);
  }
  function updateAllMintOnInsertMint_r7(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r14(delta0);
  }
  function updateInEntryOnInsertTransfer_r20(address p,int n) private    {
      updateTotalInOnInsertInEntry_r11(p,n);
  }
  function updateTotalSupplyOnIncrementAllMint_r14(int m) private    {
      totalSupply.n += m;
  }
  function updateAllowanceEntryOnInsertIncreaseAllowance_r33(address o,address s,int n) private    {
      updateAllowanceTotalOnInsertAllowanceEntry_r36(o,s,n);
  }
  function updateOutEntryOnInsertParticipant_r21(address p) private    {
      updateTotalOutOnInsertOutEntry_r23(p,int(0));
  }
  function updateTransferOnInsertRecv_transfer_r37(address s,address r,int n) private   returns (bool) {
      int balanceOf_x1 = balanceOf[s].n;
      if(n>0 && n<=balanceOf_x1) {
        updateInEntryOnInsertTransfer_r20(r,n);
        updateParticipantOnInsertTransfer_r32(r);
        updateOutEntryOnInsertTransfer_r25(s,n);
        updateParticipantOnInsertTransfer_r2(s);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateMintEntryOnInsertMint_r3(address p,int n) private    {
      updateTotalMintOnInsertMintEntry_r24(p,n);
  }
  function updateBalanceOfOnIncrementTotalOut_r8(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateAllowanceOnIncrementSpentTotal_r4(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTotalOutOnInsertOutEntry_r23(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r8(p,delta0);
  }
  function updateMintEntryOnInsertParticipant_r9(address p) private    {
      updateTotalMintOnInsertMintEntry_r24(p,int(0));
  }
  function updateAllowanceTotalOnInsertAllowanceEntry_r36(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r4(o,s,delta0);
  }
  function updateInEntryOnInsertParticipant_r29(address p) private    {
      updateTotalInOnInsertInEntry_r11(p,int(0));
  }
  function updateMintOnInsertRecv_mint_r18(address p,int n) private   returns (bool) {
      address s_0 = msg.sender;
      address o_0 = owner.p;
      int s_1 = totalSupply.n;
      int c_1 = cap.n;
      if(o_0==s_0 && n+s_1<=c_1 && n>=0) {
        updateAllMintOnInsertMint_r7(n);
        updateParticipantOnInsertMint_r31(p);
        updateMintEntryOnInsertMint_r3(p,n);
        emit Mint(p,n);
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
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r1(address o,address s,int d) private   returns (bool) {
      if(d>=0) {
        updateAllowanceEntryOnInsertIncreaseAllowance_r33(o,s,d);
        updateAllowanceParticipantOnInsertIncreaseAllowance_r39(o,s);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r12() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateAllowanceEntryOnInsertAllowanceParticipant_r26(address o,address s) private    {
      updateAllowanceTotalOnInsertAllowanceEntry_r36(o,s,int(0));
  }
  function updateAllowanceParticipantOnInsertTransferFrom_r28(address o,address s) private    {
      updateSpentEntryOnInsertAllowanceParticipant_r27(o,s);
      updateAllowanceEntryOnInsertAllowanceParticipant_r26(o,s);
  }
  function updateParticipantOnInsertTransfer_r2(address p) private    {
      updateMintEntryOnInsertParticipant_r9(p);
      updateInEntryOnInsertParticipant_r29(p);
      updateOutEntryOnInsertParticipant_r21(p);
  }
  function updateParticipantOnInsertMint_r31(address p) private    {
      updateMintEntryOnInsertParticipant_r9(p);
      updateInEntryOnInsertParticipant_r29(p);
      updateOutEntryOnInsertParticipant_r21(p);
  }
  function updateOwnerOnInsertConstructor_r22() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateSpentEntryOnInsertTransferFrom_r35(address o,address s,int n) private    {
      updateSpentTotalOnInsertSpentEntry_r5(o,s,n);
  }
  function updateSpentEntryOnInsertAllowanceParticipant_r27(address o,address s) private    {
      updateSpentTotalOnInsertSpentEntry_r5(o,s,int(0));
  }
  function updateOutEntryOnInsertTransfer_r25(address p,int n) private    {
      updateTotalOutOnInsertOutEntry_r23(p,n);
  }
  function updateTotalInOnInsertInEntry_r11(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r8(p,delta0);
  }
  function updateTotalBalancesOnInsertConstructor_r10() private    {
      // Empty()
  }
  function updateTransferFromOnInsertRecv_transferFrom_r6(address o,address r,address s,int n) private   returns (bool) {
      int allowance_x2 = allowance[o][s].n;
      if(n>0 && n<allowance_x2) {
        updateSpentEntryOnInsertTransferFrom_r35(o,s,n);
        updateAllowanceParticipantOnInsertTransferFrom_r28(o,s);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateParticipantOnInsertTransfer_r32(address p) private    {
      updateMintEntryOnInsertParticipant_r9(p);
      updateInEntryOnInsertParticipant_r29(p);
      updateOutEntryOnInsertParticipant_r21(p);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateCapOnInsertConstructor_r34(int c) private    {
      cap = CapTuple(c,true);
  }
  function updateSpentTotalOnInsertSpentEntry_r5(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r4(o,s,delta0);
  }
  function updateTotalMintOnInsertMintEntry_r24(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r8(p,delta0);
  }
  function updateBalanceOfOnIncrementTotalMint_r8(address p,int m) private    {
      balanceOf[p].n += m;
  }
}