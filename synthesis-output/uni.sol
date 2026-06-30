contract Uni {
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
  struct AllMintTuple {
    int n;
    bool _valid;
  }
  struct MinterTuple {
    address p;
    bool _valid;
  }
  mapping(address=>mapping(address=>AllowanceTuple)) allowance;
  mapping(address=>BalanceOfTuple) balanceOf;
  TotalSupplyTuple totalSupply;
  AllMintTuple allMint;
  MinterTuple minter;
  event Mint(address p,int amount);
  event TransferFrom(address from,address to,address spender,int amount);
  event SetMinter(address p);
  event IncreaseAllowance(address o,address s,int n);
  event Transfer(address from,address to,int amount);
  event InvalidTx();
  constructor(address minter,int initialSupply) public {
    updateTotalSupplyOnInsertConstructor_r3(initialSupply);
    updateTotalSupplyOnInsertConstructor_r0(initialSupply);
    updateMinterOnInsertConstructor_r34(minter);
    updateTotalBalancesOnInsertConstructor_r20(initialSupply);
  }
  function getMinter() public view  returns (address) {
      address p = minter.p;
      return p;
  }
  function mint(address p,int amount) public    {
      bool r22 = updateMintOnInsertRecv_mint_r22(p,amount);
      if(r22==false) {
        revert("Rule condition failed");
      }
  }
  function increaseAllowance(address o,address s,int n) public    {
      bool r35 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r35(o,s,n);
      if(r35==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalSupply() public view  returns (int) {
      int n = totalSupply.n;
      return n;
  }
  function setMinter(address p) public    {
      bool r10 = updateSetMinterOnInsertRecv_setMinter_r10(p);
      if(r10==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function transfer(address from,address to,int amount) public    {
      bool r33 = updateTransferOnInsertRecv_transfer_r33(from,to,amount);
      if(r33==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r9 = updateTransferFromOnInsertRecv_transferFrom_r9(from,to,spender,amount);
      if(r9==false) {
        revert("Rule condition failed");
      }
  }
  function getAllowance(address p,address s) public view  returns (int) {
      int n = allowance[p][s].n;
      return n;
  }
  function updateAllowanceParticipantOnInsertIncreaseAllowance_r40(address o,address s) private    {
      updateSpentEntryOnInsertAllowanceParticipant_r27(o,s);
      updateAllowanceEntryOnInsertAllowanceParticipant_r26(o,s);
  }
  function updateOutEntryOnInsertParticipant_r1(address p) private    {
      updateTotalOutOnInsertOutEntry_r23(p,int(0));
  }
  function updateMintEntryOnInsertParticipant_r8(address p) private    {
      updateTotalMintOnInsertMintEntry_r24(p,int(0));
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r35(address o,address s,int d) private   returns (bool) {
      if(d>0) {
        updateAllowanceParticipantOnInsertIncreaseAllowance_r40(o,s);
        updateAllowanceEntryOnInsertIncreaseAllowance_r4(o,s,d);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
  function updateParticipantOnInsertTransfer_r21(address p) private    {
      updateMintEntryOnInsertParticipant_r8(p);
      updateOutEntryOnInsertParticipant_r1(p);
      updateInEntryOnInsertParticipant_r29(p);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r36(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateBalanceOfOnIncrementTotalIn_r7(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateParticipantOnInsertTransfer_r32(address p) private    {
      updateMintEntryOnInsertParticipant_r8(p);
      updateOutEntryOnInsertParticipant_r1(p);
      updateInEntryOnInsertParticipant_r29(p);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateTransferOnInsertRecv_transfer_r33(address s,address r,int n) private   returns (bool) {
      if(n>0) {
        updateParticipantOnInsertTransfer_r32(r);
        updateOutEntryOnInsertTransfer_r25(s,n);
        updateInEntryOnInsertTransfer_r19(r,n);
        updateParticipantOnInsertTransfer_r21(s);
        emit Transfer(s,r,n);
        return true;
      }
      return false;
  }
  function updateMinterOnInsertSetMinter_r16(address p) private    {
      minter = MinterTuple(p,true);
  }
  function updateTotalSupplyOnInsertConstructor_r3(int init) private    {
      int m = allMint.n;
      int n = init+m;
      totalSupply = TotalSupplyTuple(n,true);
  }
  function updateAllMintOnInsertMint_r6(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r3(delta0);
      allMint.n += n;
  }
  function updateTotalOutOnInsertOutEntry_r23(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r7(p,delta0);
  }
  function updateTotalBalancesOnInsertConstructor_r20(int s) private    {
      // Empty()
  }
  function updateInEntryOnInsertTransfer_r19(address p,int n) private    {
      updateTotalInOnInsertInEntry_r11(p,n);
  }
  function updateInEntryOnInsertParticipant_r29(address p) private    {
      updateTotalInOnInsertInEntry_r11(p,int(0));
  }
  function updateTotalSupplyOnIncrementAllMint_r3(int m) private    {
      totalSupply.n += m;
  }
  function updateAllowanceEntryOnInsertAllowanceParticipant_r26(address o,address s) private    {
      updateAllowanceTotalOnInsertAllowanceEntry_r38(o,s,int(0));
  }
  function updateSpentTotalOnInsertSpentEntry_r5(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r36(o,s,delta0);
  }
  function updateBalanceOfOnIncrementTotalMint_r7(address p,int m) private    {
      balanceOf[p].n += m;
  }
  function updateAllowanceParticipantOnInsertTransferFrom_r28(address o,address s) private    {
      updateSpentEntryOnInsertAllowanceParticipant_r27(o,s);
      updateAllowanceEntryOnInsertAllowanceParticipant_r26(o,s);
  }
  function updateAllowanceTotalOnInsertAllowanceEntry_r38(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r36(o,s,delta0);
  }
  function updateSetMinterOnInsertRecv_setMinter_r10(address p) private   returns (bool) {
      address s = msg.sender;
      address m = minter.p;
      if(m==s) {
        updateMinterOnInsertSetMinter_r16(p);
        emit SetMinter(p);
        return true;
      }
      return false;
  }
  function updateSpentEntryOnInsertAllowanceParticipant_r27(address o,address s) private    {
      updateSpentTotalOnInsertSpentEntry_r5(o,s,int(0));
  }
  function updateOutEntryOnInsertTransfer_r25(address p,int n) private    {
      updateTotalOutOnInsertOutEntry_r23(p,n);
  }
  function updateMintEntryOnInsertMint_r2(address p,int n) private    {
      updateTotalMintOnInsertMintEntry_r24(p,n);
  }
  function updateAllowanceOnIncrementSpentTotal_r36(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateParticipantOnInsertMint_r31(address p) private    {
      updateMintEntryOnInsertParticipant_r8(p);
      updateOutEntryOnInsertParticipant_r1(p);
      updateInEntryOnInsertParticipant_r29(p);
  }
  function updateMinterOnInsertConstructor_r34(address m) private    {
      minter = MinterTuple(m,true);
  }
  function updateSpentEntryOnInsertTransferFrom_r37(address o,address s,int n) private    {
      updateSpentTotalOnInsertSpentEntry_r5(o,s,n);
  }
  function updateAllowanceEntryOnInsertIncreaseAllowance_r4(address o,address s,int n) private    {
      updateAllowanceTotalOnInsertAllowanceEntry_r38(o,s,n);
  }
  function updateMintOnInsertRecv_mint_r22(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address m = minter.p;
      if(m==s) {
        updateMintEntryOnInsertMint_r2(p,n);
        updateAllMintOnInsertMint_r6(n);
        updateParticipantOnInsertMint_r31(p);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateTotalSupplyOnInsertConstructor_r0(int s) private    {
      totalSupply = TotalSupplyTuple(s,true);
  }
  function updateTotalMintOnInsertMintEntry_r24(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r7(p,delta0);
  }
  function updateTransferFromOnInsertRecv_transferFrom_r9(address o,address r,address s,int n) private   returns (bool) {
      int totalSupply_n = totalSupply.n;
      if(n>0 && 0!=totalSupply_n) {
        updateSpentEntryOnInsertTransferFrom_r37(o,s,n);
        updateAllowanceParticipantOnInsertTransferFrom_r28(o,s);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalOut_r7(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateTotalInOnInsertInEntry_r11(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r7(p,delta0);
  }
}