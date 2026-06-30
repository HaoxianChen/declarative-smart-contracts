contract Erc20capped {
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
  event Burn(address p,int amount);
  event IncreaseAllowance(address p,address s,int n);
  event Transfer(address from,address to,int amount);
  event InvalidTx();
  constructor(int cap) public {
    updateCapOnInsertConstructor_r4(cap);
    updateOwnerOnInsertConstructor_r21();
    updateTotalSupplyOnInsertConstructor_r11();
    updateTotalBalancesOnInsertConstructor_r8();
  }
  function getCap() public view  returns (int) {
      int n = cap.n;
      return n;
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function increaseAllowance(address p,address s,int n) public    {
      bool r39 = updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r39(p,s,n);
      if(r39==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,int amount) public    {
      bool r41 = updateTransferOnInsertRecv_transfer_r41(from,to,amount);
      if(r41==false) {
        revert("Rule condition failed");
      }
  }
  function mint(address p,int amount) public    {
      bool r16 = updateMintOnInsertRecv_mint_r16(p,amount);
      if(r16==false) {
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
  function transferFrom(address from,address to,address spender,int amount) public    {
      bool r34 = updateTransferFromOnInsertRecv_transferFrom_r34(from,to,spender,amount);
      if(r34==false) {
        revert("Rule condition failed");
      }
  }
  function burn(address p,int amount) public    {
      bool r42 = updateBurnOnInsertRecv_burn_r42(p,amount);
      if(r42==false) {
        revert("Rule condition failed");
      }
  }
  function updateBurnOnInsertRecv_burn_r42(address p,int n) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      int balanceOf_x1 = balanceOf[p].n;
      if(o==s && n<=balanceOf_x1) {
        updateParticipantOnInsertBurn_r28(p);
        updateAllBurnOnInsertBurn_r18(n);
        updateBurnEntryOnInsertBurn_r2(p,n);
        emit Burn(p,n);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnIncrementTotalBurn_r6(address p,int b) private    {
      balanceOf[p].n -= b;
  }
  function updateTotalSupplyOnInsertConstructor_r11() private    {
      totalSupply = TotalSupplyTuple(0,true);
  }
  function updateMintEntryOnInsertParticipant_r1(address p) private    {
      updateTotalMintOnInsertMintEntry_r26(p,int(0));
  }
  function updateTotalBurnOnInsertBurnEntry_r5(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalBurn_r6(p,delta0);
  }
  function updateParticipantOnInsertTransfer_r38(address p) private    {
      updateOutEntryOnInsertParticipant_r20(p);
      updateBurnEntryOnInsertParticipant_r12(p);
      updateInEntryOnInsertParticipant_r36(p);
      updateMintEntryOnInsertParticipant_r1(p);
  }
  function updateTransferOnInsertTransferFrom_r35(address o,address r,int n) private    {
      updateInEntryOnInsertTransfer_r17(r,n);
      updateOutEntryOnInsertTransfer_r29(o,n);
      updateParticipantOnInsertTransfer_r38(r);
      updateParticipantOnInsertTransfer_r22(o);
      emit Transfer(o,r,n);
  }
  function updateAllowanceEntryOnInsertIncreaseAllowance_r46(address o,address s,int n) private    {
      updateAllowanceTotalOnInsertAllowanceEntry_r45(o,s,n);
  }
  function updateAllowanceParticipantOnInsertTransferFrom_r32(address o,address s) private    {
      updateSpentEntryOnInsertAllowanceParticipant_r31(o,s);
      updateAllowanceEntryOnInsertAllowanceParticipant_r30(o,s);
  }
  function updateParticipantOnInsertBurn_r28(address p) private    {
      updateOutEntryOnInsertParticipant_r20(p);
      updateBurnEntryOnInsertParticipant_r12(p);
      updateInEntryOnInsertParticipant_r36(p);
      updateMintEntryOnInsertParticipant_r1(p);
  }
  function updateTotalBalancesOnInsertConstructor_r8() private    {
      // Empty()
  }
  function updateSpentTotalOnInsertSpentEntry_r47(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementSpentTotal_r43(o,s,delta0);
  }
  function updateBurnEntryOnInsertParticipant_r12(address p) private    {
      updateTotalBurnOnInsertBurnEntry_r5(p,int(0));
  }
  function updateAllMintOnInsertMint_r0(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllMint_r19(delta0);
  }
  function updateBalanceOfOnIncrementTotalIn_r6(address p,int i) private    {
      balanceOf[p].n += i;
  }
  function updateParticipantOnInsertTransfer_r22(address p) private    {
      updateOutEntryOnInsertParticipant_r20(p);
      updateBurnEntryOnInsertParticipant_r12(p);
      updateInEntryOnInsertParticipant_r36(p);
      updateMintEntryOnInsertParticipant_r1(p);
  }
  function updateAllowanceParticipantOnInsertIncreaseAllowance_r24(address o,address s) private    {
      updateSpentEntryOnInsertAllowanceParticipant_r31(o,s);
      updateAllowanceEntryOnInsertAllowanceParticipant_r30(o,s);
  }
  function updateInEntryOnInsertTransfer_r17(address p,int n) private    {
      updateTotalInOnInsertInEntry_r10(p,n);
  }
  function updateSpentEntryOnInsertAllowanceParticipant_r31(address o,address s) private    {
      updateSpentTotalOnInsertSpentEntry_r47(o,s,int(0));
  }
  function updateTransferOnInsertRecv_transfer_r41(address s,address r,int n) private   returns (bool) {
      int allowance_x2 = allowance[r][s].n;
      if(n>0 && 0!=allowance_x2) {
        updateInEntryOnInsertTransfer_r17(r,n);
        updateParticipantOnInsertTransfer_r38(r);
        updateOutEntryOnInsertTransfer_r29(s,n);
        updateParticipantOnInsertTransfer_r22(s);
        emit Transfer(s,r,n);
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
  function updateTotalInOnInsertInEntry_r10(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalIn_r6(p,delta0);
  }
  function updateTotalSupplyOnIncrementAllMint_r19(int m) private    {
      totalSupply.n += m;
  }
  function updateOutEntryOnInsertParticipant_r20(address p) private    {
      updateTotalOutOnInsertOutEntry_r25(p,int(0));
  }
  function updateTotalSupplyOnIncrementAllBurn_r19(int b) private    {
      totalSupply.n -= b;
  }
  function updateTransferFromOnInsertRecv_transferFrom_r34(address o,address r,address s,int n) private   returns (bool) {
      int cap_n = cap.n;
      if(n>0 && 0!=cap_n) {
        updateSpentEntryOnInsertTransferFrom_r44(o,s,n);
        updateAllowanceParticipantOnInsertTransferFrom_r32(o,s);
        updateTransferOnInsertTransferFrom_r35(o,r,n);
        emit TransferFrom(o,r,s,n);
        return true;
      }
      return false;
  }
  function updateSpentEntryOnInsertTransferFrom_r44(address o,address s,int n) private    {
      updateSpentTotalOnInsertSpentEntry_r47(o,s,n);
  }
  function updateCapOnInsertConstructor_r4(int c) private    {
      cap = CapTuple(c,true);
  }
  function updateAllowanceOnIncrementAllowanceTotal_r43(address o,address s,int m) private    {
      allowance[o][s].n += m;
  }
  function updateParticipantOnInsertMint_r37(address p) private    {
      updateOutEntryOnInsertParticipant_r20(p);
      updateBurnEntryOnInsertParticipant_r12(p);
      updateInEntryOnInsertParticipant_r36(p);
      updateMintEntryOnInsertParticipant_r1(p);
  }
  function updateAllowanceTotalOnInsertAllowanceEntry_r45(address o,address s,int n) private    {
      int delta0 = int(n);
      updateAllowanceOnIncrementAllowanceTotal_r43(o,s,delta0);
  }
  function updateAllBurnOnInsertBurn_r18(int n) private    {
      int delta0 = int(n);
      updateTotalSupplyOnIncrementAllBurn_r19(delta0);
  }
  function updateOutEntryOnInsertTransfer_r29(address p,int n) private    {
      updateTotalOutOnInsertOutEntry_r25(p,n);
  }
  function updateTotalMintOnInsertMintEntry_r26(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalMint_r6(p,delta0);
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateBalanceOfOnIncrementTotalMint_r6(address p,int m) private    {
      balanceOf[p].n += m;
  }
  function updateAllowanceEntryOnInsertAllowanceParticipant_r30(address o,address s) private    {
      updateAllowanceTotalOnInsertAllowanceEntry_r45(o,s,int(0));
  }
  function updateInEntryOnInsertParticipant_r36(address p) private    {
      updateTotalInOnInsertInEntry_r10(p,int(0));
  }
  function updateBurnEntryOnInsertBurn_r2(address p,int n) private    {
      updateTotalBurnOnInsertBurnEntry_r5(p,n);
  }
  function updateMintEntryOnInsertMint_r3(address p,int n) private    {
      updateTotalMintOnInsertMintEntry_r26(p,n);
  }
  function updateOwnerOnInsertConstructor_r21() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateBalanceOfOnIncrementTotalOut_r6(address p,int o) private    {
      balanceOf[p].n -= o;
  }
  function updateAllowanceOnIncrementSpentTotal_r43(address o,address s,int l) private    {
      allowance[o][s].n -= l;
  }
  function updateTotalOutOnInsertOutEntry_r25(address p,int n) private    {
      int delta0 = int(n);
      updateBalanceOfOnIncrementTotalOut_r6(p,delta0);
  }
  function updateMintOnInsertRecv_mint_r16(address p,int n) private   returns (bool) {
      address s_0 = msg.sender;
      address o_0 = owner.p;
      int s_1 = totalSupply.n;
      int c_1 = cap.n;
      if(o_0==s_0 && n+s_1<=c_1 && n>=0) {
        updateAllMintOnInsertMint_r0(n);
        updateParticipantOnInsertMint_r37(p);
        updateMintEntryOnInsertMint_r3(p,n);
        emit Mint(p,n);
        return true;
      }
      return false;
  }
  function updateIncreaseAllowanceOnInsertRecv_increaseAllowance_r39(address o,address s,int d) private   returns (bool) {
      if(d>0) {
        updateAllowanceEntryOnInsertIncreaseAllowance_r46(o,s,d);
        updateAllowanceParticipantOnInsertIncreaseAllowance_r24(o,s);
        emit IncreaseAllowance(o,s,d);
        return true;
      }
      return false;
  }
}