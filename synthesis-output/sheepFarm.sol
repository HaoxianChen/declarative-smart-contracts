import "./sheepFarm_udf.sol";
contract SheepFarm is SheepFarmUDF {
  struct VillageGemsTuple {
    int n;
    bool _valid;
  }
  struct TotalSheepsTuple {
    uint n;
    bool _valid;
  }
  struct TotalVillagesTuple {
    uint n;
    bool _valid;
  }
  struct VillageNeighborTuple {
    address ref;
    bool _valid;
  }
  struct VillageWoolTuple {
    int n;
    bool _valid;
  }
  mapping(address=>VillageGemsTuple) villageGems;
  TotalVillagesTuple totalVillages;
  TotalSheepsTuple totalSheeps;
  mapping(address=>VillageNeighborTuple) villageNeighbor;
  mapping(address=>VillageWoolTuple) villageWool;
  event InvalidTx();
  event WithdrawMoney(address user,int amount);
  event UpgradeVillage(address user,uint farm,uint sheep,int yDelta,int cost);
  event AddGems(address user,int gems);
  event CollectMoney(address user,int wool,uint t);
  event Register(address user,address ref,int gems,uint t);
  constructor() public {
    updateTotalSheepsOnInsertConstructor_r10();
    updateTotalVillagesOnInsertConstructor_r0();
  }
  function upgradeVillage(uint farm,uint sheep) public    {
      bool r13 = updateUpgradeVillageOnInsertRecv_upgradeVillage_r13(farm,sheep);
      if(r13==false) {
        revert("Rule condition failed");
      }
  }
  function getTotalVillages() public view  returns (uint) {
      uint n = totalVillages.n;
      return n;
  }
  function getTotalSheeps() public view  returns (uint) {
      uint n = totalSheeps.n;
      return n;
  }
  function getVillageGems(address user) public view  returns (int) {
      int n = villageGems[user].n;
      return n;
  }
  function withdrawMoney(int amount) public    {
      bool r1 = updateWithdrawMoneyOnInsertRecv_withdrawMoney_r1(amount);
      if(r1==false) {
        revert("Rule condition failed");
      }
  }
  function register(address ref) public    {
      bool r8 = updateRegisterOnInsertRecv_register_r8(ref);
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function collectMoney() public    {
      bool r16 = updateCollectMoneyOnInsertRecv_collectMoney_r16();
      if(r16==false) {
        revert("Rule condition failed");
      }
  }
  function getVillageTimestamp(address user) public view  returns (uint) {
      uint t = villageTimestamp[user].t;
      return t;
  }
  function getVillageYield(address user) public view  returns (int) {
      int y = villageYield[user].y;
      return y;
  }
  function addGems(int gems) public    {
      bool r7 = updateAddGemsOnInsertRecv_addGems_r7(gems);
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function getVillageNeighbor(address user) public view  returns (address) {
      address ref = villageNeighbor[user].ref;
      return ref;
  }
  function getVillageWool(address user) public view  returns (int) {
      int n = villageWool[user].n;
      return n;
  }
  function updateVillageTimestampOnInsertCollectMoney_r3(address user,uint t) private    {
      villageTimestamp[user] = VillageTimestampTuple(t,true);
  }
  function updateVillageWoolOnIncrementWoolEarned_r20(address user,int e) private    {
      villageWool[user].n += e;
  }
  function updateGemIncomeOnInsertAddGems_r14(address user,int g) private    {
      updateGemsEarnedOnInsertGemIncome_r2(user,g);
  }
  function updateWoolEarnedOnInsertCollectMoney_r24(address user,int w) private    {
      int delta0 = int(w);
      updateVillageWoolOnIncrementWoolEarned_r20(user,delta0);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateVillageGemsOnIncrementGemsEarned_r15(address user,int e) private    {
      villageGems[user].n += e;
  }
  function updateVillageYieldOnInsertUpgradeVillage_r12(address user,int y) private    {
      villageYield[user].y += y;
  }
  function updateTotalVillagesOnInsertConstructor_r0() private    {
      totalVillages = TotalVillagesTuple(0,true);
  }
  function updateCollectMoneyOnInsertRecv_collectMoney_r16() private   returns (bool) {
      address msgSender = msg.sender;
      uint t = block.timestamp;
      uint villageTimestamp_x1 = villageTimestamp[msgSender].t;
      if(villageTimestamp_x1>0) {
        int wool = this.syncYield(msgSender);
        updateVillageTimestampOnInsertCollectMoney_r3(msgSender,t);
        updateWoolEarnedOnInsertCollectMoney_r24(msgSender,wool);
        emit CollectMoney(msgSender,wool,t);
        return true;
      }
      return false;
  }
  function updateVillageTimestampOnInsertRegister_r11(address user,uint t) private    {
      villageTimestamp[user] = VillageTimestampTuple(t,true);
  }
  function updateTotalSheepsOnInsertUpgradeVillage_r5(address _user0,uint _farm1,uint _sheep2,int _yDelta3,int _cost4) private    {
      totalSheeps.n += 1;
  }
  function updateGemsEarnedOnInsertGemIncome_r2(address user,int g) private    {
      int delta0 = int(g);
      updateVillageGemsOnIncrementGemsEarned_r15(user,delta0);
  }
  function updateVillageWoolOnIncrementWoolSpent_r20(address user,int sp) private    {
      villageWool[user].n -= sp;
  }
  function updateTotalVillagesOnInsertRegister_r25(address _user0,address _ref1,int _gems2,uint _t3) private    {
      totalVillages.n += 1;
  }
  function updateGemsSpentOnInsertUpgradeVillage_r17(address user,int c) private    {
      int delta0 = int(c);
      updateVillageGemsOnIncrementGemsSpent_r15(user,delta0);
  }
  function updateUpgradeVillageOnInsertRecv_upgradeVillage_r13(uint farm,uint sheep) private   returns (bool) {
      address msgSender = msg.sender;
      address villageNeighbor_x1 = villageNeighbor[msgSender].ref;
      if(villageNeighbor_x1==msgSender) {
        int c = this.getUpgradePrice(farm,sheep);
        int y = this.getYield(farm,sheep);
        updateTotalSheepsOnInsertUpgradeVillage_r5(msgSender,farm,sheep,y,c);
        updateGemsSpentOnInsertUpgradeVillage_r17(msgSender,c);
        updateVillageYieldOnInsertUpgradeVillage_r12(msgSender,y);
        emit UpgradeVillage(msgSender,farm,sheep,y,c);
        return true;
      }
      return false;
  }
  function updateRegisterOnInsertRecv_register_r8(address ref) private   returns (bool) {
      uint t = block.timestamp;
      address user = msg.sender;
      if(t==villageTimestamp[user].t) {
        if(t==0) {
          updateGemIncomeOnInsertRegister_r19(user,int(10));
          updateTotalVillagesOnInsertRegister_r25(user,ref,int(10),t);
          updateVillageTimestampOnInsertRegister_r11(user,t);
          updateVillageNeighborOnInsertRegister_r21(user,ref);
          emit Register(user,ref,10,t);
          return true;
        }
      }
      return false;
  }
  function updateWithdrawMoneyOnInsertRecv_withdrawMoney_r1(int amount) private   returns (bool) {
      address msgSender = msg.sender;
      address villageNeighbor_x1 = villageNeighbor[msgSender].ref;
      if(amount>0 && villageNeighbor_x1==msgSender) {
        updateWoolSpentOnInsertWithdrawMoney_r22(msgSender,amount);
        emit WithdrawMoney(msgSender,amount);
        return true;
      }
      return false;
  }
  function updateAddGemsOnInsertRecv_addGems_r7(int gems) private   returns (bool) {
      address msgSender = msg.sender;
      address villageNeighbor_x1 = villageNeighbor[msgSender].ref;
      if(gems>0 && villageNeighbor_x1==msgSender) {
        updateGemIncomeOnInsertAddGems_r14(msgSender,gems);
        emit AddGems(msgSender,gems);
        return true;
      }
      return false;
  }
  function updateVillageNeighborOnInsertRegister_r21(address user,address ref) private    {
      villageNeighbor[user] = VillageNeighborTuple(ref,true);
  }
  function updateVillageGemsOnIncrementGemsSpent_r15(address user,int sp) private    {
      villageGems[user].n -= sp;
  }
  function updateWoolSpentOnInsertWithdrawMoney_r22(address user,int a) private    {
      int delta0 = int(a);
      updateVillageWoolOnIncrementWoolSpent_r20(user,delta0);
  }
  function updateTotalSheepsOnInsertConstructor_r10() private    {
      totalSheeps = TotalSheepsTuple(0,true);
  }
  function updateGemIncomeOnInsertRegister_r19(address user,int g) private    {
      updateGemsEarnedOnInsertGemIncome_r2(user,g);
  }
}