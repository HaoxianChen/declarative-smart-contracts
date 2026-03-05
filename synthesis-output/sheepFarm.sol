import "./sheepFarm_udf.sol";
contract SheepFarm is SheepFarmUDF {
  struct VillageGemsTuple {
    int n;
    bool _valid;
  }
  struct VillageTimestampTuple {
    uint t;
    bool _valid;
  }
  struct TotalSheepsTuple {
    uint n;
    bool _valid;
  }
  struct VillageYieldTuple {
    int y;
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
  mapping(address=>VillageTimestampTuple) villageTimestamp;
  TotalVillagesTuple totalVillages;
  TotalSheepsTuple totalSheeps;
  mapping(address=>VillageNeighborTuple) villageNeighbor;
  mapping(address=>VillageYieldTuple) villageYield;
  mapping(address=>VillageWoolTuple) villageWool;
  event InvalidTx();
  event WithdrawMoney(address user,int amount);
  event UpgradeVillage(address user,uint farm,uint sheep,int yDelta,int cost);
  event AddGems(address user,int gems);
  event CollectMoney(address user,int wool,uint t);
  event Register(address user,address ref,int gems,uint t);
  constructor() public {
    updateTotalVillagesOnInsertConstructor_r0();
    updateTotalSheepsOnInsertConstructor_r11();
  }
  function getTotalVillages() public view  returns (uint) {
      uint n = totalVillages.n;
      return n;
  }
  function getTotalSheeps() public view  returns (uint) {
      uint n = totalSheeps.n;
      return n;
  }
  function getVillageYield(address user) public view  returns (int) {
      int y = villageYield[user].y;
      return y;
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
  function upgradeVillage(uint farm,uint sheep) public    {
      bool r14 = updateUpgradeVillageOnInsertRecv_upgradeVillage_r14(farm,sheep);
      if(r14==false) {
        revert("Rule condition failed");
      }
  }
  function register(address ref) public    {
      bool r9 = updateRegisterOnInsertRecv_register_r9(ref);
      if(r9==false) {
        revert("Rule condition failed");
      }
  }
  function addGems(int gems) public    {
      bool r8 = updateAddGemsOnInsertRecv_addGems_r8(gems);
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function collectMoney() public    {
      bool r7 = updateCollectMoneyOnInsertRecv_collectMoney_r7();
      if(r7==false) {
        revert("Rule condition failed");
      }
  }
  function getVillageTimestamp(address user) public view  returns (uint) {
      uint t = villageTimestamp[user].t;
      return t;
  }
  function getVillageNeighbor(address user) public view  returns (address) {
      address ref = villageNeighbor[user].ref;
      return ref;
  }
  function getVillageWool(address user) public view  returns (int) {
      int n = villageWool[user].n;
      return n;
  }
  function updateVillageWoolOnIncrementWoolEarned_r20(address user,int e) private    {
      villageWool[user].n += e;
  }
  function updateVillageNeighborOnInsertRegister_r21(address user,address ref) private    {
      villageNeighbor[user] = VillageNeighborTuple(ref,true);
  }
  function updateGemIncomeOnInsertAddGems_r15(address user,int g) private    {
      updateGemsEarnedOnInsertGemIncome_r2(user,g);
  }
  function updateWoolEarnedOnInsertCollectMoney_r24(address user,int w) private    {
      int delta0 = int(w);
      updateVillageWoolOnIncrementWoolEarned_r20(user,delta0);
  }
  function updateGemsEarnedOnInsertGemIncome_r2(address user,int g) private    {
      int delta0 = int(g);
      updateVillageGemsOnIncrementGemsEarned_r16(user,delta0);
  }
  function updateVillageGemsOnIncrementGemsEarned_r16(address user,int e) private    {
      villageGems[user].n += e;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateVillageGemsOnIncrementGemsSpent_r16(address user,int sp) private    {
      villageGems[user].n -= sp;
  }
  function updateCollectMoneyOnInsertRecv_collectMoney_r7() private   returns (bool) {
      address msgSender = msg.sender;
      address user = msg.sender;
      uint t = block.timestamp;
      int villageGems_x1 = villageGems[msgSender].n;
      int wool = syncYield(user);
      if(0!=villageGems_x1) {
        updateWoolEarnedOnInsertCollectMoney_r24(user,wool);
        updateVillageTimestampOnInsertCollectMoney_r3(user,t);
        emit CollectMoney(user,wool,t);
        return true;
      }
      return false;
  }
  function updateintByint(int x,int delta) private   returns (int) {
      int newValue = x+delta;
      return newValue;
  }
  function updateWoolSpentOnInsertWithdrawMoney_r22(address user,int a) private    {
      int delta0 = int(a);
      updateVillageWoolOnIncrementWoolSpent_r20(user,delta0);
  }
  function updateTotalVillagesOnInsertConstructor_r0() private    {
      totalVillages = TotalVillagesTuple(0,true);
  }
  function updateTotalSheepsOnInsertConstructor_r11() private    {
      totalSheeps = TotalSheepsTuple(0,true);
  }
  function updateVillageTimestampOnInsertCollectMoney_r3(address user,uint t) private    {
      villageTimestamp[user] = VillageTimestampTuple(t,true);
  }
  function updateTotalSheepsOnInsertUpgradeVillage_r5(address _user0,uint _farm1,uint _sheep2,int _yDelta3,int _cost4) private    {
      totalSheeps.n += 1;
  }
  function updateRegisterOnInsertRecv_register_r9(address ref) private   returns (bool) {
      uint t = block.timestamp;
      address user = msg.sender;
      if(t==villageTimestamp[user].t) {
        if(t==0) {
          updateVillageTimestampOnInsertRegister_r12(user,t);
          updateGemIncomeOnInsertRegister_r19(user,int(10));
          updateTotalVillagesOnInsertRegister_r25(user,ref,int(10),t);
          updateVillageNeighborOnInsertRegister_r21(user,ref);
          emit Register(user,ref,10,t);
          return true;
        }
      }
      return false;
  }
  function updateVillageWoolOnIncrementWoolSpent_r20(address user,int sp) private    {
      villageWool[user].n -= sp;
  }
  function updateTotalVillagesOnInsertRegister_r25(address _user0,address _ref1,int _gems2,uint _t3) private    {
      totalVillages.n += 1;
  }
  function updateAddGemsOnInsertRecv_addGems_r8(int gems) private   returns (bool) {
      address msgSender = msg.sender;
      address user = msg.sender;
      address villageNeighbor_x1 = villageNeighbor[msgSender].ref;
      if(gems>0 && villageNeighbor_x1==msgSender) {
        updateGemIncomeOnInsertAddGems_r15(user,gems);
        emit AddGems(user,gems);
        return true;
      }
      return false;
  }
  function updateUpgradeVillageOnInsertRecv_upgradeVillage_r14(uint farm,uint sheep) private   returns (bool) {
      address msgSender = msg.sender;
      address user = msg.sender;
      address villageNeighbor_x1 = villageNeighbor[msgSender].ref;
      int c = getUpgradePrice(farm,sheep);
      int y = getYield(farm,sheep);
      if(villageNeighbor_x1==msgSender) {
        updateVillageYieldOnInsertUpgradeVillage_r13(user,y);
        updateGemsSpentOnInsertUpgradeVillage_r17(user,c);
        updateTotalSheepsOnInsertUpgradeVillage_r5(user,farm,sheep,y,c);
        emit UpgradeVillage(user,farm,sheep,y,c);
        return true;
      }
      return false;
  }
  function updateGemIncomeOnInsertRegister_r19(address user,int g) private    {
      updateGemsEarnedOnInsertGemIncome_r2(user,g);
  }
  function updateVillageTimestampOnInsertRegister_r12(address user,uint t) private    {
      villageTimestamp[user] = VillageTimestampTuple(t,true);
  }
  function updateWithdrawMoneyOnInsertRecv_withdrawMoney_r1(int amount) private   returns (bool) {
      address msgSender = msg.sender;
      address user = msg.sender;
      address villageNeighbor_x1 = villageNeighbor[msgSender].ref;
      if(amount>0 && villageNeighbor_x1==msgSender) {
        updateWoolSpentOnInsertWithdrawMoney_r22(user,amount);
        emit WithdrawMoney(user,amount);
        return true;
      }
      return false;
  }
  function updateVillageYieldOnInsertUpgradeVillage_r13(address user,int y) private    {
      villageYield[user].y += y;
  }
  function updateGemsSpentOnInsertUpgradeVillage_r17(address user,int c) private    {
      int delta0 = int(c);
      updateVillageGemsOnIncrementGemsSpent_r16(user,delta0);
  }
}