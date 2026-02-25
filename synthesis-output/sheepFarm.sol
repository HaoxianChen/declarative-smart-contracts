contract SheepFarm {
  struct VillageNeighborTuple {
    address neighbor;
    bool _valid;
  }
  struct InitializedTuple {
    bool b;
    bool _valid;
  }
  struct NeighborCountTuple {
    uint n;
    bool _valid;
  }
  struct TotalVillagesTuple {
    uint n;
    bool _valid;
  }
  struct VillageGemsTuple {
    uint gems;
    bool _valid;
  }
  struct VillageTimestampTuple {
    uint time;
    bool _valid;
  }
  mapping(address=>VillageNeighborTuple) villageNeighbor;
  mapping(address=>NeighborCountTuple) neighborCount;
  TotalVillagesTuple totalVillages;
  mapping(address=>VillageGemsTuple) villageGems;
  mapping(address=>VillageTimestampTuple) villageTimestamp;
  InitializedTuple initialized;
  event InvalidTx();
  event Register(address user,address neighbor,uint bonus,uint time);
  constructor() public {
    updateInitializedOnInsertConstructor_r7();
  }
  function getTotalVillages() public view  returns (uint) {
      uint n = totalVillages.n;
      return n;
  }
  function getVillageTimestamp(address user) public view  returns (uint) {
      uint time = villageTimestamp[user].time;
      return time;
  }
  function register(address neighbor) public    {
      bool r6 = updateRegisterOnInsertRecv_register_r6(neighbor);
      if(r6==false) {
        revert("Rule condition failed");
      }
  }
  function getVillageNeighbor(address user) public view  returns (address) {
      address neighbor = villageNeighbor[user].neighbor;
      return neighbor;
  }
  function getNeighborCount(address neighbor) public view  returns (uint) {
      uint n = neighborCount[neighbor].n;
      return n;
  }
  function getVillageGems(address user) public view  returns (uint) {
      uint gems = villageGems[user].gems;
      return gems;
  }
  function getInitialized() public view  returns (bool) {
      bool b = initialized.b;
      return b;
  }
  function updateRegisterOnInsertRecv_register_r6(address neighbor) private   returns (bool) {
      uint time = block.timestamp;
      address user = msg.sender;
      uint t = villageTimestamp[user].time;
      if(t==0) {
        updateTotalVillagesOnInsertRegister_r3(user,neighbor,uint(10),time);
        updateVillageNeighborOnInsertRegister_r5(user,neighbor);
        updateVillageTimestampOnInsertRegister_r1(user,time);
        updateNeighborCountOnInsertRegister_r2(user,neighbor,uint(10),time);
        updateVillageGemsOnInsertRegister_r4(user,uint(10));
        emit Register(user,neighbor,10,time);
        return true;
      }
      return false;
  }
  function updateNeighborCountOnInsertRegister_r2(address _user0,address neighbor,uint _bonus2,uint _time3) private    {
      neighborCount[neighbor].n += 1;
  }
  function updateInitializedOnInsertConstructor_r7() private    {
      initialized = InitializedTuple(true,true);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateVillageGemsOnInsertRegister_r4(address user,uint b) private    {
      villageGems[user].gems += b;
  }
  function updateVillageTimestampOnInsertRegister_r1(address user,uint time) private    {
      villageTimestamp[user] = VillageTimestampTuple(time,true);
  }
  function updateTotalVillagesOnInsertRegister_r3(address _user0,address _neighbor1,uint _bonus2,uint _time3) private    {
      totalVillages.n += 1;
  }
  function updateVillageNeighborOnInsertRegister_r5(address user,address neighbor) private    {
      villageNeighbor[user] = VillageNeighborTuple(neighbor,true);
  }
}