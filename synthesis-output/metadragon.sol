contract Metadragon {
  struct InitializedTuple {
    bool b;
    bool _valid;
  }
  struct TransformCountTuple {
    uint n;
    bool _valid;
  }
  mapping(address=>TransformCountTuple) transformCount;
  InitializedTuple initialized;
  event InvalidTx();
  event Update(address user,address to,uint value);
  constructor() public {
    updateInitializedOnInsertConstructor_r3();
  }
  function getInitialized() public view  returns (bool) {
      bool b = initialized.b;
      return b;
  }
  function update(address to,uint value) public    {
      bool r1 = updateUpdateOnInsertRecv_update_r1(to,value);
      if(r1==false) {
        revert("Rule condition failed");
      }
  }
  function getTransformCount(address user) public view  returns (uint) {
      uint n = transformCount[user].n;
      return n;
  }
  function updateUpdateOnInsertRecv_update_r1(address to,uint value) private   returns (bool) {
      address t_1 = address(this);
      address user = msg.sender;
      if(value>=30001 && to==t_1) {
        updateTransformCountOnInsertUpdate_r2(user,to,value);
        emit Update(user,to,value);
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
  function updateInitializedOnInsertConstructor_r3() private    {
      initialized = InitializedTuple(true,true);
  }
  function updateTransformCountOnInsertUpdate_r2(address user,address _to1,uint _value2) private    {
      transformCount[user].n += 1;
  }
}