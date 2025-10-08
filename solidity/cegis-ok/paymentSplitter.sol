contract PaymentSplitter {
  event Release(address p,uint n);
  function release(address p) public    {
      bool r0 = updateReleaseOnInsertRecv_release_r0(p);
      if(r0==false) {
        revert("Rule condition failed");
      }
  }
  function updateReleaseOnInsertRecv_release_r0(address p) private   returns (bool) {
      updateSendOnInsertRelease_r4(p,n);
      emit Release(p,n);
      return true;
      return false;
  }
  function updateSendOnInsertRelease_r4(address p,uint n) private    {
      payable(p).send(n);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
}