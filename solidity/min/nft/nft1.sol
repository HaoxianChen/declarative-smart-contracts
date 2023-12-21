contract Nft {
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct LatestTransferTuple {
    address from;
    address to;
    uint time;
    bool _valid;
  }
  struct ApprovalTuple {
    bool b;
    bool _valid;
  }
  struct IsApprovedForAllTuple {
    bool b;
    bool _valid;
  }
  struct BalanceOfTuple {
    uint n;
    bool _valid;
  }
  mapping(uint=>LatestTransferTuple) latestTransfer;
  mapping(address=>mapping(uint=>mapping(address=>ApprovalTuple))) approval;
  mapping(address=>mapping(address=>IsApprovedForAllTuple)) isApprovedForAll;
  mapping(address=>BalanceOfTuple) balanceOf;
  OwnerTuple owner;
  event Transfer(uint tokenId,address from,address to,uint time);
  event SetApprovedForAll(address owner,address operator,bool b);
  event SetApproval(address o,uint tokenId,address p,bool b);
  constructor() public {
    updateOwnerOnInsertConstructor_r12();
  }
  function getOwnerOf(uint tokenId) public view  returns (address) {
      address p = ownerOf(tokenId);
      return p;
  }
  function mint(uint tokenId,address to) public    {
      bool r1 = updateTransferOnInsertRecv_mint_r1(tokenId,to);
      if(r1==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address to,uint tokenId) public    {
      bool r11 = updateTransferOnInsertRecv_transfer_r11(to,tokenId);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function getIsApprovedForAll(address owner,address operator) public view  returns (bool) {
      bool b = isApprovedForAll[owner][operator].b;
      return b;
  }
  function getExists(uint tokenId) public view  returns (bool) {
      bool b = exists(tokenId);
      return b;
  }
  function getBalanceOf(address p) public view  returns (uint) {
      uint n = balanceOf[p].n;
      return n;
  }
  function transferFrom(address from,address to,uint tokenId) public    {
      bool r5 = updateTransferOnInsertRecv_transferFrom_r5(from,to,tokenId);
      bool r7 = updateTransferOnInsertRecv_transferFrom_r7(from,to,tokenId);
      if(r5==false && r7==false) {
        revert("Rule condition failed");
      }
  }
  function setApprovalForAll(address operator,bool _approved) public    {
      bool r0 = updateSetApprovedForAllOnInsertRecv_setApprovalForAll_r0(operator,_approved);
      if(r0==false) {
        revert("Rule condition failed");
      }
  }
  function setApproval(uint tokenId,address p,bool b) public    {
      bool r8 = updateSetApprovalOnInsertRecv_setApproval_r8(tokenId,p,b);
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function getApproved(uint tokenId,address p) public view  returns (bool) {
      bool b = approved(tokenId,p);
      return b;
  }
  function burn(uint tokenId) public    {
      bool r14 = updateTransferOnInsertRecv_burn_r14(tokenId);
      if(r14==false) {
        revert("Rule condition failed");
      }
  }
  function updateIsApprovedForAllOnInsertSetApprovedForAll_r3(address p,address o,bool b) private    {
      isApprovedForAll[p][o] = IsApprovedForAllTuple(b,true);
  }
  function updateTransferOnInsertRecv_mint_r1(uint tokenId,address to) private   returns (bool) {
      address s = msg.sender;
      uint time = block.timestamp;
      if(s==owner.p) {
        if(false==exists(tokenId)) {
          if(to!=address(0)) {
            updateLatestTransferOnInsertTransfer_r15(tokenId,address(0),to,time);
            emit Transfer(tokenId,address(0),to,time);
            return true;
          }
        }
      }
      return false;
  }
  function updateSetApprovalOnInsertRecv_setApproval_r8(uint tokenId,address p,bool b) private   returns (bool) {
      address o = msg.sender;
      updateApprovalOnInsertSetApproval_r6(o,tokenId,p,b);
      emit SetApproval(o,tokenId,p,b);
      return true;
      return false;
  }
  function updateTransferOnInsertRecv_transfer_r11(address r,uint tokenId) private   returns (bool) {
      uint time = block.timestamp;
      address s = msg.sender;
      if(r!=address(0)) {
        updateLatestTransferOnInsertTransfer_r15(tokenId,s,r,time);
        emit Transfer(tokenId,s,r,time);
        return true;
      }
      return false;
  }
  function updateTransferOnInsertRecv_transferFrom_r5(address s,address r,uint tokenId) private   returns (bool) {
      uint time = block.timestamp;
      if(true==approved(tokenId,s)) {
        if(r!=address(0)) {
          updateLatestTransferOnInsertTransfer_r15(tokenId,s,r,time);
          emit Transfer(tokenId,s,r,time);
          return true;
        }
      }
      return false;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateApprovalOnInsertSetApproval_r6(address o,uint tokenId,address p,bool b) private    {
      approval[o][tokenId][p] = ApprovalTuple(b,true);
  }
  function exists(uint tokenId) private view  returns (bool) {
      address to = latestTransfer[tokenId].to;
      if(to!=address(0)) {
        return true;
      }
  }
  function updateTransferOnInsertRecv_burn_r14(uint tokenId) private   returns (bool) {
      address s = msg.sender;
      uint time = block.timestamp;
      if(s==owner.p) {
        if(true==exists(tokenId)) {
          address p = ownerOf(tokenId);
          updateLatestTransferOnInsertTransfer_r15(tokenId,p,address(0),time);
          emit Transfer(tokenId,p,address(0),time);
          return true;
        }
      }
      return false;
  }
  function updateOwnerOnInsertConstructor_r12() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateSetApprovedForAllOnInsertRecv_setApprovalForAll_r0(address o,bool b) private   returns (bool) {
      address p = msg.sender;
      updateIsApprovedForAllOnInsertSetApprovedForAll_r3(p,o,b);
      emit SetApprovedForAll(p,o,b);
      return true;
      return false;
  }
  function ownerOf(uint tokenId) private view  returns (address) {
      address p = latestTransfer[tokenId].to;
      if(p!=address(0)) {
        return p;
      }
  }
  function approved(uint tokenId,address p) private view  returns (bool) {
      address o = ownerOf(tokenId);
      bool b = approval[o][tokenId][p].b;
      return b;
  }
  function updateLatestTransferOnInsertTransfer_r15(uint tokenId,address s,address r,uint t) private    {
      uint _max = latestTransfer[tokenId].time;
      if(t>_max) {
        latestTransfer[tokenId] = LatestTransferTuple(s,r,t,true);
      }
  }
  function updateTransferOnInsertRecv_transferFrom_r7(address s,address r,uint tokenId) private   returns (bool) {
      address o = msg.sender;
      uint time = block.timestamp;
      if(true==isApprovedForAll[s][o].b) {
        if(r!=address(0)) {
          updateLatestTransferOnInsertTransfer_r15(tokenId,s,r,time);
          emit Transfer(tokenId,s,r,time);
          return true;
        }
      }
      return false;
  }
}