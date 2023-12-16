contract Nft {
  struct OwnerOfTuple {
    address p;
    bool _valid;
  }
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct ApprovedTuple {
    bool b;
    bool _valid;
  }
  struct LatestTransferTuple {
    address from;
    address to;
    uint time;
    bool _valid;
  }
  struct ApprovalTuple {
    address p;
    bool b;
    bool _valid;
  }
  struct ExistsTuple {
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
  mapping(uint=>OwnerOfTuple) ownerOf;
  OwnerTuple owner;
  mapping(uint=>mapping(address=>ApprovedTuple)) approved;
  mapping(uint=>LatestTransferTuple) latestTransfer;
  mapping(address=>mapping(uint=>ApprovalTuple)) approval;
  mapping(uint=>ExistsTuple) exists;
  mapping(address=>mapping(address=>IsApprovedForAllTuple)) isApprovedForAll;
  mapping(address=>BalanceOfTuple) balanceOf;
  event Transfer(uint tokenId,address from,address to,uint time);
  event SetApprovedForAll(address owner,address operator,bool b);
  event SetApproval(address o,uint tokenId,address p,bool b);
  constructor() public {
    updateOwnerOnInsertConstructor_r12();
  }
  function transfer(address to,uint tokenId) public    {
      bool r11 = updateTransferOnInsertRecv_transfer_r11(to,tokenId);
      if(r11==false) {
        revert("Rule condition failed");
      }
  }
  function getApproved(uint tokenId,address p) public view  returns (bool) {
      bool b = approved[tokenId][p].b;
      return b;
  }
  function burn(uint tokenId) public    {
      bool r14 = updateTransferOnInsertRecv_burn_r14(tokenId);
      if(r14==false) {
        revert("Rule condition failed");
      }
  }
  function getOwnerOf(uint tokenId) public view  returns (address) {
      address p = ownerOf[tokenId].p;
      return p;
  }
  function getIsApprovedForAll(address owner,address operator) public view  returns (bool) {
      bool b = isApprovedForAll[owner][operator].b;
      return b;
  }
  function getBalanceOf(address p) public view  returns (uint) {
      uint n = balanceOf[p].n;
      return n;
  }
  function setApprovalForAll(address operator,bool _approved) public    {
      bool r0 = updateSetApprovedForAllOnInsertRecv_setApprovalForAll_r0(operator,_approved);
      if(r0==false) {
        revert("Rule condition failed");
      }
  }
  function getExists(uint tokenId) public view  returns (bool) {
      bool b = exists[tokenId].b;
      return b;
  }
  function setApproval(uint tokenId,address p,bool b) public    {
      bool r8 = updateSetApprovalOnInsertRecv_setApproval_r8(tokenId,p,b);
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,uint tokenId) public    {
      bool r7 = updateTransferOnInsertRecv_transferFrom_r7(from,to,tokenId);
      bool r5 = updateTransferOnInsertRecv_transferFrom_r5(from,to,tokenId);
      if(r7==false && r5==false) {
        revert("Rule condition failed");
      }
  }
  function mint(uint tokenId,address to) public    {
      bool r1 = updateTransferOnInsertRecv_mint_r1(tokenId,to);
      if(r1==false) {
        revert("Rule condition failed");
      }
  }
  function updateIsApprovedForAllOnInsertSetApprovedForAll_r3(address p,address o,bool b) private    {
      isApprovedForAll[p][o] = IsApprovedForAllTuple(b,true);
  }
  function updateLatestTransferOnInsertTransfer_r15(uint tokenId,address s,address r,uint t) private    {
      uint _max = latestTransfer[tokenId].time;
      if(t>_max) {
        updateExistsOnInsertLatestTransfer_r13(tokenId,r);
        updateOwnerOfOnInsertLatestTransfer_r9(tokenId,r);
        latestTransfer[tokenId] = LatestTransferTuple(s,r,t,true);
      }
  }
  function updateApprovalOnInsertSetApproval_r6(address o,uint tokenId,address p,bool b) private    {
      updateApprovedOnInsertApproval_r2(o,tokenId,p,b);
      approval[o][tokenId] = ApprovalTuple(p,b,true);
  }
  function updateSetApprovedForAllOnInsertRecv_setApprovalForAll_r0(address o,bool b) private   returns (bool) {
      address p = msg.sender;
      updateIsApprovedForAllOnInsertSetApprovedForAll_r3(p,o,b);
      emit SetApprovedForAll(p,o,b);
      return true;
      return false;
  }
  function updateExistsOnDeleteLatestTransfer_r13(uint tokenId,address to) private    {
      if(to!=address(0)) {
        if(true==exists[tokenId].b) {
          exists[tokenId] = ExistsTuple(false,false);
        }
      }
  }
  function updateApprovedOnInsertApproval_r2(address o,uint tokenId,address p,bool b) private    {
      ApprovalTuple memory toDelete = approval[o][tokenId];
      if(toDelete._valid==true) {
        updateApprovedOnDeleteApproval_r2(o,tokenId,toDelete.p,toDelete.b);
      }
      if(o==ownerOf[tokenId].p) {
        approved[tokenId][p] = ApprovedTuple(b,true);
      }
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateSetApprovalOnInsertRecv_setApproval_r8(uint tokenId,address p,bool b) private   returns (bool) {
      address o = msg.sender;
      if(o==ownerOf[tokenId].p) {
        updateApprovalOnInsertSetApproval_r6(o,tokenId,p,b);
        emit SetApproval(o,tokenId,p,b);
        return true;
      }
      return false;
  }
  function updateApprovedOnDeleteApproval_r2(address o,uint tokenId,address p,bool b) private    {
      if(o==ownerOf[tokenId].p) {
        if(b==approved[tokenId][p].b) {
          approved[tokenId][p] = ApprovedTuple(false,false);
        }
      }
  }
  function updateApprovedOnDeleteOwnerOf_r2(uint tokenId,address o) private    {
      address p = approval[o][tokenId].p;
      bool b = approval[o][tokenId].b;
      if(b==approved[tokenId][p].b) {
        approved[tokenId][p] = ApprovedTuple(false,false);
      }
  }
  function updateOwnerOfOnDeleteLatestTransfer_r9(uint tokenId,address p) private    {
      if(p!=address(0)) {
        updateBalanceOfOnDeleteOwnerOf_r4(tokenId,p);
        updateApprovedOnDeleteOwnerOf_r2(tokenId,p);
        if(p==ownerOf[tokenId].p) {
          ownerOf[tokenId] = OwnerOfTuple(address(address(0)),false);
        }
      }
  }
  function updateBalanceOfOnDeleteOwnerOf_r4(uint _tokenId0,address p) private    {
      balanceOf[p].n -= 1;
  }
  function updateTransferOnInsertRecv_burn_r14(uint tokenId) private   returns (bool) {
      address s = msg.sender;
      uint time = block.timestamp;
      if(s==owner.p) {
        if(true==exists[tokenId].b) {
          address p = ownerOf[tokenId].p;
          updateLatestTransferOnInsertTransfer_r15(tokenId,p,address(0),time);
          emit Transfer(tokenId,p,address(0),time);
          return true;
        }
      }
      return false;
  }
  function updateTransferOnInsertRecv_transferFrom_r7(address s,address r,uint tokenId) private   returns (bool) {
      address o = msg.sender;
      uint time = block.timestamp;
      if(s==ownerOf[tokenId].p) {
        if(true==isApprovedForAll[s][o].b) {
          if(r!=address(0)) {
            updateLatestTransferOnInsertTransfer_r15(tokenId,s,r,time);
            emit Transfer(tokenId,s,r,time);
            return true;
          }
        }
      }
      return false;
  }
  function updateTransferOnInsertRecv_transferFrom_r5(address s,address r,uint tokenId) private   returns (bool) {
      uint time = block.timestamp;
      if(true==approved[tokenId][s].b) {
        if(r!=address(0)) {
          updateLatestTransferOnInsertTransfer_r15(tokenId,s,r,time);
          emit Transfer(tokenId,s,r,time);
          return true;
        }
      }
      return false;
  }
  function updateOwnerOfOnInsertLatestTransfer_r9(uint tokenId,address p) private    {
      LatestTransferTuple memory toDelete = latestTransfer[tokenId];
      if(toDelete._valid==true) {
        updateOwnerOfOnDeleteLatestTransfer_r9(tokenId,toDelete.to);
      }
      if(p!=address(0)) {
        updateApprovedOnInsertOwnerOf_r2(tokenId,p);
        updateBalanceOfOnInsertOwnerOf_r4(tokenId,p);
        ownerOf[tokenId] = OwnerOfTuple(p,true);
      }
  }
  function updateBalanceOfOnInsertOwnerOf_r4(uint _tokenId0,address p) private    {
      OwnerOfTuple memory toDelete = ownerOf[_tokenId0];
      if(toDelete._valid==true) {
        updateBalanceOfOnDeleteOwnerOf_r4(_tokenId0,toDelete.p);
      }
      balanceOf[p].n += 1;
  }
  function updateOwnerOnInsertConstructor_r12() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateExistsOnInsertLatestTransfer_r13(uint tokenId,address to) private    {
      LatestTransferTuple memory toDelete = latestTransfer[tokenId];
      if(toDelete._valid==true) {
        updateExistsOnDeleteLatestTransfer_r13(tokenId,toDelete.to);
      }
      if(to!=address(0)) {
        exists[tokenId] = ExistsTuple(true,true);
      }
  }
  function updateTransferOnInsertRecv_transfer_r11(address r,uint tokenId) private   returns (bool) {
      uint time = block.timestamp;
      address s = msg.sender;
      if(s==ownerOf[tokenId].p) {
        if(r!=address(0)) {
          updateLatestTransferOnInsertTransfer_r15(tokenId,s,r,time);
          emit Transfer(tokenId,s,r,time);
          return true;
        }
      }
      return false;
  }
  function updateApprovedOnInsertOwnerOf_r2(uint tokenId,address o) private    {
      OwnerOfTuple memory toDelete = ownerOf[tokenId];
      if(toDelete._valid==true) {
        updateApprovedOnDeleteOwnerOf_r2(tokenId,toDelete.p);
      }
      address p = approval[o][tokenId].p;
      bool b = approval[o][tokenId].b;
      approved[tokenId][p] = ApprovedTuple(b,true);
  }
  function updateTransferOnInsertRecv_mint_r1(uint tokenId,address to) private   returns (bool) {
      address s = msg.sender;
      uint time = block.timestamp;
      if(s==owner.p) {
        if(false==exists[tokenId].b) {
          if(to!=address(0)) {
            updateLatestTransferOnInsertTransfer_r15(tokenId,address(0),to,time);
            emit Transfer(tokenId,address(0),to,time);
            return true;
          }
        }
      }
      return false;
  }
}