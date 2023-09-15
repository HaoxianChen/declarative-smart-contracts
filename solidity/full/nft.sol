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
  event Approval(address o,uint tokenId,address p,bool b);
  event IsApprovedForAll(address owner,address operator,bool b);
  constructor() public {
    updateOwnerOnInsertConstructor_r10();
  }
  function getApproved(uint tokenId,address p) public view  returns (bool) {
      bool b = approved[tokenId][p].b;
      return b;
  }
  function getOwnerOf(uint tokenId) public view  returns (address) {
      address p = ownerOf[tokenId].p;
      return p;
  }
  function getIsApprovedForAll(address owner,address operator) public view  returns (bool) {
      bool b = isApprovedForAll[owner][operator].b;
      return b;
  }
  function transferFrom(address from,address to,uint tokenId) public    {
      bool r6 = updateTransferOnInsertRecv_transferFrom_r6(from,to,tokenId);
      bool r7 = updateTransferOnInsertRecv_transferFrom_r7(from,to,tokenId);
      if(r6==false && r7==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address to,uint tokenId) public    {
      bool r9 = updateTransferOnInsertRecv_transfer_r9(to,tokenId);
      if(r9==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (uint) {
      uint n = balanceOf[p].n;
      return n;
  }
  function mint(uint tokenId,address to) public    {
      bool r3 = updateTransferOnInsertRecv_mint_r3(tokenId,to);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function getExists(uint tokenId) public view  returns (bool) {
      bool b = exists[tokenId].b;
      return b;
  }
  function setApprovalForAll(address operator,bool _approved) public    {
      bool r2 = updateIsApprovedForAllOnInsertRecv_setApprovalForAll_r2(operator,_approved);
      if(r2==false) {
        revert("Rule condition failed");
      }
  }
  function burn(uint tokenId) public    {
      bool r12 = updateTransferOnInsertRecv_burn_r12(tokenId);
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function setApproval(uint tokenId,address p,bool b) public    {
      bool r1 = updateApprovalOnInsertRecv_setApproval_r1(tokenId,p,b);
      if(r1==false) {
        revert("Rule condition failed");
      }
  }
  function updateExistsOnDeleteLatestTransfer_r11(uint tokenId,address to) private    {
      if(to!=address(0)) {
        if(true==exists[tokenId].b) {
          exists[tokenId] = ExistsTuple(false,false);
        }
      }
  }
  function updateTransferOnInsertRecv_transfer_r9(address r,uint tokenId) private   returns (bool) {
      uint time = block.timestamp;
      address s = msg.sender;
      if(s==ownerOf[tokenId].p) {
        if(r!=address(0)) {
          updateLatestTransferOnInsertTransfer_r13(tokenId,s,r,time);
          emit Transfer(tokenId,s,r,time);
          return true;
        }
      }
      return false;
  }
  function updateLatestTransferOnInsertTransfer_r13(uint tokenId,address s,address r,uint t) private    {
      uint _max = latestTransfer[tokenId].time;
      if(t>_max) {
        updateExistsOnInsertLatestTransfer_r11(tokenId,r);
        updateOwnerOfOnInsertLatestTransfer_r0(tokenId,r);
        latestTransfer[tokenId] = LatestTransferTuple(s,r,t,true);
      }
  }
  function updateTransferOnInsertRecv_burn_r12(uint tokenId) private   returns (bool) {
      address s = msg.sender;
      uint time = block.timestamp;
      if(s==owner.p) {
        if(true==exists[tokenId].b) {
          address p = ownerOf[tokenId].p;
          updateLatestTransferOnInsertTransfer_r13(tokenId,p,address(0),time);
          emit Transfer(tokenId,p,address(0),time);
          return true;
        }
      }
      return false;
  }
  function updateApprovedOnDeleteOwnerOf_r4(uint tokenId,address o) private    {
      address p = approval[o][tokenId].p;
      bool b = approval[o][tokenId].b;
      if(b==approved[tokenId][p].b) {
        approved[tokenId][p] = ApprovedTuple(false,false);
      }
  }
  function updateTransferOnInsertRecv_mint_r3(uint tokenId,address to) private   returns (bool) {
      address s = msg.sender;
      uint time = block.timestamp;
      if(s==owner.p) {
        if(false==exists[tokenId].b) {
          if(to!=address(0)) {
            updateLatestTransferOnInsertTransfer_r13(tokenId,address(0),to,time);
            emit Transfer(tokenId,address(0),to,time);
            return true;
          }
        }
      }
      return false;
  }
  function updateApprovedOnInsertOwnerOf_r4(uint tokenId,address o) private    {
      OwnerOfTuple memory toDelete = ownerOf[tokenId];
      if(toDelete._valid==true) {
        updateApprovedOnDeleteOwnerOf_r4(tokenId,toDelete.p);
      }
      address p = approval[o][tokenId].p;
      bool b = approval[o][tokenId].b;
      approved[tokenId][p] = ApprovedTuple(b,true);
  }
  function updateOwnerOfOnInsertLatestTransfer_r0(uint tokenId,address p) private    {
      LatestTransferTuple memory toDelete = latestTransfer[tokenId];
      if(toDelete._valid==true) {
        updateOwnerOfOnDeleteLatestTransfer_r0(tokenId,toDelete.to);
      }
      if(p!=address(0)) {
        updateApprovedOnInsertOwnerOf_r4(tokenId,p);
        updateBalanceOfOnInsertOwnerOf_r5(tokenId,p);
        ownerOf[tokenId] = OwnerOfTuple(p,true);
      }
  }
  function updateTransferOnInsertRecv_transferFrom_r7(address s,address r,uint tokenId) private   returns (bool) {
      address o = msg.sender;
      uint time = block.timestamp;
      if(s==ownerOf[tokenId].p) {
        if(true==isApprovedForAll[s][o].b) {
          if(r!=address(0)) {
            updateLatestTransferOnInsertTransfer_r13(tokenId,s,r,time);
            emit Transfer(tokenId,s,r,time);
            return true;
          }
        }
      }
      return false;
  }
  function updateApprovalOnInsertRecv_setApproval_r1(uint tokenId,address p,bool b) private   returns (bool) {
      address o = msg.sender;
      if(o==ownerOf[tokenId].p) {
        updateApprovedOnInsertApproval_r4(o,tokenId,p,b);
        approval[o][tokenId] = ApprovalTuple(p,b,true);
        emit Approval(o,tokenId,p,b);
        return true;
      }
      return false;
  }
  function updateExistsOnInsertLatestTransfer_r11(uint tokenId,address to) private    {
      LatestTransferTuple memory toDelete = latestTransfer[tokenId];
      if(toDelete._valid==true) {
        updateExistsOnDeleteLatestTransfer_r11(tokenId,toDelete.to);
      }
      if(to!=address(0)) {
        exists[tokenId] = ExistsTuple(true,true);
      }
  }
  function updateIsApprovedForAllOnInsertRecv_setApprovalForAll_r2(address o,bool b) private   returns (bool) {
      address p = msg.sender;
      isApprovedForAll[p][o] = IsApprovedForAllTuple(b,true);
      emit IsApprovedForAll(p,o,b);
      return true;
      return false;
  }
  function updateBalanceOfOnDeleteOwnerOf_r5(uint _tokenId0,address p) private    {
      balanceOf[p].n -= 1;
  }
  function updateApprovedOnDeleteApproval_r4(address o,uint tokenId,address p,bool b) private    {
      if(o==ownerOf[tokenId].p) {
        if(b==approved[tokenId][p].b) {
          approved[tokenId][p] = ApprovedTuple(false,false);
        }
      }
  }
  function updateOwnerOnInsertConstructor_r10() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateBalanceOfOnInsertOwnerOf_r5(uint _tokenId0,address p) private    {
      OwnerOfTuple memory toDelete = ownerOf[_tokenId0];
      if(toDelete._valid==true) {
        updateBalanceOfOnDeleteOwnerOf_r5(_tokenId0,toDelete.p);
      }
      balanceOf[p].n += 1;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateOwnerOfOnDeleteLatestTransfer_r0(uint tokenId,address p) private    {
      if(p!=address(0)) {
        updateBalanceOfOnDeleteOwnerOf_r5(tokenId,p);
        updateApprovedOnDeleteOwnerOf_r4(tokenId,p);
        if(p==ownerOf[tokenId].p) {
          ownerOf[tokenId] = OwnerOfTuple(address(address(0)),false);
        }
      }
  }
  function updateApprovedOnInsertApproval_r4(address o,uint tokenId,address p,bool b) private    {
      ApprovalTuple memory toDelete = approval[o][tokenId];
      if(toDelete._valid==true) {
        updateApprovedOnDeleteApproval_r4(o,tokenId,toDelete.p,toDelete.b);
      }
      if(o==ownerOf[tokenId].p) {
        approved[tokenId][p] = ApprovedTuple(b,true);
      }
  }
  function updateTransferOnInsertRecv_transferFrom_r6(address s,address r,uint tokenId) private   returns (bool) {
      uint time = block.timestamp;
      if(true==approved[tokenId][s].b) {
        if(r!=address(0)) {
          updateLatestTransferOnInsertTransfer_r13(tokenId,s,r,time);
          emit Transfer(tokenId,s,r,time);
          return true;
        }
      }
      return false;
  }
}