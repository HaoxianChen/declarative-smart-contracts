contract Nft {
  struct OwnerOfTuple {
    address p;
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
    int n;
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
  mapping(uint=>OwnerOfTuple) ownerOf;
  mapping(uint=>ExistsTuple) exists;
  mapping(address=>mapping(address=>IsApprovedForAllTuple)) isApprovedForAll;
  mapping(address=>BalanceOfTuple) balanceOf;
  OwnerTuple owner;
  mapping(uint=>mapping(address=>ApprovedTuple)) approved;
  event Approved(uint tokenId,address spender,bool b);
  event InvalidTx();
  event DeltaBal(address p,int d);
  event IsApprovedForAll(address owner,address operator,bool b);
  event Exists(uint tokenId,bool b);
  event OwnerOf(uint tokenId,address p);
  constructor() public {
    updateOwnerOnInsertConstructor_r34();
  }
  function burn(uint tokenId,address from,address prevApproved,uint t) public    {
      bool r32 = updateApprovedOnInsertRecv_burn_r32(tokenId,prevApproved);
      bool r31 = updateDeltaBalOnInsertRecv_burn_r31(from);
      bool r26 = updateOwnerOfOnInsertRecv_burn_r26(tokenId);
      bool r12 = updateExistsOnInsertRecv_burn_r12(tokenId);
      if(r32==false && r31==false && r26==false && r12==false) {
        revert("Rule condition failed");
      }
  }
  function setApproval(uint tokenId,address spender,bool b,address owner,address prevApproved,uint t) public    {
      bool r19 = updateApprovedOnInsertRecv_setApproval_r19(tokenId,spender,b);
      bool r15 = updateApprovedOnInsertRecv_setApproval_r15(tokenId,prevApproved);
      if(r19==false && r15==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function getOwnerOf(uint tokenId) public view  returns (address) {
      address p = ownerOf[tokenId].p;
      return p;
  }
  function getIsApprovedForAll(address owner,address operator) public view  returns (bool) {
      bool b = isApprovedForAll[owner][operator].b;
      return b;
  }
  function transfer(address from,address to,uint tokenId,address prevApproved,uint t) public    {
      bool r2 = updateOwnerOfOnInsertRecv_transfer_r2(to,tokenId);
      bool r33 = updateExistsOnInsertRecv_transfer_r33(tokenId);
      bool r6 = updateDeltaBalOnInsertRecv_transfer_r6(to);
      bool r3 = updateDeltaBalOnInsertRecv_transfer_r3(from);
      bool r22 = updateApprovedOnInsertRecv_transfer_r22(tokenId,prevApproved);
      if(r22==false && r2==false && r6==false && r33==false && r3==false) {
        revert("Rule condition failed");
      }
  }
  function getApproved(uint tokenId,address spender) public view  returns (bool) {
      bool b = approved[tokenId][spender].b;
      return b;
  }
  function setApprovalForAll(address owner,address operator,bool b,uint t) public    {
      bool r14 = updateIsApprovedForAllOnInsertRecv_setApprovalForAll_r14(owner,operator,b);
      if(r14==false) {
        revert("Rule condition failed");
      }
  }
  function getExists(uint tokenId) public view  returns (bool) {
      bool b = exists[tokenId].b;
      return b;
  }
  function mint(uint tokenId,address to,uint t) public    {
      bool r9 = updateOwnerOfOnInsertRecv_mint_r9(tokenId,to);
      bool r28 = updateExistsOnInsertRecv_mint_r28(tokenId);
      bool r35 = updateDeltaBalOnInsertRecv_mint_r35(to);
      if(r9==false && r28==false && r35==false) {
        revert("Rule condition failed");
      }
  }
  function transferFrom(address from,address to,uint tokenId,address operator,address prevApproved,uint t) public    {
      bool r17 = updateDeltaBalOnInsertRecv_transferFrom_r17(from);
      bool r7 = updateExistsOnInsertRecv_transferFrom_r7(tokenId);
      bool r5 = updateOwnerOfOnInsertRecv_transferFrom_r5(to,tokenId);
      bool r10 = updateDeltaBalOnInsertRecv_transferFrom_r10(to);
      bool r27 = updateApprovedOnInsertRecv_transferFrom_r27(tokenId,prevApproved);
      if(r10==false && r27==false && r5==false && r7==false && r17==false) {
        revert("Rule condition failed");
      }
  }
  function updateExistsOnInsertRecv_transfer_r33(uint tokenId) private   returns (bool) {
      address s_1 = msg.sender;
      if(to!=address(0) && from!=address(0) && from==s_1) {
        exists[tokenId] = ExistsTuple(true,true);
        emit Exists(tokenId,true);
        return true;
      }
      return false;
  }
  function updateDeltaBalOnInsertRecv_transfer_r3(address from) private   returns (bool) {
      address s_1 = msg.sender;
      if(from!=address(0) && to!=address(0) && from==s_1) {
        updateBalanceOfOnInsertDeltaBal_r13(from,-1);
        emit DeltaBal(from,-1);
        return true;
      }
      return false;
  }
  function updateDeltaBalOnInsertRecv_transfer_r6(address to) private   returns (bool) {
      address s_1 = msg.sender;
      if(to!=address(0) && from!=address(0) && from==s_1) {
        updateBalanceOfOnInsertDeltaBal_r13(to,int(1));
        emit DeltaBal(to,1);
        return true;
      }
      return false;
  }
  function updateApprovedOnInsertRecv_transferFrom_r27(uint tokenId,address prev) private   returns (bool) {
      address s_1 = msg.sender;
      if(prev!=address(0) && to!=address(0) && from!=address(0) && op==s_1) {
        approved[tokenId][prev] = ApprovedTuple(false,true);
        emit Approved(tokenId,prev,false);
        return true;
      }
      return false;
  }
  function updateApprovedOnInsertRecv_setApproval_r15(uint tokenId,address prev) private   returns (bool) {
      address s_1 = msg.sender;
      if(prev!=address(0) && spender!=address(0) && o==s_1) {
        approved[tokenId][prev] = ApprovedTuple(false,true);
        emit Approved(tokenId,prev,false);
        return true;
      }
      return false;
  }
  function updateDeltaBalOnInsertRecv_mint_r35(address to) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(to!=address(0) && o_1==s_1) {
        updateBalanceOfOnInsertDeltaBal_r13(to,int(1));
        emit DeltaBal(to,1);
        return true;
      }
      return false;
  }
  function updateOwnerOfOnInsertRecv_burn_r26(uint tokenId) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(from!=address(0) && o_1==s_1) {
        ownerOf[tokenId] = OwnerOfTuple(address(0),true);
        emit OwnerOf(tokenId,address(0));
        return true;
      }
      return false;
  }
  function updateIsApprovedForAllOnInsertRecv_setApprovalForAll_r14(address owner,address op,bool b) private   returns (bool) {
      address s_1 = msg.sender;
      if(o!=op && op!=address(0) && o==s_1) {
        isApprovedForAll[owner][op] = IsApprovedForAllTuple(b,true);
        emit IsApprovedForAll(owner,op,b);
        return true;
      }
      return false;
  }
  function updateApprovedOnInsertRecv_burn_r32(uint tokenId,address prev) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(prev!=address(0) && from!=address(0) && o_1==s_1) {
        approved[tokenId][prev] = ApprovedTuple(false,true);
        emit Approved(tokenId,prev,false);
        return true;
      }
      return false;
  }
  function updateDeltaBalOnInsertRecv_burn_r31(address from) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(from!=address(0) && o_1==s_1) {
        updateBalanceOfOnInsertDeltaBal_r13(from,-1);
        emit DeltaBal(from,-1);
        return true;
      }
      return false;
  }
  function updateDeltaBalOnInsertRecv_transferFrom_r17(address from) private   returns (bool) {
      address s_1 = msg.sender;
      if(from!=address(0) && to!=address(0) && op==s_1) {
        updateBalanceOfOnInsertDeltaBal_r13(from,-1);
        emit DeltaBal(from,-1);
        return true;
      }
      return false;
  }
  function updateExistsOnInsertRecv_transferFrom_r7(uint tokenId) private   returns (bool) {
      address s_1 = msg.sender;
      if(to!=address(0) && from!=address(0) && op==s_1) {
        exists[tokenId] = ExistsTuple(true,true);
        emit Exists(tokenId,true);
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
  function updateOwnerOfOnInsertRecv_transferFrom_r5(address to,uint tokenId) private   returns (bool) {
      address s_1 = msg.sender;
      if(to!=address(0) && from!=address(0) && op==s_1) {
        ownerOf[tokenId] = OwnerOfTuple(to,true);
        emit OwnerOf(tokenId,to);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnInsertDeltaBal_r13(address p,int d) private    {
      balanceOf[p].n += d;
  }
  function updateOwnerOfOnInsertRecv_mint_r9(uint tokenId,address to) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(to!=address(0) && o_1==s_1) {
        ownerOf[tokenId] = OwnerOfTuple(to,true);
        emit OwnerOf(tokenId,to);
        return true;
      }
      return false;
  }
  function updateApprovedOnInsertRecv_transfer_r22(uint tokenId,address prev) private   returns (bool) {
      address s_1 = msg.sender;
      if(prev!=address(0) && to!=address(0) && from!=address(0) && from==s_1) {
        approved[tokenId][prev] = ApprovedTuple(false,true);
        emit Approved(tokenId,prev,false);
        return true;
      }
      return false;
  }
  function updateOwnerOfOnInsertRecv_transfer_r2(address to,uint tokenId) private   returns (bool) {
      address s_1 = msg.sender;
      if(to!=address(0) && from!=address(0) && from==s_1) {
        ownerOf[tokenId] = OwnerOfTuple(to,true);
        emit OwnerOf(tokenId,to);
        return true;
      }
      return false;
  }
  function updateExistsOnInsertRecv_mint_r28(uint tokenId) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(to!=address(0) && o_1==s_1) {
        exists[tokenId] = ExistsTuple(true,true);
        emit Exists(tokenId,true);
        return true;
      }
      return false;
  }
  function updateExistsOnInsertRecv_burn_r12(uint tokenId) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(from!=address(0) && o_1==s_1) {
        exists[tokenId] = ExistsTuple(false,true);
        emit Exists(tokenId,false);
        return true;
      }
      return false;
  }
  function updateApprovedOnInsertRecv_setApproval_r19(uint tokenId,address spender,bool b) private   returns (bool) {
      address s_1 = msg.sender;
      if(spender!=address(0) && o==s_1) {
        approved[tokenId][spender] = ApprovedTuple(b,true);
        emit Approved(tokenId,spender,b);
        return true;
      }
      return false;
  }
  function updateOwnerOnInsertConstructor_r34() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateDeltaBalOnInsertRecv_transferFrom_r10(address to) private   returns (bool) {
      address s_1 = msg.sender;
      if(to!=address(0) && from!=address(0) && op==s_1) {
        updateBalanceOfOnInsertDeltaBal_r13(to,int(1));
        emit DeltaBal(to,1);
        return true;
      }
      return false;
  }
}