import "./nft_udf.sol";
contract Nft is NFTUDF {
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
  struct OwnerTuple {
    address p;
    bool _valid;
  }
  struct ApprovedTuple {
    bool b;
    bool _valid;
  }
  struct BalanceOfTuple {
    int n;
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
    updateOwnerOnInsertConstructor_r33();
  }
  function mint(uint tokenId,address to,uint t) public    {
      bool r25 = updateExistsOnInsertRecv_mint_r25(tokenId,to,t);
      bool r26 = updateOwnerOfOnInsertRecv_mint_r26(tokenId,to,t);
      bool r34 = updateDeltaBalOnInsertRecv_mint_r34(tokenId,to,t);
      if(r25==false && r26==false && r34==false) {
        revert("Rule condition failed");
      }
  }
  function setApprovalForAll(address owner,address operator,bool b,uint t) public    {
      bool r27 = updateIsApprovedForAllOnInsertRecv_setApprovalForAll_r27(owner,operator,b,t);
      if(r27==false) {
        revert("Rule condition failed");
      }
  }
  function getBalanceOf(address p) public view  returns (int) {
      int n = balanceOf[p].n;
      return n;
  }
  function getIsApprovedForAll(address owner,address operator) public view  returns (bool) {
      bool b = isApprovedForAll[owner][operator].b;
      return b;
  }
  function getExists(uint tokenId) public view  returns (bool) {
      bool b = exists[tokenId].b;
      return b;
  }
  function setApproval(uint tokenId,address spender,bool b,address owner,address prevApproved,uint t) public    {
      bool r17 = updateApprovedOnInsertRecv_setApproval_r17(tokenId,spender,b,owner,prevApproved,t);
      bool r15 = updateApprovedOnInsertRecv_setApproval_r15(tokenId,spender,b,owner,prevApproved,t);
      if(r17==false && r15==false) {
        revert("Rule condition failed");
      }
  }
  function transfer(address from,address to,uint tokenId,address prevApproved,uint t) public    {
      bool r3 = updateOwnerOfOnInsertRecv_transfer_r3(from,to,tokenId,prevApproved,t);
      bool r20 = updateApprovedOnInsertRecv_transfer_r20(from,to,tokenId,prevApproved,t);
      bool r4 = updateDeltaBalOnInsertRecv_transfer_r4(from,to,tokenId,prevApproved,t);
      bool r32 = updateExistsOnInsertRecv_transfer_r32(from,to,tokenId,prevApproved,t);
      bool r8 = updateDeltaBalOnInsertRecv_transfer_r8(from,to,tokenId,prevApproved,t);
      if(r8==false && r4==false && r3==false && r32==false && r20==false) {
        revert("Rule condition failed");
      }
  }
  function getOwnerOf(uint tokenId) public view  returns (address) {
      address p = ownerOf[tokenId].p;
      return p;
  }
  function burn(uint tokenId,address from,address prevApproved,uint t) public    {
      bool r30 = updateDeltaBalOnInsertRecv_burn_r30(tokenId,from,prevApproved,t);
      bool r31 = updateApprovedOnInsertRecv_burn_r31(tokenId,from,prevApproved,t);
      bool r24 = updateOwnerOfOnInsertRecv_burn_r24(tokenId,from,prevApproved,t);
      bool r13 = updateExistsOnInsertRecv_burn_r13(tokenId,from,prevApproved,t);
      if(r30==false && r31==false && r24==false && r13==false) {
        revert("Rule condition failed");
      }
  }
  function getApproved(uint tokenId,address spender) public view  returns (bool) {
      bool b = approved[tokenId][spender].b;
      return b;
  }
  function transferFrom(address from,address to,uint tokenId,address operator,address prevApproved,uint t) public    {
      bool r37 = updateApprovedOnInsertRecv_transferFrom_r37(from,to,tokenId,operator,prevApproved,t);
      bool r14 = updateDeltaBalOnInsertRecv_transferFrom_r14(from,to,tokenId,operator,prevApproved,t);
      bool r2 = updateExistsOnInsertRecv_transferFrom_r2(from,to,tokenId,operator,prevApproved,t);
      bool r11 = updateDeltaBalOnInsertRecv_transferFrom_r11(from,to,tokenId,operator,prevApproved,t);
      bool r9 = updateOwnerOfOnInsertRecv_transferFrom_r9(from,to,tokenId,operator,prevApproved,t);
      if(r14==false && r2==false && r9==false && r11==false && r37==false) {
        revert("Rule condition failed");
      }
  }
  function updateDeltaBalOnInsertRecv_transferFrom_r11(address from,address to,uint tokenId,address operator,address prevApproved,uint t) private   returns (bool) {
      address s_1 = msg.sender;
      if(from!=address(0) && to!=address(0) && op!=address(0) && op==s_1) {
        updateBalanceOfOnInsertDeltaBal_r36(to,int(1));
        emit DeltaBal(to,1);
        return true;
      }
      return false;
  }
  function updateDeltaBalOnInsertRecv_transferFrom_r14(address from,address to,uint tokenId,address operator,address prevApproved,uint t) private   returns (bool) {
      address s_1 = msg.sender;
      if(from!=address(0) && to!=address(0) && op!=address(0) && op==s_1) {
        updateBalanceOfOnInsertDeltaBal_r36(from,-1);
        emit DeltaBal(from,-1);
        return true;
      }
      return false;
  }
  function updateExistsOnInsertRecv_transferFrom_r2(address from,address to,uint tokenId,address operator,address prevApproved,uint t) private   returns (bool) {
      address s_1 = msg.sender;
      if(from!=address(0) && to!=address(0) && op!=address(0) && op==s_1) {
        exists[tokenId] = ExistsTuple(true,true);
        emit Exists(tokenId,true);
        return true;
      }
      return false;
  }
  function updateOwnerOfOnInsertRecv_transferFrom_r9(address from,address to,uint tokenId,address operator,address prevApproved,uint t) private   returns (bool) {
      address s_1 = msg.sender;
      if(from!=address(0) && to!=address(0) && op!=address(0) && op==s_1) {
        ownerOf[tokenId] = OwnerOfTuple(to,true);
        emit OwnerOf(tokenId,to);
        return true;
      }
      return false;
  }
  function updateOwnerOfOnInsertRecv_transfer_r3(address from,address to,uint tokenId,address prevApproved,uint t) private   returns (bool) {
      address s_1 = msg.sender;
      if(to!=address(0) && from!=address(0) && from==s_1) {
        ownerOf[tokenId] = OwnerOfTuple(to,true);
        emit OwnerOf(tokenId,to);
        return true;
      }
      return false;
  }
  function updateDeltaBalOnInsertRecv_transfer_r8(address from,address to,uint tokenId,address prevApproved,uint t) private   returns (bool) {
      address s_1 = msg.sender;
      if(to!=address(0) && from!=address(0) && from==s_1) {
        updateBalanceOfOnInsertDeltaBal_r36(to,int(1));
        emit DeltaBal(to,1);
        return true;
      }
      return false;
  }
  function updateApprovedOnInsertRecv_transferFrom_r37(address from,address to,uint tokenId,address operator,address prevApproved,uint t) private   returns (bool) {
      address s_1 = msg.sender;
      if(op==s_1 && op!=address(0) && to!=address(0) && from!=address(0) && prev!=address(0)) {
        approved[tokenId][prev] = ApprovedTuple(false,true);
        emit Approved(tokenId,prev,false);
        return true;
      }
      return false;
  }
  function updateApprovedOnInsertRecv_setApproval_r17(uint tokenId,address spender,bool b,address owner,address prevApproved,uint t) private   returns (bool) {
      address s_1 = msg.sender;
      if(spender!=address(0) && o==s_1) {
        approved[tokenId][spender] = ApprovedTuple(b,true);
        emit Approved(tokenId,spender,b);
        return true;
      }
      return false;
  }
  function updateDeltaBalOnInsertRecv_mint_r34(uint tokenId,address to,uint t) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(to!=address(0) && o_1==s_1) {
        updateBalanceOfOnInsertDeltaBal_r36(to,int(1));
        emit DeltaBal(to,1);
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
  function updateIsApprovedForAllOnInsertRecv_setApprovalForAll_r27(address owner,address operator,bool b,uint t) private   returns (bool) {
      address s_1 = msg.sender;
      int balanceOf_x1 = balanceOf[op].n;
      if(owner!=op && op!=address(0) && owner==s_1 && balanceOf_x1>0) {
        isApprovedForAll[owner][op] = IsApprovedForAllTuple(b,true);
        emit IsApprovedForAll(owner,op,b);
        return true;
      }
      return false;
  }
  function updateExistsOnInsertRecv_transfer_r32(address from,address to,uint tokenId,address prevApproved,uint t) private   returns (bool) {
      address s_1 = msg.sender;
      if(to!=address(0) && from!=address(0) && from==s_1) {
        exists[tokenId] = ExistsTuple(true,true);
        emit Exists(tokenId,true);
        return true;
      }
      return false;
  }
  function updateExistsOnInsertRecv_burn_r13(uint tokenId,address from,address prevApproved,uint t) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(from!=address(0) && o_1==s_1) {
        exists[tokenId] = ExistsTuple(false,true);
        emit Exists(tokenId,false);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnInsertDeltaBal_r36(address p,int d) private    {
      balanceOf[p].n += d;
  }
  function updateDeltaBalOnInsertRecv_burn_r30(uint tokenId,address from,address prevApproved,uint t) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(from!=address(0) && o_1==s_1) {
        updateBalanceOfOnInsertDeltaBal_r36(from,-1);
        emit DeltaBal(from,-1);
        return true;
      }
      return false;
  }
  function updateDeltaBalOnInsertRecv_transfer_r4(address from,address to,uint tokenId,address prevApproved,uint t) private   returns (bool) {
      address s_1 = msg.sender;
      if(from!=address(0) && to!=address(0) && from==s_1) {
        updateBalanceOfOnInsertDeltaBal_r36(from,-1);
        emit DeltaBal(from,-1);
        return true;
      }
      return false;
  }
  function updateOwnerOfOnInsertRecv_mint_r26(uint tokenId,address to,uint t) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(to!=address(0) && o_1==s_1) {
        ownerOf[tokenId] = OwnerOfTuple(to,true);
        emit OwnerOf(tokenId,to);
        return true;
      }
      return false;
  }
  function updateApprovedOnInsertRecv_burn_r31(uint tokenId,address from,address prevApproved,uint t) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(prev!=address(0) && from!=address(0) && o_1==s_1) {
        approved[tokenId][prev] = ApprovedTuple(false,true);
        emit Approved(tokenId,prev,false);
        return true;
      }
      return false;
  }
  function updateOwnerOnInsertConstructor_r33() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateOwnerOfOnInsertRecv_burn_r24(uint tokenId,address from,address prevApproved,uint t) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(from!=address(0) && o_1==s_1) {
        ownerOf[tokenId] = OwnerOfTuple(address(0),true);
        emit OwnerOf(tokenId,address(0));
        return true;
      }
      return false;
  }
  function updateApprovedOnInsertRecv_setApproval_r15(uint tokenId,address spender,bool b,address owner,address prevApproved,uint t) private   returns (bool) {
      address s_1 = msg.sender;
      if(prev!=address(0) && spender!=address(0) && o==s_1) {
        approved[tokenId][prev] = ApprovedTuple(false,true);
        emit Approved(tokenId,prev,false);
        return true;
      }
      return false;
  }
  function updateExistsOnInsertRecv_mint_r25(uint tokenId,address to,uint t) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(to!=address(0) && o_1==s_1) {
        exists[tokenId] = ExistsTuple(true,true);
        emit Exists(tokenId,true);
        return true;
      }
      return false;
  }
  function updateApprovedOnInsertRecv_transfer_r20(address from,address to,uint tokenId,address prevApproved,uint t) private   returns (bool) {
      address s_1 = msg.sender;
      if(prev!=address(0) && to!=address(0) && from!=address(0) && from==s_1) {
        approved[tokenId][prev] = ApprovedTuple(false,true);
        emit Approved(tokenId,prev,false);
        return true;
      }
      return false;
  }
}