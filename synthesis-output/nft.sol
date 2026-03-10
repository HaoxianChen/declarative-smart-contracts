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
    updateOwnerOnInsertConstructor_r10();
  }
  function mint(uint tokenId,address to,uint t) public    {
      bool r6 = updateOwnerOfOnInsertRecv_mint_r6(tokenId,to,t);
      bool r7 = updateExistsOnInsertRecv_mint_r7(tokenId,to,t);
      bool r31 = updateDeltaBalOnInsertRecv_mint_r31(tokenId,to,t);
      if(r6==false && r7==false && r31==false) {
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
  function getApproved(uint tokenId,address spender) public view  returns (bool) {
      bool b = approved[tokenId][spender].b;
      return b;
  }
  function transfer(address from,address to,uint tokenId,address prevApproved,uint t) public    {
      bool r37 = updateDeltaBalOnInsertRecv_transfer_r37(from,to,tokenId,prevApproved,t);
      bool r13 = updateOwnerOfOnInsertRecv_transfer_r13(from,to,tokenId,prevApproved,t);
      bool r18 = updateDeltaBalOnInsertRecv_transfer_r18(from,to,tokenId,prevApproved,t);
      bool r34 = updateApprovedOnInsertRecv_transfer_r34(from,to,tokenId,prevApproved,t);
      bool r22 = updateExistsOnInsertRecv_transfer_r22(from,to,tokenId,prevApproved,t);
      if(r13==false && r22==false && r34==false && r18==false && r37==false) {
        revert("Rule condition failed");
      }
  }
  function burn(uint tokenId,address from,address prevApproved,uint t) public    {
      bool r29 = updateExistsOnInsertRecv_burn_r29(tokenId,from,prevApproved,t);
      bool r16 = updateOwnerOfOnInsertRecv_burn_r16(tokenId,from,prevApproved,t);
      bool r28 = updateDeltaBalOnInsertRecv_burn_r28(tokenId,from,prevApproved,t);
      bool r9 = updateApprovedOnInsertRecv_burn_r9(tokenId,from,prevApproved,t);
      if(r29==false && r16==false && r28==false && r9==false) {
        revert("Rule condition failed");
      }
  }
  function getExists(uint tokenId) public view  returns (bool) {
      bool b = exists[tokenId].b;
      return b;
  }
  function transferFrom(address from,address to,uint tokenId,address operator,address prevApproved,uint t) public    {
      bool r17 = updateOwnerOfOnInsertRecv_transferFrom_r17(from,to,tokenId,operator,prevApproved,t);
      bool r1 = updateExistsOnInsertRecv_transferFrom_r1(from,to,tokenId,operator,prevApproved,t);
      bool r25 = updateDeltaBalOnInsertRecv_transferFrom_r25(from,to,tokenId,operator,prevApproved,t);
      bool r33 = updateDeltaBalOnInsertRecv_transferFrom_r33(from,to,tokenId,operator,prevApproved,t);
      bool r36 = updateApprovedOnInsertRecv_transferFrom_r36(from,to,tokenId,operator,prevApproved,t);
      if(r17==false && r25==false && r1==false && r36==false && r33==false) {
        revert("Rule condition failed");
      }
  }
  function setApproval(uint tokenId,address spender,bool b,address owner,address prevApproved,uint t) public    {
      bool r24 = updateApprovedOnInsertRecv_setApproval_r24(tokenId,spender,b,owner,prevApproved,t);
      bool r30 = updateApprovedOnInsertRecv_setApproval_r30(tokenId,spender,b,owner,prevApproved,t);
      if(r24==false && r30==false) {
        revert("Rule condition failed");
      }
  }
  function setApprovalForAll(address owner,address operator,bool b,uint t) public    {
      bool r12 = updateIsApprovedForAllOnInsertRecv_setApprovalForAll_r12(owner,operator,b,t);
      if(r12==false) {
        revert("Rule condition failed");
      }
  }
  function updateExistsOnInsertRecv_burn_r29(uint tokenId,address from,address prevApproved,uint t) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        exists[tokenId] = ExistsTuple(false,true);
        emit Exists(tokenId,false);
        return true;
      }
      return false;
  }
  function updateOwnerOfOnInsertRecv_transfer_r13(address from,address to,uint tokenId,address prevApproved,uint t) private   returns (bool) {
      if(to!=address(0)) {
        ownerOf[tokenId] = OwnerOfTuple(to,true);
        emit OwnerOf(tokenId,to);
        return true;
      }
      return false;
  }
  function updateDeltaBalOnInsertRecv_transfer_r37(address from,address to,uint tokenId,address prevApproved,uint t) private   returns (bool) {
      address s_1 = msg.sender;
      int balanceOf_x1 = balanceOf[from].n;
      if(from!=address(0) && from==s_1 && 0!=balanceOf_x1) {
        updateBalanceOfOnInsertDeltaBal_r35(from,-1);
        emit DeltaBal(from,-1);
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
  function updateApprovedOnInsertRecv_transfer_r34(address from,address to,uint tokenId,address prevApproved,uint t) private   returns (bool) {
      if(prevApproved!=address(0)) {
        approved[tokenId][prevApproved] = ApprovedTuple(false,true);
        emit Approved(tokenId,prevApproved,false);
        return true;
      }
      return false;
  }
  function updateExistsOnInsertRecv_transfer_r22(address from,address to,uint tokenId,address prevApproved,uint t) private   returns (bool) {
      exists[tokenId] = ExistsTuple(true,true);
      emit Exists(tokenId,true);
      return true;
      return false;
  }
  function updateApprovedOnInsertRecv_burn_r9(uint tokenId,address from,address prevApproved,uint t) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(prevApproved!=address(0) && o==s) {
        approved[tokenId][prevApproved] = ApprovedTuple(false,true);
        emit Approved(tokenId,prevApproved,false);
        return true;
      }
      return false;
  }
  function updateApprovedOnInsertRecv_setApproval_r24(uint tokenId,address spender,bool b,address owner,address prevApproved,uint t) private   returns (bool) {
      if(spender!=address(0)) {
        approved[tokenId][spender] = ApprovedTuple(b,true);
        emit Approved(tokenId,spender,b);
        return true;
      }
      return false;
  }
  function updateApprovedOnInsertRecv_transferFrom_r36(address from,address to,uint tokenId,address operator,address prevApproved,uint t) private   returns (bool) {
      address s_1 = msg.sender;
      if(operator==s_1 && operator!=address(0) && to!=address(0) && from!=address(0) && prevApproved!=address(0)) {
        approved[tokenId][prevApproved] = ApprovedTuple(false,true);
        emit Approved(tokenId,prevApproved,false);
        return true;
      }
      return false;
  }
  function updateOwnerOfOnInsertRecv_mint_r6(uint tokenId,address to,uint t) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(to!=address(0) && o_1==s_1) {
        ownerOf[tokenId] = OwnerOfTuple(to,true);
        emit OwnerOf(tokenId,to);
        return true;
      }
      return false;
  }
  function updateOwnerOfOnInsertRecv_burn_r16(uint tokenId,address from,address prevApproved,uint t) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        ownerOf[tokenId] = OwnerOfTuple(address(0),true);
        emit OwnerOf(tokenId,address(0));
        return true;
      }
      return false;
  }
  function updateOwnerOfOnInsertRecv_transferFrom_r17(address from,address to,uint tokenId,address operator,address prevApproved,uint t) private   returns (bool) {
      address s_1 = msg.sender;
      if(from!=address(0) && to!=address(0) && operator!=address(0) && operator==s_1) {
        ownerOf[tokenId] = OwnerOfTuple(to,true);
        emit OwnerOf(tokenId,to);
        return true;
      }
      return false;
  }
  function updateDeltaBalOnInsertRecv_transferFrom_r25(address from,address to,uint tokenId,address operator,address prevApproved,uint t) private   returns (bool) {
      address s_1 = msg.sender;
      if(from!=address(0) && to!=address(0) && operator!=address(0) && operator==s_1) {
        updateBalanceOfOnInsertDeltaBal_r35(to,int(1));
        emit DeltaBal(to,1);
        return true;
      }
      return false;
  }
  function updateDeltaBalOnInsertRecv_transferFrom_r33(address from,address to,uint tokenId,address operator,address prevApproved,uint t) private   returns (bool) {
      address s_1 = msg.sender;
      if(from!=address(0) && to!=address(0) && operator!=address(0) && operator==s_1) {
        updateBalanceOfOnInsertDeltaBal_r35(from,-1);
        emit DeltaBal(from,-1);
        return true;
      }
      return false;
  }
  function updateExistsOnInsertRecv_mint_r7(uint tokenId,address to,uint t) private   returns (bool) {
      address s = msg.sender;
      address o = owner.p;
      if(o==s) {
        exists[tokenId] = ExistsTuple(true,true);
        emit Exists(tokenId,true);
        return true;
      }
      return false;
  }
  function updateExistsOnInsertRecv_transferFrom_r1(address from,address to,uint tokenId,address operator,address prevApproved,uint t) private   returns (bool) {
      address s_1 = msg.sender;
      if(from!=address(0) && to!=address(0) && operator!=address(0) && operator==s_1) {
        exists[tokenId] = ExistsTuple(true,true);
        emit Exists(tokenId,true);
        return true;
      }
      return false;
  }
  function updateApprovedOnInsertRecv_setApproval_r30(uint tokenId,address spender,bool b,address owner,address prevApproved,uint t) private   returns (bool) {
      if(prevApproved!=address(0)) {
        approved[tokenId][prevApproved] = ApprovedTuple(false,true);
        emit Approved(tokenId,prevApproved,false);
        return true;
      }
      return false;
  }
  function updateDeltaBalOnInsertRecv_transfer_r18(address from,address to,uint tokenId,address prevApproved,uint t) private   returns (bool) {
      if(to!=address(0)) {
        updateBalanceOfOnInsertDeltaBal_r35(to,int(1));
        emit DeltaBal(to,1);
        return true;
      }
      return false;
  }
  function updateDeltaBalOnInsertRecv_burn_r28(uint tokenId,address from,address prevApproved,uint t) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(from!=address(0) && o_1==s_1) {
        updateBalanceOfOnInsertDeltaBal_r35(from,-1);
        emit DeltaBal(from,-1);
        return true;
      }
      return false;
  }
  function updateIsApprovedForAllOnInsertRecv_setApprovalForAll_r12(address owner,address operator,bool b,uint t) private   returns (bool) {
      address s_1 = msg.sender;
      if(owner!=operator && operator!=address(0) && owner==s_1) {
        isApprovedForAll[owner][operator] = IsApprovedForAllTuple(b,true);
        emit IsApprovedForAll(owner,operator,b);
        return true;
      }
      return false;
  }
  function updateOwnerOnInsertConstructor_r10() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateDeltaBalOnInsertRecv_mint_r31(uint tokenId,address to,uint t) private   returns (bool) {
      address s_1 = msg.sender;
      address o_1 = owner.p;
      if(to!=address(0) && o_1==s_1) {
        updateBalanceOfOnInsertDeltaBal_r35(to,int(1));
        emit DeltaBal(to,1);
        return true;
      }
      return false;
  }
  function updateBalanceOfOnInsertDeltaBal_r35(address p,int d) private    {
      balanceOf[p].n += d;
  }
}