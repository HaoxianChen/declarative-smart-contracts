contract Voting {
  struct QuorumSizeTuple {
    uint q;
    bool _valid;
  }
  struct IsVoterTuple {
    bool b;
    bool _valid;
  }
  struct OwnerTuple {
    address s;
    bool _valid;
  }
  struct HasWinnerTuple {
    bool b;
    bool _valid;
  }
  struct VotesTuple {
    uint c;
    bool _valid;
  }
  struct VotedTuple {
    bool b;
    bool _valid;
  }
  struct WinningProposalTuple {
    uint p;
    bool _valid;
  }
  mapping(address=>IsVoterTuple) isVoter;
  OwnerTuple owner;
  HasWinnerTuple hasWinner;
  mapping(uint=>VotesTuple) votes;
  mapping(address=>VotedTuple) voted;
  WinningProposalTuple winningProposal;
  QuorumSizeTuple quorumSize;
  event Vote(address v,uint proposal);
  event InvalidTx();
  event AddVoter(address v);
  constructor(uint q) public {
    updateOwnerOnInsertConstructor_r9();
    updateQuorumSizeOnInsertConstructor_r1(q);
  }
  function getOwner() public view  returns (address) {
      address s = owner.s;
      return s;
  }
  function getHasWinner() public view  returns (bool) {
      bool b = hasWinner.b;
      return b;
  }
  function addVoter(address v) public    {
      bool r3 = updateAddVoterOnInsertRecv_addVoter_r3(v);
      if(r3==false) {
        revert("Rule condition failed");
      }
  }
  function getIsVoter(address v) public view  returns (bool) {
      bool b = isVoter[v].b;
      return b;
  }
  function getWinningProposal() public view  returns (uint) {
      uint p = winningProposal.p;
      return p;
  }
  function vote(address v,uint proposal) public    {
      bool r8 = updateVoteOnInsertRecv_vote_r8(v,proposal);
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function getVotes(uint proposal) public view  returns (uint) {
      uint c = votes[proposal].c;
      return c;
  }
  function getVoted(address v) public view  returns (bool) {
      bool b = voted[v].b;
      return b;
  }
  function updateAddVoterOnInsertRecv_addVoter_r3(address v) private   returns (bool) {
      address s = msg.sender;
      address o = owner.s;
      if(o==s) {
        updateIsVoterOnInsertAddVoter_r4(v);
        emit AddVoter(v);
        return true;
      }
      return false;
  }
  function updateWinningProposalOnInsertWins_r14(uint p,bool b) private    {
      // Empty()
      if(b==true) {
        winningProposal = WinningProposalTuple(p,true);
      }
  }
  function updateQuorumSizeOnInsertConstructor_r1(uint q) private    {
      quorumSize = QuorumSizeTuple(q,true);
  }
  function updateOwnerOnInsertConstructor_r9() private    {
      address s = msg.sender;
      owner = OwnerTuple(s,true);
  }
  function updateWinsOnDeleteVotes_r13(uint p,uint c) private    {
      uint q = quorumSize.q;
      if(c>=q) {
        updateWinningProposalOnDeleteWins_r14(p,bool(true));
        updateHasWinnerOnDeleteWins_r6(bool(true));
      }
  }
  function updateVotedOnInsertVote_r10(address v) private    {
      voted[v] = VotedTuple(true,true);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateHasWinnerOnInsertWins_r6(bool b) private    {
      if(b==true) {
        hasWinner = HasWinnerTuple(true,true);
      }
  }
  function updateVotesOnInsertVote_r7(address _v0,uint p) private    {
      int delta2 = int(1);
      updateWinsOnIncrementVotes_r13(p,delta2);
      votes[p].c += 1;
  }
  function updateVotedOnInsertIsVoter_r11(address v) private    {
      voted[v] = VotedTuple(false,true);
  }
  function updateWinsOnIncrementVotes_r13(uint p,int c) private    {
      int _delta = int(c);
      uint x_votes_p_c = votes[p].c;
      uint newValue = updateuintByint(x_votes_p_c,_delta);
      updateWinsOnInsertVotes_r13(p,newValue);
  }
  function updateHasWinnerOnDeleteWins_r6(bool b) private    {
      if(b==true) {
        hasWinner = HasWinnerTuple(false,false);
      }
  }
  function updateWinningProposalOnDeleteWins_r14(uint p,bool b) private    {
      if(b==true) {
        winningProposal = WinningProposalTuple(0,false);
      }
  }
  function updateWinsOnInsertVotes_r13(uint p,uint c) private    {
      VotesTuple memory toDelete = votes[p];
      if(toDelete._valid==true) {
        updateWinsOnDeleteVotes_r13(p,toDelete.c);
      }
      uint q = quorumSize.q;
      if(c>=q) {
        updateWinningProposalOnInsertWins_r14(p,bool(true));
        updateHasWinnerOnInsertWins_r6(bool(true));
      }
  }
  function updateVoteOnInsertRecv_vote_r8(address v,uint p) private   returns (bool) {
      bool isVoter_x1 = isVoter[v].b;
      bool b_1 = isVoter[v].b;
      bool b_0 = voted[v].b;
      if(b_0!=true && b_1==true && isVoter_x1==false) {
        updateVotesOnInsertVote_r7(v,p);
        updateVotedOnInsertVote_r10(v);
        emit Vote(v,p);
        return true;
      }
      return false;
  }
  function updateIsVoterOnInsertAddVoter_r4(address v) private    {
      updateVotedOnInsertIsVoter_r11(v);
      isVoter[v] = IsVoterTuple(true,true);
  }
}