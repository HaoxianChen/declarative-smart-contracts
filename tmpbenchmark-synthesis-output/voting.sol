contract Voting {
  struct IsVoterTuple {
    bool b;
    bool _valid;
  }
  struct WinningProposalTuple {
    uint proposal;
    bool _valid;
  }
  struct VotedTuple {
    bool b;
    bool _valid;
  }
  struct VotesTuple {
    int c;
    bool _valid;
  }
  struct WinsTuple {
    bool b;
    bool _valid;
  }
  struct HasWinnerTuple {
    bool b;
    bool _valid;
  }
  struct QuorumSizeTuple {
    int q;
    bool _valid;
  }
  mapping(address=>IsVoterTuple) isVoter;
  WinningProposalTuple winningProposal;
  mapping(address=>VotedTuple) voted;
  HasWinnerTuple hasWinner;
  mapping(uint=>VotesTuple) votes;
  QuorumSizeTuple quorumSize;
  mapping(uint=>WinsTuple) wins;
  event Vote(address v,uint proposal);
  event InvalidTx();
  event Finalize(uint proposal);
  constructor() public {
    updateWinningProposalOnInsertConstructor_r14();
    updateQuorumSizeOnInsertConstructor_r1();
    updateHasWinnerOnInsertConstructor_r15();
  }
  function getVotes(uint proposal) public view  returns (int) {
      int c = votes[proposal].c;
      return c;
  }
  function getWinningProposal() public view  returns (uint) {
      uint proposal = winningProposal.proposal;
      return proposal;
  }
  function getHasWinner() public view  returns (bool) {
      bool b = hasWinner.b;
      return b;
  }
  function finalize(uint proposal) public    {
      bool r10 = updateFinalizeOnInsertRecv_finalize_r10(proposal);
      if(r10==false) {
        revert("Rule condition failed");
      }
  }
  function getIsVoter(address v) public view  returns (bool) {
      bool b = isVoter[v].b;
      return b;
  }
  function vote(address v,uint proposal) public    {
      bool r2 = updateVoteOnInsertRecv_vote_r2(v,proposal);
      if(r2==false) {
        revert("Rule condition failed");
      }
  }
  function getVoted(address p) public view  returns (bool) {
      bool b = voted[p].b;
      return b;
  }
  function getWins(uint proposal) public view  returns (bool) {
      bool b = wins[proposal].b;
      return b;
  }
  function updateFinalizeOnInsertRecv_finalize_r10(uint p) private   returns (bool) {
      int q_1 = quorumSize.q;
      bool b_0 = hasWinner.b;
      int c_1 = votes[p].c;
      if(b_0!=true && c_1>=q_1) {
        updateWinsOnInsertFinalize_r0(p);
        updateHasWinnerOnInsertFinalize_r8(p);
        updateWinningProposalOnInsertFinalize_r12(p);
        emit Finalize(p);
        return true;
      }
      return false;
  }
  function updateVotedOnInsertVote_r4(address v) private    {
      voted[v] = VotedTuple(true,true);
  }
  function updateWinsOnInsertFinalize_r0(uint p) private    {
      int q = quorumSize.q;
      int c = votes[p].c;
      if(c>=q) {
        wins[p] = WinsTuple(true,true);
      }
  }
  function updateWinningProposalOnInsertConstructor_r14() private    {
      winningProposal = WinningProposalTuple(0,true);
  }
  function updateHasWinnerOnInsertFinalize_r8(uint p) private    {
      int q = quorumSize.q;
      int c = votes[p].c;
      if(c>=q) {
        hasWinner = HasWinnerTuple(true,true);
      }
  }
  function updateVotesOnInsertVoteUnit_r5(uint p,int one) private    {
      votes[p].c += one;
  }
  function updateQuorumSizeOnInsertConstructor_r1() private    {
      quorumSize = QuorumSizeTuple(1,true);
  }
  function updateWinningProposalOnInsertFinalize_r12(uint p) private    {
      int q = quorumSize.q;
      int c = votes[p].c;
      if(c>=q) {
        winningProposal = WinningProposalTuple(p,true);
      }
  }
  function updateVoteUnitOnInsertVote_r6(uint p) private    {
      updateVotesOnInsertVoteUnit_r5(p,int(1));
  }
  function updateVoteOnInsertRecv_vote_r2(address v,uint p) private   returns (bool) {
      bool b_0 = hasWinner.b;
      bool b_2 = voted[v].b;
      bool b_1 = isVoter[v].b;
      if(b_0!=true && b_1!=false && b_2!=true) {
        updateVotedOnInsertVote_r4(v);
        updateVoteUnitOnInsertVote_r6(p);
        emit Vote(v,p);
        return true;
      }
      return false;
  }
  function updateHasWinnerOnInsertConstructor_r15() private    {
      hasWinner = HasWinnerTuple(false,true);
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
}