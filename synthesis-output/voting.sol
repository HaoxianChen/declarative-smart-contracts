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
    updateHasWinnerOnInsertConstructor_r14();
    updateIsVoterOnInsertConstructor_r16();
    updateQuorumSizeOnInsertConstructor_r1();
    updateWinningProposalOnInsertConstructor_r17();
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
  function updateQuorumSizeOnInsertConstructor_r1() private    {
      quorumSize = QuorumSizeTuple(1,true);
  }
  function updateWinsOnInsertFinalize_r0(uint p) private    {
      int q = quorumSize.q;
      int c = votes[p].c;
      if(c>=q) {
        wins[p] = WinsTuple(true,true);
      }
  }
  function updateFinalizeOnInsertRecv_finalize_r10(uint proposal) private   returns (bool) {
      int q_1 = quorumSize.q;
      bool b_0 = hasWinner.b;
      int c_1 = votes[proposal].c;
      if(b_0!=true && c_1>=q_1) {
        updateHasWinnerOnInsertFinalize_r8(proposal);
        updateWinningProposalOnInsertFinalize_r12(proposal);
        updateWinsOnInsertFinalize_r0(proposal);
        emit Finalize(proposal);
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
  function updateVotesOnInsertVoteUnit_r5(uint p,int one) private    {
      votes[p].c += one;
  }
  function updateVoteUnitOnInsertVote_r6(uint p) private    {
      updateVotesOnInsertVoteUnit_r5(p,int(1));
  }
  function updateWinningProposalOnInsertFinalize_r12(uint p) private    {
      int q = quorumSize.q;
      int c = votes[p].c;
      if(c>=q) {
        winningProposal = WinningProposalTuple(p,true);
      }
  }
  function updateHasWinnerOnInsertFinalize_r8(uint p) private    {
      int q = quorumSize.q;
      int c = votes[p].c;
      if(c>=q) {
        hasWinner = HasWinnerTuple(true,true);
      }
  }
  function updateIsVoterOnInsertConstructor_r16() private    {
      address v = msg.sender;
      isVoter[v] = IsVoterTuple(true,true);
  }
  function updateWinningProposalOnInsertConstructor_r17() private    {
      winningProposal = WinningProposalTuple(0,true);
  }
  function updateVoteOnInsertRecv_vote_r2(address v,uint proposal) private   returns (bool) {
      bool b_0 = hasWinner.b;
      bool b_2 = voted[v].b;
      bool b_1 = isVoter[v].b;
      if(b_0!=true && b_1!=false && b_2!=true) {
        updateVotedOnInsertVote_r4(v);
        updateVoteUnitOnInsertVote_r6(proposal);
        emit Vote(v,proposal);
        return true;
      }
      return false;
  }
  function updateHasWinnerOnInsertConstructor_r14() private    {
      hasWinner = HasWinnerTuple(false,true);
  }
  function updateVotedOnInsertVote_r4(address v) private    {
      voted[v] = VotedTuple(true,true);
  }
}