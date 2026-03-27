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
  struct HasWinnerTuple {
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
  event Finalize(uint proposal);
  constructor() public {
    updateQuorumSizeOnInsertConstructor_r7();
    updateHasWinnerOnInsertConstructor_r10();
    updateWinningProposalOnInsertConstructor_r12();
    updateIsVoterOnInsertConstructor_r11();
  }
  function getVotes(uint proposal) public view  returns (int) {
      int c = votes[proposal].c;
      return c;
  }
  function finalize(uint proposal) public    {
      bool r8 = updateFinalizeOnInsertRecv_finalize_r8(proposal);
      if(r8==false) {
        revert("Rule condition failed");
      }
  }
  function getWinningProposal() public view  returns (uint) {
      uint proposal = winningProposal.proposal;
      return proposal;
  }
  function getHasWinner() public view  returns (bool) {
      bool b = hasWinner.b;
      return b;
  }
  function getVoted(address p) public view  returns (bool) {
      bool b = voted[p].b;
      return b;
  }
  function getWins(uint proposal) public view  returns (bool) {
      bool b = wins[proposal].b;
      return b;
  }
  function vote(address v,uint proposal) public    {
      bool r6 = updateVoteOnInsertRecv_vote_r6(v,proposal);
      if(r6==false) {
        revert("Rule condition failed");
      }
  }
  function getIsVoter(address v) public view  returns (bool) {
      bool b = isVoter[v].b;
      return b;
  }
  function updateHasWinnerOnInsertFinalize_r4(uint p) private    {
      int q = quorumSize.q;
      int c = votes[p].c;
      if(c>=q) {
        hasWinner = HasWinnerTuple(true,true);
      }
  }
  function updateVoteOnInsertRecv_vote_r6(address v,uint proposal) private   returns (bool) {
      updateVoteUnitOnInsertVote_r9(v,proposal);
      updateVotedOnInsertVote_r3(v);
      emit Vote(v,proposal);
      return true;
      return false;
  }
  function updateWinsOnInsertFinalize_r0(uint p) private    {
      int q = quorumSize.q;
      int c = votes[p].c;
      if(c>=q) {
        wins[p] = WinsTuple(true,true);
      }
  }
  function updateVotesOnInsertVoteUnit_r1(uint p,int one) private    {
      votes[p].c += one;
  }
  function updateuintByint(uint x,int delta) private   returns (uint) {
      int convertedX = int(x);
      int value = convertedX+delta;
      uint convertedValue = uint(value);
      return convertedValue;
  }
  function updateVotedOnInsertVote_r3(address v) private    {
      voted[v] = VotedTuple(true,true);
  }
  function updateFinalizeOnInsertRecv_finalize_r8(uint proposal) private   returns (bool) {
      bool hasWinner_b = hasWinner.b;
      if(hasWinner_b==false) {
        updateHasWinnerOnInsertFinalize_r4(proposal);
        updateWinningProposalOnInsertFinalize_r5(proposal);
        updateWinsOnInsertFinalize_r0(proposal);
        emit Finalize(proposal);
        return true;
      }
      return false;
  }
  function updateHasWinnerOnInsertConstructor_r10() private    {
      hasWinner = HasWinnerTuple(false,true);
  }
  function updateWinningProposalOnInsertConstructor_r12() private    {
      winningProposal = WinningProposalTuple(0,true);
  }
  function updateIsVoterOnInsertConstructor_r11() private    {
      address v = msg.sender;
      isVoter[v] = IsVoterTuple(true,true);
  }
  function updateWinningProposalOnInsertFinalize_r5(uint p) private    {
      int q = quorumSize.q;
      int c = votes[p].c;
      if(c>=q) {
        winningProposal = WinningProposalTuple(p,true);
      }
  }
  function updateQuorumSizeOnInsertConstructor_r7() private    {
      quorumSize = QuorumSizeTuple(1,true);
  }
  function updateVoteUnitOnInsertVote_r9(address v,uint p) private    {
      updateVotesOnInsertVoteUnit_r1(p,int(1));
  }
}