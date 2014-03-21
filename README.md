Implementation of Instant Runoff Voting algorithm (as described in
http://en.wikipedia.org/wiki/IRV).

A `Ballot` is represented by a (Candidate, Rank) pair.

The `tally` function accepts a list of `Ballot`s and returns the list
of pairs (Candidate, voteCount). Candidates eliminated in earlier
rounds are not included in this list.
