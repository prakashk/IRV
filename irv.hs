-- Instant Runoff Voting (IRV) algorithm
-- http://en.wikipedia.org/wiki/IRV

-- Inputs:
--   * Number of candidates
--   * List of ballots
--     - Each ballot is a list of candidates in order of preference (from most to least)
--       by each voter; absence of ranking of any candidate is allowed.

-- Output:
--   * Winning candidate and the votes received.

import Data.Function (on)
import Data.List (minimumBy, maximumBy)
import qualified Data.Map as M

type Candidate  = String
type Ballot     = [Candidate]
type VoteCounts = M.Map Candidate Integer

-- find if there is a winner (by simple majority)
doWeHaveAWinner :: VoteCounts -> Bool
doWeHaveAWinner voteCounts =
  let totalVotes = sum $ M.elems voteCounts
      maxVotes = maximum $ M.elems voteCounts
  in totalVotes < 2 * maxVotes

-- reassign votes of a candidate to all others
-- the resulting ballot will have one less candidate

reassignVotes :: Candidate -> [Ballot] -> [Ballot]
reassignVotes loser ballots =
  map (reassign loser) ballots
  where
    reassign l blt = filter (/= l) blt

-- count (1st-ranked) votes for each candidate
-- and return the list of (candidate, vote-count) pairs

roundResult :: [Ballot] -> VoteCounts
roundResult ballots = M.fromListWith (+) $ map (\b -> (head b, 1)) ballots

-- candidate with least votes
roundLoser :: VoteCounts -> (Candidate, Integer)
roundLoser votes = minimumBy (compare `on` snd) $ M.toList votes

-- candidate with most votes
roundWinner :: VoteCounts -> (Candidate, Integer)
roundWinner votes = maximumBy (compare `on` snd) $ M.toList votes

-- count votes until a winner is found

tally :: [Ballot] -> VoteCounts
tally ballots =
  let result = roundResult ballots
  in
    if doWeHaveAWinner result
    then
      result
    else
      tally $ reassignVotes (fst $ roundLoser result) ballots

-- winner of the vote
winner :: [Ballot] -> (Candidate, Integer)
winner = roundWinner . tally

main = do
  let ballots = [["A", "B", "C"],
                 ["B", "A", "C"],
                 ["C", "B", "A"],
                 ["A", "C", "B"],
                 ["B", "A", "C"]]
  print $ winner ballots
  let ballots_2 = [["A", "C"],
                   ["B", "A"],
                   ["C", "A", "B"],
                   ["A", "C"],
                   ["B", "A", "C"]]
  print $ winner ballots_2
