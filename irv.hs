-- Instant Runoff Voting (IRV) algorithm
-- http://en.wikipedia.org/wiki/IRV

-- Inputs:
--   * Number of candidates
--   * List of ballots
--     - Each ballot is a list of rankings of candidates by each voter;
--     absence of ranking of any candidate is allowed, and is indicated by 0.

-- Output:
--   * Winning candidate and the votes received.

import Data.Function (on)
import Data.List (minimumBy, maximumBy)

type Candidate = String
type Rank      = Int
type CandRank  = (Candidate, Rank)
type Ballot    = [CandRank]

-- find if there is a winner (by simple majority)
doWeHaveAWinner :: [(Candidate, Rank)] -> Bool
doWeHaveAWinner votes =
  let totalVotes = sum $ map (snd) votes
      maxVotes = maximum $ map (snd) votes
  in totalVotes < 2 * maxVotes

-- candidate with least votes
roundLoser :: [(Candidate, Int)] -> (Candidate, Int)
roundLoser votes = minimumBy (compare `on` snd) votes

-- candidate with most votes
roundWinner :: [(Candidate, Int)] -> (Candidate, Int)
roundWinner votes = maximumBy (compare `on` snd) votes

-- reassign votes of a candidate to all others
-- the resulting ballot will have one less candidate

reassignVotes :: Candidate -> [Ballot] -> [Ballot]
reassignVotes loser ballots =
  let lrank l b = snd $ head $ filter (\(c, _) -> c == l) b
      reassign l b = map (\(c, r) -> (c, if r > (lrank l b) then r-1 else r))
                     $ filter (\(c, _) -> c /= l) b
  in
    map (\b -> reassign loser b) ballots

-- count (1st-ranked) votes for each candidate
-- and return the list of (candidate, vote-count) pairs

roundResult :: [Ballot] -> [(Candidate, Int)]
roundResult ballots =
  let accumulateVotes (c, v1) (_, v2) = (c, v1 + v2)
      firstRankOrBust blt = map (\(c, r) -> (c, if r == 1 then 1 else 0)) blt
  in foldl1 (zipWith (accumulateVotes)) $ map (firstRankOrBust) ballots

-- count votes until a winner is found

tally :: [Ballot] -> [(Candidate, Int)]
tally ballots =
  let result = roundResult ballots
  in
    if doWeHaveAWinner result
    then
      result
    else
      tally $ reassignVotes (fst $ roundLoser result) ballots

main = do
  let ballots = [[("A", 1), ("B", 3), ("C", 2)],
                 [("A", 2), ("B", 1), ("C", 3)],
                 [("A", 3), ("B", 2), ("C", 1)],
                 [("A", 1), ("B", 3), ("C", 2)],
                 [("A", 2), ("B", 1), ("C", 3)]]
  print $ roundWinner $ tally ballots
