-- Instant Runoff Voting (IRV) algorithm
-- http://en.wikipedia.org/wiki/IRV

-- Inputs:
--   * Number of candidates
--   * List of ballots
--     - Each ballot is a list of rankings of candidates by each voter;
--     absence of ranking of any candidate is allowed, and is indicated by 0.

-- Output:
--   * List of candidates and the votes received, sorted in descending order
--     of vote count.

import Data.Function
import Data.List (minimumBy, maximumBy)

type Candidate = Char
type Rank      = Int
type CandRank  = (Candidate, Rank)
type Ballot    = [CandRank]

-- count (1st-ranked) votes for each candidate
-- and return the list of vote-counts

roundResult :: [Ballot] -> [(Candidate, Int)]
roundResult ballots =
  let accumulateVotes (c, v1) (_, v2) = (c, v1 + v2)
      firstRankOrBust blt = map (\(c, r) -> if r == 1 then (c, 1) else (c, 0)) blt
  in foldl1 (zipWith (accumulateVotes)) $ map (firstRankOrBust) ballots

-- find if there is a winner (by simple majority)
doWeHaveAWinner :: [Int] -> Bool
doWeHaveAWinner votes =
  let totalVotes = sum votes
      maxVotes = maximum votes
  in totalVotes < 2 * maxVotes

-- least vote-getter to be eliminated
roundLoser :: [(Candidate, Int)] -> Candidate
roundLoser votes = fst $ minimumBy (compare `on` snd) votes

-- most vote-getter to gain loser's votes
roundWinner :: [(Candidate, Int)] -> Candidate
roundWinner votes = fst $ maximumBy (compare `on` snd) votes

-- reassign votes of a candidate to all others
-- the resulting ballot will have one less candidate

reassignVotes :: Candidate -> [Ballot] -> [Ballot]
reassignVotes loser ballots =
    let lrank l b = snd $ head $ filter (\(c, _) -> c == l) b
        reassign l b = map (\(c, r) -> (c, if r > (lrank l b) then r-1 else r)) $ filter (\(c, _) -> c /= l) b
     in
        map (\b -> reassign loser b) ballots
