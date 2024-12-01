module Day01 where

import Data.Foldable (Foldable (foldl'), find)
import qualified Data.Set as Set
import Debug.Trace (trace)
import Grid
import Linear hiding (trace)
import Problem (Solution, mkSolution)
import Text.Parsec (char, many, optional, string, (<|>))
import Text.Parsec.Number (int)
import Text.Parsec.Text (Parser)

type Input = [(Heading, Int)]

parser :: Parser Input
parser = many $ (,) <$> parseHeading <*> int <* optional (string ", ")
  where
    parseHeading = (char 'R' *> pure HR) <|> (char 'L' *> pure HL)

part1 = sum . ((<$>) abs) . fst . (foldl' applyMove ((V2 0 0), U))
  where
    applyMove (loc, d) (h, n) =
      let d' = turn h d
       in (loc + pure n * dirToVec d', d')

part2 input = sum $ abs <$> visit input ((V2 0 0), U) Set.empty
  where
    visit ((h, n) : ds) (l, d) visited =
      ( let nd = turn h d
            locs = take n . drop 1 $ iterate ((+) (dirToVec nd)) l
         in case find (`Set.member` visited) locs of
              Just l' -> l'
              Nothing -> visit ds (last locs, nd) (foldr Set.insert visited locs)
      )

solve :: Solution
solve = mkSolution parser (part1, part2)
