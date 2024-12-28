module Day22 where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Foldable
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Problem
import Text.Parsec
import Text.Parsec.Number
import Text.Parsec.Text

type Input = [Int]

parser :: Parser Input
parser = (int `sepEndBy` newline) <* eof

evolve :: Int -> Int
evolve =
  (prune . ap mix (2048 *))
    . (prune . ap mix (`div` 32))
    . (prune . ap mix (64 *))
  where
    mix x y = x `xor` y
    prune x = x `mod` 16777216

part1 = sum . map ((!! 2000) . iterate evolve)

part2 seeds =
  let prices = map (`mod` 10) . take 2001 . iterate evolve <$> seeds
   in let deltaToPrice =
            ( \pl ->
                Map.fromList . reverse $
                  ( (\s -> (getDeltas . take 5 $ s, s !! 4))
                      <$> tails pl
                  )
            )
              <$> prices
       in maximum $
            (sum . catMaybes . (<$> deltaToPrice) . Map.lookup)
              <$> (Set.toList . Set.fromList $ prices >>= (filter ((== 4) . length) . (take 4 <$>) . tails . getDeltas))
  where
    getDeltas pl = toList $ subtract <$> ZipList pl <*> ZipList (drop 1 pl)

solve :: Solution
solve = mkSolution parser (part1, part2)
