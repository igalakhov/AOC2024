module Day01 where

import Control.Applicative (ZipList (ZipList))
import Data.Foldable (Foldable (foldl'), find)
import Data.List (sort)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Debug.Trace (trace)
import Grid
import Linear hiding (trace)
import Problem (Solution, mkSolution)
import Text.Parsec (char, many, newline, optional, spaces, string, (<|>))
import Text.Parsec.Number (int)
import Text.Parsec.Text (Parser)

type Input = [(Int, Int)]

parser :: Parser Input
parser = many $ ((,) <$> (int <* spaces) <*> int) <* newline

part1 input = sum $ abs <$> ((-) <$> ZipList (sort $ fst <$> input) <*> ZipList (sort $ snd <$> input))

part2 input = sum $ (\x -> x * fromMaybe 0 (Map.lookup x freq)) . fst <$> input
  where
    freq = foldr ((\e m -> Map.insertWith (+) e 1 m) . snd) Map.empty input

solve :: Solution
solve = mkSolution parser (part1, part2)
