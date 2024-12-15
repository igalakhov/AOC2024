module Day03 where

import Data.Foldable (Foldable (foldl'), find)
import Data.Functor (($>))
import Data.List.Split (chunksOf)
import qualified Data.Set as Set
import Debug.Trace (trace)
import Grid
import Linear hiding (trace)
import Problem (Solution, mkSolution)
import Text.Parsec (char, many, many1, newline, optional, spaces, string, (<|>))
import Text.Parsec.Number (int)
import Text.Parsec.Text (Parser)
import Text.Parsec.Token (GenTokenParser (whiteSpace))

type Input = [(Int, Int, Int)]

parser :: Parser Input
parser = many $ spaces *> ((,,) <$> int <* spaces <*> int <* spaces <*> int) <* newline

works (x, y, z) = (x < y + z) && (y < x + z) && (z < x + y)

part1 input = sum $ fromEnum . works <$> input

part2 input = sum $ fromEnum . works . unravel <$> chunksOf 3 (concatMap (`map` input) getters)
  where
    getters = [\(x, _, _) -> x, \(_, y, _) -> y, \(_, _, z) -> z]
    wl = (\(x, y, z) -> x) <$> input
    unravel [x, y, z] = (x, y, z)

solve :: Solution
solve = mkSolution parser (part1, part2)
