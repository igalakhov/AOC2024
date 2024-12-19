module Day18 where

import Algorithm.Search (bfs, dfs)
import Control.Applicative (ZipList (ZipList))
import Control.Monad
import Control.Monad.Trans.State.Lazy
import Data.Either
import Data.Foldable (Foldable (foldl'), find)
import Data.Function ((&))
import Data.Functor (($>))
import Data.List
import Data.List.Split (chunksOf)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, isJust, isNothing, listToMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.String (IsString (fromString))
import Data.Tuple
import Debug.Trace (trace)
import GHC.Integer (integerToInt)
import Grid
import Linear
import Problem (Solution, mkSolution)
import Text.Parsec (anyChar, char, eof, letter, many, many1, newline, oneOf, optional, sepBy, sepBy1, sepEndBy1, space, spaces, string, try, (<|>))
import Text.Parsec.Number (int)
import Text.Parsec.Text (Parser)
import Text.Parsec.Token (GenTokenParser (whiteSpace))

type Input = [(Int, Int)]

parser :: Parser Input
parser = many $ (,) <$> int <* char ',' <*> int <* spaces

search map =
  bfs
    ( \(x, y) ->
        [ (x, y)
          | (x, y) <- [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)],
            0 <= x,
            x <= 70,
            0 <= y,
            y <= 70,
            Set.notMember (x, y) map
        ]
    )
    (== (70, 70))
    (0, 0)

part1 = (length <$>) . search . Set.fromList . take 1024

part2 pts =
  fmap (\(_, (x, y)) -> show x ++ "," ++ show y) $
    find (isNothing . fst) $
      (\x -> ((search . Set.fromList) x, last x))
        <$> (take <$> [1024 ..] <*> pure pts)

solve :: Solution
solve = mkSolution parser (part1, part2)
