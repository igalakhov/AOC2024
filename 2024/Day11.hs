module Day11 where

import Control.Monad
import Control.Monad.Trans.State.Lazy
import Data.Either
import Data.Foldable (Foldable (foldl'), find)
import Data.Function ((&))
import Data.Functor (($>))
import Data.List
import Data.List.Split (chunksOf)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, listToMaybe)
import qualified Data.Set as Set
import Data.String (IsString (fromString))
import Data.Tuple
import Debug.Trace (trace)
import GHC.Integer (integerToInt)
import Grid
import Problem (Solution, mkSolution)
import Text.Parsec (anyChar, char, eof, letter, many, many1, newline, oneOf, optional, sepBy, sepBy1, sepEndBy1, space, spaces, string, try, (<|>))
import Text.Parsec.Number (int)
import Text.Parsec.Text (Parser)
import Text.Parsec.Token (GenTokenParser (whiteSpace))

type Input = Map.Map Int Int

parser :: Parser Input
parser = (Map.unionsWith (+)) . ((`Map.singleton` 1) <$>) <$> many (int <* spaces)

toDigits 0 = []
toDigits x = toDigits (x `div` 10) ++ [(x `mod` 10)]

toNumber = go . reverse
  where
    go [] = 0
    go (x : xs) = 10 * go xs + x

step stones = Map.unionsWith (+) $ Map.toList stones >>= (\(s, c) -> (`Map.singleton` c) <$> (stepOne s))
  where
    stepOne 0 = [1]
    stepOne x =
      let d = toDigits x
       in if even . length $ d
            then
              [ toNumber . take (length d `div` 2) $ d,
                toNumber . drop (length d `div` 2) $ d
              ]
            else [x * 2024]

ntimes n = sum . map snd . Map.toList . (!! n) . iterate step

part1 = ntimes 25

part2 = ntimes 75

solve :: Solution
solve = mkSolution parser (part1, part2)
