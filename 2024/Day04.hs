module Day04 where

import Data.Either
import Data.Foldable (Foldable (foldl'), find)
import Data.Function ((&))
import Data.Functor (($>))
import Data.List
import Data.List.Split (chunksOf)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.String (IsString (fromString))
import Data.Tuple
import Debug.Trace (trace)
import GHC.Integer (integerToInt)
import Grid
import Problem (Solution, mkSolution)
import Text.Parsec (anyChar, char, eof, letter, many, many1, newline, oneOf, optional, sepBy1, sepEndBy1, space, spaces, string, try, (<|>))
import Text.Parsec.Number (int)
import Text.Parsec.Text (Parser)
import Text.Parsec.Token (GenTokenParser (whiteSpace))

type Input = [[Char]]

parser :: Parser Input
parser = many1 $ (many1 (oneOf "XMAS.")) <* (() <$ newline <|> eof)

part1 input = sum . take 4 $ stuff <$> iterate transpose input
  where
    stuff input =
      let diags1 = (transpose . diags) input
          diags2 = drop 1 $ transpose . reverse $ diags (reverse (reverse <$> input))
       in length $ filter (isPrefixOf "XMAS") ((diags1 ++ diags2 ++ input) >>= tails)
    diags [] = []
    diags (x : xs) = x : (drop 1 <$> diags xs)

part2 = id

solve :: Solution
solve = mkSolution parser (part1, part2)
