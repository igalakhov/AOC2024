module Day04 where

import Data.Foldable (Foldable (foldl'), find)
import Data.Functor (($>))
import Data.List (elemIndex, sort)
import Data.List.Split (chunksOf)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace (trace)
import Grid
import Linear hiding (trace)
import Problem (Solution, mkSolution)
import Text.Parsec (char, letter, many, many1, newline, optional, spaces, string, (<|>))
import Text.Parsec.Number (int)
import Text.Parsec.Text (Parser)
import Text.Parsec.Token (GenTokenParser (whiteSpace))

-- (chars, checksum)
type Input = [(String, Int, String)]

parser :: Parser Input
parser = many $ ((,,) <$> (filter ('-' /=) <$> many (letter <|> char '-')) <*> int <* char '[' <*> many letter <* char ']') <* newline

part1 input = sum $ check <$> input
  where
    check (code, rid, cs) = if cs == take (length cs) (snd <$> sort ((\(x, y) -> (-y, x)) <$> Map.toList (foldr (\e m -> Map.insertWith (+) e 1 m) Map.empty code))) then rid else 0

part2 input = fst <$> find ((==) "northpoleobjectstorage" . snd) ((\(code, rid, _) -> (rid, shiftChar rid <$> code)) <$> input)
  where
    shiftChar amt chr = let Just idx = elemIndex chr alphabet in alphabet !! ((idx + amt) `mod` length alphabet)
      where
        alphabet = ['a' .. 'z']

solve :: Solution
solve = mkSolution parser (part1, part2)
