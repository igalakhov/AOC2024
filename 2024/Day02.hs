module Day02 where

import Data.Either
import Data.Foldable (Foldable (foldl'), find)
import Data.Function ((&))
import Data.Functor (($>))
import Data.List
import Data.List.Split (chunksOf)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.String (IsString (fromString))
import Data.Tuple
import Debug.Trace (trace)
import GHC.Integer (integerToInt)
import Grid
import Problem (Solution, mkSolution)
import Text.Parsec (char, eof, letter, many, many1, newline, optional, sepBy1, sepEndBy1, space, spaces, string, try, (<|>))
import Text.Parsec.Number (int)
import Text.Parsec.Text (Parser)
import Text.Parsec.Token (GenTokenParser (whiteSpace))

type Input = [[Int]]

parser :: Parser Input
parser = many $ many (int <* optional (char ' ')) <* newline

check l = (all (uncurry (<)) shifted || all (uncurry (>)) shifted) && all (\(x, y) -> abs (x - y) `elem` [1 .. 3]) shifted
  where
    shifted = zip l (drop 1 l)

part1 :: Input -> Int
part1 = sum . map (fromEnum . check)

part2 :: Input -> Int
part2 = sum . map (fromEnum . check')
  where
    check' l = any check [take x l ++ drop (x + 1) l | x <- [0 .. length l]]

solve :: Solution
solve = mkSolution parser (part1, part2)
