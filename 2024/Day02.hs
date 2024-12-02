module Day02 where

import Data.Foldable (Foldable (foldl'), find)
import Data.Functor (($>))
import qualified Data.Set as Set
import Debug.Trace (trace)
import Grid
import Problem (Solution, mkSolution)
import Text.Parsec (char, many, many1, newline, optional, string, (<|>), spaces, letter, try, eof, space, sepBy1, sepEndBy1)
import Text.Parsec.Number (int)
import Text.Parsec.Text (Parser)
import Text.Parsec.Token (GenTokenParser(whiteSpace))
import Data.List.Split (chunksOf)
import qualified Data.Map as Map
import Data.String (IsString(fromString))
import GHC.Integer (integerToInt)
import Data.List
import Data.Tuple
import Data.Function ((&))
import Data.Either

type Input = [[Int]]

parser :: Parser Input
parser = many $ many (int <* optional (char ' ')) <* newline

check l = (all (uncurry (<)) shifted || all (uncurry (>)) shifted) && all (\(x, y) -> abs (x-y) `elem` [1..3]) shifted 
    where
        shifted = zip l (drop 1 l)

part1 :: Input -> Int
part1 = sum . map (fromEnum . check)

part2 :: Input -> Int
part2 = sum . map (fromEnum . check')
    where 
        check' l = any check [take x l ++ drop (x+1) l | x <- [0 .. length l]]

solve :: Solution
solve = mkSolution parser (part1, part2)

