module Day05  where

import Data.Foldable (Foldable (foldl'), find)
import Data.Functor (($>))
import qualified Data.Set as Set
import Debug.Trace (trace)
import Grid
import Problem (Solution, mkSolution)
import Text.Parsec (oneOf, char, many, many1, newline, optional, string, (<|>), spaces, letter, try, eof, space, sepBy1, sepEndBy1, anyChar, sepBy)
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
import Data.Maybe (catMaybes)

type Input = (Set.Set (Int, Int), [[Int]])


test constraints l = all (`Set.notMember` constraints) ((tails l) >>= mkPairs)
    where 
        mkPairs [] = []
        mkPairs (x:xs) = map ((flip (,)) x) xs

median l = l !! (length l `div` 2)

parser :: Parser Input
parser = (,) <$> (Set.fromList <$> parseOrders) <* newline <*> parseLists
    where 
        parseOrders = many $ (,) <$> int <* char '|' <*> int <* newline
        parseLists = many $ (int `sepBy` (char ',')) <* newline

part1 (constraints, orders) = sum $ median <$> filter (test constraints) orders 
    where 


part2 (constraints, orders) = 
        sum $ 
        median . (sortBy (\x y -> if Set.member (x, y) constraints then LT else GT)) <$>
        filter (not . (test constraints)) orders 

solve :: Solution
solve = mkSolution parser (part1, part2)

