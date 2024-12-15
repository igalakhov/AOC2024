module Day10  where

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
import Data.Maybe (catMaybes, listToMaybe)
import Control.Monad
import Control.Monad.Trans.State.Lazy

type Input = Map.Map (Int, Int) Int

parser :: Parser Input
parser = mkGrid <$> many1 (many (oneOf "01234567890.") <* newline) 
    where 
        mkGrid x = Map.fromList [
            ((i, j), read [c])
            |
            (i, r) <- zip [1..] x,
            (j, c) <- zip [1..] r,
            c /= '.' && c /= '\n'
         ]

paths f map = sum $ (length . f . paths . fst) <$> filter ((== 0) . snd) (Map.toList map)
    where       
        paths (x, y) = case Map.lookup (x, y) map of 
          Nothing -> [] 
          Just 9 -> [(x, y)] 
          Just v -> concat [
            paths (x', y') 
            | (x', y') <- [(x+1, y), (x-1, y), (x, y+1), (x, y-1)],
            case Map.lookup (x', y') map of 
                Nothing -> False 
                Just v' -> v+1 == v'
           ]

part1 = paths nub

part2 = paths id

solve :: Solution
solve = mkSolution parser (part1, part2)

