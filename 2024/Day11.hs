module Day11  where

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
        stepOne x = let d = toDigits x in 
            if even . length $ d then 
                [toNumber . take (length d `div` 2) $ d, 
                 toNumber . drop (length d `div` 2) $ d 
                 ]
            else [x*2024]

ntimes n = sum . map snd . Map.toList . (!! n) . iterate step

part1 = ntimes 25 

part2 = ntimes 75 

solve :: Solution
solve = mkSolution parser (part1, part2)

