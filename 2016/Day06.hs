module Day06 where

import Data.Foldable (Foldable (foldl'), find)
import Data.Functor (($>))
import qualified Data.Set as Set
import Debug.Trace (trace)
import Grid
import Problem (Solution, mkSolution)
import Text.Parsec (char, many, many1, newline, optional, string, (<|>), spaces, letter)
import Text.Parsec.Number (int)
import Text.Parsec.Text (Parser)
import Text.Parsec.Token (GenTokenParser(whiteSpace))
import Data.List.Split (chunksOf)
import qualified Data.Map as Map
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString
import Data.String (IsString(fromString))
import Data.ByteString (unpack)
import GHC.Integer (integerToInt)
import Data.List
import Data.Tuple

type Input = [String]

parser :: Parser Input
parser = many $ many letter <* newline 

freq :: Ord a => [a] -> Map.Map a Int
freq = foldr (\e -> Map.insertWith (+) e 1) Map.empty

part1 input = snd . maximum . map swap . Map.toList . freq <$> transpose input
    
part2 input = snd . minimum . map swap . Map.toList . freq <$> transpose input

solve :: Solution
solve = mkSolution parser (part1, part2)
