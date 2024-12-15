module Day06 where

import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString (unpack)
import qualified Data.ByteString
import Data.Foldable (Foldable (foldl'), find)
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
import Text.Parsec (char, letter, many, many1, newline, optional, spaces, string, (<|>))
import Text.Parsec.Number (int)
import Text.Parsec.Text (Parser)
import Text.Parsec.Token (GenTokenParser (whiteSpace))

type Input = [String]

parser :: Parser Input
parser = many $ many letter <* newline

freq :: (Ord a) => [a] -> Map.Map a Int
freq = foldr (\e -> Map.insertWith (+) e 1) Map.empty

part1 input = snd . maximum . map swap . Map.toList . freq <$> transpose input

part2 input = snd . minimum . map swap . Map.toList . freq <$> transpose input

solve :: Solution
solve = mkSolution parser (part1, part2)
