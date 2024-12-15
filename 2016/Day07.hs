module Day07 where

import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString (unpack)
import qualified Data.ByteString
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
import Text.Parsec (char, letter, many, many1, newline, optional, spaces, string, try, (<|>))
import Text.Parsec.Number (int)
import Text.Parsec.Text (Parser)
import Text.Parsec.Token (GenTokenParser (whiteSpace))

-- Left (not in brackets) | Right (in brackets)
type Input = [[Either String String]]

parser :: Parser Input
parser =
  many $
    (many $ (Left <$> many1 letter) <|> (Right <$> (char '[' *> many letter <* char ']')))
      <* newline

part1 input = sum $ fromEnum . works <$> input
  where
    works word =
      let (outside, inside) = partitionEithers word
       in (any hasABBA outside) && (all (not . hasABBA) inside)
    hasABBA (x : y : y' : x' : l') | x /= y && y == y' && x == x' = True
    hasABBA (l : l') = hasABBA l'
    hasABBA _ = False

part2 input = sum $ fromEnum . works <$> input
  where
    works word =
      let (outside, inside) = partitionEithers word
       in (not . Set.null)
            ( Set.intersection
                (Set.fromList $ outside >>= triplets)
                (Set.fromList $ (\(x, y, z) -> (y, z, y)) <$> (inside >>= triplets))
            )
    triplets l = [(a, b, c) | (a, b, c) <- zip3 l (drop 1 l) (drop 2 l), a == c && a /= b]

solve :: Solution
solve = mkSolution parser (part1, part2)
