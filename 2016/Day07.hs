module Day07 where

import Data.Foldable (Foldable (foldl'), find)
import Data.Functor (($>))
import qualified Data.Set as Set
import Debug.Trace (trace)
import Grid
import Problem (Solution, mkSolution)
import Text.Parsec (char, many, many1, newline, optional, string, (<|>), spaces, letter, try)
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
import Data.Function ((&))
import Data.Either

-- Left (not in brackets) | Right (in brackets)
type Input = [[Either String String]]

parser :: Parser Input
parser = many $ 
            (many $ (Left <$> many1 letter) <|> (Right <$> (char '[' *> many letter <* char ']')))
            <* newline

part1 input = sum $ fromEnum . works <$> input 
    where
        works word = let 
            (outside, inside) = partitionEithers word
            in 
            (any hasABBA outside) && (all (not . hasABBA) inside)
        hasABBA (x:y:y':x':l') | x /= y && y==y' && x==x' = True
        hasABBA (l:l') = hasABBA l'
        hasABBA _ = False
   
part2 input = ""

solve :: Solution
solve = mkSolution parser (part1, part2)
