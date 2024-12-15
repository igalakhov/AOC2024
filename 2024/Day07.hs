module Day07 where

import Control.Applicative (ZipList (ZipList))
import Control.Monad
import Control.Monad.Trans.State.Lazy
import Data.Either
import Data.Foldable (Foldable (foldl'), find)
import Data.Function ((&))
import Data.Functor (($>))
import Data.List
import Data.List.Split (chunksOf)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, isJust, listToMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.String (IsString (fromString))
import Data.Tuple
import Debug.Trace (trace)
import GHC.Integer (integerToInt)
import Grid
import Problem (Solution, mkSolution)
import Text.Parsec (anyChar, char, eof, letter, many, many1, newline, oneOf, optional, sepBy, sepBy1, sepEndBy1, space, spaces, string, try, (<|>))
import Text.Parsec.Number (int)
import Text.Parsec.Text (Parser)
import Text.Parsec.Token (GenTokenParser (whiteSpace))

type Input = [(Int, [Int])]

parser :: Parser Input
parser = many $ ((,) <$> int <* char ':' <*> (many (char ' ' *> int))) <* newline

search fs (tgt, vals) = any (== tgt) (go (reverse vals))
  where
    go [v] = [v]
    go (v : vs) = [f v v' | v' <- go vs, f <- fs]

part1 = sum . map fst . filter (search [(+), (*)])

part2 = sum . map fst . filter (search [(+), (*), flip cc])
  where
    cc x y | y < 10 = 10 * x + y
    cc x y = 10 * (cc x (y `div` 10)) + (y `mod` 10)

solve :: Solution
solve = mkSolution parser (part1, part2)
