module Day19 where

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

type Input = ([String], [String])

parser :: Parser Input
parser =
  (,)
    <$> (many letter `sepBy` string ", ")
    <* newline
    <* newline
    <*> (many1 letter `sepEndBy1` spaces)

loeb x = go where go = fmap ($ go) x

num ps w =
  (!! 0) . loeb $
    map
      ( \idx l ->
          ( if idx == length w
              then
                1
              else
                sum $
                  ( \p ->
                      if p `isPrefixOf` drop idx w
                        then
                          (l !! (idx + length p))
                        else
                          0
                  )
                    <$> ps
          )
      )
      [0 .. length w]

part1 (ps, words) = length $ filter ((> 0) . num ps) words

part2 (ps, words) = sum $ map (num ps) words

solve :: Solution
solve = mkSolution parser (part1, part2)
