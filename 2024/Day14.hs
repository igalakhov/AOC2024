module Day14 where

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
import Linear
import Problem (Solution, mkSolution)
import Text.Parsec (anyChar, char, eof, letter, many, many1, newline, oneOf, optional, sepBy, sepBy1, sepEndBy1, space, spaces, string, try, (<|>))
import Text.Parsec.Number (int)
import Text.Parsec.Text (Parser)
import Text.Parsec.Token (GenTokenParser (whiteSpace))

type Input = [(V2 Int, V2 Int)]

w = 101

h = 103

parser :: Parser Input
parser =
  many $
    (,)
      <$> (string "p=" *> pair)
      <* char ' '
      <*> (string "v=" *> pair)
      <* newline
  where
    pair = V2 <$> int <* char ',' <*> int

move n (p, v) =
  let V2 x y = p + n * v
   in V2 (x `mod` w) (y `mod` h)

part1 = product . map length . group . sort . mapMaybe quadrant . (move 100 <$>)
  where
    quadrant (V2 x _) | x == w `div` 2 = Nothing
    quadrant (V2 _ y) | y == h `div` 2 = Nothing
    quadrant (V2 x y) = Just $ 2 * (2 * x `div` w) + (2 * y `div` h)

part2 =
  listToMaybe
    . map fst
    . filter ((== 500) . snd)
    . zip [0 ..]
    . (map $ length . nub . (map fst))
    . (iterate $ map (\pv -> (move 1 pv, snd pv)))

solve :: Solution
solve = mkSolution parser (part1, part2)
