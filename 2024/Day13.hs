module Day13 where

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

type Input = [((Int, Int), (Int, Int), (Int, Int))]

parser :: Parser Input
parser =
  many
    ( (,,)
        <$> ((,) <$> (string "Button A: X+" *> int) <* string ", Y+" <*> int <* newline)
        <*> ((,) <$> (string "Button B: X+" *> int) <* string ", Y+" <*> int <* newline)
        <*> ((,) <$> (string "Prize: X=" *> int) <* string ", Y=" <*> int <* newline)
        <* (() <$ newline <|> eof)
    )

part1 = sum . catMaybes . map cost
  where
    cost ((x1, y1), (x2, y2), (t1, t2)) =
      (\l -> if null l then Nothing else Just (minimum l)) $
        map (\(x, y) -> 3 * x + y) $
          filter (\(p1, p2) -> x1 * p1 + x2 * p2 == t1 && y1 * p1 + y2 * p2 == t2) $
            [(p1, (t1 - p1 * x1) `div` x2) | p1 <- [0 .. 100]]

-- x*x1 + y*x2 = t1
-- x*y1 + y*y2 = t2
part2 = sum . map sol
  where
    sol ((x1, y1), (x2, y2), (t1', t2')) =
      let t1 = 10000000000000 + t1'
       in let t2 = 10000000000000 + t2'
           in let x = (t2 * x2 - t1 * y2) `div` (x2 * y1 - x1 * y2)
               in let y = (t2 * x1 - t1 * y1) `div` (x1 * y2 - x2 * y1)
                   in if x * x1 + y * x2 == t1 && x * y1 + y * y2 == t2
                        then
                          3 * x + y
                        else
                          0

solve :: Solution
solve = mkSolution parser (part1, part2)
