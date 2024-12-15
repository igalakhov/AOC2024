module Day08 where

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

type Input = ((Int, Int), Map.Map Char [(Int, Int)])

parser :: Parser Input
parser = getGrid <$> many anyChar
  where
    getGrid s =
      let w = words s
       in ( (length w, length (w !! 0)),
            Map.unionsWith
              (++)
              [ Map.singleton v [(i, j)]
                | (i, r) <- zip [1 ..] w,
                  (j, v) <- zip [1 ..] r,
                  v /= '.'
              ]
          )

inrange n m (x, y) = 1 <= x && x <= n && 1 <= y && y <= m

sol antinodes ((n, m), grid) =
  length . nub . filter (inrange n m) $
    Map.elems grid >>= \pts -> [antinodes p1 p2 | p1 <- pts, p2 <- pts, p1 /= p2] >>= id

part1 = sol antinodes
  where
    antinodes (x1, y1) (x2, y2) = let dx = (x1 - x2) in let dy = (y1 - y2) in [(x1 + dx, y1 + dy), (x2 - dx, y2 - dy)]

part2 grid = sol antinodes grid
  where
    ((n, m), _) = grid
    antinodes (x1, y1) (x2, y2) =
      let dx = (x1 - x2)
       in let dy = (y1 - y2)
           in takeWhile (inrange n m) [(x1 + dx * d, y1 + dy * d) | d <- [0 ..]]
                ++ takeWhile (inrange n m) [(x1 - dx * d, y1 - dy * d) | d <- [0 ..]]

solve :: Solution
solve = mkSolution parser (part1, part2)
