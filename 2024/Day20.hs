module Day20 where

import Control.Monad (guard)
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

-- (walls, start, end)
type Input = (Set.Set (Int, Int), (Int, Int), (Int, Int))

parser :: Parser Input
parser = mkInput <$> ((many $ oneOf "#.SE") `sepBy1` newline)
  where
    mkInput g =
      let matches c = [(i, j) | (i, r) <- zip [1 ..] g, (j, v) <- zip [1 ..] r, v == c]
       in ( Set.fromList $ matches '#',
            (matches 'S') !! 0,
            (matches 'E') !! 0
          )

numPaths cheatLen (walls, start, end) =
  length $
    filter
      (>= 100)
      [ (toStart Map.! n1) + (toEnd Map.! n2) - (toEnd Map.! start) - dist (n1, n2)
        | (n1, n2) <- cheats cheatLen (Map.keys toEnd)
      ]
  where
    toEnd = getDists end
    toStart = getDists start
    tot = toEnd Map.! start
    dist ((x1, y1), (x2, y2)) = (abs $ x1 - x2) + (abs $ y1 - y2)
    cheats len pts = filter ((<= len) . dist) $ (,) <$> pts <*> pts
    getDists target = execState (visit 0 target) Map.empty
      where
        neighbors (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
        visit d pos = do
          vis <- get
          if pos `Map.member` vis || pos `Set.member` walls
            then
              pure ()
            else do
              put (Map.insert pos d vis)
              sequence_ $ visit (d + 1) <$> neighbors pos

part1 = numPaths 2

part2 = numPaths 20

solve :: Solution
solve = mkSolution parser (part1, part2)
