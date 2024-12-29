module Day04 where

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Linear
import Problem
import Text.Parsec
import Text.Parsec.Text

type Input = Map.Map (V2 Int) Char

parser :: Parser Input
parser = (mkGrid <$>) . many1 $ (many1 (oneOf "XMAS.")) <* optional newline
  where
    mkGrid l =
      Map.fromList
        [ (V2 i j, c)
          | (i, r) <- zip [1 ..] l,
            (j, c) <- zip [1 ..] r
        ]

part1 grid =
  length . filter (== "XMAS") $
    [ catMaybes . take 4 $ (`Map.lookup` grid) <$> iterate (+ d) c
      | c <- Map.keys grid,
        d <-
          [ V2 x y
            | x <- [-1, 0, 1],
              y <- [-1, 0, 1],
              x /= 0 || y /= 0
          ]
    ]

part2 grid =
  length . filter (== ["A", "MS", "MS"]) $
    [ sort . catMaybes . ((`Map.lookup` grid) . (+ c) <$>) <$> [[V2 0 0], [V2 (-1) (-1), V2 1 1], [V2 (-1) 1, V2 1 (-1)]]
      | c <- Map.keys grid
    ]

solve :: Solution
solve = mkSolution parser (part1, part2)
