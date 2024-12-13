module Year2024 (solutions) where

import qualified Data.Map as Map
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import Problem (Solutions)

solutions :: Solutions
solutions =
  Map.fromList
    [ (1, Day01.solve),
      (2, Day02.solve),
      (3, Day03.solve),
      (4, Day04.solve),
      (5, Day05.solve),
      (7, Day07.solve),
      (8, Day08.solve),
      (9, Day09.solve),
      (10, Day10.solve),
      (11, Day11.solve),
      (12, Day12.solve),
      (13, Day13.solve),
      (14, Day14.solve)
    ]
