module Year2016 (solutions) where

import qualified Data.Map as Map
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import Problem (Solutions)

solutions :: Solutions
solutions =
  Map.fromList
    [ (1, Day01.solve),
      (2, Day02.solve),
      (3, Day03.solve),
      (4, Day04.solve),
      (5, Day05.solve),
      (6, Day06.solve),
      (7, Day07.solve)
    ]
