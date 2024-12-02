module Year2024 (solutions) where

import qualified Data.Map as Map
import qualified Day01
import qualified Day02
import Problem (Solutions)

solutions :: Solutions
solutions =
  Map.fromList [
    (1, Day01.solve),
    (2, Day02.solve)
    ]
