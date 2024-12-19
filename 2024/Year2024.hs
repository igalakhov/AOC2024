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
import qualified Day18
import Problem (Solutions)

solutions :: Solutions
solutions =
  Map.fromList $
    zip
      [1 ..]
      [ Day01.solve,
        Day02.solve,
        Day03.solve,
        Day04.solve,
        Day05.solve,
        undefined,
        Day07.solve,
        Day08.solve,
        Day09.solve,
        Day10.solve,
        Day11.solve,
        Day12.solve,
        Day13.solve,
        Day14.solve,
        undefined,
        undefined,
        undefined,
        Day18.solve,
        undefined,
        undefined,
        undefined,
        undefined,
        undefined,
        undefined,
        undefined
      ]
