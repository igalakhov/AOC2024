module Day02 where

import Data.Foldable (Foldable (foldl'), find)
import Data.Functor (($>))
import qualified Data.Set as Set
import Debug.Trace (trace)
import Grid
import Linear hiding (trace)
import Problem (Solution, mkSolution)
import Text.Parsec (char, many, many1, newline, optional, string, (<|>))
import Text.Parsec.Number (int)
import Text.Parsec.Text (Parser)

type Input = [[Direction]]

parser :: Parser Input
parser = many (many (char 'L' $> L <|> char 'R' $> R <|> char 'U' $> U <|> char 'D' $> D) <* newline)

part1 input = concatMap show $ process input (V2 1 1)
  where
    process (l : l') pos = let pos' = processLine pos l in value pos' : process l' pos'
    process [] _ = []
    processLine = foldl' move
    move pos dir =
      let pos' = pos + dirToVec dir
       in if all (\x -> x >= 0 && x <= 2) pos' then pos' else pos
    value (V2 x y) = 1 + x + (2 - y) * 3

part2 input = process input (V2 0 2)
  where
    process (l : l') pos = let pos' = processLine pos l in value pos' : process l' pos'
    process [] _ = []
    processLine = foldl' move
    move pos dir =
      let pos' = pos + dirToVec dir
       in if posOk pos' then pos' else pos
    posOk (V2 x y) = x >= 0 && x <= 4 && y >= 0 && y <= 4 && ((grid !! y) !! x) /= 'X'
    value (V2 x y) = (grid !! (4 - y)) !! x
    grid =
      [ "XX1XX",
        "X234X",
        "56789",
        "XABCX",
        "XXDXX"
      ]

solve :: Solution
solve = mkSolution parser (part1, part2)
