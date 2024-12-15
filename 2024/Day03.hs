module Day03 where

import Data.Either
import Data.Foldable (Foldable (foldl'), find)
import Data.Function ((&))
import Data.Functor (($>))
import Data.List
import Data.List.Split (chunksOf)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.String (IsString (fromString))
import Data.Tuple
import Debug.Trace (trace)
import GHC.Integer (integerToInt)
import Grid
import Problem (Solution, mkSolution)
import Text.Parsec (anyChar, char, eof, letter, many, many1, newline, optional, sepBy1, sepEndBy1, space, spaces, string, try, (<|>))
import Text.Parsec.Number (int)
import Text.Parsec.Text (Parser)
import Text.Parsec.Token (GenTokenParser (whiteSpace))

data Ins
  = Mul Int Int
  | Do
  | Dont
  deriving (Show)

type Input = [Ins]

parser :: Parser Input
parser =
  catMaybes
    <$> ( many $
            (Just <$> try parseMul)
              <|> (Just Do <$ try (string "do()"))
              <|> (Just Dont <$ try (string "don't()"))
              <|> (Nothing <$ anyChar)
        )
  where
    parseMul = Mul <$> ((string "mul(") *> int) <*> ((char ',') *> int <* (char ')'))

part1 = sum . (map val)
  where
    val (Mul x y) = x * y
    val _ = 0

part2 = solve True
  where
    solve True (Mul x y : l) = x * y + solve True l
    solve True (Dont : l) = solve False l
    solve False (Do : l) = solve True l
    solve s (_ : l) = solve s l
    solve _ _ = 0

solve :: Solution
solve = mkSolution parser (part1, part2)
