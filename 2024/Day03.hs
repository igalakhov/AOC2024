module Day03 where

import Data.Foldable (Foldable (foldl'), find)
import Data.Functor (($>))
import qualified Data.Set as Set
import Debug.Trace (trace)
import Grid
import Problem (Solution, mkSolution)
import Text.Parsec (char, many, many1, newline, optional, string, (<|>), spaces, letter, try, eof, space, sepBy1, sepEndBy1, anyChar)
import Text.Parsec.Number (int)
import Text.Parsec.Text (Parser)
import Text.Parsec.Token (GenTokenParser(whiteSpace))
import Data.List.Split (chunksOf)
import qualified Data.Map as Map
import Data.String (IsString(fromString))
import GHC.Integer (integerToInt)
import Data.List
import Data.Tuple
import Data.Function ((&))
import Data.Either
import Data.Maybe (catMaybes)

data Ins = 
    Mul Int Int
  | Do
  | Dont 
  deriving (Show)

type Input = [Ins]


parser :: Parser Input
parser = catMaybes <$> (many $ 
        (Just <$> try parseMul) 
    <|> (Just Do <$ try (string "do()"))
    <|> (Just Dont <$ try (string "don't()"))
    <|> (Nothing <$ anyChar))
    where 
        parseMul = Mul <$> ((string "mul(") *> int) <*> ((char ',') *> int <* (char ')'))

part1 = sum . (map val) 
    where 
        val (Mul x y) = x*y 
        val _ = 0

part2 = solve True
    where
        solve True (Mul x y : l) = x*y + solve True l
        solve True (Dont : l) = solve False l
        solve False (Do : l) = solve True l
        solve s (_ : l) = solve s l
        solve _ _ = 0

solve :: Solution
solve = mkSolution parser (part1, part2)

