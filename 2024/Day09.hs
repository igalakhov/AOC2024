module Day09 where

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

type Input = [Int]

parser :: Parser Input
parser = ((\x -> read [x]) <$>) <$> many (oneOf "1234567890")

checksum l = sum $ (*) <$> ZipList [0 ..] <*> ZipList l

decode l = zip l (concat . transpose $ [Just <$> [0 ..], repeat Nothing])

part1 l =
  let decoded = decode l >>= \(amt, val) -> val <$ [1 .. amt]
   in checksum $ take (length . filter isJust $ decoded) $ go (decoded) (reverse . catMaybes $ decoded)
  where
    go ((Just x) : xs) v = x : go xs v
    go (Nothing : xs) (v : vs) = v : go xs vs

part2 l =
  let decoded = decode l
   in checksum $ (go (reverse decoded) decoded) >>= (\(c, v) -> maybe 0 id v <$ [1 .. c])
  where
    go ((_, Nothing) : vs) l = go vs l
    go ((c, Just v) : vs) l = go vs (fit (c, v) l)
    go [] l = l

    fit (c, v) ((c', Nothing) : vs) | c' >= c = (c, Just v) : (c' - c, Nothing) : (merge . remove v $ vs)
    fit (_, v) ((c, Just v') : vs) | v == v' = (c, Just v') : vs
    fit cv (v : vs) = v : fit cv vs
    fit _ [] = []

    remove v ((c, Just v') : vs) | v == v' = (c, Nothing) : vs
    remove v (v' : vs) = v' : remove v vs
    remove v [] = []

    merge ((c1, Nothing) : (c2, Nothing) : cs) = merge $ (c1 + c2, Nothing) : cs
    merge (c : cs) = c : merge cs
    merge [] = []

solve :: Solution
solve = mkSolution parser (part1, part2)
