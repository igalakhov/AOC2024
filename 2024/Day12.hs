module Day12  where

import Data.Foldable (Foldable (foldl'), find)
import Data.Functor (($>))
import qualified Data.Set as Set
import Debug.Trace (trace)
import Grid
import Problem (Solution, mkSolution)
import Text.Parsec (oneOf, char, many, many1, newline, optional, string, (<|>), spaces, letter, try, eof, space, sepBy1, sepEndBy1, anyChar, sepBy)
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
import Data.Maybe (catMaybes, listToMaybe)
import Control.Monad
import Control.Monad.Trans.State.Lazy

type Input = Map.Map (Int, Int) Char

parser :: Parser Input
parser = (\l -> Map.fromList $ zip [(i, j) | i <- [1..length l], j <- [1..length $ l !! 0]] (l >>= id))
        <$> 
        (many $ many letter <* newline)

neighbors (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

region map start = execState (region' map (map Map.! start) start) Set.empty
    where 
        region' map val pos = do
            seen <- visited pos 
            if seen || Map.notMember pos map || (map Map.! pos) /= val then 
                pure () 
            else do
                visit pos
                let (x, y) = pos 
                sequence_ $ region' map val <$> neighbors (x, y) 
        visited (x, y) = Set.member (x, y) <$> get
        visit (x, y) = Set.insert (x, y) <$> get >>= put

collectRegions map = collectRegions' (Map.keys $ map) 
    where collectRegions' left = case listToMaybe left of 
            Nothing -> []
            Just start -> let r = region map start in (map Map.! start, r) : (collectRegions' (left \\ (Set.toList r)))

part1 map = sum $ (\region -> (length . perim $ region) * (length $ snd region)) <$> collectRegions map
    where 
        perim (val, region) = filter (\pos -> Map.notMember pos map || map Map.! pos /= val) (Set.toList region >>= neighbors)

rotate = (reverse <$>) . transpose

corners = (concat <$>) . (>>= id) . take (length templates * 2) $ iterate (rotate <$>) templates
    where 
        templates = [
            [[0, 0, 0],
             [1, 1, 0],
             [1, 1, 0]],
            [[0, 0, 1],
             [1, 1, 0],
             [1, 1, 0]],
            [[1, 1, 0],
             [1, 1, 1],
             [1, 1, 1]]
            ]

part2 map = sum $
         (\r -> (countCorners r * length r `div` 9)) <$>
         (Set.fromList . expand . Set.toList . snd) <$>
         collectRegions map 
    where 
        countCorners region = 
            length $
            filter (`elem` corners) $
            (\(x, y) -> [if Set.member (x+di, y+dj) region then 1 else 0 | (di, dj) <- deltas ])
            <$> Set.toList region
        expand region = region >>= (\(x, y) -> [(3*x + di, 3*y + dj) | (di, dj) <- deltas])
        deltas = [(di, dj) | di <- [-1..1], dj <- [-1..1]]

solve :: Solution
solve = mkSolution parser (part1, part2)

