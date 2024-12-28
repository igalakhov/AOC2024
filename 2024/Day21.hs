module Day21 where

import Control.Applicative (ZipList (ZipList))
import Control.Monad.Trans.State
import Data.Char (isNumber)
import Data.Foldable (minimumBy)
import qualified Data.Foldable as List
import Data.Function (on)
import qualified Data.IntMap.Strict as IntMap
import Data.List (nub, permutations, tails)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace (trace)
import Problem
import Text.Parsec hiding (State)
import Text.Parsec.Text

-- (walls, start, end)
type Input = [String]

data Button
  = Arrow ArrowButton
  | Number Char
  deriving (Show)

data ArrowButton = U | D | L | R | A | X
  deriving (Show, Eq, Ord)

gridCord g c = let [ret] = [(i, j) | (i, r) <- zip [1 ..] g, (j, v) <- zip [1 ..] r, v == c] in ret

numberGrid =
  [ "789",
    "456",
    "123",
    "X0A"
  ]

arrowGrid =
  [ [X, U, A],
    [L, D, R]
  ]

gridPaths (x, y) (x', y') =
  let dx = x' - x
   in let dy = y' - y
       in nub $
            (map concat) . permutations $
              [ replicate (abs dx) (if dx > 0 then D else U),
                replicate (abs dy) (if dy > 0 then R else L)
              ]

genPaths grid banned d =
  filter
    ( all (`notElem` banned)
        . map (foldl' (flip move) (gridCord grid d) . reverse)
        . tails
        . reverse
    )
    . (gridPaths `on` (gridCord grid)) d
  where
    move L (x, y) = (x, y - 1)
    move R (x, y) = (x, y + 1)
    move U (x, y) = (x - 1, y)
    move D (x, y) = (x + 1, y)

arrowPaths = genPaths arrowGrid [(1, 1)]

numberPaths = genPaths numberGrid [(4, 1)]

parser :: Parser Input
parser = many $ (many1 . oneOf $ "1234567890A") <* newline

combinePresses =
  fmap ((`Map.singleton` 1) . concat . reverse)
    . foldl (\n b -> [[b' ++ [A]] ++ n' | n' <- n, b' <- b]) [[]]

type CacheFunc = State (IntMap.IntMap (Map.Map [ArrowButton] Int))

solveOne :: Int -> [ArrowButton] -> CacheFunc (Map.Map [ArrowButton] Int)
solveOne lvl l = do
  let key = mkKey (lvl, l)
  cache <- get
  case IntMap.lookup key cache of
    Just ret -> pure ret
    Nothing -> do
      ret <- (minimumBy (compare `on` (sum . Map.elems)) <$>) . sequence $ solveMany (lvl - 1) <$> combinePresses (arrowPaths <$> ZipList (A : l) <*> ZipList l)
      put $ IntMap.insert key ret cache
      pure ret
  where
    mkKey (lvl, l) = lvl + 100 * (foldr (\e v -> v * 5 + (toInt e)) 0 l)
      where
        toInt L = 0
        toInt R = 1
        toInt D = 2
        toInt U = 3
        toInt A = 4

solveMany :: Int -> Map.Map [ArrowButton] Int -> CacheFunc (Map.Map [ArrowButton] Int)
solveMany 0 l = pure l
solveMany lvl l =
  (Map.unionsWith (+) <$>) . sequence $
    Map.toList l
      >>= ( \(p, cnt) ->
              (map (solveOne lvl) . map (++ [A]) . init . splitOn [A]) p
          )

solveDepth depth = sum . map (\c -> (value c) * (cost c))
  where
    value = read . filter isNumber
    cost code =
      minimum . map (sum . Map.elems) $
        ($ IntMap.empty) . evalState . sequence . (map $ solveMany depth) . combinePresses $
          numberPaths <$> ZipList ('A' : code) <*> ZipList code

part1 = solveDepth 4

part2 = solveDepth 27

solve :: Solution
solve = mkSolution parser (part1, part2)
