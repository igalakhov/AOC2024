module Main where

import qualified Data.Map as Map
import Problem (Solution)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import qualified Year2016
import Prelude

allSolutions :: Map.Map (Int, Int) Solution
allSolutions =
  foldr (Map.union) (Map.empty) ((uncurry (Map.mapKeys . (,))) <$> solutionsByYear)
  where
    solutionsByYear = [(2016, Year2016.solutions)]

main :: IO ()
main = do
  args <- getArgs
  case (readMaybe <$> args) :: [Maybe Int] of
    [Just year, Just day] -> case (Map.lookup (year, day) allSolutions) of
      Just solution -> runSolution year day solution
      Nothing -> putStrLn $ "Unknown year/day: " ++ (show (year, day))
    _ -> putStrLn $ "Bad args: " ++ (show args)
  where
    runSolution year day solution = do
      let fileName = "inputs/" ++ (show year) ++ "_" ++ (show day) ++ ".txt"
      putStrLn $ "Reading input from " ++ fileName
      input <- readFile fileName
      (answer1, answer2) <- solution input
      putStrLn $ "Part 1: " ++ answer1
      putStrLn $ "Part 2: " ++ answer2
      return ()
