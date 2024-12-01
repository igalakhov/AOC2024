{-# LANGUAGE OverloadedStrings #-}

module Problem where

import Data.List (intercalate)
import qualified Data.Map as Map
import Data.String (IsString (fromString))
import Text.Parsec (runParser)
import Text.Parsec.Error (errorMessages, messageString)
import Text.Parsec.Text (Parser)

type Error a = Either String a

-- A solution is something that takes a list of lines and returns a part one and part two solution as a string
type Solution = String -> IO (String, String)

type Solutions = Map.Map Int Solution

mkSolution :: (Show p1) => (Show p2) => Parser a -> ((a -> p1), (a -> p2)) -> Solution
mkSolution parser (part1, part2) input = case runParser parser () "" (fromString input) of
  Left err -> let err' = "ERROR: " ++ (show err) in return (err', err')
  Right parsedInput -> return (show $ part1 parsedInput, show $ part2 parsedInput)
