module Day05 where

import Data.Foldable (Foldable (foldl'), find)
import Data.Functor (($>))
import qualified Data.Set as Set
import Debug.Trace (trace)
import Grid
import Linear hiding (trace)
import Problem (Solution, mkSolution)
import Text.Parsec (char, many, many1, newline, optional, string, (<|>), spaces, letter)
import Text.Parsec.Number (int)
import Text.Parsec.Text (Parser)
import Text.Parsec.Token (GenTokenParser(whiteSpace))
import Data.List.Split (chunksOf)
import qualified Data.Map as Map
import Data.List (sort, elemIndex)
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString
import Data.String (IsString(fromString))
import Data.ByteString (unpack)
import GHC.Integer (integerToInt)

-- (chars, checksum)
type Input = String

parser :: Parser Input
parser = many letter 

toHex = (!!) (['0' .. '9'] ++ ['a' .. 'f'])

part1 input = take 8 $ (toHex . fromInteger . toInteger . (!! 5) . bytes) <$> (filter (\x -> all (0 ==) (take 5 (bytes x))) $ ((++) input . show) <$> [1..])
    where 
        bytes s = (unpack $ MD5.hash (fromString s)) >>= (\x -> [x `div` 16, x `mod` 16])
    
part2 input = (map (toHex . snd)) . sort . take 8 . filterUnique $ filter ((<= 7) . fst) (((\[x, y] -> (x, y)) . (take 2 . drop 5) . (map (fromInteger. toInteger)) . bytes) <$> (filter (\x -> all (0 ==) (take 5 (bytes x))) $ ((++) input . show) <$> [1..]))
    where 
        bytes s = (unpack $ MD5.hash (fromString s)) >>= (\x -> [x `div` 16, x `mod` 16])
        filterUnique = filterUnique' Set.empty
            where filterUnique' seen ((pos, val):l) = if pos `Set.notMember` seen then (pos, val) : filterUnique' (Set.insert pos seen) l else filterUnique' seen l

solve :: Solution
solve = mkSolution parser (part1, part2)
