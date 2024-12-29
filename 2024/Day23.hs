module Day23 where

import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Problem
import Text.Parsec
import Text.Parsec.Text

type Input = Map.Map String [String]

parser :: Parser Input
parser = mkMap <$> many ((,) <$> many letter <* char '-' <*> many letter <* newline)
  where
    mkMap pairs = Map.unionsWith (++) $ pairs >>= (\(f, t) -> [Map.singleton f [t], Map.singleton t [f]])

threeCliques adj =
  Set.toList
    . Set.fromList
    $ [ sort [v1, v2, v3]
        | v1 <- Map.keys adj,
          v2 <- adj Map.! v1,
          v1 < v2,
          v3 <- intersect (adj Map.! v1) (adj Map.! v2)
      ]

part1 =
  length
    . filter (any ("t" `isPrefixOf`))
    . threeCliques

part2 adj = intercalate "," . findMaximum . threeCliques $ adj
  where
    findMaximum [clique] = clique
    findMaximum cliques = findMaximum . expandCliques $ cliques
    expandCliques cliques =
      Set.toList . Set.fromList $
        [ sort $ cand : cl
          | cl <- cliques,
            cand <- Set.toList $ foldr Set.intersection (Set.fromList $ adj Map.! (cl !! 0)) (Set.fromList . (adj Map.!) <$> cl),
            isClique adj (cand : cl)
        ]
    isClique adj l = all (\v -> all (\w -> v == w || v `elem` adj Map.! w) l) l

solve :: Solution
solve = mkSolution parser (part1, part2)
