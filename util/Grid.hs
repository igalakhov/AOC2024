module Grid where

import Linear

data Direction
  = U
  | D
  | R
  | L
  deriving (Show)

data Heading
  = HL
  | HR
  deriving (Show)

turn :: Heading -> Direction -> Direction
turn HL U = L
turn HR U = R
turn HL D = R
turn HR D = L
turn HL R = U
turn HR R = D
turn HL L = D
turn HR L = U

dirToVec :: Direction -> V2 Int
dirToVec U = V2 0 1
dirToVec D = V2 0 (-1)
dirToVec R = V2 1 0
dirToVec L = V2 (-1) 0
