{- |
Module:       Grids
Description:  Functions for working with 2D grids.
-}
module Grids where

import Data.Map (Map)
import Data.Map qualified as Map

-- | Build a grid representation of a list of rows of columns with row-major coordinates.
gridMap :: [[a]] -> Map (Int,Int) a
gridMap zss = 
  Map.fromList
    [ ((y,x), z)
      | (y, zs) <- zip [0..] zss
      , (x, z ) <- zip [0..] zs
    ]

-- | Cardinal direction neighbors.
cardinal :: (Int, Int) -> [(Int, Int)]
cardinal (y,x) =
  [          (y-1,x),
    (y,x-1), {- c -} (y,x+1),
             (y+1,x)]

-- | A 3x3 room centered at the input point.
region3 :: (Int,Int) -> [(Int,Int)]
region3 (y,x) =
  [(y-1,x-1),(y-1,x),(y-1,x+1)
  ,(y  ,x-1),(y  ,x),(y  ,x+1)
  ,(y+1,x-1),(y+1,x),(y+1,x+1)]

-- | The walls around a 3x3 room centered at the input point.
walls3 :: (Int,Int) -> [(Int,Int)]
walls3 (y,x) =
  [          (y-2,x-1),(y-2,x),(y-2,x+1)
  ,(y-1,x-2),                           (y-1,x+2)
  ,(y  ,x-2),          {- c -}          (y  ,x+2)
  ,(y+1,x-2),                           (y+1,x+2)
  ,          (y+2,x-1),(y+2,x),(y+2,x+1)
  ]

-- | The elements needed to complete a 2x2 room with the
-- input point in the top left.
twoArc :: (Int,Int) -> [(Int,Int)]
twoArc (y,x) =
  [{- c -} (y  ,x+1)
  ,(y+1,x),(y+1,x+1)]
