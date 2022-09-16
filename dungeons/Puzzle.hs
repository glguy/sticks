{-# LANGUAGE UndecidableInstances #-}
{- |
Module:       Puzzle
Description:  Type and functions for specifying puzzles.
-}
module Puzzle (
  Puzzle(..),
  Elt(..),
  isPath,
  isChest,
  clueCols,
  rows,
  makePuzzle,
) where

import Data.List (transpose)
import Derive.Codec (TraversableCodec(..))
import Ersatz (Boolean, true, false, Equatable, Codec(..))
import GHC.Generics (Generic)

-----------------------------------------------------------------------
-- Puzzle grid element representation
-----------------------------------------------------------------------

-- | A single element in the puzzle
data Elt a
  = M -- ^ monster
  | C -- ^ chest
  | O a -- ^ true is open path, false is wall
  deriving (Read, Show, Traversable, Foldable, Functor, Generic, Equatable, Eq, Ord)

-- | Predicate for paths (anything that's not a wall).
isPath :: Boolean b => Elt b -> b
isPath = \case
  O o -> o
  _   -> true

-- | Predicate for chests.
isChest :: Boolean b => Elt a -> b
isChest = \case
  C -> true
  _ -> false

-- | Representation of a whole puzzle.
data Puzzle a = Puzzle {
  topClues :: [Int],           -- ^ column wall counts
  clueRows :: [(Int, [Elt a])] -- ^ row wall counts and row elements
  }
  deriving (Read, Show, Traversable, Foldable, Functor, Generic, Equatable, Eq, Ord)
  deriving Codec via TraversableCodec Puzzle a

-- | Row view of puzzle elements
rows :: Puzzle a -> [[Elt a]]
rows = map snd . clueRows

-- | Column view of puzzle elements
cols :: Puzzle a -> [[Elt a]]
cols = transpose . rows

-- | Combined row and column count pairs.
clueCols :: Puzzle a -> [(Int, [Elt a])]
clueCols p = zip (topClues p) (cols p)

-- | Build a puzzle with row and column clues given a solution.
makePuzzle :: [[Elt Bool]] -> Puzzle Bool
makePuzzle solution =
  Puzzle {
    topClues = clue <$> transpose solution,
    clueRows = [(clue row, row) | row <- solution] }
  where
    clue = length . filter (not . isPath)
