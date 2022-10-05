{-# Language UndecidableInstances #-}
{-|
Module      : Puzzle
Description : Puzzle definition for the Unruly game
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

Solve a puzzle played on a square grid by marking
each cell with one of two colors satisfying the
following constraints:

  * All rows and columns must have the same number of
    each color marking
  * All rows must be unique
  * All columns must be unique
  * No three adjacent cells can have the same color

-}
module Puzzle (Board(..), Cell(..), validBoard) where

import Prelude hiding ((&&), or, and, all, not, any)
import Data.List (tails, transpose)
import Ersatz
import Derive.Codec ( TraversableCodec(..) )
import Symbolic ( (==?) )
import Symbolic.Counting ( exactly )

newtype Board a = Board [[Cell a]]
    deriving (Read, Show, Functor, Foldable, Traversable)
    deriving Codec via TraversableCodec Board a

data Cell a = Given Bool | Open a
    deriving (Read, Show, Functor, Foldable, Traversable)
    deriving Codec via TraversableCodec Cell a

cellBoolean :: Boolean b => Cell b -> b
cellBoolean (Given x) = bool x
cellBoolean (Open  x) = x

sameBooleans :: Boolean a => [a] -> [a] -> a
sameBooleans (x:xs) (y:ys) = x ==? y && sameBooleans xs ys
sameBooleans []    []      = true
sameBooleans _     _       = false

goodVec :: Boolean a => [a] -> a
goodVec xs =
    all (\x -> or x && nand x) (runs 3 xs) &&
    exactly (length xs `div` 2) xs

runs :: Int -> [a] -> [[a]]
runs n xs = zipWith const (map (take n) (tails xs)) (drop (n-1) xs)

pairs :: [a] -> [(a, a)]
pairs xs = [(x,y) | x:ys <- tails xs, y <- ys]

validBoard :: Boolean a => Board a -> a
validBoard (Board b) = good rows && good cols
    where
        rows = map (map cellBoolean) b
        cols = transpose rows
        good xs =
            all goodVec xs &&
            not (any (uncurry sameBooleans) (pairs xs))
