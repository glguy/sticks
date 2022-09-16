{-|
Module      : PathSolver
Description : Find shortest path to a final state
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

This module generates the shortest action sequence
for building a final state by performing a breadth-
first search to remove sticks until no sticks remain.

-}
module PathSolver (Action(..), actBlock, solveActions) where

import Control.Lens (over, set, alaf)
import Data.Maybe (listToMaybe)
import Data.Functor.Compose (Compose(Compose))

import Block
import Searching.Search (bfsOn)

data Action
    = ActUp
    | ActDown
    | ActLeft
    | ActRight
    | ActInsert (Stick Bool)
    deriving (Read, Show, Eq, Ord)

actBlock :: Int -> Action -> Block Bool -> Block Bool
actBlock i a = over (stick i) (actStick a)

actStick :: Action -> Stick Bool -> Stick Bool
actStick = \case
    ActDown     -> shiftDown
    ActUp       -> shiftUp
    ActLeft     -> turnLeft
    ActRight    -> turnRight
    ActInsert s -> const s

-----------------------------------------------------------------------

solveActions :: Block Bool -> Maybe [(Int, Action)]
solveActions final =
    listToMaybe [as | (b, as) <- bfsOn fst next (final, []), and b]

next :: (Block Bool, [(Int, Action)]) -> [(Block Bool, [(Int, Action)])]
next (b,as) =
    [ (b',(i,a):as)
    | i <- [0..5]
    , (a, b') <- alaf Compose (stick i) (edits i b) b
    , checkBlock b'
    ]

edits :: Int -> Block Bool -> Stick Bool -> [(Action, Stick Bool)]
edits i b s
    | s == noStick  = []
    | removable i b = [(ActInsert s, noStick    )]
    | otherwise     = [(ActDown    , shiftUp   s) | not (posHi s)] <>
                      [(ActUp      , shiftDown s) | not (posLo s)] <>
                      [(ActRight   , turnLeft  s),
                       (ActLeft    , turnRight s)]

removable :: Int -> Block Bool -> Bool
removable i b = checkBlock (set (stick i) solidStick b)
