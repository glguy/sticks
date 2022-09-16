{-|
Module      : Main
Description : Solver for the Unruly game
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
module Main (main) where

import Prelude hiding ((&&), not, all, and, any, or)
import Ersatz (assert, exists)
import Symbolic (solve)

import Puzzle (validBoard)
import Parser (parser)
import Renderer (render)

main :: IO ()
main =
 do puzzle <- parser <$> getContents
    res <- solve
     do rows <- traverse (const exists) puzzle
        assert (validBoard rows)
        pure rows
    case res of
      Nothing  -> putStrLn "No solution"
      Just sln -> render sln