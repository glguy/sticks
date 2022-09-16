{- |
Module:       Rendering
Description:  Functions for rendering solved puzzles.
-}
module Rendering (printSolutions) where

import Data.Foldable (for_)
import Data.List (transpose)
import Data.Map qualified as Map
import Grids (gridMap, cardinal)
import Puzzle (Puzzle, Elt(O, M, C), clueRows, topClues, isPath, rows)
import System.Console.ANSI (setSGR, Color(..), ConsoleIntensity(..), SGR(..), ColorIntensity(..), ConsoleLayer(..))

-- | Print a solution to the terminal using ANSI formatting.
printSolutions :: [Puzzle Bool] -> IO ()
printSolutions ps =
 do for_ ps \p ->
     do setSGR [SetConsoleIntensity BoldIntensity]
        putStr (" " <> concatMap show (topClues p))
        setSGR [Reset]
    putStrLn ""

    for_ (transpose (prepareRows <$> ps)) \zs ->
     do sequence_ zs
        putStrLn ""

prepareRows :: Puzzle Bool -> [IO ()]
prepareRows p =
  [
   do setSGR [SetConsoleIntensity BoldIntensity]
      putStr (show n)
      setSGR [Reset]
      for_ (zip [0..] row) \(x,e) ->
        elt (y,x) e
    | (y,(n,row)) <- zip [0..] (clueRows p)
  ]
  where
    m = gridMap (rows p)

    isPathAt k = any isPath (Map.lookup k m)

    write f b c =
     do setSGR [SetColor Background Dull b, SetColor Foreground Dull f]
        putChar c
        setSGR [Reset]

    elt k = \case
      M       -> write Blue  Red    c
      C       -> write Blue  Yellow c
      O False -> write Green Black  '╳'
      O True  -> write Blue  Cyan   c
      where
        c =
          case isPathAt <$> cardinal k of
            [u,l,r,d] -> lineArt u l r d
            _         -> error "printSolution: impossible"

lineArt ::
  Bool {- ^ up    -} ->
  Bool {- ^ left  -} ->
  Bool {- ^ right -} ->
  Bool {- ^ down  -} ->
  Char
lineArt False False False False = ' '
lineArt False False False True  = '╷'
lineArt False False True  False = '╶'
lineArt False False True  True  = '┌'
lineArt False True  False False = '╴'
lineArt False True  False True  = '┐'
lineArt False True  True  False = '─'
lineArt False True  True  True  = '┬'
lineArt True  False False False = '╵'
lineArt True  False False True  = '│'
lineArt True  False True  False = '└'
lineArt True  False True  True  = '├'
lineArt True  True  False False = '┘'
lineArt True  True  False True  = '┤'
lineArt True  True  True  False = '┴'
lineArt True  True  True  True  = '┼'
