{-|
Module      : Main
Description : Sliding stick puzzle solver
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

Solve a wooden sliding stick puzzle finding
all the shortest solutions to all the possible
valid end-states and emit an animation showing
the shortest solution.

@
   +-----+
  \/     \/|
 \/ 1 2 \/ |
+-----+ 6|
|  3  |5\/
|  4  |\/
+-----+
@

-}
module Main where

import Prelude hiding ((||), not, any, and, all, (&&))
import Control.Lens (toListOf)
import Data.Maybe (mapMaybe)
import Data.List (sortOn)
import Data.Foldable (traverse_, for_)
import Ersatz (Boolean, bool, true, false, encode, assert, solveWith, cryptominisat5, (/==), Result(Unsatisfied, Satisfied))
import Text.Printf (printf)

import Block (sides, Block(Block), Side(..), Stick(Stick))
import SymbolicSolver (finalExists)
import PathSolver (Action(..), solveActions)
import Render (animate)

block0 :: Boolean a => Block a
block0 = Block
    (mk "▄▂▄▄▄" "▄▄▄▄▄" "▄▄▄▄▄" "▄▄▄▄▄")
    (mk "▂▂▄▂▄" "▄▂▄▄▄" "▄▄▄▄▄" "▄▂▂▄▄")
    (mk "▄▂▄▄▄" "▄▄▄▄▄" "▄▂▂▄▄" "▄▂▄▄▄")
    (mk "▂▂▄▄▄" "▄▄▄▄▄" "▄▄▄▄▄" "▄▂▄▄▄")
    (mk "▄▂▄▄▄" "▄▂▄▄▄" "▄▂▂▄▄" "▄▄▄▄▄")
    (mk "▂▂▄▂▄" "▄▂▂▄▄" "▄▄▄▄▄" "▄▂▄▄▄")
    where
    mk a b c d = Stick false true false (side a) (side b) (side c) (side d)
    side [x,y,z,w,v] = bool.('▂'==) <$> Side x y z w v
    side _ = error "bad side"

main :: IO ()
main =
 do finals <- allFinals
    case sortOn length (mapMaybe solveActions finals) of
        [] -> putStrLn "No solutions"
        p:ps ->
         do writeFile "animation.pov" (animate p)
            for_ (p:ps) \x ->
             do printf "Path length: %d\n" (length x)
                printPath x
                putStrLn ""

-- | Generate all the arrangements of the block with all
-- sticks inserted and centered.
allFinals :: IO [Block Bool]
allFinals = go []
    where
        go seen =
         do res <- solveWith cryptominisat5
             do new <- finalExists block0
                for_ seen \old -> assert (new /== encode old)
                pure new
            case res of
                (Satisfied, Just b) -> go (b:seen)
                (Unsatisfied, _) -> pure seen
                _ -> fail "bad solver"

-- | Print out instructions for performing a solution.
printPath :: [(Int, Action)] -> IO ()
printPath = traverse_ \(i, a) ->
    printf "%d: %s\n" (i+1)
    case a of
        ActUp       -> "slide up"
        ActDown     -> "slide down"
        ActRight    -> "turn right"
        ActLeft     -> "turn left"
        ActInsert s -> showStick s

-----------------------------------------------------------------------
-- simple block rendering
-----------------------------------------------------------------------

showStick :: Stick Bool -> String
showStick = unwords . map showSide . toListOf sides

showSide :: Side Bool -> String
showSide = foldMap \x -> if x then "▂" else "▄"
