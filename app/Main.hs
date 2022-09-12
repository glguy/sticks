{-

@  +-----+
  /     /|
 / 1 2 / |
+-----+ 6|
|  3  |5/
|  4  |/
+-----+
@

-}
module Main where

import Prelude hiding ((||), not, any, and, all, (&&))
import Control.Lens
import Data.Foldable (traverse_)
import Ersatz
import Data.Map (Map)
import Data.Map qualified as Map
import Text.Printf

import Block (Block(..), Stick(..), Side(..), sides)
import SymbolicSolver (candidateExists)
import PathSolver ( Action(..), findPath, simplify)
import Render

block0 :: Boolean a => Block a
block0 = Block
    (stick "▄▂▄▄▄" "▄▄▄▄▄" "▄▄▄▄▄" "▄▄▄▄▄")
    (stick "▂▂▄▂▄" "▄▂▄▄▄" "▄▄▄▄▄" "▄▂▂▄▄")
    (stick "▄▂▄▄▄" "▄▄▄▄▄" "▄▂▂▄▄" "▄▂▄▄▄")
    (stick "▂▂▄▄▄" "▄▄▄▄▄" "▄▄▄▄▄" "▄▂▄▄▄")
    (stick "▄▂▄▄▄" "▄▂▄▄▄" "▄▂▂▄▄" "▄▄▄▄▄")
    (stick "▂▂▄▂▄" "▄▂▂▄▄" "▄▄▄▄▄" "▄▂▄▄▄")
    where
    stick a b c d = Stick false true false (side a) (side b) (side c) (side d)
    side [x,y,z,w,v] = bool.('▂'==) <$> Side x y z w v
    side _ = error "bad side"

main :: IO ()
main = solver Map.empty
    

-- | Print out all the solutions to the 'block0' puzzle.
solver ::
    Map (Block Bool) [[Block Bool]] {- ^ known solutions -} ->
    IO ()
solver seen =
 do res <- solveWith cryptominisat5
     do (a,b,c) <- candidateExists block0
        ifor_ seen \sol stepss ->
            assert
                if null stepss
                    then c /== encode sol
                    else c === encode sol ==> all (\x -> b /== encode x) stepss
        pure (a,b,c)

    case res of
        (Satisfied, Just (order,steps,sol))
            | let order' = map fromInteger order
            , Just path <- simplify <$> findPath order' (steps++[sol]) ->
             do printf "length: %d\n" (length path)
                if length path <= 10 then
                   do printPath path
                      writeFile "animation.pov" (animate path)
                      solver (Map.insertWith (++) sol [steps] seen)
                else 
                    solver (Map.insert sol [steps] seen)

            | otherwise -> solver (Map.insertWith (++) sol [steps] seen)

        (Unsatisfied, _) -> pure ()
        _ -> fail "what the what?"

printPath :: [(Int, Action)] -> IO ()
printPath = traverse_ \(i, a) ->
    printf "%d: %s\n" (i+1)
    case a of
        ActUp       -> "slide up"
        ActDown     -> "slide down"
        ActLeft     -> "turn left"
        ActRight    -> "turn right"
        ActInsert s -> showStick s

-----------------------------------------------------------------------
-- simple block rendering
-----------------------------------------------------------------------

showStick :: Stick Bool -> String
showStick = unwords . map showSide . toListOf sides

showSide :: Side Bool -> String
showSide = foldMap \x -> if x then "▂" else "▄"
