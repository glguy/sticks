{-

@
  /-----/|
 / 1 2 / |
+-----+ 6|
|  3  |5/
|  4  |/
+-----+
@

-}
module Main where

import Prelude hiding ((||), not, any, and, all, (&&))
import Control.Lens (toListOf, each)
import Data.Foldable (for_)
import Ersatz

import Block (Block(..), Stick(..), Side(..))
import ManualSolve (manualSolve, brute)
import AutomaticSolve (fullsolve)

block0 :: Boolean a => Block a
block0 = Block
    (Stick
        (Side x u x x x)
        (Side x x x x x)
        (Side x x x x x)
        (Side x x x x x))
    (Stick
        (Side u u x u x)
        (Side x u x x x)
        (Side x x x x x)
        (Side x u u x x))
    (Stick
        (Side x u x x x)
        (Side x x x x x)
        (Side x u u x x)
        (Side x u x x x))
    (Stick
        (Side u u x x x)
        (Side x x x x x)
        (Side x x x x x)
        (Side x u x x x))
    (Stick
        (Side x u x x x)
        (Side x u x x x)
        (Side x u u x x)
        (Side x x x x x))
    (Stick
        (Side u u x u x)
        (Side x u u x x)
        (Side x x x x x)
        (Side x u x x x))
    where
    u = true
    x = false

main :: IO ()
main = go [] []
    where
    go seen partials =
     do res <- solveWith cryptominisat5
         do (a,b,c) <- fullsolve block0
            assert (all (c /==) seen)
            assert (all (c:b /==) partials)
            pure (a,b,c)
        case res of
            (Satisfied, Just (order,steps,sol))
              | manualSolve order (steps++[sol])
              , let sticks = toListOf each sol ->
                 do putStrLn "# top.. left. bottm right"
                    for_ order \i ->
                        putStrLn (show (i+1) ++ " " ++ showStick (sticks !! fromInteger i))
                    go (encode sol : seen) []
              | otherwise -> go seen (map encode (sol:steps):partials)
            _ -> pure ()

-----------------------------------------------------------------------
-- simple block rendering
-----------------------------------------------------------------------

showStick :: Stick Bool -> String
showStick (Stick x y z w) = unwords (map showSide [x,y,z,w])

showSide :: Side Bool -> String
showSide = foldMap showCut

showCut :: Bool -> String
showCut True = "▂"
showCut False = "▄"
