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
import Data.Map qualified as Map

import Block (Block(..), Stick(..), Side(..))
import ManualSolve (manualSolve)
import AutomaticSolve (fullsolve)

block0 :: Boolean a => Block a
block0 = Block
    (Stick (Side x u x x x) (Side x x x x x) (Side x x x x x) (Side x x x x x))
    (Stick (Side u u x u x) (Side x u x x x) (Side x x x x x) (Side x u u x x))
    (Stick (Side x u x x x) (Side x x x x x) (Side x u u x x) (Side x u x x x))
    (Stick (Side u u x x x) (Side x x x x x) (Side x x x x x) (Side x u x x x))
    (Stick (Side x u x x x) (Side x u x x x) (Side x u u x x) (Side x x x x x))
    (Stick (Side u u x u x) (Side x u u x x) (Side x x x x x) (Side x u x x x))
    where
    u = true
    x = false

main :: IO ()
main = go [] Map.empty
    where
    go seen partials =
     do res <- solveWith cryptominisat5
         do (a,b,c) <- fullsolve block0
            assert (all (c /==) (map encode seen))
            assert (all (\(sol,stepss) -> c === encode sol ==> all (b /==) (map encode stepss)) (Map.assocs partials))
            pure (a,b,c)
        case res of
            (Satisfied, Just (order,steps,sol))
              | manualSolve order (steps++[sol])
              , let sticks = toListOf each sol ->
                 do putStrLn "# top.. left. bottm right"
                    for_ order \i ->
                        putStrLn (show (i+1) ++ " " ++ showStick (sticks !! fromInteger i))
                    go (sol : seen) (Map.delete sol partials)
              | otherwise -> go seen (Map.insertWith (++) sol [steps] partials)
            (Unsatisfied, _) -> pure ()
            _ -> fail "what the what?"

-----------------------------------------------------------------------
-- simple block rendering
-----------------------------------------------------------------------

showStick :: Stick Bool -> String
showStick = unwords . map showSide . toListOf each

showSide :: Side Bool -> String
showSide = foldMap \x -> if x then "▂" else "▄"
