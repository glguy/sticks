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
import Control.Lens (toListOf, ifor_)
import Data.Foldable (for_)
import Ersatz
import Data.Map (Map)
import Data.Map qualified as Map

import Block (Block(..), Stick(..), Side(..), sticks, sides)
import ManualSolve (pathCheck)
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
main = solver Map.empty

-- | Print out all the solutions to the 'block0' puzzle.
solver ::
    Map (Block Bool) [[Block Bool]] {- ^ known solutions -} ->
    IO ()
solver seen =
 do res <- solveWith cryptominisat5
     do (a,b,c) <- fullsolve block0
        ifor_ seen \sol stepss ->
            assert
                if null stepss
                    then c /== encode sol
                    else c === encode sol ==> all (\x -> b /== encode x) stepss
        pure (a,b,c)

    case res of
        (Satisfied, Just (order,steps,sol))
            | let order' = map fromInteger order
            , pathCheck order' (steps++[sol]) ->                
             do printSolution order' sol
                solver (Map.insert sol [] seen)

            | otherwise -> solver (Map.insertWith (++) sol [steps] seen)

        (Unsatisfied, _) -> pure ()
        _ -> fail "what the what?"

printSolution :: [Int] -> Block Bool -> IO ()
printSolution order sol =
 do let xs = toListOf sticks sol
    putStrLn "# top.. left. bottm right"
    for_ order \i ->
        putStrLn (show (i+1) ++ " " ++ showStick (xs !! i))

-----------------------------------------------------------------------
-- simple block rendering
-----------------------------------------------------------------------

showStick :: Stick Bool -> String
showStick = unwords . map showSide . toListOf sides

showSide :: Side Bool -> String
showSide = foldMap \x -> if x then "▂" else "▄"
