module ManualSolve (pathCheck) where

import Control.Lens
import Control.Comonad.Store ( ComonadStore(experiment) )

import Block
import Searching.Search (bfs)

pathCheck ::
    [Int] {- ^ order of insertion -} ->
    [Block Bool] {- ^ SAT solver route -} ->
    Bool {- ^ route is valid -}
pathCheck order steps =
    go order steps
    where
        setAt i = set (sticks . index i)
        getAt i = (^?! sticks . index i)

        -- replaces the extra gaps in the stages and copies over the inserted stick
        go (i:ks) (x:y:zs) = pathExists x' y && go ks (y:zs)
            where
                x' = setAt i (getAt i y) x
        go _ _ = True

pathExists :: Block Bool -> Block Bool -> Bool
pathExists src tgt = tgt `elem` bfs blockStep src

blockStep :: Block Bool -> [Block Bool]
blockStep b = filter checkBlock (experiment editStick =<< holesOf (traverseOf sticks) b)
    where
        editStick s = slider s ++ slidel s ++ [turnLeft s, turnRight s]
        slider = sides \(Side x y z w v) -> [Side False x y z w | not v]
        slidel = sides \(Side x y z w v) -> [Side y z w v False | not x]
