module ManualSolve (manualSolve) where

import Block
import Control.Lens
import Control.Comonad.Store ( ComonadStore(experiment) )
import Search (bfs)

manualSolve ::
    [Integer] {- ^ order of insertion -} ->
    [Block Bool] {- ^ SAT solver route -} ->
    Bool {- ^ route is valid -}
manualSolve order steps =
    go (map fromInteger order) steps
    where
        setAt i = set (indexing each . index i)
        getAt i = (^?! indexing each . index i)

        -- replaces the extra gaps in the stages and copies over the inserted stick
        go (i:ks) (x:y:zs) = pathExists x' y && go ks (y:zs)
            where
                x' = setAt i (getAt i y) x
        go _ _ = True

pathExists :: Block Bool -> Block Bool -> Bool
pathExists src tgt = tgt `elem` bfs blockStep src

blockStep :: Block Bool -> [Block Bool]
blockStep b = filter checkBlock (experiment editStick =<< holesOf each b)
    where
        editStick s = slider s ++ slidel s ++ [turnLeft s, turnRight s]
        slider = each \(Side x y z w v) -> [Side False x y z w | not v]
        slidel = each \(Side x y z w v) -> [Side y z w v False | not x]
