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
    go (map fromIntegral order <> [-1]) steps
    where
        -- replaces the extra gaps in the stages and copies over the inserted stick
        go (i:j:ks) (x:y:zs) = pathExists x' y' && go (j:ks) (y:zs)
            where
                x' = set (indexing each . index i)
                       (y ^?! indexing each . index i)
                       x
                y' = set (indexing each . index j)
                       gapStick
                       y
        go _ _ = True

pathExists :: Block Bool -> Block Bool -> Bool
pathExists src tgt = tgt `elem` bfs blockStep src

blockStep :: Block Bool -> [Block Bool]
blockStep b = filter checkBlock (experiment editStick =<< holesOf each b)
    where
        editStick s = slider s ++ slidel s ++ [turnLeft s, turnRight s]
        slider = each \(Side x y z w v) -> [Side False x y z w | not v]
        slidel = each \(Side x y z w v) -> [Side y z w v False | not x]
