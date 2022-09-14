module SymbolicSolver (finalExists) where

import Prelude hiding (all, (||), (&&))
import Control.Lens (dropping, partsOf)
import Ersatz (assert, Bit, MonadSAT)

import Block (checkBlock, flips, sticks, turns, Block)
import Symbolic.ChooseBit (ChooseBit)
import Symbolic.Select (selectList, runSelect, selectPermutation)

selectPermutation' :: MonadSAT s m => ChooseBit a => [a] -> m [a]
selectPermutation' xs = map runSelect <$> selectPermutation xs

selectList' :: MonadSAT s m => ChooseBit b => (a -> [b]) -> a -> m b
selectList' f x = runSelect <$> selectList (f x)

finalExists :: MonadSAT s m => Block Bit -> m (Block Bit)
finalExists start =
 do turned   <- sticks (selectList' turns) start
    permuted <- partsOf (dropping 1 sticks) selectPermutation' turned
    final    <- dropping 1 sticks (selectList' flips) permuted  
    assert (checkBlock final)
    pure final
