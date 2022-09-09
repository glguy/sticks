module SymbolicSolver (candidateExists) where

import Prelude hiding (all)
import Control.Lens (iover, dropping, partsOf)
import Control.Monad ((<=<))
import Data.Traversable (for)
import Ersatz (assert, Bit, Bits, (===), MonadSAT)

import Block
import Symbolic.ChooseBit (ChooseBit(chooseBit))
import Symbolic.Select (selectList, runSelect, selectPermutation)

selectPermutation' :: MonadSAT s m => ChooseBit a => [a] -> m [a]
selectPermutation' xs = map runSelect <$> selectPermutation xs

selectList' :: MonadSAT s m => ChooseBit b => (a -> [b]) -> a -> m b
selectList' f x = runSelect <$> selectList (f x)

candidateExists :: MonadSAT s m => Block Bit -> m ([Bits], [Block Bit], Block Bit)
candidateExists start =
 do -- Permute, flip, and spin the sticks into their final locations.
    -- To reduce useless symmetries, the first stick is locked into
    -- the first location unflipped.
    final <-
        dropping 1 sticks (selectList' flips) =<<
        partsOf (dropping 1 sticks) selectPermutation' =<<
        sticks (selectList' turns) start
    assert (checkBlock final)

    order <- selectPermutation' [0,1,2,3,4,5]

    -- generate the sequence of adding in sticks one-by-one
    -- each element in this sequence has an index of an open
    -- location and all the previously placed sticks
    let steps_ :: [(Bits, Block Bit)]
        steps_ = zip order (scanr (setStick gapStick) final order)

    -- check that at each step the block can be shifted into a state where the next stick can be inserted
    steps <- for steps_ \(i, step) ->
     do x <- sticks
            (selectList' (turns <=< shifts))
            (setStick solidStick i step)
        assert (checkBlock x)
        pure (setStick gapStick i x)

    pure (order, steps, final)

-- | Insert the stick into the block at the zero-based index.
setStick :: Stick Bit -> Bits {- ^ index -} -> Block Bit -> Block Bit
setStick x i = iover sticks \j y -> chooseBit y x (i === fromIntegral j)
