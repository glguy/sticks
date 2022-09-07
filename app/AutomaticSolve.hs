module AutomaticSolve where

import Prelude hiding (all)
import Control.Lens (indexing, iover, dropping, partsOf, each)
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

transformBlock :: MonadSAT s m => Block Bit -> (Block Bit -> m (Block Bit)) -> m (Block Bit)
transformBlock b f =
 do b' <- f b
    assert (checkBlock b')
    pure b'

fullsolve :: MonadSAT s m => Block Bit -> m ([Bits], [Block Bit], Block Bit)
fullsolve start =
 do -- Permute, flip, and spin the sticks into their final locations.
    -- To reduce useless symmetries, the first stick is locked into
    -- the first location unflipped.
    final <-
        transformBlock start $
        dropping 1 each (selectList' flips) <=<
        partsOf (dropping 1 each) selectPermutation' <=<
        each (selectList' turns)

    order <- selectPermutation' [0,1,2,3,4,5 :: Bits]

    -- generate the sequence of adding in sticks one-by-one
    let steps_ = scanr (setStick gapStick) final order

    -- check that at each step the block can be shifted into a state where the next stick can be inserted
    steps <- for (zip order steps_) \(i,step) ->
        setStick gapStick i <$>
        transformBlock
            (setStick solidStick i step)
            (each (selectList' (turns <=< shifts)))

    pure (order, steps, final)

-- | Insert the stick into the block at the zero-based index.
setStick :: Stick Bit -> Bits {- ^ index -} -> Block Bit -> Block Bit
setStick x i = iover (indexing each) \j y -> chooseBit y x (i === fromIntegral j)
