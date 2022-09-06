module AutomaticSolve where

import Block
import ChooseBit ( ChooseBit(chooseBit) )
import Control.Lens
import Control.Monad ((<=<))
import Data.Traversable (for)
import Ersatz
import Select (selectList, runSelect, selectPermutation)

selectPermutation' :: MonadSAT s m => ChooseBit a => [a] -> m [a]
selectPermutation' xs = map runSelect <$> selectPermutation xs

selectList' :: MonadSAT s m => ChooseBit a => [a] -> m a
selectList' xs = runSelect <$> selectList xs

fullsolve :: MonadSAT s m => Block Bit -> m ([Bits], [Block Bit], Block Bit)
fullsolve start =
 do b1 <- each (selectList' . turns) start
    b2 <- partsOf (dropping 1 each) selectPermutation' b1
    b3 <- dropping 1 each (selectList' . flips) b2

    -- check final configuration is valid
    assert (checkBlock b3)

    order <- selectPermutation' [0,1,2,3,4,5 :: Bits]

    let -- insert a full stick into the hole to check that the stick in the next step can be inserted
        sss = zipWith (\i xs -> iover (indexing each) (\j x -> chooseBit x (pure false) (i === fromIntegral j)) xs) order
            $ scanl (copyStick b3) emptyBlock order

    -- check that at each step the block can be shifted into a state where the next stick can be inserted
    steps <- for sss \ss_ ->
     do shifted <- each (selectList' . (turns <=< shifts)) ss_
        assert (checkBlock shifted)
        pure shifted

    pure (order, steps, b3)

-- | Block with no sticks inserted
emptyBlock :: Boolean a => Block a
emptyBlock = Block gapStick gapStick gapStick gapStick gapStick gapStick

copyStick ::
  Block Bit {- source -} ->
  Block Bit {- target -} ->
  Bits      {- index  -} ->
  Block Bit
copyStick (Block s0 s1 s2 s3 s4 s5) (Block a0 a1 a2 a3 a4 a5) i =
    Block (f 0 s0 a0) (f 1 s1 a1) (f 2 s2 a2) (f 3 s3 a3) (f 4 s4 a4) (f 5 s5 a5)
  where
    f j s x = chooseBit x s (i===j)
