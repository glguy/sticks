{-|
Module      : SymbolicSolver
Description : SAT-solver driven state enumeration
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

This module enumerates valid final states for
the puzzle. It runs approximately 4x faster than
merely enumerating all final states and filting
for valid ones using backtracking.

-}
module SymbolicSolver (finalExists) where

import Prelude hiding (all, (||), (&&))
import Control.Lens (dropping, partsOf, indexing, iover)
import Data.Traversable (for)
import Ersatz (assert, Bit, MonadSAT, Bits)

import Block (checkBlock, flips, sticks, turns, Block, Stick(..))
import Symbolic.ChooseBit (ChooseBit)
import Symbolic.Select (selectList', runSelect, selectPermutation')

finalExists :: MonadSAT s m => Block Bit -> m (Block Bit)
finalExists start =
 do turned   <- sticks (selectList' turns) start
    permuted <- partsOf (dropping 1 sticks) selectPermutation' turned
    final    <- dropping 1 sticks (selectList' flips) permuted  
    assert (checkBlock final)
    pure final

-----------------------------------------------------------------------

completeSearch ::
    MonadSAT s m => Block Bit -> m ([Bits], [Block Bit], Block Bit)
completeSearch start =
 do -- Permute, flip, and spin the sticks into their final locations.
    -- To reduce useless symmetries, the first stick is locked into
    -- the first location unflipped.
    final <- finalExists start

    order <- selectPermutation' [0,1,2,3,4,5]

    -- generate the sequence of adding in sticks one-by-one
    -- each element in this sequence has an index of an open
    -- location and all the previously placed sticks
    let steps_ :: [(Bits, Block Bit)]
        steps_ = zip order (scanr (setStick noStick) final order)

    -- check that at each step the block can be shifted into
    -- a state where the next stick can be inserted
    steps <- for steps_ \(i, step) ->
     do x <- sticks (selectList' . shifts) step
        assert (checkBlock (setStick solidStick i x))
        pure (setStick noStick i x)

    pure (order, steps, final)

setStick :: Stick Bit -> Bits -> Block Bit -> Block Bit
setStick s i = iover (indexing sticks) \j t -> chooseBit t s (i === fromIntegral j)

shifts :: Stick Bit -> [Stick Bit]
shifts (Stick lo mi hi x y z w) =
    [ Stick t f f x y z w
    , Stick f t f x y z w
    , Stick f f t x y z w
    ]
    where
        t = lo || mi || hi
        f = lo && mi && hi
