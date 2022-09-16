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
import Control.Lens (dropping, partsOf)
import Ersatz (assert, Bit, MonadSAT)

import Block (checkBlock, flips, sticks, turns, Block)
import Symbolic.ChooseBit (ChooseBit)
import Symbolic.Select (selectList, runSelect, selectPermutation)

finalExists :: MonadSAT s m => Block Bit -> m (Block Bit)
finalExists start =
 do turned   <- sticks (selectList' turns) start
    permuted <- partsOf (dropping 1 sticks) selectPermutation' turned
    final    <- dropping 1 sticks (selectList' flips) permuted  
    assert (checkBlock final)
    pure final
