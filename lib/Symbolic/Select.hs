{-# Language TypeFamilies #-}
{-|
Module      : Symbolic.Select
Description : Generalized symbolic selection
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

Symbolic choice from a finite set of concrete values.

-}
module Symbolic.Select
  ( Select
  , runSelect
  , select
  , selectList
  , mergeSelects
  , selectPermutation
  , selectPermutationN
  ) where

import Control.Applicative (liftA2)
import Control.Monad (replicateM, ap)
import Data.List (tails)
import Data.List.NonEmpty (NonEmpty(..))
import Ersatz
import Prelude hiding (any, and, or, (&&), (||) ,not)

import Symbolic.ChooseBit (ChooseBit(chooseBit))

-- | A set of choices and an index of the chosen element of that set
data Select a = Selected a | Choose (Select a) (Select a) Bit
    deriving (Show, Functor, Foldable, Traversable)

-- | Symbolic selection from a non-empty list of alternatives.
selectList :: MonadSAT s m => [a] -> m (Select a)
selectList []     = error "selectList: empty list"
selectList (x:xs) = select (x :| xs)

-- | Symbolic selection from a non-empty list of alternatives.
select :: MonadSAT s m => NonEmpty a -> m (Select a)
select = mergeSelects . fmap Selected

mergeSelects :: MonadSAT s m => NonEmpty (Select a) -> m (Select a)
mergeSelects (x :| []    ) = pure x
mergeSelects (x :| y : zs) =
 do b <- exists
    mergeSelects (Choose x y b :| reduce b zs)

reduce :: Bit -> [Select a] -> [Select a]
reduce b (x1:x2:xs) = Choose x1 x2 b : reduce b xs
reduce _ xs = xs

runSelect :: ChooseBit a => Select a -> a
runSelect = \case
    Selected x -> x
    Choose x y b -> chooseBit (runSelect x) (runSelect y) b

instance Codec (Select a) where
    type Decoded (Select a) = a
    encode = Selected
    decode _ (Selected x) = pure x
    decode sol (Choose x y b) =
         do b' <- decode sol b
            decode sol if b' then y else x

instance Applicative Select where
    pure = Selected
    (<*>) = ap
    x <* _ = x
    _ *> x = x

instance Monad Select where
    Selected x   >>= f = f x
    Choose x y b >>= f = Choose (x >>= f) (y >>= f) b

instance ChooseBit (Select a) where
    chooseBit = Choose

instance Eq a => Equatable (Select a) where
    x === y = runSelect (liftA2 (\x' y' -> bool (x' == y')) x y)

instance Ord a => Orderable (Select a) where
    x <? y = runSelect (liftA2 (\x' y' -> bool (x' < y')) x y)

-- | Select a permutation of all the elements in a list.
selectPermutation :: MonadSAT s m => [a] -> m [Select a]
selectPermutation xs = selectPermutationN (length xs) xs

-- | Select a permutation of @n@ of the elements in a list.
selectPermutationN :: MonadSAT s m => Int -> [a] -> m [Select a]
selectPermutationN n xs
    | n < 0 || length xs < n = error "selectPermutationN: n out of range"
    | otherwise =
     do ys <- replicateM n (selectList xs)
        assert (unsafeUniqueSelects ys)
        pure ys

-- | This function is only intended to be used to two Select
-- values that were constructed in the exact same fashion.
-- It compares the internal locations of the select, not the
-- actual values.
unsafeUniqueSelects :: [Select a] -> Bit
unsafeUniqueSelects ys = and [ different y z | y:zs <- tails ys, z <- zs ]
    where
    different (Choose f1 t1 b1) (Choose f2 t2 b2) =
        (not b1 && not b2 ==> different f1 f2) &&
        (    b1 &&     b2 ==> different t1 t2)
    different _ _ = false
