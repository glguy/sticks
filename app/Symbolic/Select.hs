{-# Language TypeFamilies #-}
module Symbolic.Select
  ( Select
  , runSelect
  , select
  , selectList
  , mergeSelects
  , selectPermutation
  , selectPermutationN
  ) where

import Control.Applicative
import Control.Monad.State
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
{-# SPECIALIZE selectList :: [a] -> StateT SAT IO (Select a) #-}

-- | Symbolic selection from a non-empty list of alternatives.
select :: MonadSAT s m => NonEmpty a -> m (Select a)
select = mergeSelects . fmap Selected

mergeSelects :: MonadSAT s m => NonEmpty (Select a) -> m (Select a)
mergeSelects (x :| xs) =
    case xs of
        []   -> pure x
        y:ys -> 
         do b  <- exists
            zs <- reduce ys
            mergeSelects (Choose x y b :| zs)

reduce :: MonadSAT s m => [Select a] -> m [Select a]
reduce (x1:x2:xs) =
 do b   <- exists
    xs' <- reduce xs
    pure (Choose x1 x2 b : xs')
reduce xs = pure xs

runSelect :: ChooseBit a => Select a -> a
runSelect = \case
    Selected x -> x
    Choose x y b -> chooseBit (runSelect x) (runSelect y) b

instance Codec (Select a) where
    type Decoded (Select a) = a
    encode = Selected
    decode sol = \case
        Selected x -> pure x
        Choose x y b ->
         do b' <- decode sol b
            decode sol if b' then y else x

instance Applicative Select where
    pure = Selected
    (<*>) = ap

instance Monad Select where
    Selected x   >>= f = f x
    Choose x y b >>= f = Choose (x >>= f) (y >>= f) b

instance ChooseBit (Select a) where
    chooseBit = Choose

instance Eq a => Equatable (Select a) where
    x === y = runSelect (liftA2 (\x' y' -> bool (x' == y')) x y)

instance Ord a => Orderable (Select a) where
    x <? y = runSelect (liftA2 (\x' y' -> bool (x' < y')) x y)

selectPermutation :: MonadSAT s m => [a] -> m [Select a]
selectPermutation xs = selectPermutationN (length xs) xs

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
unsafeUniqueSelects ys =
    nor [ sameSelect y z | y:zs <- tails ys, z <- zs ]
    where
    sameSelect (Choose f1 t1 b1) (Choose f2 t2 b2) =
        not b1 && not b2 && sameSelect f1 f2 ||
            b1 &&     b2 && sameSelect t1 t2
    sameSelect _ _ = true
