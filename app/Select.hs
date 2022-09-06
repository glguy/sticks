{-# Language TypeFamilies, DeriveTraversable #-}
module Select
  ( Select
  , runSelect
  , select
  , selectList
  , mergeSelects
  , selectPermutation
  , selectPermutationN
  ) where

import Ersatz
import ChooseBit ( ChooseBit(..) )
import Data.List (tails)
import Data.List.NonEmpty (NonEmpty(..))
import Control.Applicative
import Control.Monad.State
import Prelude hiding (any, and, or, (&&), (||) ,not)

-- | A set of choices and an index of the chosen element of that set
data Select a = Selected a | Choice (Select a) (Select a) Bit
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
mergeSelects (x  :| [])      = return x
mergeSelects (x1 :| x2 : xs) =
  do b   <- exists
     xs' <- reduce xs
     mergeSelects (Choice x1 x2 b :| xs')

reduce :: MonadSAT s m => [Select a] -> m [Select a]
reduce (x1:x2:xs) =
     do b   <- exists
        xs' <- reduce xs
        return (Choice x1 x2 b : xs')
reduce xs = return xs

runSelect :: ChooseBit a => Select a -> a
runSelect (Selected x) = x
runSelect (Choice x y b) = chooseBit (runSelect x) (runSelect y) b

instance Codec (Select a) where

  type Decoded (Select a) = a

  encode = Selected

  decode _ (Selected x) = return x
  decode sol (Choice x y b) =
    do b' <- decode sol b
       decode sol (if b' then y else x)

instance Applicative Select where
  pure = Selected
  (<*>) = ap

instance Monad Select where
  Selected x   >>= f = f x
  Choice x y b >>= f = Choice (x >>= f) (y >>= f) b

instance ChooseBit (Select a) where
  chooseBit = Choice

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
     return ys

-- | This function is only intended to be used to two Select
-- values that were constructed in the exact same fashion.
-- It compares the internal locations of the select, not the
-- actual values.
unsafeUniqueSelects :: [Select a] -> Bit
unsafeUniqueSelects ys =
  nor [ sameSelect y z
      | y:zs <- tails ys
      , z    <- zs ]
  where
    sameSelect (Choice f1 t1 b1) (Choice f2 t2 b2) =
          not b1 && not b2 && sameSelect f1 f2 ||
              b1 &&     b2 && sameSelect t1 t2
    sameSelect _ _ = true
