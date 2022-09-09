{-# Language UndecidableInstances #-}
module Derive.Applicative (GenericApplicative(..)) where

import GHC.Generics

class GApplicative f where
    gpure :: x -> f x
    gap :: f (a -> b) -> f a -> f b

instance GApplicative f => GApplicative (M1 i c f) where
    gpure x = M1 (gpure x)
    gap (M1 f) (M1 x) = M1 (gap f x)

instance (GApplicative f, GApplicative g) => GApplicative (f :*: g) where
    gpure x = gpure x :*: gpure x
    gap (f1 :*: f2) (x1 :*: x2) = gap f1 x1 :*: gap f2 x2

instance GApplicative Par1 where
    gpure x = Par1 x
    gap (Par1 f) (Par1 x) = Par1 (f x)

instance Applicative f => GApplicative (Rec1 f) where
    gpure x = Rec1 (pure x)
    gap (Rec1 f) (Rec1 x) = Rec1 (f <*> x)

instance GApplicative U1 where
    gpure _ = U1
    gap _ _ = U1

newtype GenericApplicative f a = GA (f a)
    deriving Functor

instance (Functor f, Generic1 f, GApplicative (Rep1 f)) => Applicative (GenericApplicative f) where
    pure x = GA (to1 (gpure x))
    GA f <*> GA x = GA (to1 (gap (from1 f) (from1 x)))
