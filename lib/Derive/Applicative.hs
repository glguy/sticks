{-# Language UndecidableInstances #-}
{-|
Module      : Derive.Applicative
Description : Derived Applicative instances
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

This module is intended to be used with DerivingVia
in order to generate Applicative instances for product
types.

-}
module Derive.Applicative (GenericApplicative(..)) where

import Control.Applicative (liftA2)
import GHC.Generics

class GApplicative f where
    gpure :: x -> f x
    gap :: f (a -> b) -> f a -> f b
    gliftA2 :: (a -> b -> c) -> f a -> f b -> f c

instance GApplicative f => GApplicative (M1 i c f) where
    gpure x = M1 (gpure x)
    gap (M1 f) (M1 x) = M1 (gap f x)
    gliftA2 f (M1 x) (M1 y) = M1 (gliftA2 f x y)
instance (GApplicative f, GApplicative g) => GApplicative (f :*: g) where
    gpure x = gpure x :*: gpure x
    gap (f1 :*: f2) (x1 :*: x2) = gap f1 x1 :*: gap f2 x2
    gliftA2 f (x1 :*: x2) (y1 :*: y2) = gliftA2 f x1 y1 :*: gliftA2 f x2 y2
instance GApplicative Par1 where
    gpure x = Par1 x
    gap (Par1 f) (Par1 x) = Par1 (f x)
    gliftA2 f (Par1 x) (Par1 y) = Par1 (f x y)

instance Applicative f => GApplicative (Rec1 f) where
    gpure x = Rec1 (pure x)
    gap (Rec1 f) (Rec1 x) = Rec1 (f <*> x)
    gliftA2 f (Rec1 x) (Rec1 y) = Rec1 (liftA2 f x y)

instance Monoid e => GApplicative (K1 i e) where
    gpure _ = K1 mempty
    gap (K1 x) (K1 y) = K1 (x <> y)
    gliftA2 _ (K1 x) (K1 y) = K1 (x <> y)

instance GApplicative U1 where
    gpure _ = U1
    gap _ _ = U1
    gliftA2 _ _ _ = U1

newtype GenericApplicative f a = GA (f a)
    deriving Functor

instance (Functor f, Generic1 f, GApplicative (Rep1 f)) => Applicative (GenericApplicative f) where
    pure x = GA (to1 (gpure x))
    GA f <*> GA x = GA (to1 (gap (from1 f) (from1 x)))
    liftA2 f (GA x) (GA y) = GA (to1 (gliftA2 f (from1 x) (from1 y)))
