{-|
Module      : Derive.Each
Description : Derived Each implementations
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

This module uses GHC.Generics to derive
Traversals suitable for use with the Each
class in lens.

-}
module Derive.Each (genericEach) where

import Control.Lens (Traversal, confusing)
import GHC.Generics
import GHC.Generics.Lens (generic)

genericEach :: (Generic s, Generic t, GEach (Rep s) (Rep t) a b) => Traversal s t a b
genericEach = confusing (generic . geach)
{-# Inline genericEach #-}

class GEach s t a b where
    geach :: Traversal (s x) (t y) a b

instance GEach s t a b => GEach (M1 i c s) (M1 j d t) a b where
    geach f (M1 x) = M1 <$> geach f x
    {-# Inline geach #-}

instance (GEach s1 t1 a b, GEach s2 t2 a b) => GEach (s1 :*: s2) (t1 :*: t2) a b where
    geach f (x :*: y) = (:*:) <$> geach f x <*> geach f y
    {-# Inline geach #-}

instance (GEach s1 t1 a b, GEach s2 t2 a b) => GEach (s1 :+: s2) (t1 :+: t2) a b where
    geach f (L1 x) = L1 <$> geach f x
    geach f (R1 x) = R1 <$> geach f x
    {-# Inline geach #-}

instance GEach V1 V1 a b where
    geach _ = \case {}
    {-# Inline geach #-}

instance GEach U1 U1 a b where
    geach _ U1 = pure U1
    {-# Inline geach #-}

instance GEach (K1 i a) (K1 i b) a b where
    geach f (K1 x) = K1 <$> f x
    {-# Inline geach #-}
