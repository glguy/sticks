{-# Language TypeFamilies #-}
{-|
Module      : Derive.Codec
Description : Derived Codec instances for Traversables
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

This module is intended to be used with DerivingVia
in order to generate Codec instances for a type that
is parameterized over it's boolean representation.

-}
module Derive.Codec (TraversableCodec(..)) where

import Ersatz (Codec(..))

newtype TraversableCodec f a = TC (f a)

instance (Traversable f, Codec a) => Codec (TraversableCodec f a) where
    type Decoded (TraversableCodec f a) = f (Decoded a)
    decode sln (TC x) = traverse (decode sln) x
    encode x = TC (fmap encode x)
