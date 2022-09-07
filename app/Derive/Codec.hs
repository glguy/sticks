{-# Language TypeFamilies #-}
module Derive.Codec where

import Ersatz

newtype TraversableCodec f a = TC (f a)

instance (Traversable f, Codec a) => Codec (TraversableCodec f a) where
    type Decoded (TraversableCodec f a) = f (Decoded a)
    decode sln (TC x) = traverse (decode sln) x
    encode x = TC (fmap encode x)
