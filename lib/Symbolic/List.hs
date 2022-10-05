{-# Language GADTs, TypeFamilies #-}
module Symbolic.List where

import Prelude hiding ((&&), (||), not)
import Ersatz
import Control.Applicative (liftA2)

class ListChoice b where
    listChoice :: b -> List b a -> List b a -> List b a

instance ListChoice Bool where
    listChoice False x _ = x
    listChoice True _ x = x

instance ListChoice Bit where
    listChoice = Choice

toConcrete :: List Bool a -> [a]
toConcrete (Cons x xs) = x : toConcrete xs
toConcrete Nil         = []

fromConcrete :: [a] -> List b a
fromConcrete = foldr Cons Nil

data List b a where
    Nil    ::                                    List b   a
    Cons   :: a   -> List b a   ->               List b   a
    Choice :: Bit -> List Bit a -> List Bit a -> List Bit a

instance Show a => Show (List b a) where
    showsPrec _ Nil = showString "Nil"
    showsPrec p (Cons x xs) =
        showParen (p >= 11) $
        showString "Cons " .
        showsPrec 11 x .
        showChar  ' ' .
        showsPrec 11 xs
    showsPrec p (Choice b l r) =
        showParen (p >= 11) $
        showString "Choice " .
        showsPrec 11 b .
        showChar  ' ' .
        showsPrec 11 l .
        showChar  ' ' .
        showsPrec 11 r

instance Equatable a => Equatable (List b a) where
    Nil       === Nil       = true
    Nil       === Cons{}    = false
    Cons{}    === Nil       = false
    Cons x xs === Cons y ys = x === y && xs === ys
    Choice b l r === y      = choose (l===y) (r===y) b
    x === Choice b l r      = choose (x===l) (x===r) b

instance Orderable a => Orderable (List b a) where
    Nil       <? Nil       = false
    Nil       <? Cons{}    = true
    Cons{}    <? Nil       = false
    Cons x xs <? Cons y ys = x <? y || x === y && xs <? ys
    Choice b l r <? y      = choose (l <? y) (r <? y) b
    x <? Choice b l r      = choose (x <? l) (x <? r) b

instance (b ~ Bool, Eq a) => Eq (List b a) where
    x == y = toConcrete x == toConcrete y

instance (b ~ Bool, Ord a) => Ord (List b a) where
    x <= y = toConcrete x <= toConcrete y

instance Codec a => Codec (List b a) where
    type Decoded (List b a) = [Decoded a]

    encode = foldr Cons Nil . map encode

    decode _   Nil            = pure []
    decode sln (Cons x xs)    = liftA2 (:) (decode sln x) (decode sln xs)
    decode sln (Choice b x y) =
     do b' <- decode sln b
        decode sln if b' then y else x