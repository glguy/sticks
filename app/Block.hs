{-# Language MultiParamTypeClasses, UndecidableInstances #-}
module Block where

import Prelude hiding ((||), and)
import Control.Lens (over, Each(each) )
import Ersatz
import GHC.Generics (Generic, Generic1)

import Derive.Applicative (GenericApplicative(..))
import Derive.Codec (TraversableCodec(..))
import Derive.Each (genericEach)
import Symbolic.ChooseBit (GenericChooseBit(..), ChooseBit)

data Side a = Side { cut1, cut2, cut3, cut4, cut5 :: a }
    deriving (Read, Show, Eq, Ord, Functor, Foldable, Traversable, Generic, Generic1, Equatable)
    deriving ChooseBit   via GenericChooseBit   (Side a)
    deriving Codec       via TraversableCodec   Side a
    deriving Applicative via GenericApplicative Side

data Stick a = Stick { top, left, bottom, right :: Side a }
    deriving (Read, Show, Eq, Ord, Functor, Foldable, Traversable, Generic, Generic1, Equatable)
    deriving ChooseBit   via GenericChooseBit   (Stick a)
    deriving Codec       via TraversableCodec   Stick a
    deriving Applicative via GenericApplicative Stick

data Block a = Block { stick1, stick2, stick3, stick4, stick5, stick6 :: Stick a }
    deriving (Read, Show, Eq, Ord, Functor, Foldable, Traversable, Generic, Generic1, Equatable)
    deriving ChooseBit   via GenericChooseBit   (Block a)
    deriving Codec       via TraversableCodec   Block a
    deriving Applicative via GenericApplicative Block

instance Each (Stick a) (Stick b) (Side a) (Side b) where
    each = genericEach
    {-# Inline each #-}

instance Each (Block a) (Block b) (Stick a) (Stick b) where
    each = genericEach
    {-# Inline each #-}

shifts :: Boolean a => Stick a -> [Stick a]
shifts s = [s, over each shiftl s, over each shiftr s]

shiftl :: Boolean a => Side a -> Side a
shiftl (Side _ a b c d) = Side a b c d false

shiftr :: Boolean a => Side a -> Side a
shiftr (Side b c d e _) = Side false b c d e

flips :: Stick a -> [Stick a]
flips s = [s, flipStick s]

flipStick :: Stick a -> Stick a
flipStick (Stick x y z w) = Stick (f x) (f w) (f z) (f y)
    where
        f (Side a b c d e) = Side e d c b a

gapStick :: Boolean a => Stick a
gapStick = Stick s s s s
    where
        s = Side false true true true false

solidStick :: Boolean a => Stick a
solidStick = pure false

turnLeft :: Stick a -> Stick a
turnLeft (Stick x y z w) = Stick w x y z

turnRight :: Stick a -> Stick a
turnRight (Stick x y z w) = Stick y z w x

turns :: Stick a -> [Stick a]
turns (Stick x y z w) =
    [ Stick x y z w
    , Stick y z w x
    , Stick z w x y
    , Stick w x y z
    ]


checkBlock :: Boolean b => Block b -> b
checkBlock (Block s1 s2 s3 s4 s5 s6) =
    and
    [ cut2 (right  s1) || cut3 (left   s3)
    , cut3 (top    s1) || cut4 (left   s5)
    , cut3 (bottom s1) || cut4 (right  s6)
    , cut4 (right  s1) || cut3 (left   s4)

    , cut2 (left   s2) || cut3 (right  s3)
    , cut3 (top    s2) || cut2 (left   s5)
    , cut3 (bottom s2) || cut2 (right  s6)
    , cut4 (left   s2) || cut3 (right  s4)

    , cut2 (bottom s3) || cut3 (top    s5)
    , cut4 (bottom s3) || cut3 (top    s6)

    , cut2 (top    s4) || cut3 (bottom s5)
    , cut4 (top    s4) || cut3 (bottom s6)
    ]
