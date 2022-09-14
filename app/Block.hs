{-# Language RankNTypes, ViewPatterns, UndecidableInstances #-}
module Block where

import Prelude hiding ((||), (&&), and)
import Control.Lens ((<&>), Lens', Traversal, Traversal')
import Ersatz ( Boolean((||), true, false, (&&), and), Codec, Equatable )
import GHC.Generics (Generic, Generic1)

import Derive.Applicative (GenericApplicative(..))
import Derive.Codec (TraversableCodec(..))
import Derive.Each (genericEach)
import Symbolic.ChooseBit (GenericChooseBit(..), ChooseBit)

-- | Five positions of a side of a stick marked true for cutouts
data Side a = Side a a a a a
    deriving (Read, Show, Eq, Ord, Functor, Foldable, Traversable, Generic, Generic1, Equatable)
    deriving ChooseBit   via GenericChooseBit   (Side a)
    deriving Codec       via TraversableCodec   Side a
    deriving Applicative via GenericApplicative Side

data Stick a = Stick { posLo, posMid, posHi :: a, top, left, bottom, right :: Side a }
    deriving (Read, Show, Eq, Ord, Functor, Foldable, Traversable, Generic, Generic1, Equatable)
    deriving ChooseBit   via GenericChooseBit   (Stick a)
    deriving Codec       via TraversableCodec   Stick a
    deriving Applicative via GenericApplicative Stick

data Block a = Block { stick1, stick2, stick3, stick4, stick5, stick6 :: Stick a }
    deriving (Read, Show, Eq, Ord, Functor, Foldable, Traversable, Generic, Generic1, Equatable)
    deriving ChooseBit   via GenericChooseBit   (Block a)
    deriving Codec       via TraversableCodec   Block a
    deriving Applicative via GenericApplicative Block

sticks :: Traversal (Block a) (Block b) (Stick a) (Stick b)
sticks = genericEach
{-# INLINE sticks #-}

stick :: Int -> Lens' (Block a) (Stick a)
stick 0 f (Block x1 x2 x3 x4 x5 x6) = f x1 <&> \x' -> Block x' x2 x3 x4 x5 x6
stick 1 f (Block x1 x2 x3 x4 x5 x6) = f x2 <&> \x' -> Block x1 x' x3 x4 x5 x6
stick 2 f (Block x1 x2 x3 x4 x5 x6) = f x3 <&> \x' -> Block x1 x2 x' x4 x5 x6
stick 3 f (Block x1 x2 x3 x4 x5 x6) = f x4 <&> \x' -> Block x1 x2 x3 x' x5 x6
stick 4 f (Block x1 x2 x3 x4 x5 x6) = f x5 <&> \x' -> Block x1 x2 x3 x4 x' x6
stick 5 f (Block x1 x2 x3 x4 x5 x6) = f x6 <&> \x' -> Block x1 x2 x3 x4 x5 x'
stick _ _ _ = error "stick: bad index"
{-# Inline stick #-}

sides :: Traversal' (Stick a) (Side a)
sides f (Stick a b c x y z w) = Stick a b c <$> f x <*> f y <*> f z <*> f w
{-# INLINE sides #-}

flipStick :: Stick a -> Stick a
flipStick (Stick lo mi hi x y z w) = Stick hi mi lo (f x) (f w) (f z) (f y)
    where
        f (Side a b c d e) = Side e d c b a

emptyBlock :: Boolean a => Block a
emptyBlock = pure true

noStick :: Boolean a => Stick a
noStick = pure true

solidStick :: Boolean a => Stick a
solidStick = pure false

turnLeft :: Stick a -> Stick a
turnLeft (Stick lo mi hi x y z w) = Stick lo mi hi w x y z

turnRight :: Stick a -> Stick a
turnRight (Stick lo mi hi x y z w) = Stick lo mi hi y z w x

shiftUp :: Boolean a => Stick a -> Stick a
shiftUp (Stick a b _ x y z w) = Stick false a b x y z w

shiftDown :: Boolean a => Stick a -> Stick a
shiftDown (Stick _ b c x y z w) = Stick b c false x y z w

-----------------------------------------------------------------------

flips :: Stick a -> [Stick a]
flips s = [s, flipStick s]

turns :: Stick a -> [Stick a]
turns (Stick lo mi hi x y z w) =
    [ Stick lo mi hi x y z w
    , Stick lo mi hi y z w x
    , Stick lo mi hi z w x y
    , Stick lo mi hi w x y z
    ]

-----------------------------------------------------------------------

cut :: Boolean b => Stick b -> b -> b -> b -> b
cut (Stick lo mi hi _ _ _ _) x y z = lo&&x || mi&&y || hi&&z

cut2 :: Boolean b => (Stick b -> Side b) -> Stick b -> b
cut2 sel s@(sel -> Side x y z _ _) = cut s x y z

cut3 :: Boolean b => (Stick b -> Side b) -> Stick b -> b
cut3 sel s@(sel -> Side _ x y z _) = cut s x y z

cut4 :: Boolean b => (Stick b -> Side b) -> Stick b -> b
cut4 sel s@(sel -> Side _ _ x y z) = cut s x y z

-- | Predicate to check that all stick intersections contain at least one cutout.
checkBlock :: Boolean b => Block b -> b
checkBlock (Block s1 s2 s3 s4 s5 s6) =
    and
    [ cut2 right  s1 || cut3 left   s3
    , cut3 top    s1 || cut4 left   s5
    , cut3 bottom s1 || cut4 right  s6
    , cut4 right  s1 || cut3 left   s4

    , cut2 left   s2 || cut3 right  s3
    , cut3 top    s2 || cut2 left   s5
    , cut3 bottom s2 || cut2 right  s6
    , cut4 left   s2 || cut3 right  s4

    , cut4 bottom s3 || cut3 top    s5
    , cut2 bottom s3 || cut3 top    s6

    , cut4 top    s4 || cut3 bottom s5
    , cut2 top    s4 || cut3 bottom s6
    ]
