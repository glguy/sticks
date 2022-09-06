{-# Language UndecidableInstances, FlexibleContexts #-}
module ChooseBit where

import Prelude hiding ((&&), not)
import Ersatz
import GHC.Generics

-- | Class for values that can be symbolically selected.
class ChooseBit a where
  chooseBit ::
    a   {- ^ false case -} ->
    a   {- ^ true  case -} ->
    Bit {- ^ selector   -} ->
    a

newtype GenericChooseBit a = GCB a

instance (GChooseBit (Rep a), Generic a) => ChooseBit (GenericChooseBit a) where
  chooseBit (GCB x) (GCB y) b = GCB (to (gchooseBit (from x) (from y) b))

instance ChooseBit Bit where chooseBit = choose

instance ChooseBit Bits where
  chooseBit (Bits x0) (Bits y0) b = Bits (merge x0 y0)
    where
      merge []     ys     = map (    b &&) ys
      merge xs     []     = map (not b &&) xs
      merge (x:xs) (y:ys) = choose x y b : merge xs ys

deriving via GenericChooseBit Bit1 instance ChooseBit Bit1
deriving via GenericChooseBit Bit2 instance ChooseBit Bit2
deriving via GenericChooseBit Bit3 instance ChooseBit Bit3
deriving via GenericChooseBit Bit4 instance ChooseBit Bit4
deriving via GenericChooseBit Bit5 instance ChooseBit Bit5
deriving via GenericChooseBit Bit6 instance ChooseBit Bit6
deriving via GenericChooseBit Bit7 instance ChooseBit Bit7
deriving via GenericChooseBit Bit8 instance ChooseBit Bit8
deriving via GenericChooseBit (a,b) instance (ChooseBit a, ChooseBit b) => ChooseBit (a,b)
deriving via GenericChooseBit (a,b,c) instance (ChooseBit a, ChooseBit b, ChooseBit c) => ChooseBit (a,b,c)

class GChooseBit f where
  gchooseBit :: f a -> f a -> Bit -> f a

instance GChooseBit f => GChooseBit (M1 i c f) where
  gchooseBit (M1 x) (M1 y) b = M1 (gchooseBit x y b)

instance (GChooseBit f, GChooseBit g) => GChooseBit (f :*: g) where
  gchooseBit (x1 :*: x2) (y1 :*: y2) b = gchooseBit x1 y1 b :*: gchooseBit x2 y2 b

instance ChooseBit a => GChooseBit (K1 i a) where
  gchooseBit (K1 x) (K1 y) b = K1 (chooseBit x y b)

instance GChooseBit U1 where
  gchooseBit _ _ _ = U1

instance GChooseBit V1 where
  gchooseBit x _ _ = case x of {}
