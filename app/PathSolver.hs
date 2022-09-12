{-# Language MonadComprehensions #-}
module PathSolver (Action(..), findPath, simplify, actBlock) where

import Control.Lens
import Data.List (find)
import Control.Lens.Internal.Context (Pretext(..))
import Control.Monad.Trans.Writer ( WriterT(..) )
import Control.Monad.Trans.Class ( MonadTrans(lift) )

import Block (Block, Stick(Stick), sticks, stick, turnLeft, turnRight, shiftUp, shiftDown, checkBlock)
import Searching.Search ( bfsOn )

data Action
    = ActUp
    | ActDown
    | ActLeft
    | ActRight
    | ActInsert (Stick Bool)
    deriving (Read, Show, Eq, Ord)

-- | Compress sequences of actions on sticks that inserted just prior.
simplify :: [(Int,Action)] -> [(Int,Action)]
simplify ((i, ActInsert s):(j, a):as) | i==j = simplify ((i, ActInsert (actStick a s)):as)
simplify (x:xs) = x : simplify xs
simplify [] = []

findPath ::
    [Int] {- ^ order of insertion -} ->
    [Block Bool] {- ^ SAT solver route -} ->
    Maybe [(Int,Action)] {- ^ route is valid -}
findPath (i:ks) (x:y:zs) =
 do let s = y ^?! stick i
    p1 <- findPath1 (set (stick i) s x) y
    ps <- findPath ks (y:zs)
    pure ((i,ActInsert s) : p1 <> ps)
findPath _ _ = Just []

findPath1 :: Block Bool -> Block Bool -> Maybe [(Int, Action)]
findPath1 src tgt =
    fmap snd $
    find ((tgt==) . fst) $
    bfsOn fst (\b -> runWriterT (WriterT [b] >>= next)) (src,[])

next :: Block Bool -> WriterT [(Int, Action)] [] (Block Bool)
next b =
    [ b'
    | h  <- lift (holesOf sticks b)
    , b' <- irunPretext h edits
    , checkBlock b'
    ]

irunPretext :: Functor f => Pretext (Indexed i) a b t -> (i -> a -> f b) -> f t
irunPretext h f = runPretext h (Indexed f)

edits :: Int -> Stick Bool -> WriterT [(Int, Action)] [] (Stick Bool)
edits i s = WriterT $
    [(t,           [(i,ActUp   )]) | t <- slideUp   s] <>
    [(t,           [(i,ActDown )]) | t <- slideDown s] <>
    [(turnLeft  s, [(i,ActLeft )]),
     (turnRight s, [(i,ActRight)])]

slideUp :: Stick Bool -> [Stick Bool]
slideUp (Stick lo mi hi x y z w) = [Stick False lo mi x y z w | not hi]

slideDown :: Stick Bool -> [Stick Bool]
slideDown (Stick lo mi hi x y z w) = [Stick mi hi False x y z w | not lo]

actBlock :: Int -> Action -> Block Bool -> Block Bool
actBlock i a b = over (stick i) (actStick a) b

actStick :: Action -> Stick Bool -> Stick Bool
actStick = \case
    ActDown     -> shiftDown
    ActUp       -> shiftUp
    ActLeft     -> turnLeft
    ActRight    -> turnRight
    ActInsert s -> const s
