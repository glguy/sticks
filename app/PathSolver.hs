{-# Language PartialTypeSignatures #-}
module PathSolver (Action(..), findPath, simplify, actBlock) where

import Control.Lens
import Data.List (find)
import Control.Lens.Internal.Context (Pretext(..))
import Data.Functor.Compose

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
    fmap (reverse . snd) $
    find ((tgt==) . fst) $
    bfsOn fst (\(b,xs) -> map (fmap (:xs)) (next b)) (src, [])

next :: Block Bool -> [(Block Bool, (Int, Action))]
next b =
    [ (b',a)
        | h <- holesOf sticks b
        , (a,b') <- (getCompose . runPretext h . rmap Compose) (Indexed editStick)
        , checkBlock b'
    ]

editStick :: Int -> Stick Bool -> [((Int, Action), Stick Bool)]
editStick i s =
    [((i,ActUp  ), t) | t <- slideUp s] <>
    [((i,ActDown), t) | t <- slideDown s] <>
    [((i,ActLeft),turnLeft s),
     ((i,ActRight),turnRight s)]

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
