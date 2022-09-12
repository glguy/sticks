{-# Language PartialTypeSignatures #-}
module PathSolver (Action(..), findPath, simplify, applyAction) where

import Control.Lens
import Data.List (find)
import Control.Lens.Internal.Context (Pretext(..))
import Data.Functor.Compose

import Block
import Searching.Search ( bfsOn )

data Action
    = ActUp
    | ActDown
    | ActLeft
    | ActRight
    | ActInsert (Stick Bool)
    deriving (Read, Show, Eq, Ord)

simplify :: [(Int,Action)] -> [(Int,Action)]
simplify ((i, ActInsert s):(j, ActLeft ):as) | i==j = simplify ((i, ActInsert (turnLeft  s)):as)
simplify ((i, ActInsert s):(j, ActRight):as) | i==j = simplify ((i, ActInsert (turnRight s)):as)
simplify ((i, ActInsert s):(j, ActUp   ):as) | i==j = simplify ((i, ActInsert (shiftUp   s)):as)
simplify ((i, ActInsert s):(j, ActDown ):as) | i==j = simplify ((i, ActInsert (shiftDown s)):as)
simplify (x:xs) = x : simplify xs
simplify [] = []

findPath ::
    [Int] {- ^ order of insertion -} ->
    [Block Bool] {- ^ SAT solver route -} ->
    Maybe [(Int,Action)] {- ^ route is valid -}
findPath (i:ks) (x:y:zs) =
 do let s = getAt i y
    p1 <- findPath1 (setAt i s x) y
    ps <- findPath ks (y:zs)
    pure ((i,ActInsert s) : p1 <> ps)
findPath _ _ = Just []

findPath1 :: Block Bool -> Block Bool -> Maybe [(Int, Action)]
findPath1 src tgt =
    fmap (reverse . snd) $
    find ((tgt==) . fst) $
    bfsOn fst (\(b,xs) -> map (fmap (:xs)) (next b)) (src, [])

setAt :: Int -> Stick b -> Block b -> Block b
setAt i = set (sticks . index i)

getAt :: Int -> Block a -> Stick a
getAt i = (^?! sticks . index i)

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

applyAction :: (Int, Action) -> Block Bool -> Block Bool
applyAction (i,a) b = over (sticks . index i) act b
    where
        act = case a of
            ActDown     -> shiftDown
            ActUp       -> shiftUp
            ActLeft     -> turnLeft
            ActRight    -> turnRight
            ActInsert s -> const s

shiftUp :: Stick Bool -> Stick Bool
shiftUp (Stick a b c x y z w) | c = undefined | otherwise = Stick False a b x y z w

shiftDown :: Stick Bool -> Stick Bool
shiftDown (Stick a b c x y z w) | a = undefined | otherwise = Stick b c False x y z w
