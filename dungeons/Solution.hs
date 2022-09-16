{-# LANGUAGE MonadComprehensions #-}
{- |
Module:       Solution
Description:  Functions for puzzle solutions.
-}
module Solution (solutionExists, validPuzzle) where

import Prelude hiding (all, (&&), (||), not, any, and, or)
import Control.Monad (replicateM)
import Symbolic.Counting (exactly, atLeast, atMost)
import Data.Foldable (for_)
import Data.Map qualified as Map
import Data.Traversable (for)
import Ersatz
    ( Bit,
      Bits(Bits),
      Boolean((&&), (==>), (||), false, not, any, all, and, or),
      MonadSAT,
      assert, encode, exists,
      (===), (/==), (<?))
import Grids (cardinal, gridMap, region3, twoArc, walls3)
import Puzzle
    ( Puzzle(clueRows),
      Elt(C, O, M),
      clueCols, isChest, isPath, rows, topClues)

-- | Predicate for solved puzzles.
validPuzzle :: Boolean a => Puzzle a -> a
validPuzzle p = validCounts p && validCells p

-- | Predicate for puzzles with valid row and column count.
validCounts :: Boolean a => Puzzle a -> a
validCounts p =
  all
    (\(n,xs) -> exactly n (not . isPath <$> xs))
    (clueRows p <> clueCols p)

-- | Predicate for puzzles with valid local element constraints.
validCells :: Boolean a => Puzzle a -> a
validCells p = all (uncurry validCell) (Map.assocs m)
  where
    validCell k = \case
      O o -> o ==> inChestRoom k || atLeast 2 (entrances cardinal k) && no2x2 k
      M   -> exactly 1 (entrances cardinal k)
      C   -> inChestRoom k

    m = gridMap (rows p)
    at k = Map.lookup k m

    inChestRoom = any (\x -> Map.findWithDefault false x cache) . region3
      where
        cache = Map.mapMaybeWithKey (\k _ -> isChestRoomCenter k) m
        isChestRoomCenter k =
          [ all isPath room && exactly 1 (entrances walls3 k)
            | room <- traverse at (region3 k)
            , exactly 1 (isChest <$> room)
          ]

    entrances f k = [isPath z | Just z <- at <$> f k]

    no2x2 c = not (any (all isPath) (traverse at (twoArc c)))

-- | Check that all the paths in a puzzle solution are connected.
--
-- This is achieved by assigning each path a number and checking
-- that every path is touching a smaller number. Only one square
-- is allowed to be assigned the special zero number, which does
-- not need to be connected to anything else.
connected :: MonadSAT s m => Puzzle Bit -> m ()
connected p =
 do -- count up expected number of open spaces
    let n = length (concat (rows p)) - sum (topClues p)

    -- pick a number of bits large enough to allow every path to have a unique number
    let mkIndex = Bits <$> replicateM (bitsNeeded n) exists

    -- assign each location a number
    m <- for (gridMap (rows p)) \e ->
     do i <- mkIndex
        pure (e,i)

    -- at most one path can be assigned index 0
    assert (atMost 1 [isPath e && 0 === i | (e,i) <- Map.elems m])

    let neighborCheck k i =
          0 === i ||
          or [isPath e && j <? i | c <- cardinal k, Just (e,j) <- [Map.lookup c m]]

    -- every path element is adjacent to a smaller path element or is the zero element
    assert (and [isPath e ==> neighborCheck k i | (k,(e,i)) <- Map.assocs m])

-- | Base-2 integer logarithm rounded up
bitsNeeded :: Int -> Int
bitsNeeded = go 0
  where
    go acc n
      | n < 2     = acc
      | otherwise = go (1+acc) ((n+1) `div` 2)

-- | Generate a solution to the input puzzle.
solutionExists ::
  MonadSAT s m =>
  [Puzzle Bool] {- ^ excluded solutions -} ->
  Puzzle a      {- ^ puzzle to solve    -} ->
  m (Puzzle Bit)
solutionExists old p =
 do b <- for p \_ -> exists
    assert (validPuzzle b)
    for_ old \o -> assert (encode o /== b)
    connected b
    pure b
