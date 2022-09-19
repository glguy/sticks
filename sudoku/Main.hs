{-|
Module      : Main
Description : Sudoku solver
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

Yet another Sudoku solver. This one happens to represent
each cell with 9 bits, one for each possible assignment
of number to that cell.

-}
module Main where

import Prelude hiding (all)
import Control.Monad (replicateM)
import Data.Char (digitToInt, intToDigit)
import Data.List (transpose, elemIndex, intercalate)
import Data.List.Split (chunksOf)
import Ersatz (assert, exists, Bit, Boolean(false, all, true), MonadSAT)
import Symbolic (solve)
import Symbolic.Counting (exactly)

-- | Rows of columns of list of 9 bits corresponding to cell assignment
type Puzzle a = [[[a]]]

valid :: Boolean a => Puzzle a -> a
valid p = all (exactly 1) (cells <> rows <> cols <> squares)
    where
        cells   = concat p
        rows    = concatMap transpose p
        cols    = concatMap transpose (transpose p)
        squares = concatMap transpose (toSquares 3 p)

toPuzzle :: MonadSAT s m => [[Char]] -> m (Puzzle Bit)
toPuzzle = traverse $ traverse \case
    c | '1' <= c, c <= '9', let n = digitToInt c ->
        pure (replicate (n-1) false <> [true] <> replicate (9-n) false)
    _ -> replicateM 9 exists

fromPuzzle :: Puzzle Bool -> [[Char]]
fromPuzzle xs = map (map (maybe '.' (intToDigit . (1+)) . elemIndex True)) xs

main :: IO ()
main =
 do input <- lines <$> getContents
    res <- solve
     do p <- toPuzzle input
        assert (valid p)
        pure p
    case res of
        Nothing -> putStrLn "Bad puzzle"
        Just p -> putStr (addLines (fromPuzzle p))

addLines :: [[Char]] -> String
addLines
    = intercalate "───┼───┼───\n"
    . map (unlines . map (intercalate "│" . chunksOf 3))
    . chunksOf 3

-----------------------------------------------------------------------

toSquares :: Int -> [[a]] -> [[a]]
toSquares n
    = concatMap (map concat . transpose)
    . chunksOf n
    . map (chunksOf n)
