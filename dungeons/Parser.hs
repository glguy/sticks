{- |
Module:       Parser
Description:  Functions for parsing puzzles.
-}
module Parser (parse, parseSolutionArray) where

import Data.Char ( isDigit, digitToInt )
import Puzzle (Puzzle(Puzzle), Elt(..), makePuzzle)
import Data.List.Split
import Data.List (transpose)

-- | Parse a simple ASCII puzzle representation.
--
-- * @C@ for chests
-- * @M@ for monsters
-- * @.@ for empty spaces
--
-- @
--  0002
-- 1....
-- 0.C.M
-- 1....
-- @
parse :: String -> Maybe (Puzzle ())
parse input =
  case lines input of
    (' ':ns):rs ->
      (() <$) <$>
      (Puzzle
        <$> traverse parseDigit ns
        <*> traverse parseRow rs)
    _ -> Nothing

parseDigit :: Char -> Maybe Int
parseDigit c
  | isDigit c = Just $! digitToInt c
  | otherwise = Nothing

parseRow :: String -> Maybe (Int, [Elt Bool])
parseRow (n:xs) = (,) <$> parseDigit n <*> traverse parseCell xs
parseRow [] = Nothing

parseSolutionArray :: String -> Maybe [Puzzle Bool]
parseSolutionArray str =
  sequence
  [ makePuzzle <$> traverse (traverse parseCell) puz
    | puz_row <- wordsBy null (map words (lines str))
    , puz <- transpose puz_row
  ]

parseCell :: Char -> Maybe (Elt Bool)
parseCell 'O' = Just (O False)
parseCell '.' = Just (O True)
parseCell 'C' = Just C
parseCell 'M' = Just M
parseCell _   = Nothing