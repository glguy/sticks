{-|
Module      : Parser
Description : Parse an Unruly starting state
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

Parse a string using @x@ and @o@ to mark the two colors.

-}
module Parser (parser) where

import Data.Map (Map)
import Data.Map qualified as Map
import Puzzle

cluesToBoard :: Int -> Map (Int,Int) Bool -> Board ()
cluesToBoard n clues = Board
    [[maybe (Open ()) Given (Map.lookup (r,c) clues) | c <- [0..n-1]] | r <- [0..n-1]]

parser :: String -> Board ()
parser str =
    let rows = lines str in
    cluesToBoard
        (length rows)
        (Map.fromList
            [ ((y,x), col == 'o')
            | (y,row) <- zip [0..] (lines str)
            , (x,col) <- zip [0..] row
            , col `elem` "ox"])