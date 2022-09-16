{-|
Module      : Renderer
Description : Console renderer for Unruly
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Renderer (render) where

import Puzzle
import Data.Foldable (for_)
import System.Console.ANSI

render :: Board Bool -> IO ()
render (Board rows) =
    for_ rows \row ->
     do for_ row \x ->
         do let (color, sigil) =
                    case x of
                        Given True  -> (Red , '★')
                        Given False -> (Blue, '★')
                        Open  True  -> (Red , '■')
                        Open  False -> (Blue, '■')
            setSGR [SetColor Foreground Dull color]
            putChar sigil
            setSGR [Reset]
        putChar '\n'
