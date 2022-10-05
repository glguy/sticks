module Symbolic (solve, (==?)) where

import Prelude hiding ((&&), (||), not)
import Ersatz
import Control.Monad.Trans.State

solve :: Codec a => StateT SAT IO a -> IO (Maybe (Decoded a))
solve m =
 do res <- solveWith anyminisat m
    case res of
        (Satisfied, Just x) -> pure (Just x)
        (Unsatisfied, _   ) -> pure Nothing
        _ -> fail "solve: bad solver result"

(==?) :: Boolean a => a -> a -> a
x ==? y = choose (not y) y x
infix 4 ==?