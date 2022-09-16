module Main where

import Prelude hiding ((||))
import Data.Foldable (traverse_)
import Ersatz (assert, Boolean((||)), Bits, Equatable((===)))
import Symbolic (solve)
import Symbolic.ChooseBit (ChooseBit(..))
import Symbolic.Select (selectPermutation, selectPermutation')

main :: IO ()
main =
 do let names = ["Blake","Colleen","Hazel","Virgil"]
    Just (pms, lengths) <- solve
     do pms     <- selectPermutation ["Atlee", "Asquith", "Balfour", "North"]
        lengths <- selectPermutation' [6,8,10,12 :: Bits]

        assert (pick (pure "Balfour") pms lengths ===
                pick (pure "North"  ) pms lengths + 2)
        
        assert (pick "Virgil" names lengths ===
                pick "Hazel"  names lengths + 4)
        
        let x3 = (pick (pure "Balfour") pms lengths, pick "Blake" names lengths)
        assert (x3 === (8,10) || x3 === (10,8))

        assert (pick "Virgil" names lengths === 8 ||
                pick "Virgil" names pms     === pure "Atlee")

        pure (pms, lengths)
    
    traverse_ print (zip3 names pms lengths)

pick :: (Equatable a, ChooseBit b) => a -> [a] -> [b] -> b
pick p (_:xs) (y:ys) = foldl (\acc (a,b) -> chooseBit acc b (p === a)) y (zip xs ys)
pick _ _ _ = error "pick: bad arguments"
