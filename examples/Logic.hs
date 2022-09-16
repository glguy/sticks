module Main where

import Ersatz
import Prelude hiding ((||))
import Data.Foldable (traverse_)
import Symbolic.Select
import Symbolic.ChooseBit

selectPermutation' :: MonadSAT s m => ChooseBit a => [a] -> m [a]
selectPermutation' xs = map runSelect <$> selectPermutation xs

main :: IO ()
main =
 do let names = ["Blake","Colleen","Hazel","Virgil"]
    (Satisfied, Just (pms, lengths)) <- solveWith cryptominisat5
     do pms <- selectPermutation ["Atlee", "Asquith", "Balfour", "North"]
        lengths <- selectPermutation' [6,8,10,12 :: Bits]

        assert (pick (pure "Balfour") pms lengths === pick (pure "North") pms lengths + 2)
        
        assert (pick "Hazel" names lengths + 4 === pick "Virgil" names lengths)
        
        let x3 = (pick (pure "Balfour") pms lengths, pick "Blake" names lengths)
        assert (x3 === (8,10) || x3 === (10,8))

        assert (pick "Virgil" names lengths === 8 ||
                pick "Virgil" names pms === pure "Atlee")

        pure (pms, lengths)
    
    traverse_ print (zip3 names pms lengths)


pick :: (Equatable a, ChooseBit b) => a -> [a] -> [b] -> b
pick p (_:xs) (y:ys) = go y xs ys
    where
        go acc (x:xs) (y:ys) = go (chooseBit acc y (p === x)) xs ys
        go acc _ _ = acc
