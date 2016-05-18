module NonNegMatricies (nonNegMatricies) where

import Matrix
import Prelude
import Control.Monad (join)
import qualified Data.Vector as V 
import qualified Data.Set as Set

-- remove repititions from a list
mkUniq :: Ord a => [a] -> [a]
mkUniq = Set.toList . Set.fromList

listLengthSum :: Int -> Int -> [[Int]]
listLengthSum l 0 = [take l $ repeat 0]
listLengthSum l n = mkUniq $ join $ addOneToEachEntry <$> (listLengthSum l (n-1))


addOneToEachEntry :: [Int] -> [[Int]]
addOneToEachEntry [] = []
addOneToEachEntry (x:xs) = [((x+1):xs)] ++ ( (x:) <$> (addOneToEachEntry xs))

-- nonNegMatricies row_sums col_sums
nonNegMatricies :: [Int] -> [Int] -> [[[Int]]]
nonNegMatricies [r] c = if ((sum c) == r) && and ((0 <=) <$> c) then [[c]] else []
nonNegMatricies (r:rs) c = [ x:m | x <- listLengthSum (length c) r ,m <- nonNegMatricies rs (f <$> (zip c x)) ]
  where f (p,q) = p - q
