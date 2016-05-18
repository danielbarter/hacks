module SymmetricFunctions where

import Data.List (subsequences)
import Partition
import GHC.Exts (groupWith)
import Bratelli
import SingleBox
import Data.Ratio

--symmetricQuotientMap :: (Num a) => [a] -> [a]
symmetricQuotientMap x 
  = fmap sum $ tail $ (fmap . fmap) prod $ groupWith length $ subsequences x
  where prod x = foldr (*) 1 x


dotProduct :: (Num a) => [a] -> [a] -> a
dotProduct l [] = 0
dotProduct [] l = 0
dotProduct (x:xs) (y:ys) = x * y + (dotProduct xs ys)

symCoefficients n = 
 (fmap . fmap) (\x -> x % 1)  $ fmap (symmetricQuotientMap . toList) $ verticiesOfLevel n



falseIfRepeats ls = (length ls) == (length $ mkUniq ls)

-- random vectors produced using python
rv1 = [92, 31, 60, 82, 99, 52, 19, 14, 11, 45, 17, 54, 29, 15, 79, 87, 81, 78, 36, 10, 
       36, 24, 28, 73, 70, 47, 26, 41, 100, 15, 71, 13, 68, 98, 5, 98, 20, 48, 59, 25, 
       18, 71, 4, 67, 2, 91, 11, 76, 50, 99, 60, 61, 23, 70, 7, 60, 60, 75, 11, 54, 
       11, 41, 63, 42, 81, 48, 32, 39, 92, 30, 56, 19, 22, 31, 18, 21, 25, 9, 80, 53, 
       41, 59, 27, 48, 95, 7, 5, 3, 95, 30, 98, 32, 40, 60, 79, 57, 55, 100, 51]


rv2 =  [60, 97, 19, 42, 70, 99, 14, 23, 92, 21, 55, 18, 84, 98, 90, 4, 37, 3, 31, 52, 
       87, 8, 74, 1, 15, 45, 82, 19, 85, 77, 39, 95, 66, 38, 20, 90, 66, 86, 30, 62, 
       21, 14, 84, 96, 58, 22, 18, 75, 84, 44, 87, 54, 50, 35, 88, 60, 3, 72, 66, 87, 
       89, 65, 25, 16, 73, 71, 56, 18, 60, 82, 17, 27, 75, 2, 85, 39, 69, 91, 45, 48, 
       83, 55, 25, 10, 65, 45, 60, 85, 93, 32, 9, 14, 24, 18, 6, 89, 82, 14, 53]

rv = (uncurry (%)) <$> (zip rv1 rv2) :: [Ratio Int]

f n = falseIfRepeats $ (dotProduct rv) <$> (symCoefficients n)
