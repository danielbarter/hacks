module DoubleBox where

import Bratelli
import Partition
import Control.Monad (join)

-- first coordinate = row length
-- second coordinate = row
zipWithPosition :: [Int] -> [(Int,Int)]
zipWithPosition l = zip l [0,1..]

rowSucc :: Int -> (Int,Int) -> Int
n `rowSucc` (x,y) = if (n == y) then x+1 else x

incrementRow :: Int -> [Int] -> [Int]
incrementRow n l | n < (length l)  = fmap (rowSucc n) $ zipWithPosition l
                 | n == (length l) = l ++ [1]
                 | otherwise = []


addBox :: [Int] -> [([Int],[Int])]
addBox l = fmap f [0,1..(length l)]
  where f n = (incrementRow n l, [n])

addBox' :: [([Int],[Int])] -> [([Int],[Int])]
addBox' x = join $ f <$> x
  where f :: ([Int],[Int]) -> [([Int],[Int])]
        f (p, rs) = [ (p',(rs ++ r')) | (p',r') <- addBox p]

edge l = mkUniq $ fmap fst $ filter f $ addBox' $ addBox l
  where f (x,y) = (y !! 0) /= (y !! 1)

instance BratelliVertex Partition where
  bratelliEdge p = join $ (t . fromList) <$> (edge l)
    where l = toList p
          t Nothing  = []
          t (Just x) = [x]
  firstVertex = fromList' [1,1]

algebraDimension :: Int -> Int
algebraDimension n = 
  sum $ fmap (\x -> x^2) $ length <$> ((pathsOfLevelGrouped n) :: [[[Partition]]])
