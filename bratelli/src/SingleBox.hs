module SingleBox where

import Control.Monad (join)
import Bratelli
import Partition

incrementEachElement :: [Int] -> [[Int]]
incrementEachElement []     = []
incrementEachElement l@(x:xs) = ((x+1):xs) : (fmap (x :) (incrementEachElement xs))

instance BratelliVertex Partition where
  bratelliEdge p = join $ (t . fromList) <$> (incrementEachElement l ++ [l ++ [1]])
    where l = toList p
          t Nothing  = []
          t (Just x) = [x]
  firstVertex = fromList' [1]

partitionsOfLevel :: Int -> [Partition]
partitionsOfLevel n = verticiesOfLevel n

oneStepCentralizerSize :: Int -> Int
oneStepCentralizerSize n =
  ( sum ( fmap (length . bratelliEdge) $ partitionsOfLevel (n-1))) -
  (length $ partitionsOfLevel n)
