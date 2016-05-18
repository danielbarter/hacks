module Bratelli ( BratelliVertex(..)
                , verticiesOfLevel
                , pathsOfLevel
                , mkUniq
                , pathsOfLevelGrouped
                ) where

import Control.Monad (join)
import qualified Data.Set as Set 
import GHC.Exts (groupWith)

class BratelliVertex a where
  bratelliEdge :: a -> [a]
  firstVertex    :: a


-- remove repititions from a list
mkUniq :: Ord a => [a] -> [a]
mkUniq = Set.toList . Set.fromList


verticiesOfLevel :: (Ord a, BratelliVertex a) => Int -> [a]
verticiesOfLevel 0 = []
verticiesOfLevel 1 = [firstVertex]
verticiesOfLevel n = mkUniq $ join $ bratelliEdge <$> vs
  where vs = verticiesOfLevel (n-1)


pathsOfLevel :: (BratelliVertex a) => Int -> [[a]]
pathsOfLevel 0 = []
pathsOfLevel 1 = [[firstVertex]]
pathsOfLevel n = [ v : p  | p <- ps ,v <- (bratelliEdge $ head p) ]
  where ps = pathsOfLevel (n-1)

pathsOfLevelGrouped :: (Ord a ,BratelliVertex a, Eq a) => Int -> [[[a]]]
pathsOfLevelGrouped n = groupWith head $ pathsOfLevel n

