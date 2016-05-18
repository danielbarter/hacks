{-# LANGUAGE FlexibleInstances #-}

module G2 where

import Bratelli

type Weight = (Int,Int)

isDominant :: Weight -> Bool
isDominant (a,b) = (0 <= a) && (0 <= b)

edges :: Weight -> [Weight]
edges (a,b) = case a of 
  0 -> filter isDominant [(a+1,b),(a+1,b-1),(a+2,b-1)]
  _ -> filter isDominant
           [(a-2,b+1),(a-1,b+1),(a-1,b),(a,b),(a+1,b),(a+1,b-1),(a+2,b-1)]


instance BratelliVertex (Int,Int) where
  bratelliEdge = edges
  firstVertex = (1,0)
