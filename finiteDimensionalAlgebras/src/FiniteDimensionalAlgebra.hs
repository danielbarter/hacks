module FiniteDimensionalAlgebra ( Algebra
                                , buildAlgebra
                                , leftMultTensor
                                , adjointMap
                                , buildLieAlgebra
                                , isCommutative
                                , center
                                , centralizer
                                , howManyTimes
                                , centralizerIsKernel
                                )  where

import qualified Data.List as L
import qualified Data.Matrix as M

import Permutations (unsafe)
import Data.Ratio
import LinearSolve


-- [a] is the basis. a -> a -> [a] is the multiplication map
data Algebra a = MultMap [a] (a -> a -> [a])

validAlgebra :: (Eq a) => [a] -> (a -> a -> [a]) -> Bool
validAlgebra b m = and [ f (m x y) b | x <- b, y <- b ] 
  where f [] ys     = True
        f (x:xs) ys = (x `elem` ys) && (f xs ys)

buildAlgebra :: (Eq a) => [a] -> (a -> a -> [a]) -> Either String (Algebra a)
buildAlgebra b m = if validAlgebra b m then Right $ MultMap b m
                                       else Left "your multiplication map is not closed"

howManyTimes :: (Eq a) => a -> [a] -> Int
howManyTimes x []     = 0
howManyTimes x (y:ys) = if x == y then (howManyTimes x ys) + 1
                                  else (howManyTimes x ys)

multBasis :: (Eq a) => (Algebra a) -> a -> a -> [Int]
multBasis (MultMap basis m) x y = [ howManyTimes b (m x y) | b <- basis]

leftMultMatrix :: (Eq a) => (Algebra a) -> a -> [[Int]]
leftMultMatrix alg@(MultMap basis m) x = L.transpose $ (multBasis alg x) <$> basis

leftMultTensor :: (Eq a, Num b) => (Algebra a) -> [M.Matrix b]
leftMultTensor alg@(MultMap basis m) =  
  fmap M.fromLists $ (fmap . fmap . fmap) fromIntegral ((leftMultMatrix alg) <$> basis)


-------------------------------------------------------------
-------------------------------------------------------------

type LeftAlgebra a = [M.Matrix a]
type RightAlgebra a = [M.Matrix a]
type LieAlgebra a = [M.Matrix a]

transpose13 :: LeftAlgebra a -> RightAlgebra a
transpose13 l = 
  M.fromLists <$> (L.transpose $ L.transpose <$> L.transpose (M.toLists <$> l))

buildLieAlgebra :: (Num a) =>  LeftAlgebra a -> LieAlgebra a
buildLieAlgebra l = (uncurry (-)) <$> (zip l $ transpose13 l)

adjointMap :: [M.Matrix a] -> M.Matrix a
adjointMap l = M.fromLists (L.transpose $ M.toList <$> l)

isCommutative :: (Num a, Eq a) => LeftAlgebra a -> Bool
isCommutative l = and [x * y == y * x | x <- l, y <- l]

center :: (Eq a, Fractional a) =>  (LeftAlgebra a) -> [M.Matrix a]
center alg = kernel $ adjointMap $ (buildLieAlgebra alg)

-- list needs to be sorted. column indexing starts at 0.
keepCols :: [Int] -> M.Matrix a -> M.Matrix a
keepCols l m = foldr1 (M.<|>) [ M.colVector $ M.getCol (n+1) m | n <- l ]

centralizer :: (Eq a, Fractional a) => (LeftAlgebra a) -> [Int] -> [M.Matrix a]
centralizer alg sub = kernel $ adjointMap $ (keepCols sub) <$> (buildLieAlgebra alg)

centralizerIsKernel :: (Eq a, Fractional a) => (LeftAlgebra a) -> [Int] -> M.Matrix a
centralizerIsKernel alg sub = adjointMap $ (keepCols sub) <$> (buildLieAlgebra alg)

-- TL_3 left multiplication tensor
a1 = M.fromLists [[1,0,0,0,0],[0,1,0,0,0],[0,0,1,0,0],[0,0,0,1,0],[0,0,0,0,1]]
a2 = M.fromLists [[0,0,0,0,0],[1,-2,0,1,0],[0,0,0,0,0],[0,0,0,0,0],[0,0,1,0,-2]]
a3 = M.fromLists [[0,0,0,0,0],[0,0,0,0,0],[1,0,-2,0,1],[0,1,0,-2,0],[0,0,0,0,0]]
a4 = M.fromLists [[0,0,0,0,0],[0,0,0,0,0],[0,0,1,0,-2,0],[1,-2,0,1,0],[0,0,0,0,0]]
a5 = M.fromLists [[0,0,0,0,0],[0,1,0,-2,0],[0,0,0,0,0],[0,0,0,0,0],[1,0,-2,0,1]]
a = [a1,a2,a3,a4,a5] :: LeftAlgebra (Rational)
