module FiniteDimAlgebra (LeftAlgebra,
                         RightAlgebra,
                         LieAlgebra,
                         transpose13,
                         buildLieAlgebra,
                         adjointMap,
                         element,
                         isCommutative
                        ) where

import Matrix hiding (transpose)
import Data.List (transpose)
import Data.Ratio


{-
Let A be a finite dimensional algebra with basis a1,a2,...,ad

L : A -> End(A) 
    a |-> (x |-> ax) 
   ai |-> li 


R : A -> End(A) 
    a |-> (x |-> xa)
   ai |-> ri

LeftAlgebra = [l1,l2,...,ld]
RightAlgebra = [r1,r2,...,rd]

r(ipq) = l(qpi) as 3-tensors

therefore r = s1 s2 s1 l, where s1,s2 are the simple reflections in S3
-}



type LeftAlgebra a = [Matrix a]
type RightAlgebra a = [Matrix a]
type LieAlgebra a = [Matrix a]

transpose13 :: LeftAlgebra a -> RightAlgebra a
transpose13 l = fromLists <$> (transpose $ transpose <$> transpose (toLists <$> l))

buildLieAlgebra :: (Num a) =>  LeftAlgebra a -> LieAlgebra a
buildLieAlgebra l = (uncurry (-)) <$> (zip l $ transpose13 l)

adjointMap :: [Matrix a] -> Matrix a
adjointMap l = fromLists (transpose $ toList <$> l)

element :: (Num a) => [(a,Matrix a)] -> Matrix a
element c = foldr1 (+) $ (uncurry scaleMatrix) <$> c

isCommutative :: (Num a, Eq a) => LeftAlgebra a -> Bool
isCommutative l = and [x * y == y * x | x <- l, y <- l]


--------------------------------------------------------------------------
--------------------------------------------------------------------------
---produce structure matricies from a basis list and multiplication map---
--------------------------------------------------------------------------
--------------------------------------------------------------------------

structMatrix :: (Eq a) => (a -> a -> a) -> [a] -> a -> [[Rational]]
structMatrix m xs x = transpose $ fmap (c xs) $ (uncurry m) <$> zip (repeat x) xs
  where c ys y  = b <$> fmap (y == ) ys
        b True  = 1
        b False = 0

structMatricies :: (Eq a) => (a -> a -> a) -> [a] -> [Matrix Rational]
structMatricies m xs = [ fromLists (structMatrix m xs x) | x <- xs ]
