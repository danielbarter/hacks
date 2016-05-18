module Main where

import Permutations
import Data.List (transpose,subsequences)
import Data.Monoid ((<>),mconcat)
import Data.Ratio
import qualified Data.Matrix as M

n = 3

partition = [ [2*i-1,2*i] | i <- [1..n]]
subgroupGenerators = (unsafe . fromCycle) <$> partition
subgroupBasis = mconcat <$> (subsequences subgroupGenerators) 

basis = nonNegMatricies z z
  where z = take n $ repeat 2

lift = unsafe . liftPermutationMatrix
project p = permutationMatrix p partition

mult i j = project <$> [pi <> p <> pj | p <- subgroupBasis]
  where pi = lift $ basis !! i
        pj = lift $ basis !! j

multBasis i j = [ howManyTimes b (mult i j) | b <- basis]
leftMultMatrix i = transpose $ (multBasis i) <$> [0,1..((length basis) - 1)]

leftMultTensor = leftMultMatrix <$> [0,1..((length basis) -1)]
leftMultTensorRational = (fmap . fmap . fmap) (\x -> (x % 2^n)) leftMultTensor

howManyTimes :: (Eq a) => a -> [a] -> Int
howManyTimes x []     = 0
howManyTimes x (y:ys) = if x == y then (howManyTimes x ys) + 1
                                  else (howManyTimes x ys)



main = writeFile "/home/danielbarter/matrix" (show leftMultTensorRational)
