module Sym2CentralizerAlg where

import LinearSolve
import FiniteDimensionalAlgebra
import Permutations
import Data.Ratio
import Data.List (transpose,subsequences)
import Data.Monoid ((<>),mconcat)

import qualified Data.Matrix as M
import qualified Data.Set as S

n = 3

partition = [ [2*i-1,2*i] | i <- [1..n]]
subgroupGenerators = (unsafe . fromCycle) <$> partition
subgroupBasis = mconcat <$> (subsequences subgroupGenerators)

lift = unsafe . liftPermutationMatrix
project p = permutationMatrix p partition

basis = nonNegMatricies z z
  where z = take n $ repeat 2

sub :: [Int]
sub = fmap fst $ filter ((\m -> (M.getElem n n m) == 2) . snd) (zip [0,1..] basis)

mult x y = project <$> [(lift x) <> p <> (lift y) | p <- subgroupBasis]

alg = unsafe $ buildAlgebra basis mult
algTensor = (leftMultTensor alg) :: [M.Matrix Double]
algTensorRescaled = fmap (M.scaleMatrix (1 / 2^n)) algTensor

centerBasis = 
  fmap (filter (\x -> (snd x) /= 0)) $ fmap (zip basis) $ M.toList <$> 
    center algTensorRescaled

oneStepCentralizerIsKernel = centralizerIsKernel algTensorRescaled sub

oneStepCentralizerBasis = fmap (filter (\x -> (snd x) /= 0)) $ fmap (zip basis) $
  M.toList <$> (centralizer algTensorRescaled sub)


