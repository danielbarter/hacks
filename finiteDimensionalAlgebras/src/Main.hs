module Main where

import qualified Data.Matrix as M

import Sym2CentralizerAlg
import LinearSolve

m :: M.Matrix Rational
m = M.fromLists [[0,1,0,0,0,0,0,0,0],
                 [0,0,0,0,1,0,0,1,0],
                 [0,0,1,0,0,0,0,0,0],
                 [1,0,0,0,0,0,0,0,0],
                 [0,1,0,0,0,0,0,0,0],
                 [0,0,0,1,0,0,0,0,0],
                 [0,0,1,0,0,0,0,0,0],
                 [0,0,0,0,0,1,0,0,1],
                 [0,0,0,0,1,0,0,0,0],
                 [0,0,0,0,0,1,0,0,0],
                 [0,0,0,0,0,0,1,0,0]]

main = return ()

printMatrix :: IO ()
printMatrix = writeFile "/home/danielbarter/result" $ show $ M.toLists $ oneStepCentralizerIsKernel


readMatrix = readFile "/home/danielbarter/Documents/git_repos/finiteDimensionalAlgebras/result"

parseMatrix :: IO [[Double]]
parseMatrix = do s <- readMatrix
                 return $ read s

