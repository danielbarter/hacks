module LinearSolve (kernel, image, cokernel) where

import Data.Matrix
import Control.Monad.State.Lazy
import Data.Ratio
import qualified Data.Vector as V

-- | Scale a column by a given factor.
scaleCol :: Num a => a -> Int -> Matrix a -> Matrix a
scaleCol = mapCol . const . (*)

-- | Add to one column a scalar multiple of another column. c1 -> c1 + l * c2. 
combineCols :: Num a => Int -> a -> Int -> Matrix a -> Matrix a
combineCols c1 l c2 m = mapCol (\i x -> x + l * getElem i c2 m) c1 m
 
--this is not a fast way to solve linear systems..... 
--The idea is that row operations are the same as changing basis on the codomain
--and column operations are the same as changing basis on the domain. 


-- are you on the edge of the matrix?
type R = Bool -- right
type T = Bool -- top
type L = Bool -- left
type B = Bool -- bottom
data Edge = Edge R T L B deriving (Show)

data Error = IllegalMove deriving (Show)

type RowRec a = Matrix a
type ColRec a = Matrix a
type RowNum = Int
type ColNum = Int
type Depth = Int -- the depth tells you how far along the diagonal you have cleared
type SolverState a = 
  (RowRec a, Matrix a, ColRec a, RowNum, ColNum, Edge, Depth)


-- Solver monad transformer
type Solver a m = StateT (SolverState a) m

initialState :: (Num a) => Matrix a -> SolverState a
initialState mat = 
  (identity $ nrows mat, mat, identity $ ncols mat,1, 1, Edge False True True False,1)

-- the initial state is (I,M,I,1,1).
-- the final state will be ( A, I(p,q) = AMB, B, min(p,q), min(p,q) ) 
-- where p = # rows M, q = # cols M
-- B is a change of basis matrix from new basis to old basis on domain
-- A is a change of basis matrix from old basis to new basis on codomain


--
--     ~ ~ ~ ~ ~ ~ ~ ~
--     ~ ~ ~ ~ ~ ~ ~ ~
--     ~ ~ * * * * * *
--     ~ ~ * ~ ~ ~ ~ ~
--     ~ ~ * ~ ~ ~ ~ ~
--     ~ ~ * ~ ~ ~ ~ ~
--
-- We call the * entries the 3-hook.
--
-- grabHook gets the hook for the current depth and returns a list 
-- containing all the nonzero entries together with there locations
--
-- if the hook is nonzero, after switching some rows and columns, getHook' returns the 
-- hook with the pivot 1


type HookRec a = (a,RowNum, ColNum)

grabHook :: (Num a, Monad m, Eq a) => Solver a m ( [HookRec a],HookRec a,[HookRec a] )
grabHook = do (_,m,_,_,_,_,d) <- get
              let p = nrows m
              let q = ncols m
              goTo p d
              vertical''   <- getVerticalStrip p d
              pivot'       <- getPivot
              horizontal'' <- getHorizontalStrip q d
              let vertical'    = f <$> (zip vertical'' [(x,d) | x <- [p,(p-1)..(d+1)]])
              let horizontal' = f <$> (zip horizontal'' [(d,y) | y <- [(d+1),(d+2)..q]])
              let vertical = filter g vertical'
              let horizontal = filter g horizontal'
              let pivot = (pivot',d,d)
              return (vertical,pivot,horizontal)
  where getVerticalStrip p d   = sequence $ take (p-d) $ repeat (getEntry <* up)
        getPivot               = getEntry
        getHorizontalStrip q d = sequence $ take (q-d) $ repeat (right >> getEntry)
        f (x,(y,z)) = (x,y,z)
        g (x,y,z) = (x /= 0)


grabHook' :: (Fractional a, Monad m, Eq a) => Solver a m ([HookRec a],[HookRec a])
grabHook' = do (vertical,pivot,horizontal) <- grabHook
               (_,_,_,_,_,_,d) <- get
               let l = p1 pivot
               if l /= 0 then (scaleRow' (recip l) d) >> (fmap p13 grabHook)
                         else case (vertical,horizontal) of 
                           ([],[])  -> return ([],[])
                           (r:rs,_) -> (switchRows' d $ p2 r) >> grabHook'
                           (_,c:cs) -> (switchCols' d $ p3 c) >> grabHook' 
  where p13 (x,y,z)  = (x,z)
        p1 (x,y,z) = x 
        p2 (x,y,z) = y
        p3 (x,y,z) = z

cleanHook :: (Fractional a, Monad m, Eq a) => Solver a m ()
cleanHook = do (vertical,horizontal) <- grabHook'
               (_,_,_,_,_,_,d) <- get
               if (vertical,horizontal) == ([],[])
                 then return ()
                 else do foldr (>>) (return ()) $ (f d) <$> vertical
                         foldr (>>) (return ()) $ (g d) <$> horizontal
  where f d (l,r,c)  = combineRows' r (-l) d
        g d (l,r,c)  = combineCols' c (-l) d


cleanHookRec :: (Fractional a, Monad m, Eq a) => Solver a m ()
cleanHookRec = do (_,m,_,_,_,e@(Edge r t l b),d) <- get
                  let p = nrows m
                  let q = ncols m
                  if ( d > min p q ) then return ()
                                      else do cleanHook
                                              incDepth
                                              cleanHookRec


-- this should all be refactored
getZeroDiagCols :: (Num a, Monad m, Eq a) => Solver a m [Int]
getZeroDiagCols = do (_,m,_,_,_,_,_) <- get
                     let p = nrows m
                     let q = ncols m
                     let diag = zip [1,2..(min p q)] (V.toList $ getDiag m)
                     let l = fst <$> (filter f diag)
                     return l
  where f (i,a) = (a == 0)



getNonZeroDiagCols :: (Num a, Monad m, Eq a) => Solver a m [Int]
getNonZeroDiagCols = do (_,m,_,_,_,_,_) <- get
                        let p = nrows m
                        let q = ncols m
                        let diag = zip [1,2..(min p q)] (V.toList $ getDiag m)
                        let l = fst <$> (filter f diag)
                        return l
  where f (i,a) = (a /= 0)




kernelBasis :: (Fractional a, Monad m, Eq a) => Solver a m [Matrix a]
kernelBasis = do cleanHookRec
                 (_,_,b,_,_,_,_) <- get
                 l  <- getZeroDiagCols
                 return $ colVector <$> ((flip getCol b) <$> l)


-- this is currently incorrect. We need to invert the matrix A first.
imageBasis :: (Fractional a, Monad m, Eq a) => Solver a m [Matrix a]
imageBasis = do cleanHookRec
                (a,_,_,_,_,_,_) <- get
                l  <- getNonZeroDiagCols
                return $ colVector <$> ((flip getCol a) <$> l)

kernel :: (Fractional a, Eq a) => Matrix a -> [Matrix a]
kernel m = evalState (kernelBasis) (initialState m)

-- this is currently incorrect because imageBasis is incorrect
image :: (Fractional a, Eq a) => Matrix a -> [Matrix a]
image m = evalState (imageBasis) (initialState m)

cokernel = undefined

------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------

getEntry :: (Num a, Monad m) => Solver a m a
getEntry = do (a,m,b,i,j,e,d) <- get
              return $ getElem i j m

scaleRow' :: (Num a, Monad m) => a -> RowNum -> Solver a m ()
scaleRow' l r = do (a,m,b,i,j,e,d) <- get
                   let a' = scaleRow l r a
                   let m' = scaleRow l r m
                   put (a',m',b,i,j,e,d)
                   return ()
                    

scaleCol' :: (Num a, Monad m) => a -> ColNum -> Solver a m ()
scaleCol' l c = do (a,m,b,i,j,e,d) <- get
                   let b' = scaleCol l c b
                   let m' = scaleCol l c m
                   put (a,m',b',i,j,e,d)
                   return ()
      

combineRows' :: (Num a, Monad m) => RowNum -> a -> RowNum -> Solver a m ()
combineRows' r1 l r2 = do (a,m,b,i,j,e,d) <- get
                          let a' = combineRows r1 l r2 a
                          let m' = combineRows r1 l r2 m
                          put (a',m',b,i,j,e,d)
                          return ()



combineCols' :: (Num a, Monad m) => ColNum -> a -> ColNum -> Solver a m ()
combineCols' c1 l c2 = do (a,m,b,i,j,e,d) <- get
                          let b' = combineCols c1 l c2 b
                          let m' = combineCols c1 l c2 m
                          put (a,m',b',i,j,e,d)
                          return ()

switchRows' :: (Num a, Monad m) => RowNum -> RowNum -> Solver a m ()
switchRows' r1 r2 = do (a,m,b,i,j,e,d) <- get
                       let a' = switchRows r1 r2 a
                       let m' = switchRows r1 r2 m
                       put (a',m',b,i,j,e,d)
                       return ()

switchCols' :: (Num a, Monad m) => ColNum -> ColNum -> Solver a m ()
switchCols' c1 c2 = do (a,m,b,i,j,e,d) <- get
                       let b' = switchCols c1 c2 b
                       let m' = switchCols c1 c2 m
                       put (a,m',b',i,j,e,d)
                       return ()

right :: (Num a, Monad m) => Solver a m (Maybe Error)
right = do (a,m,c,i,j,e@(Edge r t l b),d) <- get
           if r then return $ Just IllegalMove
                else do let j' = j + 1
                        put (a,m,c,i,j',e,d)
                        updateEdge
                        return Nothing

up :: (Num a, Monad m) => Solver a m (Maybe Error)
up = do (a,m,c,i,j,e@(Edge r t l b),d) <- get
        if t then return $ Just IllegalMove
             else do let i' = i - 1
                     put (a,m,c,i',j,e,d)
                     updateEdge
                     return Nothing

left :: (Num a, Monad m) => Solver a m (Maybe Error)
left = do (a,m,c,i,j,e@(Edge r t l b),d) <- get
          if l then return $ Just IllegalMove
               else do let j' = j - 1
                       put (a,m,c,i,j',e,d)
                       updateEdge
                       return Nothing


down :: (Num a, Monad m) => Solver a m (Maybe Error)
down = do (a,m,c,i,j,e@(Edge r t l b),d) <- get
          if b then return $ Just IllegalMove
               else do let i' = i + 1
                       put (a,m,c,i',j,e,d)
                       updateEdge
                       return Nothing

goTo :: (Num a, Monad m) => RowNum -> ColNum -> Solver a m (Maybe Error)
goTo r c = do (a,m,b,i,j,e,d) <- get
              let p = nrows m
              let q = ncols m
              if (1 <= r && r <= p && 1 <= c && c <= q)
                then do put (a,m,b,r,c,e,d)
                        updateEdge
                        return Nothing
                else return $ Just IllegalMove
             

-- updateEdge shouldn't be called except in right,up,left,down, goTo
updateEdge :: (Num a, Monad m) => Solver a m ()
updateEdge = do (a,m,b,i,j,e,d) <- get
                let p = nrows m
                let q = ncols m
                let e' = Edge (j == q) (i == 1) (j == 1) (i == p)
                put (a,m,b,i,j,e',d)
                return ()
incDepth :: (Num a, Monad m) => Solver a m ()
incDepth = do (a,m,b,i,j,e,d) <- get
              let d' = d + 1
              put (a,m,b,i,j,e,d')
              return ()


------------------------------------------------------------------
------------------------------------------------------------------

m :: Matrix Rational
m =   fromLists [[0,1,0,0,0,0,0,0,0],
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
