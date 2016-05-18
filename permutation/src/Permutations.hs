module Permutations ( Permutation
                    , fromInline
                    , fromCycle
                    , unsafe
                    , mkUniq
                    , mkUniq'
                    , action
                    , permIntersect
                    , permutationMatrix
                    , nonNegMatricies
                    , ppNonNegMatricies
                    , isPermMatrix
                    , liftPermutationMatrix
                    ) where

import qualified Data.List                as L
import qualified Data.Map                 as M
import qualified Data.Set                 as S
import qualified Text.PrettyPrint         as PP
import qualified Data.Monoid              as MON
import qualified Data.Matrix              as MAT
import qualified Control.Monad.State.Lazy as ST
import qualified Control.Monad.Except     as E
import qualified Control.Monad.Reader     as R
import qualified Data.Functor.Identity    as I

import Control.Monad (join)

-- In P x n, x is the permutation, n ambient symmetric group
data Permutation = P (M.Map Int Int) Int


-----------------------------------------------------------------
---------------pretty printing permutations----------------------
-----------------------------------------------------------------

-- safe maximum
maximum' :: [Int] -> Int
maximum' l = case l of [] -> 0
                       l  -> maximum l

-- when printing a matrix of strings, you need to pad them
pad :: [[String]] -> [[String]]
pad t = (fmap . fmap) p t
    where m = maximum' $ maximum' <$> (fmap . fmap) length t
          p :: String -> String
          p s = take m (s ++ (repeat ' '))


ppPermutation :: Permutation -> PP.Doc
ppPermutation (P m _) = foldr1 (PP.$$) (fmap (foldr1 (PP.<+>)) r)
  where   r       =  (fmap . fmap) PP.text (p (f <$> (M.toList m)))
          f (p,q) = [show p, show q]
          p       = L.transpose . pad

instance Show Permutation where
  show = PP.render . ppPermutation


------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------


-- remove repititions from a list
mkUniq :: Ord a => [a] -> [a]
mkUniq = S.toList . S.fromList

mkUniq' :: (Ord a) => [MAT.Matrix a] -> [MAT.Matrix a]
mkUniq' = (fmap MAT.fromLists) . mkUniq . (fmap MAT.toLists)

noRepetitions :: (Ord a) => [a] -> Bool
noRepetitions l = (length l) == ((length . mkUniq) l)

-- slow for large lists
validInline :: [Int] -> Bool
validInline l = (noRepetitions l) && ((L.sort l) == [1..(length l)])


fromInline :: [Int] -> Either String Permutation
fromInline l = if validInline l then Right (P (M.fromList $ zip [1,2..n] l) n)
                                else Left "not valid inline notation" 
  where n = length l

-- https://www.youtube.com/watch?v=9ez0HhgCwIQ
unsafe :: Either a b -> b
unsafe (Right x) = x
unsafe (Left s ) = undefined

validCycle :: [Int] -> Bool
validCycle = noRepetitions

rotate :: [a] -> [a]
rotate [] = []
rotate (x:xs) = xs ++ [x]

fromCycle :: [Int] -> Either String Permutation
fromCycle l = if (validCycle l)  
                then Right $ P (M.fromList ((zip l (rotate l)) ++ (zip unused unused))) m
                else Left $ "not valid cycle notation"
  where m = maximum l
        unused = [1,2..m] L.\\ l

swap (x,y) = (y,x)

--unify :: (Permutation,Permutation) -> (Permutation,Permutation)
unify ((P x m), (P y n)) = if (m <= n) then ((P x' n), (P y n))
                                       else swap $ unify ((P y n), (P x m))
  where x' = M.fromList ((M.toList x) ++ (zip [m+1,m+2..n] [m+1,m+2..n]))

mult :: Permutation -> Permutation -> Permutation
mult p q = P z n
  where ((P x n),(P y m)) = unify (p,q) 
        z = M.fromList [(i, x M.! (y M.! i)) | i <- [1,2..n]]

instance Monoid Permutation where
  mappend = mult
  mempty = unsafe $ fromInline [1]


--------------------------------------------------------------------------------
------------------permutation matricies-----------------------------------------
--------------------------------------------------------------------------------


action :: Permutation -> Int -> Int
action (P x _) n = case M.lookup n x of Just m  -> m
                                        Nothing -> n

-- # (A \cap \sigma(B)) 
permIntersect :: Permutation -> [Int] -> [Int] -> Int
permIntersect p a b = S.size $ S.intersection sa sb
  where sa = S.fromList a
        sb = S.fromList (fmap (action p) b)

--matrixOfPairs :: [a] -> MAT.Matrix a
matrixOfPairs p = MAT.fromList l l [ (x,y) | x <- p, y <- p]
  where l = length p

-- [[Int]] is a partition of the domain
permutationMatrix :: Permutation -> [[Int]] -> MAT.Matrix Int
permutationMatrix p xs = fmap (uncurry $ permIntersect p) (matrixOfPairs xs)


listLengthSum :: Int -> Int -> [[Int]]
listLengthSum l 0 = [take l $ repeat 0]
listLengthSum l n = mkUniq $ join $ addOneToEachEntry <$> (listLengthSum l (n-1))

addOneToEachEntry :: [Int] -> [[Int]]
addOneToEachEntry [] = []
addOneToEachEntry (x:xs) = [((x+1):xs)] ++ ( (x:) <$> (addOneToEachEntry xs))

nonNegMatricies' :: [Int] -> [Int] -> [[[Int]]]
nonNegMatricies' [r] c = if ((sum c) == r) && and ((0 <=) <$> c) then [[c]] else []
nonNegMatricies' (r:rs) c = 
  [ x:m | x <- listLengthSum (length c) r ,m <- nonNegMatricies' rs (f <$> (zip c x)) ]
    where f (p,q) = p - q

-- nonNegMatricies row_sums col_sums
nonNegMatricies :: [Int] -> [Int] -> [MAT.Matrix Int]
nonNegMatricies r c = MAT.fromLists <$> (nonNegMatricies' r c)

ppNonNegMatrix :: MAT.Matrix Int -> PP.Doc
ppNonNegMatrix m = 
  foldr1 (PP.$$) ((foldr1 (PP.<>)) <$> ((fmap . fmap) PP.int $ MAT.toLists m))

ppNonNegMatricies :: [MAT.Matrix Int] -> PP.Doc
ppNonNegMatricies ms = foldr (PP.<+>) PP.empty (ppNonNegMatrix <$> ms)

--------------------------------------------------------------------------------
-----------------------lifting permutation matricies----------------------------
--------------------------------------------------------------------------------


-- ( [((i,j),v)] , p, c, a )
-- [((i,j),v)] is called the data stack
-- (i,j) are matrix coordinates
-- v is the value at those coordinates
-- p is the permutation we are building
-- c measures how many more entries we must add to the permutation before we pop from 
-- the data stack
-- a is the addition stack

type State = ([((Int,Int),Int)],M.Map Int Int,Int, [(Int,Int)])

-- (d,s)
-- d is the dimension of the square matrix
-- s is the row and column sums
type Env = (Int, Int)

data Error = CounterNotZero
           | CounterZero 
           | DataStackEmpty
           | AdditionStackEmpty
           | KeyAlreadyPresent
           deriving (Show,Eq)

type Mon a = (E.ExceptT Error (ST.StateT State (R.ReaderT Env (I.Identity)))) a

runMon :: Mon a -> State -> Env -> Either Error a
runMon mon s r = I.runIdentity (R.runReaderT (ST.evalStateT (E.runExceptT mon) s) r)

isPermMatrix :: (Ord a, Num a) => MAT.Matrix a -> Bool
isPermMatrix m = ( p == q ) && (length rowsums == 1) && (rowsums == colsums)
  where p = MAT.nrows m
        q = MAT.ncols m
        rows = MAT.toLists m
        cols = L.transpose rows
        rowsums = mkUniq  (sum <$> rows)
        colsums = mkUniq  (sum <$> cols)

-- you should check that you are sticking a permutation matrix in first
unsafeInit :: MAT.Matrix Int -> (State, Env)
unsafeInit m = 
  ((zip [(i,j) | i <- [1,2..d], j <- [1,2..d]] (MAT.toList m), M.empty,0,[]),(d,s))
  where d = MAT.nrows m
        s = sum $ head $ MAT.toLists m

popDataStack :: Mon ((Int,Int),Int)
popDataStack = do (l,p,c,a) <- ST.get
                  if c /= 0 then E.throwError CounterNotZero
                            else case l of []     -> E.throwError DataStackEmpty
                                           (x:xs) -> do ST.put (xs,p,c,[])
                                                        return x

refillAdditionStack :: ((Int,Int),Int) -> Mon ()
refillAdditionStack ((i,j),v) = do (l,p,c,a) <- ST.get
                                   (d,s) <- R.ask
                                   let a' = [(x,y) | x <- [(j-1)*s+1..j*s],
                                                     y <- [(i-1)*s+1..i*s]]
                                   ST.put (l,p,v,a')
                                   return ()


popAdditionStack :: Mon (Int,Int)
popAdditionStack = do (l,p,c,a) <- ST.get
                      case a of []     -> E.throwError AdditionStackEmpty
                                (x:xs) -> do ST.put (l,p,c,xs)
                                             return x

updatePermutation :: (Int,Int) -> Mon ()
updatePermutation (j,i) = 
  do (l,p,c,a) <- ST.get
     if c == 0 then E.throwError CounterZero
               else if (M.member j p) || (inRange i p)
                       then E.throwError KeyAlreadyPresent
                       else do let p' = M.insert j i p
                               ST.put (l,p',c-1,a)
                               return ()
  where inRange i p = or (fmap (i ==) p)

clearAdditionStack' :: Mon ()
clearAdditionStack' = 
  popAdditionStack >>= updatePermutation >> clearAdditionStack'

clearAdditionStack :: Mon ()
clearAdditionStack = E.catchError clearAdditionStack' handler
  where handler e = case e of AdditionStackEmpty -> return ()
                              CounterZero        -> return ()
                              KeyAlreadyPresent  -> clearAdditionStack
                              _                  -> return ()

clearDataStack' = 
  popDataStack >>= refillAdditionStack >> clearAdditionStack >> clearDataStack'

clearDataStack = E.catchError clearDataStack' handler
  where handler e = case e of CounterNotZero -> clearAdditionStack >> clearDataStack
                              DataStackEmpty -> return ()
                              _              -> return ()

computePermutation = do clearDataStack
                        (l,p,c,a) <- ST.get
                        (d,s) <- R.ask
                        return $ P p (d*s)

liftPermutationMatrix :: MAT.Matrix Int -> Either Error Permutation
liftPermutationMatrix m = runMon computePermutation s e
  where (s,e) = unsafeInit m
