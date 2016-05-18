module QuantumGroups.CarterFlathSaitoA1 where

import Math.Matrix
import Math.Polynomial
import Math.Polynomial.Rational 
import Math.Polynomial.NumInstance
import Math.Cyclotomic

type F = RationalPoly Cyclotomic
type M = Matrix F

{-
if e1,...,en is a basis for V and f1,...,fm is a basis for W then
e1 f1, e1 f2, e1 f3,...,en,f1,...en,fm is a basis for V tensor W.
-}
kroneckerProduct :: ( Num a ) => Matrix a -> Matrix a -> Matrix a 
kroneckerProduct x y = foldr1 (<->) $ foldr1 (<|>) <$> (fmap . fmap) (uncurry scaleMatrix) ((fmap . fmap) (\n -> (n,y)) x')
    where x' = toLists x

-- the variable A
a :: F
a = (poly LE [0,1]) % (poly LE [1]) 

q = a * a

-- 1/A
ia = recipRational a

iq = recipRational q

quantumInteger :: Int -> F
quantumInteger n = (q^n - iq^n) * (recipRational (q - iq))

ee :: M
ee = fromList 2 2 [0,0,1,0]

ff :: M 
ff = fromList 2 2 [0,1,0,0]

kk :: M
kk = fromList 2 2 [ia,0,0,a]

ik :: M
ik = fromList 2 2 [a,0,0,ia]

cup :: Int ->  M
cup n | n <= 0   = 0 
      | n == 1 = fromList 4 1 [0,scaleRational i a, scaleRational (-i) ia, 0]
      | n > 1  = multStd (foldr1 kroneckerProduct (id ++ [cup 1] ++ id)) (cup (n-1)) 
    where id :: [M]
          id = take (n-1) $ repeat (identity 2)

cap :: Int -> M
cap n | n <= 0 = 0 
      | n == 1 = fromList 1 4 [0,scaleRational i a, scaleRational (-i) ia,0]
      | n > 1 = multStd (cap (n-1)) (foldr1 kroneckerProduct (id ++ [cap 1] ++ id))
    where id :: [M]
          id = take (n-1) $ repeat (identity 2)

positiveCrossing :: M
positiveCrossing = elementwise (+) (scaleMatrix a m1) (scaleMatrix ia m2)
    where m1 = multStd (cup 1) (cap 1) 
          m2 = kroneckerProduct (identity 2) (identity 2)

negativeCrossing :: M
negativeCrossing = elementwise (+) (scaleMatrix ia m1) (scaleMatrix a m2)
    where m1 = multStd (cup 1) (cap 1)
          m2 = kroneckerProduct (identity 2) (identity 2)

-- n is the number of strands, k is the location of cup cap.
h :: Int -> Int -> M
h n k = foldr1 kroneckerProduct (ida ++ [multStd (cup 1) (cap 1)] ++ idb)
    where ida = take (k-1) $ repeat (identity 2)
          idb = take (n-k-1) $ repeat (identity 2)

jonesWentzl :: Int -> M
jonesWentzl n | n <= 0 = 0
              | n == 1 = identity 2
              | n > 1  = elementwise (+) t1 t2
    where t1 = kroneckerProduct (jonesWentzl (n-1)) (identity 2)
          t2 = scaleMatrix c (foldr1 multStd [t1,h n (n-1),t1])
          c :: F
          c  =  (quantumInteger (n-1)) * (recipRational $ quantumInteger n)



