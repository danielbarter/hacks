{-# LANGUAGE ViewPatterns #-}
-- |This stuff is somewhat experimental.  I am not sure how useful the 
-- rational function manipulation stuff is, overall, due to truncation
-- error issues in polynomial division.  Works well with exact Rational
-- coefficients though, and the evaluation functions are fine for any
-- Fractional type.
module Math.Polynomial.Rational where

import Math.Polynomial
import Data.AdditiveGroup

data RationalPoly a = RationalPoly
    { reduced :: !Bool
    , numerator :: Poly a
    , denominator :: Poly a
    }

instance (AdditiveGroup a, Show a, Num a, Eq a ) => Show (RationalPoly a) where
    show (RationalPoly _ n d) | (n == zero) = "0"
                              | otherwise = (show n) ++ "/" ++ (show d) 

instance Functor RationalPoly where
    fmap f (RationalPoly _ p q) = RationalPoly False (fmap f p) (fmap f q)

instance ( Eq a, Fractional a, AdditiveGroup a) => Eq (RationalPoly a) where
    (==) = rationalPolyEqual

rationalPolyEqual :: ( Eq a, Fractional a, AdditiveGroup a ) => (RationalPoly a) -> (RationalPoly a) -> Bool
rationalPolyEqual p q = case (pReduced,qReduced) of 
                             (True,True)   -> (numerator p == numerator q)   && (denominator p == denominator q)
                             (True,False)  -> (numerator p == numerator q')  && (denominator p == denominator q')
                             (False,True)  -> (numerator p' == numerator q)  && (denominator p' == denominator q)
                             (False,False) -> (numerator p' == numerator q') && (denominator p' == denominator q')
    where p' = reduce p
          q' = reduce q
          pReduced = reduced p
          qReduced = reduced q

reduce r
    | reduced r = r
    | polyIsZero q  = error "%: divide by zero"
    | otherwise = RationalPoly True (p `quotPoly` elim) (q `quotPoly` elim)
        where
            p = numerator r
            q = denominator r
            q0 = head (polyCoeffs BE q)
            elim = fmap (/ q0) (gcdPoly p q)

infixl %
p % q = reduce (RationalPoly False p q)



addRational :: ( Fractional a, Num a, Eq a ) => RationalPoly a -> RationalPoly a -> RationalPoly a
addRational p q = (addPoly (multPoly np dq) (multPoly nq dp)) % (multPoly dp dq)
    where np = numerator p
          dp = denominator p
          nq = numerator q
          dq = denominator q

negateRational r = r {numerator = negatePoly (numerator r)}
scaleRational s r = r {numerator = scalePoly s (numerator r)}

multRational (reduce -> RationalPoly _ xp xq) (reduce -> RationalPoly _ yp yq)
    = multPoly xp yp % multPoly xq yq

recipRational (reduce -> RationalPoly _ p q) = q % p

divRational (reduce -> RationalPoly _ xp xq) (reduce -> RationalPoly _ yp yq)
    = multPoly xp yq % multPoly xq yp

evalRational (reduce -> RationalPoly _ p q) x = evalPoly p x / evalPoly q x

rationalDeriv (reduce -> RationalPoly _ g h) = (g' * h - g * h') % (h*h)
    where
        (*) = multPoly
        (-) = \x y -> addPoly x (negatePoly y)
        g' = polyDeriv g
        h' = polyDeriv h
