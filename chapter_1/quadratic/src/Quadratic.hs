module Quadratic  where

import Data.Complex

data Quadratic = Quadratic { a :: Double, b :: Double, c :: Double } 
    deriving Show

type RootT = Complex Double

data Roots = Roots RootT RootT deriving Show

-- | Calculates roots
roots :: Quadratic -> Roots
-- Trivial
roots (Quadratic 0 0 _) = error "Not a polynomial!"
-- polynomial of degree 1, x = -c / b
roots (Quadratic 0.0 b c) = let root = ( (-c) / b :+ 0)
                                in Roots root root
roots (Quadratic a b c) = let discriminant = b*b - 4*a*c in
                                rootsInternal (Quadratic a b c) discriminant

rootsInternal :: Quadratic -> Double -> Roots
-- D = 0
rootsInternal q d | d == 0 = let r = ( -(b q) / (2.0 * (a q)) ) 
    in Roots (r :+ 0) (r :+ 0)
-- D < 0
rootsInternal q d | d < 0 = Roots (realpart :+ complexpart) (realpart :+ (-complexpart))
    where
            plusd = -d
            twoa = 2.0 * (a q)
            complexpart = (sqrt plusd) / twoa
            realpart = - (b q) / twoa
-- D > 0
rootsInternal q d | otherwise = Roots (root1 :+ 0) (root2 :+ 0) 
    where 
        plusd = -d
        twoa = 2.0 * (a q)
        dpart = (sqrt plusd) / twoa
        prefix = - (b q) / twoa
        root1 = prefix + dpart
        root2 = prefix - dpart
