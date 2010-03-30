{-# OPTIONS_GHC -XDeriveDataTypeable #-}
module Complex
       (
         Complex(Complex),
         plus,
         mul,
         sub
       ) where

import Data.Typeable

data Complex = Complex Double Double
             deriving (Show, Eq, Typeable)
                      
{- REGISTER: plus = Complex.plus :: Complex.Complex -> Complex.Complex -> Complex.Complex -}
plus :: Complex -> Complex -> Complex
plus (Complex x y) (Complex a b) = Complex (a + x) (b + y)

{- REGISTER: mul = Complex.mul :: Complex.Complex -> Complex.Complex -> Complex.Complex -}
mul :: Complex -> Complex -> Complex
mul (Complex a b) (Complex x y) = Complex (a * x - b * y) (a * y + b * x)

{- REGISTER: sub = Complex.sub :: Complex.Complex -> Complex.Complex -> Complex.Complex -}
sub :: Complex -> Complex -> Complex
sub (Complex a b) (Complex x y) = Complex (a - x) (b - y)

                          
