{-# OPTIONS_GHC -XDeriveDataTypeable #-}
module Integer
       (
         MyInt(MyInt),
         plus,
         mul,
         sub
       ) where

import Data.Typeable

data MyInt = MyInt Integer
             deriving (Show, Eq, Typeable)
                      
{- REGISTER: plus = Integer.plus :: Integer.MyInt -> Integer.MyInt -> Integer.MyInt -}
plus :: MyInt -> MyInt -> MyInt
plus (MyInt a) (MyInt b) = MyInt (a + b)

{- REGISTER: mul = Integer.mul :: Integer.MyInt -> Integer.MyInt -> Integer.MyInt -}
mul :: MyInt -> MyInt -> MyInt
mul (MyInt a) (MyInt b) = MyInt (a * b)

{- REGISTER: sub = Integer.sub :: Integer.MyInt -> Integer.MyInt -> Integer.MyInt -}
sub :: MyInt -> MyInt -> MyInt
sub (MyInt a) (MyInt b) = MyInt (a - b)

                          
