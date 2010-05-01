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
             deriving (Show, Eq, Ord, Typeable)
                      
plus :: MyInt -> MyInt -> MyInt
plus (MyInt a) (MyInt b) = MyInt (a + b)

mul :: MyInt -> MyInt -> MyInt
mul (MyInt a) (MyInt b) = MyInt (a * b)

sub :: MyInt -> MyInt -> MyInt
sub (MyInt a) (MyInt b) = MyInt (a - b)

                          
