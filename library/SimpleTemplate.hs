{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module SimpleTemplate (
       symbolTable,
       readTable,
       showTable
       ) where

import DynamicGen
import Unsafe.Coerce

p1 :: Integer -> Double -> Double
p1 = (+) . fromIntegral

plus :: [Dynamic] -> Maybe Dynamic
plus [a, b] = $(testsAndExecutesN [
                   ([('a, [t| Integer |]),
                     ('b, [t| Double |])],
                    Static [| True |],
                    [| (unsafeCoerce p1 :: (a -> b -> b)) a b |]),
                   
                   ([('a, [t| Double |]),
                     ('b, [t| Double |])],
                    Static [| True |],
                    [| a + b |])
                ])
              
sub :: [Dynamic] -> Maybe Dynamic
sub [a, b] = $(typesAndExecutesNT [
                   ([('a, ''Integer),
                     ('b, ''Integer)],
                    [| a - b |]),
                   
                   ([('a, ''Double),
                     ('b, ''Double)],
                    [| a - b |])
                ])

genSymbolTable [("sub", [| sub |]), ("plus", [| plus |])]
genReadShowTable [
  ([t| Integer |], "Integer", [| read |], [| show |]),
  ([t| Double |], "Double",  [| read |], [| show |])
  ]
