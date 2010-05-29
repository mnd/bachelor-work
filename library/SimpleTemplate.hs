{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module SimpleTemplate (
       symbolTable,
       readTable,
       showTable
       ) where

import DynamicGen
import Data.Dynamic 

plus :: [Dynamic] -> Maybe Dynamic
plus [a, b] = $(typesAndExecutes [
                   ([([| a |], [t| Integer |]),
                     ([| b |], [t| Integer |])],
                    [| a + b |]),
                   
                   ([([| a |], [t| Double |]),
                     ([| b |], [t| Double |])],
                    [| a + b |])
                ])
              
sub :: [Dynamic] -> Maybe Dynamic
sub [a, b] = $(typesAndExecutes [
                   ([([| a |], [t| Integer |]),
                     ([| b |], [t| Integer |])],
                    [| a - b |]),
                   
                   ([([| a |], [t| Double |]),
                     ([| b |], [t| Double |])],
                    [| a - b |])
                ])
              

symbolTable = [("sub", sub), ("plus", plus)]
readTable   = $(genReadTable [
                   ("Integer", [| read |], [t| Integer |]),
                   ("Double",  [| read |], [t| Double  |])
                   ])
showTable   = $(genShowTable [
                   ([t| Integer |], "Integer", [| show |]),
                   ([t| Double  |], "Double",  [| show |])
                   ])
