import Data.Typeable
import Data.Dynamic

plus2 :: [Dynamic] -> Maybe Dynamic
plus2 [b1, b2]
  | ((dynTypeRep b1) == (typeOf (undefined :: Integer)))
    && ((dynTypeRep b2) == (typeOf (undefined :: Integer)))
 = (((fromDynamic b1) :: Maybe Integer) >>= \b1 ->
     ((fromDynamic b2) :: Maybe Integer) >>= \b2 ->
     return $ toDyn ((b1 + b2) :: Integer))
        
  | ((dynTypeRep b1) == (typeOf (undefined :: Double)))
    && ((dynTypeRep b2) == (typeOf (undefined :: Double)))
 = (((fromDynamic b1) :: Maybe Double) >>= \b1 ->
     ((fromDynamic b2) :: Maybe Double) >>= \b2 ->
     return $ toDyn ((b1 + b2) :: Double))
   
  | otherwise = Nothing


plus3 :: [Dynamic] -> Maybe Dynamic
plus3 [b1, b2, b3]
  | ((dynTypeRep b1) == (typeOf (undefined :: Integer)))
    && ((dynTypeRep b2) == (typeOf (undefined :: Integer)))
    && ((dynTypeRep b3) == (typeOf (undefined :: Integer)))
 = (((fromDynamic b1) :: Maybe Integer) >>= \b1 ->
     ((fromDynamic b2) :: Maybe Integer) >>= \b2 ->
     ((fromDynamic b3) :: Maybe Integer) >>= \b3 ->
     return $ toDyn ((b1 + b2 + b3) :: Integer))
        
  | ((dynTypeRep b1) == (typeOf (undefined :: Double)))
    && ((dynTypeRep b2) == (typeOf (undefined :: Double)))
    && ((dynTypeRep b3) == (typeOf (undefined :: Double)))
 = (((fromDynamic b1) :: Maybe Double) >>= \b1 ->
     ((fromDynamic b2) :: Maybe Double) >>= \b2 ->
     ((fromDynamic b3) :: Maybe Double) >>= \b3 ->
     return $ toDyn ((b1 + b2 + b3) :: Double))
   
  | otherwise = Nothing

symbolList :: [(String, ([Dynamic] -> Maybe Dynamic))]
symbolList = [("plus2", plus2), ("plus3", plus3)]

readArg :: String -> Dynamic
readArg ('I':'n':'t':'e':'g':'e':'r':'(':s) = toDyn (read $ init s :: Integer)
readArg ('D':'o':'u':'b':'l':'e':'(':s) = toDyn (read $ init s :: Double)


evalFunction s = let (name:as) = words s
                     args      = map readArg as
                     fun       = lookup name symbolList
                 in fun >>= \f -> f args

{-

> evalFunction "plus3 Integer(1) Integer(2) Integer(3)" >>= fromDynamic :: Maybe Integer
Just 6
> evalFunction "plus2 Integer(2) Integer(3)" >>= fromDynamic :: Maybe Integer
Just 5
> evalFunction "plus2 Double(2.2) Double(3.3)" >>= fromDynamic :: Maybe Double
Just 5.5
> evalFunction "plus3 Double(1.1) Double(2.2) Double(3.3)" >>= fromDynamic :: Maybe Double
Just 6.6

-}