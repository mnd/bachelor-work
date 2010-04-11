import Data.Typeable
import Data.Dynamic
import Unsafe.Coerce
import Monad

plus2 :: Dynamic -> Dynamic -> Maybe Dynamic
plus2 b1 b2
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


plus3 :: Dynamic -> Dynamic -> Dynamic -> Maybe Dynamic
plus3 b1 b2 b3
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

data Function = forall f. Typeable f => F f

symbolList :: [(String, Function)]
symbolList = [("plus2", F plus2), ("plus3", F plus3)]

unF1 :: Function -> (Dynamic -> Maybe Dynamic)
unF1 (F fun) = unsafeCoerce fun
unF2 :: Function -> (Dynamic -> Dynamic -> Maybe Dynamic)
unF2 (F fun) = unsafeCoerce fun
unF3 :: Function -> (Dynamic -> Dynamic -> Dynamic -> Maybe Dynamic)
unF3 (F fun) = unsafeCoerce fun
unF4 :: Function -> (Dynamic -> Dynamic -> Dynamic -> Dynamic -> Maybe Dynamic)
unF4 (F fun) = unsafeCoerce fun

-- Применяет список аргументов к функции
-- завернутой в Dynamic: toDyn (+ :: Int -> Int -> Int)
dynamicApply :: Dynamic -> [Dynamic] -> Maybe Dynamic
dynamicApply = foldM dynApply

-- Применяет к любой функции набор аргументов разных типов
apply' :: (Typeable f, Typeable a, Typeable b) => f -> [a] -> Maybe b
apply' f as = dynamicApply (toDyn f) (map toDyn as) >>= fromDynamic

apply :: Function -> [Dynamic] -> Maybe Dynamic
apply f as | length as == 1 = join $ apply' (unF1 f) as
           | length as == 2 = join $ apply' (unF2 f) as
           | length as == 3 = join $ apply' (unF3 f) as
           | length as == 4 = join $ apply' (unF4 f) as
                         

readArg :: String -> Dynamic
readArg ('I':'n':'t':'e':'g':'e':'r':'(':s) = toDyn (read $ init s :: Integer)
readArg ('D':'o':'u':'b':'l':'e':'(':s) = toDyn (read $ init s :: Double)


evalFunction'' name args = (lookup name symbolList) >>= \f -> apply f args
evalFunction'  name args = evalFunction'' name (map readArg args)
evalFunction s           = let (name:args) = words s
                           in evalFunction' name args
                              
                              
{-

> evalFunction "plus3 Integer(1) Integer(2) Integer(3)" >>= fromDynamic :: Maybe Integer
Just 6
> evalFunction "plus2 Integer(2) Integer(3)" >>= fromDynamic :: Maybe Integer
Just 5
> evalFunction "plus2 Double(2.2) Double(3.3)" >>= fromDynamic :: Maybe Double
Just 5.5
> evalFunction "plus3 Double(1.1) Double(2.2) Double(3.3)" >>= fromDynamic :: Maybe Double
Just 6.6



Проблемы данного решения:
1. unF1, unF2, …
2. apply [a1], apply [a1,a2], apply [a1,a2,a3], …

Решение: 
Реализовывать динамические функции как ([Dynamic] -> Maybe Dynamic)
вместо (Dynamic -> Dynamic -> … -> Dynamic -> Maybe Dynamic)

-}