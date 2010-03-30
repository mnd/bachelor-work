import Data.Typeable
import Data.Dynamic
import Unsafe.Coerce

apply2 :: (a -> b -> c) -> Dynamic -> Dynamic -> Maybe Dynamic
apply2 f a b
    | (
       dynTypeRep a == typeOf (undefined :: Integer)
       && dynTypeRep b == typeOf (undefined :: Integer)
      ) = do
        ar <- ((fromDynamic a) :: (Maybe Integer))
        br <- ((fromDynamic b) :: (Maybe Integer))
        Just (toDyn (((unsafeCoerce f) ar br) :: Integer))
    | (
       dynTypeRep a == typeOf (undefined :: Double)
       && dynTypeRep b == typeOf (undefined :: Double)
      ) = do
        ar <- ((fromDynamic a) :: (Maybe Double))
        br <- ((fromDynamic b) :: (Maybe Double))
        Just (toDyn (((unsafeCoerce f) ar br) :: Double))
    | (
       dynTypeRep a == typeOf (undefined :: Integer)
       && dynTypeRep b == typeOf (undefined :: Double)
      ) = do
        ar <- ((fromDynamic a) :: (Maybe Integer))
        br <- ((fromDynamic b) :: (Maybe Double))
        Just (toDyn (((unsafeCoerce f) (fromIntegral ar :: Double) br) :: Double))
    | (
       dynTypeRep a == typeOf (undefined :: Double)
       && dynTypeRep b == typeOf (undefined :: Integer)
      ) = do
        ar <- ((fromDynamic a) :: (Maybe Double))
        br <- ((fromDynamic b) :: (Maybe Integer))
        Just (toDyn (((unsafeCoerce f) ar (fromIntegral br :: Double)) :: Double))
    | otherwise = Nothing


{- 
Вызов
apply2 (+) (toDyn (1 :: Double)) (toDyn (1 :: Double)) >>= (fromDynamic :: (Dynamic -> Maybe Double))

или

apply2 ((+) :: (Integer -> Integer -> Integer)) (toDyn (1 :: Double)) (toDyn (1 :: Double)) >>= (fromDynamic :: (Dynamic -> Maybe Double))

Даёт случайный результат. 

apply2 ((+) :: (Double -> Double -> Double)) (toDyn (1 :: Double)) (toDyn (1 :: Double)) >>= (fromDynamic :: (Dynamic -> Maybe Double))

Даёт результат нормальный
-}