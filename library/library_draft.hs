import Data.Typeable
import Data.Dynamic
import Data.Maybe
import Unsafe.Coerce
import Monad

plusi = toDyn ((+) :: Integer -> Integer -> Integer)
plusd = toDyn ((+) :: Double -> Double -> Double)


-- Применяет список аргументов к функции
-- завернутой в Dynamic: toDyn (+ :: Int -> Int -> Int)
dynamicApply :: Dynamic -> [Dynamic] -> Maybe Dynamic
dynamicApply = foldM dynApply

-- Применяет к списку функций список аргументов при помощи dynamicApply
combine1 :: [Dynamic] {- [a1->a2->…->an] -} -> [Dynamic] -> Maybe Dynamic
combine1 (f:fs) args = (dynamicApply f args) `mplus` (combine1 fs args)

combine2 :: [Dynamic] {- [a1->a2->…->an] -} -> [Dynamic] -> Maybe Dynamic
combine2 fs args = foldr (mplus . flip dynamicApply args) Nothing fs

-- Аналогичные функции, но заворачивающие f в динамик самостоятельно

apply0 :: (Typeable f) => f -> [Dynamic] -> Maybe Dynamic
apply0 = dynamicApply . toDyn


-- Чтобы иметь возможность передавать набор произвольных функций нам
-- нужен список произвольных функций.
data Function = forall f. Typeable f => F f

toDynF :: Function -> Dynamic
toDynF (F f) = toDyn f

combine :: [Function] -> [Dynamic] -> Maybe Dynamic
combine = combine1 . map toDynF

{- 
combine [F ((+) :: Integer->Integer->Integer), F ((+) :: Double->Double->Double)]
        [toDyn 1.1, toDyn 2.2]
-}

-- Возвращает тип возвращаемого значения функции
typeRepResult :: TypeRep -> TypeRep
typeRepResult f =
  case splitTyConApp f of
    (tc, [_,f1]) | tc == (mkTyCon "->") -> typeRepResult f1
    _                                   -> f

    
plus :: Integer -> Double -> Double
plus a b = b + fromIntegral a

data TypeableType = forall a. Typeable a => T a

toDynT :: TypeableType -> Dynamic
toDynT (T f) = toDyn f

-- Применяет к любой функции набор аргументов разных типов
apply1 :: (Typeable f, Typeable a, Typeable b) => f -> [a] -> Maybe b
apply1 f as = dynamicApply (toDyn f) (map toDyn as) >>= fromDynamic

-- Применяет к любой функции набор аргументов разных типов
apply2 :: (Typeable f, Typeable b) => f -> [TypeableType] -> Maybe b
apply2 f as = dynamicApply (toDyn f) (map toDynT as) >>= fromDynamic        
