import Data.Typeable
import Data.Dynamic
import Unsafe.Coerce
-- Первый вариант

-- Вот так создавать Динамические методы
add1 :: Dynamic -> Dynamic -> Maybe Dynamic
add1 a b
    | ((dynTypeRep a, dynTypeRep b) ==
       (typeOf (undefined::Integer), typeOf (undefined::Integer))) = do
        ar <- ((fromDynamic a)::(Maybe Integer))
        br <- ((fromDynamic b)::(Maybe Integer))
        if True then Just $ toDyn $ (+) ar br else
            if False then Just $ toDyn $ (-) ar br else
                error "1234"
    | ((dynTypeRep a, dynTypeRep b) ==
       (typeOf (undefined::Double), typeOf (undefined::Double))) = do
        ar <- ((fromDynamic a)::(Maybe Double))
        br <- ((fromDynamic b)::(Maybe Double))
        Just (toDyn (ar + br))
    | otherwise = Nothing

add2 :: Dynamic -> Dynamic -> Maybe Dynamic
add2 a b
    | ((dynTypeRep a, dynTypeRep b) ==
       (typeOf (undefined::Integer), typeOf (undefined::Double))) = do
        ar <- ((fromDynamic a)::(Maybe Integer))
        br <- ((fromDynamic b)::(Maybe Double))
        Just (toDyn (((fromIntegral ar) + br)::Double))
    | ((dynTypeRep a, dynTypeRep b) ==
       (typeOf (undefined::Double), typeOf (undefined::Integer))) = do
        ar <- ((fromDynamic a)::(Maybe Double))
        br <- ((fromDynamic b)::(Maybe Integer))
        Just (toDyn ((ar + (fromIntegral br))::Double))
    | otherwise = Nothing

-- И как-то так их комбинировать
simpleCombine :: (a -> Maybe b) -> (a -> Maybe b) -> a -> Maybe b
simpleCombine f1 f2 args = case f1 args of
                       Just a -> Just a
                       Nothing -> f2 args

combine :: [a -> Maybe b] -> a -> Maybe b
combine [] args = Nothing
combine (f:fs) args = simpleCombine f (combine fs) args

add a b = combine [uncurry add1, uncurry add2] (a, b)

-- Минус -- громоздко

-------------------------------------------------------------
-- Второй вариант.

-- Определяем функции
addInt :: Integer -> Integer -> Integer
addInt = (+)

addDouble :: Double -> Double -> Double
addDouble = (+)


-- Пишем функцию применяющую список аргументов к функции
dynamicApply :: Dynamic {- a1->a2->…->an -} -> [Dynamic] -> Maybe Dynamic
dynamicApply f (arg:args) = case (dynApply f arg) of
                              Just f -> dynamicApply f args
                              Nothing -> Nothing
dynamicApply f [] = Just f

-- Пишем функцию применяющую список аргументов к списку функций
combineDynApply :: [Dynamic {- a1->a2->…->an -}] -> [Dynamic] -> Maybe Dynamic
combineDynApply (f:fs) args = case (dynamicApply f args) of
                                Nothing -> (combineDynApply fs args)
                                res     -> res
combineDynApply [] _ = Nothing

-- И делаем эффективные функции как
genericAdd = combineDynApply [(toDyn addInt), (toDyn addDouble)]

-- Минус -- не работает с полиморфными функциями.
-- Например для (toDyn (show)) == <<() -> [Char]>>
-- dynApply для любого аргумента будет выдавать Nothing

-----------------------------------------------------------
-- Пример                      
main = do
  putStrLn $ show $ myShow $ add (toDyn (1::Integer)) (toDyn (4::Integer))
  putStrLn $ show $ myShow $ add (toDyn (1::Double)) (toDyn (4::Double))
  putStrLn $ show $ myShow $ add (toDyn (1::Double)) (toDyn (4::Integer))

  putStrLn $ show $ myShow $ genericAdd [(toDyn (1::Integer)), (toDyn (4::Integer))]
  putStrLn $ show $ myShow $ genericAdd [(toDyn (1::Double)), (toDyn (4::Double))]
  putStrLn $ show $ myShow $ genericAdd [(toDyn (1::Double)), (toDyn (4::Integer))]
-----------------------------------------------------------           
-- Подобная функция может использоваться для считывания токенов
readAtom :: String -> Dynamic
readAtom ('i':xs) = toDyn (read xs :: Integer)
readAtom ('d':xs) = toDyn (read xs :: Double)
readAtom ('s':xs) = toDyn (xs::String)

-- Функция для вывода этих Maybe Dynamic'ов
myShow :: Maybe Dynamic -> Maybe String
myShow Nothing = Nothing
myShow (Just s) = myShow' s
    where myShow' s
              | dynTypeRep s == typeOf (undefined::Integer) =
                  ((fromDynamic s)::(Maybe Integer)) >>= return . show
              | dynTypeRep s == typeOf (undefined::Double) =
                  ((fromDynamic s)::(Maybe Double)) >>= return . show
              | otherwise = Nothing

-- Не работает
-- myShow1 :: Maybe Dynamic -> Maybe String
-- myShow1 Nothing = Nothing
-- myShow1 (Just a) = do
--   r <- unsafeCoerce show
--   case funResultTy (typeOf r) (dynTypeRep a) of
--     Nothing -> Nothing
--     Just _  -> dynApply (toDyn r) a >>= (fromDynamic :: (Dynamic -> Maybe String))
