{-# LANGUAGE TemplateHaskell #-}
module DynamicGen
       (
         module Data.Dynamic,
         -- Создаёт проверку типа по переменной типа Dynamic и её типу
         checkType, --  (Quasi m) => ExpQ -> TypeQ -> m Exp
         -- Создаёт проверку типов по списку пар (переменная, тип)
         checkTypes, --  (Quasi m) => [(ExpQ, TypeQ)] -> m Exp
         -- Извлекает ряд значений из динамиков и их типов и заменяет в коде exp переменные на извлечённые
         extractAndExecute, --  (Quasi m) => [(ExpQ, TypeQ)] -> ExpQ -> m Exp
         TestType (Static, Dynamic),
         -- Получает на вход набор переменных с типами, тест и код на случай успеха. Возвращает Just Result или Nothing
         testAndExecute, --  (Quasi m) => [(ExpQ, TypeQ)] -> TestType -> ExpQ -> m Exp
         -- Аналогичная предыдущей, только работает со списком таких параметров
         testsAndExecutes, --  (Quasi m) => [([(ExpQ, TypeQ)], TestType, ExpQ)] -> m Exp
         -- Аналогичны предыдущим, только без теста
         typeAndExecute, --  (Quasi m) => [(ExpQ, TypeQ)] -> ExpQ -> m Exp
         typesAndExecutes, --  (Quasi m) => [([(ExpQ, TypeQ)], ExpQ)] -> m Exp
         -- То же самое, но с указанием типа результата
         testAndExecute', --  (Quasi m) => [(ExpQ, TypeQ)] -> TestType -> ExpQ -> TypeQ -> m Exp
         testsAndExecutes', --  (Quasi m) => [([(ExpQ, TypeQ)], TestType, ExpQ, TypeQ)] -> m Exp
         typeAndExecute', --  (Quasi m) => [(ExpQ, TypeQ)] -> ExpQ -> TypeQ -> m Exp
         typesAndExecutes', --  (Quasi m) => [([(ExpQ, TypeQ)], ExpQ, TypeQ)] -> m Exp
         
         -- генерация таблицы символов
         genSymbolTable, --  (Quasi m) => [(String, ExpQ)] -> m [Dec]
         -- аналогично, но пытается сама определить имя
         genSymbolTable', --  (Quasi m) => [ExpQ] -> m [Dec]
         -- генерация таблицы чтений 
         genReadTable, --  (Quasi m) => [(String, ExpQ, TypeQ)] -> m [Dec]
         -- аналогично, но пытается сама определить имя
         genReadTable', --  (Quasi m) => [(ExpQ, TypeQ)] -> m [Dec]
         -- генерирация таблицы отображений
         genShowTable, --  (Quasi m) => [(TypeQ, String, ExpQ)] -> m [Dec]
         -- аналогично, но пытается сама определить имя
         genShowTable', --  (Quasi m) => [(TypeQ, ExpQ)] -> m [Dec]
         -- генерирует сразу и readTable и showTable
         genReadShowTable, --  (Quasi m) => [(TypeQ, String, ExpQ, ExpQ)] -> m [Dec]
         -- Аналогично, но сама подбирает имя
         genReadShowTable', --  (Quasi m) => [(TypeQ, ExpQ, ExpQ)] -> m [Dec]
         
       )
       where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Typeable
import Data.Dynamic
import Data.Generics
import Control.Monad

-- Создаёт проверку типа по переменной типа Dynamic и её типу
checkType :: (Quasi m) => ExpQ -> TypeQ -> m Exp
checkType v t = runQ [| (dynTypeRep $v) == typeOf (undefined :: $t) |]

-- Создаёт проверку типов по списку пар (переменная, тип)
checkTypes :: (Quasi m) => [(ExpQ, TypeQ)] -> m Exp
checkTypes [] = runQ [| True |]
checkTypes [(v,t)] = checkType v t
checkTypes vts =
  let ((v,t):vts') = reverse vts
      appendRule (v,t) exp = [| $(checkType v t) && $exp |]
  in runQ $ foldr appendRule ([| $(checkType v t) |]) vts'
  
-- Извлекает ряд значений из динамиков и их типов и заменяет в коде exp переменные на извлечённые
extractAndExecute :: (Quasi m) => [(ExpQ, TypeQ)] -> ExpQ -> m Exp
extractAndExecute vts exp = runQ $ foldr extractAndExecute' [| Just ($exp) |] (reverse vts)
  where
    extractAndExecute' (v,t) exp = do
      d <- runQ [| (fromDynamic ($v) :: (Maybe $t)) >>= \ x -> $exp |]
      let (InfixE (Just (SigE (AppE fd var1) type1)) op (Just (LamE [VarP var2] code))) = d
      return $ InfixE (Just (SigE (AppE fd var1) type1)) op (Just (LamE [VarP var2] (changeVar code var1 (VarE var2))))
         
-- Проходится по дереву и заменяет вхождения v1 на v2
changeVar code v1 v2 = everywhere (mkT (\v -> if v == v1 then v2 else v )) code

data TestType = Static ExpQ | Dynamic ExpQ

-- Получает на вход набор переменных с типами, тест и код на случай успеха. Возвращает Just Result или Nothing
testAndExecute :: (Quasi m) => [(ExpQ, TypeQ)] -> TestType -> ExpQ -> m Exp
testAndExecute vts test code = runQ [| if ($(checkTypes vts) && $(test' test))
                                       then $(extractAndExecute vts [| toDyn ($code) |])
                                       else Nothing |]
  where test' (Static e)  = runQ [| $(extractAndExecute vts [| ($e) :: Bool |]) == (Just True) |]
        test' (Dynamic e) = runQ [| ($e >>= fromDynamic) == (Just True) |]
        
        
-- Аналогичная предыдущей, только работает со списком таких параметров
testsAndExecutes :: (Quasi m) => [([(ExpQ, TypeQ)], TestType, ExpQ)] -> m Exp
testsAndExecutes pl = runQ $ foldr testsAndExecutes' [| Nothing |] (reverse pl)
  where
    testsAndExecutes' (vts, test, code) e = [| $(testAndExecute vts test code) `mplus`  $e |]

-- Аналогичны предыдущим, только без теста
typeAndExecute :: (Quasi m) => [(ExpQ, TypeQ)] -> ExpQ -> m Exp
typeAndExecute vts code = testAndExecute vts (Static [| True |]) code

typesAndExecutes :: (Quasi m) => [([(ExpQ, TypeQ)], ExpQ)] -> m Exp
typesAndExecutes pl = testsAndExecutes $ map (\ (vts, code) -> (vts, (Static [| True |]), code)) pl

-- Возможно это как-то можно переписать, но врядли. Так что пусть будут клоны предыдущих функций. Использовать только если результат не выводим
testAndExecute' :: (Quasi m) => [(ExpQ, TypeQ)] -> TestType -> ExpQ -> TypeQ -> m Exp
testAndExecute' vts test code resultType = runQ [| if ($(checkTypes vts) && $(test' test))
                                                   then $(extractAndExecute vts [| toDyn (($code) :: $resultType) |])
                                                   else Nothing |]
  where test' (Static e)  = runQ [| $(extractAndExecute vts [| ($e) :: Bool |]) == (Just True) |]
        test' (Dynamic e) = runQ [| ($e >>= fromDynamic) == (Just True) |]
        
        
testsAndExecutes' :: (Quasi m) => [([(ExpQ, TypeQ)], TestType, ExpQ, TypeQ)] -> m Exp
testsAndExecutes' pl = runQ $ foldr testsAndExecutes' [| Nothing |] (reverse pl)
  where
    testsAndExecutes' (vts, test, code, resultType) e = [| $(testAndExecute' vts test code resultType) `mplus`  $e |]


-- Аналогичны предыдущим, только без теста
typeAndExecute' :: (Quasi m) => [(ExpQ, TypeQ)] -> ExpQ -> TypeQ -> m Exp
typeAndExecute' vts code rt = testAndExecute' vts (Static [| True |]) code rt

typesAndExecutes' :: (Quasi m) => [([(ExpQ, TypeQ)], ExpQ, TypeQ)] -> m Exp
typesAndExecutes' pl = testsAndExecutes' $ map (\ (vts, code, rt) -> (vts, (Static [| True |]), code, rt)) pl

-- генерация таблицы символов
-- требует имя
genSymbolTable :: (Quasi m) => [(String, ExpQ)] -> m [Dec]
genSymbolTable fs =  
  let list = foldr genSymbolTable1 [| [] |] fs
  in  runQ [d| symbolTable :: [(String, ([Dynamic] -> Maybe Dynamic))]
               symbolTable = $list |]
  where
    genSymbolTable1 (n,f) e = [| (n, $f) : $e |]

-- не требует имени
genSymbolTable' :: (Quasi m) => [ExpQ] -> m [Dec]
genSymbolTable' fs =
  let list = foldr genSymbolTable1 [| [] |] fs
  in  runQ [d| symbolTable :: [(String, ([Dynamic] -> Maybe Dynamic))]
               symbolTable = $list |]
  where 
    genSymbolTable1 f e = [| $(toPair f) : $e |]
    toPair f = do  
      f' <- f
      let (VarE n) = f'
          name = pprint n
      [| (name, $f) |]
    
    
-- генерация таблицы чтений 
genReadTable :: (Quasi m) => [(String, ExpQ, TypeQ)] -> m [Dec]
genReadTable trs =
  let list = foldr genReadTable'' [| [] |] trs
  in  runQ [d| readTable :: [(String, (String -> Dynamic))]
               readTable = $list |]
  where
    genReadTable'' (typeString, read, resType) exp =
      [| (typeString, \s -> toDyn ((($read) s) :: $resType)) : $exp |]

-- оно же, но с попыткой вычленить имя
genReadTable' :: (Quasi m) => [(ExpQ, TypeQ)] -> m [Dec]
genReadTable' trs = 
  let list = foldr genReadTable''' [| [] |] trs
  in  runQ [d| readTable :: [(String, (String -> Dynamic))]
               readTable = $list |]
  where
    genReadTable''' (read, resType) exp = do
      t <- resType
      let (ConT ts) = t
          typeString = pprint ts
      [| (typeString, \s -> toDyn ((($read) s) :: $resType)) : $exp |]

-- генерирует таблицу отображений
genShowTable :: (Quasi m) => [(TypeQ, String, ExpQ)] -> m [Dec]
genShowTable ts =
  let list = foldr genShowTable'' [| [] |] ts
  in  runQ [d| showTable :: [(TypeRep, (Dynamic -> Maybe String))]
               showTable = $list |]
  where
    genShowTable'' (varType, typeString, show) exp = [| ((typeOf (undefined :: $varType)), \d -> (fromDynamic d :: (Maybe $varType)) >>= \o -> return ("((" ++ ($show o) ++ ") :: " ++ typeString ++")")) : $exp |]
    
-- аналогично, но пытается сама определить имя
genShowTable' :: (Quasi m) => [(TypeQ, ExpQ)] -> m [Dec]
genShowTable' ts = 
  let list = foldr genShowTable''' [| [] |] ts
  in  runQ [d| showTable :: [(TypeRep, (Dynamic -> Maybe String))]
               showTable = $list |]
  where
    genShowTable''' (varType, show) exp = do
      t <- varType
      let (ConT ts) = t
          typeString = pprint ts
      [| ((typeOf (undefined :: $varType)), \d -> (fromDynamic d :: (Maybe $varType)) >>= \o -> return ("((" ++ ($show o) ++ ") :: " ++ typeString ++")")) : $exp |]

-- генерирует сразу и readTable и showTable
genReadShowTable :: (Quasi m) => [(TypeQ, String, ExpQ, ExpQ)] -> m [Dec]
genReadShowTable is = do
  read' <- genReadTable $ map (\(ty, str, read, show) -> (str, read, ty)) is
  show' <- genShowTable $ map (\(ty, str, read, show) -> (ty, str, show)) is
  return $ read' ++ show'

-- Аналогично, но сама подбирает имя
genReadShowTable' :: (Quasi m) => [(TypeQ, ExpQ, ExpQ)] -> m [Dec]
genReadShowTable' is = do
  read' <- genReadTable' $ map (\(ty, read, show) -> (read, ty)) is
  show' <- genShowTable' $ map (\(ty, read, show) -> (ty, show)) is
  return $ read' ++ show'
  