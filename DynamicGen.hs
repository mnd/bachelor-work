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
         
         -- Блок аналогичных функций, но получающих на вход имена переменных в виде 'var
         testAndExecuteNT, --  (Quasi m) => [(Name, Name)] -> TestType -> ExpQ -> m Exp
         testsAndExecutesNT, --  (Quasi m) => [([(Name, Name)], TestType, ExpQ)] -> m Exp
         typeAndExecuteNT, --  (Quasi m) => [(Name, Name)] -> ExpQ -> m Exp
         typesAndExecutesNT, --  (Quasi m) => [([(Name, Name)], ExpQ)] -> m Exp
         testAndExecuteNT', --  (Quasi m) => [(Name, Name)] -> TestType -> ExpQ -> Name -> m Exp
         testsAndExecutesNT', --  (Quasi m) => [([(Name, Name)], TestType, ExpQ, Name)] -> m Exp
         typeAndExecuteNT', --  (Quasi m) => [(Name, Name)] -> ExpQ -> Name -> m Exp
         typesAndExecutesNT', --  (Quasi m) => [([(Name, Name)], ExpQ, Name)] -> m Exp
         
         
         -- Блок аналогичных функций, но получающих на вход имена переменных и типы в виде 'var ''Type
         testAndExecuteN, --  (Quasi m) => [(Name, TypeQ)] -> TestType -> ExpQ -> m Exp
         testsAndExecutesN, --  (Quasi m) => [([(Name, TypeQ)], TestType, ExpQ)] -> m Exp
         typeAndExecuteN, --  (Quasi m) => [(Name, TypeQ)] -> ExpQ -> m Exp
         typesAndExecutesN, --  (Quasi m) => [([(Name, TypeQ)], ExpQ)] -> m Exp
         testAndExecuteN', --  (Quasi m) => [(Name, TypeQ)] -> TestType -> ExpQ -> TypeQ -> m Exp
         testsAndExecutesN', --  (Quasi m) => [([(Name, TypeQ)], TestType, ExpQ, TypeQ)] -> m Exp
         typeAndExecuteN', --  (Quasi m) => [(Name, TypeQ)] -> ExpQ -> TypeQ -> m Exp
         typesAndExecutesN', --  (Quasi m) => [([(Name, TypeQ)], ExpQ, TypeQ)] -> m Exp

         
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
         -- аналогично но получает имя типа
         genShowTable', --  (Quasi m) => [(Name, String, ExpQ)] -> m [Dec]
         -- аналогично, но пытается сама определить имя
         genShowTable'', --  (Quasi m) => [(TypeQ, ExpQ)] -> m [Dec]
         -- генерирует сразу и readTable и showTable
         genReadShowTable, --  (Quasi m) => [(TypeQ, String, ExpQ, ExpQ)] -> m [Dec]
         -- аналогично но получает имя типа
         genReadShowTable', --  (Quasi m) => [(Name, String, ExpQ, ExpQ)] -> m [Dec]
         -- Аналогично, но сама подбирает имя
         genReadShowTable'', --  (Quasi m) => [(TypeQ, ExpQ, ExpQ)] -> m [Dec]
         
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

nntoet :: [(Name, Name)] -> [(ExpQ, TypeQ)]
nntoet ns = map (\(v, t) -> (varE v, conT t)) ns

nttoet :: [(Name, TypeQ)] -> [(ExpQ, TypeQ)]
nttoet ns = map (\(v, t) -> (varE v, t)) ns


-- Получает на вход набор переменных с типами, тест и код на случай успеха. Возвращает Just Result или Nothing
testAndExecute :: (Quasi m) => [(ExpQ, TypeQ)] -> TestType -> ExpQ -> m Exp
testAndExecute vts test code = runQ [| if ($(checkTypes vts) && $(test' test))
                                       then $(extractAndExecute vts [| toDyn ($code) |])
                                       else Nothing |]
  where test' (Static e)  = runQ [| $(extractAndExecute vts [| ($e) :: Bool |]) == (Just True) |]
        test' (Dynamic e) = runQ [| ($e >>= fromDynamic) == (Just True) |]
        
-- Получает имена
testAndExecuteNT :: (Quasi m) => [(Name, Name)] -> TestType -> ExpQ -> m Exp
testAndExecuteNT vts test code = testAndExecute (nntoet vts) test code
        
testAndExecuteN :: (Quasi m) => [(Name, TypeQ)] -> TestType -> ExpQ -> m Exp
testAndExecuteN vts test code = testAndExecute (nttoet vts) test code

-- Аналогичная предыдущей, только работает со списком таких параметров
testsAndExecutes :: (Quasi m) => [([(ExpQ, TypeQ)], TestType, ExpQ)] -> m Exp
testsAndExecutes pl = runQ $ foldr testsAndExecutes' [| Nothing |] (reverse pl)
  where
    testsAndExecutes' (vts, test, code) e = [| $(testAndExecute vts test code) `mplus`  $e |]

-- Получает имена
testsAndExecutesNT :: (Quasi m) => [([(Name, Name)], TestType, ExpQ)] -> m Exp
testsAndExecutesNT pl = testsAndExecutes $ map (\(vts, tt, e) -> (nntoet vts, tt, e)) pl

testsAndExecutesN :: (Quasi m) => [([(Name, TypeQ)], TestType, ExpQ)] -> m Exp
testsAndExecutesN pl = testsAndExecutes $ map (\(vts, tt, e) -> (nttoet vts, tt, e)) pl


-- Аналогичны предыдущим, только без теста
typeAndExecute :: (Quasi m) => [(ExpQ, TypeQ)] -> ExpQ -> m Exp
typeAndExecute vts code = testAndExecute vts (Static [| True |]) code

typeAndExecuteNT :: (Quasi m) => [(Name, Name)] -> ExpQ -> m Exp
typeAndExecuteNT vts code = testAndExecute (nntoet vts) (Static [| True |]) code

typeAndExecuteN :: (Quasi m) => [(Name, TypeQ)] -> ExpQ -> m Exp
typeAndExecuteN vts code = testAndExecute (nttoet vts) (Static [| True |]) code

typesAndExecutes :: (Quasi m) => [([(ExpQ, TypeQ)], ExpQ)] -> m Exp
typesAndExecutes pl = testsAndExecutes $ map (\ (vts, code) -> (vts, (Static [| True |]), code)) pl

typesAndExecutesNT :: (Quasi m) => [([(Name, Name)], ExpQ)] -> m Exp
typesAndExecutesNT pl = typesAndExecutes $ map (\(vts, e) -> (nntoet vts, e)) pl

typesAndExecutesN :: (Quasi m) => [([(Name, TypeQ)], ExpQ)] -> m Exp
typesAndExecutesN pl = typesAndExecutes $ map (\(vts, e) -> (nttoet vts, e)) pl

-- Возможно это как-то можно переписать, но врядли. Так что пусть будут клоны предыдущих функций. Использовать только если результат не выводим
testAndExecute' :: (Quasi m) => [(ExpQ, TypeQ)] -> TestType -> ExpQ -> TypeQ -> m Exp
testAndExecute' vts test code resultType = runQ [| if ($(checkTypes vts) && $(test' test))
                                                   then $(extractAndExecute vts [| toDyn (($code) :: $resultType) |])
                                                   else Nothing |]
  where test' (Static e)  = runQ [| $(extractAndExecute vts [| ($e) :: Bool |]) == (Just True) |]
        test' (Dynamic e) = runQ [| ($e >>= fromDynamic) == (Just True) |]


testAndExecuteNT' :: (Quasi m) => [(Name, Name)] -> TestType -> ExpQ -> Name -> m Exp
testAndExecuteNT' vts test code resultType = testAndExecute' (nntoet vts) test code (conT resultType)

testAndExecuteN' :: (Quasi m) => [(Name, TypeQ)] -> TestType -> ExpQ -> TypeQ -> m Exp
testAndExecuteN' vts test code resultType = testAndExecute' (nttoet vts) test code resultType

testsAndExecutes' :: (Quasi m) => [([(ExpQ, TypeQ)], TestType, ExpQ, TypeQ)] -> m Exp
testsAndExecutes' pl = runQ $ foldr testsAndExecutes' [| Nothing |] (reverse pl)
  where
    testsAndExecutes' (vts, test, code, resultType) e = [| $(testAndExecute' vts test code resultType) `mplus`  $e |]

testsAndExecutesNT' :: (Quasi m) => [([(Name, Name)], TestType, ExpQ, Name)] -> m Exp
testsAndExecutesNT' pl = testsAndExecutes' $ map (\(vts, test, code, rt) -> (nntoet vts, test, code, conT rt)) pl

testsAndExecutesN' :: (Quasi m) => [([(Name, TypeQ)], TestType, ExpQ, TypeQ)] -> m Exp
testsAndExecutesN' pl = testsAndExecutes' $ map (\(vts, test, code, rt) -> (nttoet vts, test, code, rt)) pl

-- Аналогичны предыдущим, только без теста
typeAndExecute' :: (Quasi m) => [(ExpQ, TypeQ)] -> ExpQ -> TypeQ -> m Exp
typeAndExecute' vts code rt = testAndExecute' vts (Static [| True |]) code rt

typeAndExecuteNT' :: (Quasi m) => [(Name, Name)] -> ExpQ -> Name -> m Exp
typeAndExecuteNT' vts code rt =  typeAndExecute' (nntoet vts) code (conT rt)

typeAndExecuteN' :: (Quasi m) => [(Name, TypeQ)] -> ExpQ -> TypeQ -> m Exp
typeAndExecuteN' vts code rt =  typeAndExecute' (nttoet vts) code rt

typesAndExecutes' :: (Quasi m) => [([(ExpQ, TypeQ)], ExpQ, TypeQ)] -> m Exp
typesAndExecutes' pl = testsAndExecutes' $ map (\ (vts, code, rt) -> (vts, (Static [| True |]), code, rt)) pl

typesAndExecutesNT' :: (Quasi m) => [([(Name, Name)], ExpQ, Name)] -> m Exp
typesAndExecutesNT' pl = typesAndExecutes' $ map (\ (vts, code, rt) -> (nntoet vts, code, conT rt)) pl

typesAndExecutesN' :: (Quasi m) => [([(Name, TypeQ)], ExpQ, TypeQ)] -> m Exp
typesAndExecutesN' pl = typesAndExecutes' $ map (\ (vts, code, rt) -> (nttoet vts, code, rt)) pl


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

-- Аналогично, но получает имя типа
genShowTable' :: (Quasi m) => [(Name, String, ExpQ)] -> m [Dec]
genShowTable' ts = genShowTable $ map (\(n,s,e) -> (conT n,s,e)) ts

-- аналогично, но пытается сама определить имя
genShowTable'' :: (Quasi m) => [(TypeQ, ExpQ)] -> m [Dec]
genShowTable'' ts = 
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
  read' <- genReadTable  $ map (\(ty, str, read, show) -> (str, read, ty)) is
  show' <- genShowTable $ map (\(ty, str, read, show) -> (ty, str, show)) is
  return $ read' ++ show'

-- Аналогично, но получает имя типа
genReadShowTable' :: (Quasi m) => [(Name, String, ExpQ, ExpQ)] -> m [Dec]
genReadShowTable' is = genReadShowTable $ map (\(n,s,r,sh) -> (conT n, s, r, sh)) is

-- Аналогично, но сама подбирает имя
genReadShowTable'' :: (Quasi m) => [(TypeQ, ExpQ, ExpQ)] -> m [Dec]
genReadShowTable'' is = do
  read' <- genReadTable' $ map (\(ty, read, show) -> (read, ty)) is
  show' <- genShowTable'' $ map (\(ty, read, show) -> (ty, show)) is
  return $ read' ++ show'
  