import Data.Char
import Data.List
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token

-----Вспомогательные функции-------------------------
spaceSymbol :: Char -> Bool
spaceSymbol ' '  = True
spaceSymbol '\n' = True
spaceSymbol '\t' = True
spaceSymbol c    = False

trimHead :: String -> String
trimHead = dropWhile spaceSymbol

trimTail :: String -> String
trimTail = reverse . trimHead . reverse

trim :: String -> String
trim s = trimTail $ trimHead s

----------Грамматика--------------------------------
word :: Parser String
word = many1 (alphaNum <|> oneOf "_'." <?> "") <?> "word"

-- пробельные символы, которые как правило будут игнорироваться
sn = (space <|> newline <|> tab)

-- список слов word через пробельные символы sn
wordList :: Parser [String]
wordList = do{ s1 <- word
          ; many sn
          ; do { s2 <- wordList
               ; return (s1:s2)
               }
            <|> return [s1]
          }

-- Для считывания до конца строки. Предпологается, что мы уже прочитали открывающую двойную ковычку и ищем закрывающую
quotedString :: Parser String
quotedString = do{ char '\\'
                 ; c1 <- anyChar
                 ; s <- quotedString
                 ; return ('\\':c1:s)
                 }
               <|> do { char '"'
                      ; return ""
                      }
               <|> do { c2 <- anyChar
                      ; s <- quotedString
                      ; return (c2:s)
                      }

-- считывает весь текст до строки "s". Всегда обрабатывает фигурные и круглые
-- скобки как парные и s внутри них не ищет.
-- Аналогично с одинарными и двойными ковычками
beforeWord :: String -> Parser String
beforeWord s = do { try (string s)
                  ; return ""
                  }
               <|> do { c <- anyChar
                      ; (case c of
                            '\'' -> do{ s0 <- beforeWord "'"
                                     ; s0' <- beforeWord s
                                     ; return ((c:s0) ++ ('\'':s0'))
                                     }
                            '(' -> do{ s1 <- beforeWord ")"
                                     ; s1' <- beforeWord s
                                     ; return ((c:s1) ++ (')':s1'))
                                     }
                            '{' -> do{ s2 <- beforeWord "}"
                                     ; s2' <- beforeWord s
                                     ; return ((c:s2) ++ ('}':s2'))
                                     }
                            '[' -> do{ s2 <- beforeWord "]"
                                     ; s2' <- beforeWord s
                                     ; return ((c:s2) ++ (']':s2'))
                                     }
                            '"' -> do{ s2 <- quotedString
                                     ; s2' <- beforeWord s
                                     ; return ((c:s2) ++ ('"':s2'))
                                     }
                            _  -> do{ s3 <- beforeWord s
                                    ; return (c:s3)
                                    })
                         }

-- "import haskell code SPACES ;" -> import haskell code
import1 :: Parser String
import1 = do{ try (string "import")
            ; s <- fmap trimTail (beforeWord ";")
            ; return ("import" ++ s)
            }
          
-- Считывает и возвращает "import haskell code when"
module1 :: Parser String
module1 = do{ try (string "module")
            ; s <- beforeWord "where"
            ; return ("module" ++ s ++ "where")
            }

-- Список слов через запятую. Используется для разбора списка типов внутри "[" "]"
list :: Parser [String]
list = do{ t <- fmap unwords wordList
             ; many sn
             ; do{ char ','
                 ; many sn
                 ; ts <- list
                 ; return (t:ts)
                 }
               <|> return [t]
             }

type Typedef = (String, [String])
-- Разбирает блок "typedef name = [ types ]" и возвращает пару (name, [types])
typedef :: Parser Typedef
typedef = do{ try (string "typedef")
            ; many1 sn
            ; name <- word
            ; many sn
            ; char '='
            ; many sn
            ; char '['
            ; many sn
            ; tl <- list
            ; many sn
            ; char ']'
            ; many sn
            ; char ';'
            ; return (name, tl)
            }

            
type TypeName = (String, String)
--  Вспомогательная функция. Преобразует список слов в пару тип и имя. Например ["Maybe", "Bool", "name"] в ("Maybe Bool", "name")
typeNameParse :: [String] -> TypeName
typeNameParse x = (unwords (init x), last x)

-- Разбирает аргументы функции, почти идентично правилу list, только слегка иначе обрабатывается 
funArgs :: Parser [TypeName]
funArgs = do{ a <- fmap typeNameParse wordList
            ; do{ char ','
                ; many sn
                ; as <- funArgs
                ; return (a:as)
                }
              <|> return [a]
            }

-- Разбирает определение функции "Type name (types with args)"
definition :: Parser FunDef
definition = do{ ftn <- fmap typeNameParse wordList
               ; many sn
               ; char '('
               ; many sn
               ; fargs <- funArgs
               ; many sn
               ; char ')'
               ; return (ftn,fargs) -- ((type, name):[(type, argname)])
               }

data When = Static String | Dynamic String
          deriving Show
-- Разбирает блок when
whenDefinition :: Parser When
whenDefinition = do{ try (string "when")
                   ; many sn
                   ; do{ char '{'
                       ; pred <- fmap (Static . trim) (beforeWord "}")
                       ; return pred
                       }
                     <|>
                     do{ char '('
                       ; pred <- fmap (Dynamic . trim) (beforeWord ")")
                       ; return pred
                       }
                   }

-- разбирает тело функции
bodyDefinition :: Parser String
bodyDefinition = do { char '{'
                    ; body <- fmap trim (beforeWord "}")
                    ; return body
                    }

type FunDef = (TypeName,[TypeName])
data Function = Function FunDef When String

-- Полностью разбирает функцию: несколько определений с опциональными блоками when и тело функции
function :: Parser [Function]
function = do{ def <- definition
             ; many sn
             ; do{ body <- bodyDefinition
                 ; return [Function def emptyWhen body]
                 }
               <|> do{ when <- whenDefinition
                     ; many sn
                     ; do{ body <- bodyDefinition
                         ; return [Function def when body]
                         }
                       <|> do{ fun <- function
                             ; let body = (\(Function _ _ b) -> b) $ head fun
                             ; return ((Function def when body):fun)
                             }
                     }
               <|> do{ fun <- function
                     ; let body = (\(Function _ _ b) -> b) $ head fun
                     ; return ((Function def emptyWhen body):fun)
                     }
             }
           where
             emptyWhen = Static "True"

data Program = Program String {- modules + imports -} [Typedef] [Function]

-- вспомогательная функция. добавляет токены в программу
merge :: Program -> Program -> Program
merge (Program s1 t1 f1) (Program s2 t2 f2) = (Program sr tr fr)
  where
    sr = s1 ++ ('\n':s2)
    tr = t1 ++ t2
    fr = f1 ++ f2   

-- В этом правиле происходит разбор блоков программы. Модуль, импорт, определение списка типов и определение функции. 
program :: Parser Program
program = do{ t <- ((module1      >>= \x -> return $ Program x  []  [])
                    <|> (import1  >>= \x -> return $ Program x  []  [])
                    <|> (typedef  >>= \x -> return $ Program "" [x] [])
                    <|> (function >>= \x -> return $ Program "" []  x))
            ; many sn
            ; do{ ts <- program
                ; return (merge t ts)
                }
              <|> return t
            }
          <|> return (Program ""  [] [])

-- Базовое правило. Определяет возможность написать опциональный блок полностью на хаскеле в начале кода, отделенный от когда generic программы строкой "\n%%"
code :: Parser Program
code = do{ many sn
         ; do { hc <- try(beforeWord "\n%%")
              ; many sn
              ; (Program s t f) <- program
              ; return $ Program (hc ++ "\n\n" ++ s) t f
              }
           <|> do { p <- program
                  ; return p
                  }
         }

--------------------- Грамматика кончилась. Обработка. -------------------------------------

-- Генерация кода
generate :: Program -> String
generate (Program s t f) = s ++ "\n" ++ restrictedModules ++ "\n" ++ (unlines $ genFunctions f')
                        ++ "\n" ++ (genSymbolTable f) ++ "\n" ++ (genRead fr) ++ "\n" ++ (genShow fs)
  where
    f1 = untypes t f
    (fr, f2) = partition (((==) "read") . funName) f1
    (fs, f') = partition (((==) "show") . funName) f2
    
-- Модули которые нужны всегда
restrictedModules = unlines ["import Data.Typeable", "import Data.Dynamic", "import Data.Maybe", "import Monad"]

-- Извлечь типы определенные через typedef
untypes :: [Typedef] -> [Function] -> [Function]
untypes ts fs = concatMap untypes' fs
  where
    -- (TypeName,[TypeName])
    untypes' func@ (Function ((nt, nn), as) w b) =
      case lookup nt ts of
        Nothing -> untypesArgs func
        Just types -> concatMap (\t -> untypesArgs (Function ((t, nn), as) w b)) types

    -- заменяет тип num-того аргумента функции func на newType
    untNArg func@(Function (n,as) w b) newType num =
      let an = snd $ as !! num
      in Function (n, changeN as (newType, an) num) w b
    
    untypesArgs func@ (Function (_,as) _ _)   = untypesArgs' func (length as)
      where
        untypesArgs' func 0 = [func]
        untypesArgs' func@ (Function (n,as) w b) inum =
          let num = inum - 1
              at  = fst (as !! num)
              an  = snd (as !! num)
          in
           case lookup at ts of
             Nothing -> untypesArgs' func num
             Just types -> concatMap (\t -> untypesArgs' (untNArg func t num) num) types
                                                 

-- заменить Nтый элемент списка новым элементом
changeN :: [a] -> a -> Int -> [a]
changeN ls t n | 0 <= n && n < length ls = (take n ls) ++ [t] ++ (drop (n+1) ls)
               | otherwise               = ls

-- Извлечение данных из функции
funTypeName :: Function -> (String, String)
funTypeName = fst . funDef
funName :: Function -> String
funName = snd . funTypeName
funType :: Function -> String
funType = fst . funTypeName

funTypesArgs :: Function -> [TypeName]
funTypesArgs = snd . funDef

funDef :: Function -> FunDef
funDef  (Function f _ _) = f
whenDef :: Function -> When
whenDef (Function _ w _) = w
bodyDef :: Function -> String 
bodyDef (Function _ _ b) = b

genFun :: [Function] -> String
genFun funs@ (f:fs) = let fname          = funName f
                          header         = fname ++ " :: [Dynamic] -> Maybe Dynamic\n"
                          testAndBody    = unlines $ genTestAndBody funs
                          defaultWay     = fname ++ " _ = Nothing"
                      in header ++ "\n" ++ testAndBody ++ "\n" ++ defaultWay

genRead :: [Function] -> String
genRead fs = let as   = map (snd . head . funTypesArgs) fs
                 bs   = map bodyDef fs
                 ts   = map funType fs
                 defs = zipWith3 (\a b t -> "(" ++ (show t) ++ ", (\\" ++ a ++ " -> toDyn ((" ++ b ++ ") :: (" ++ t ++ "))))") as bs ts
               in "readTable :: [(String, (String -> Dynamic))]\n" ++ "readTable = [" ++ (intercalate ", " defs) ++ "]"

genShow :: [Function] -> String
genShow fs = let an = "someNeverUnuseableName"
                 tas  = map (head . funTypesArgs) fs
                 bs   = map bodyDef fs
                 defs = zipWith (\(t, a) b -> "((typeOf (undefined :: (" ++ t ++ "))), (\\" ++ an
                                              ++ " -> ((fromDynamic (" ++ an ++ ") :: (Maybe (" ++ t ++ "))) >>= \\" ++ a
                                              ++ " -> return (\"((\" ++ (" ++ b ++ ") ++ \") :: " ++ t ++ ")\"))))") tas bs
             in "showTable :: [(TypeRep, (Dynamic -> Maybe String))]\n" ++ "showTable = [" ++ (intercalate ", " defs) ++ "]"

genArgs :: Function -> [String]
genArgs f = map snd (funTypesArgs f)

genTestAndBody :: [Function] -> [String]
genTestAndBody []        = []
genTestAndBody (f:fs) = let s = (funName f) ++ " [" ++ (intercalate ", " (genArgs f)) ++ "]"
                                ++ " | " ++ (intercalate " && " (genTests f))
                                ++ " && " ++ (genWhen f) ++ " = " ++ (genBody f)
                             in s : (genTestAndBody fs)

genTests :: Function -> [String]
genTests f = genTests' $ funTypesArgs f
  where
    genTests' :: [(String, String)] {- [(Type, Name)] -} -> [String]
    genTests' [] = []
    genTests' (d:ds) = let test = "((dynTypeRep " ++ (snd d) ++ ") == (typeOf (undefined :: " ++ (fst d) ++ ")))"
                       in test : (genTests' ds)

-- Извлекает аргументы из динамиков. Получает список [(Type, Name)]
genExtactArgs ::  [(String, String)] {- [(Type, Name)] -} -> String
genExtactArgs [] = []
genExtactArgs (d:ds) = "((fromDynamic " ++ (snd d) ++ ") :: Maybe " ++ (fst d) ++ ")"
                       ++ " >>= \\" ++ (snd d) ++ " -> " ++ (genExtactArgs ds)


genWhen :: Function -> String
genWhen f = "((" ++ extractingArgs when
            ++ "return (" ++ (genWhen' when) ++")) == Just True)"
  where
    when = whenDef f
    fargs = funTypesArgs f
    genWhen' (Static when)  = when
    genWhen' (Dynamic when) = "(fromJust . (fromDynamic :: Dynamic -> Maybe Bool)) (" ++ when ++ ")"
    extractingArgs (Static _) = genExtactArgs $ funTypesArgs f
    extractingArgs (Dynamic _) = ""

genBody :: Function -> String
genBody f = "(" ++ (genExtactArgs $ funTypesArgs f) ++ "return $ toDyn "
            ++ "((" ++ (bodyDef f) ++ ") :: " ++ (funType f) ++ "))"

-- Генерирует набор функций по структурам описывающим их. Код не красив. Стоит переписать.
genFunctions :: [Function] -> [String]
genFunctions [] = []
genFunctions funs@ (f:fs) =
  let name = funName f
      (fdefs, others) = partition (((==) name) . funName) funs
  in if name `notElem` registredNames then 
       (genFun fdefs) : (genFunctions others)
     else
       genFunctions others
   
   
genSymbolTable :: [Function] -> String
genSymbolTable funs = let fs = filter ((flip notElem registredNames) . funName) funs
                          fnames = nub $ map funName fs
                          fnt = map (\a -> "(" ++ (show a) ++ ", " ++ a ++ ")") fnames
                      in  "symbolTable :: [(String, ([Dynamic] -> Maybe Dynamic))]"
                          ++ "\n" ++ "symbolTable = [" ++ (intercalate ", " fnt) ++ "]"

registredNames :: [String]
registredNames = ["read", "show", "symbolTable", "readTable"]

--------------------- Обработка закончилась. Использование: runFile code "dynamic.gen" >>= return . generate --------------------------------
--------------------------------------------- то есть функции "generate" отдавать вывод парсера "code" --------------------------------------

run p input = 
  case (parse p "" input) of
    Left err -> error ("parse error at " ++ show err)
    Right x  -> x

main = do
  text <- getContents
  putStrLn $ generate $ run code text


{- На вход программа может получать два типа ввода.

>Some Many Lines Haskell Code
>For example module define or some imports
>
>%%
>
>program code

Либо просто

>program code

В первом случае разбивка хаскель кода и кода на нашем миниязыке
происходит при нахождении строки "\n%%" и всё дальнейшее является
кодом нашей программы. "programm code" может содержать произвольное
количество следующих блоков:

>'module' haskell module code 'where'

>'import' haskell import code SPACES ';'

>'typedef' name '=' '[' List ',' Of ',' Types ']' ';'

>name '(' arg1name ',' arg2Name ',' argNName ')' ';'

>ResultType functionName   '(' Arg1Type  arg1Name ',' Arg2Type  arg2Name ',' argNType  argNName ')' 'when' '{' some haskell code '}'
>Result1Type function1Name '(' Arg1Type1 arg1Name ',' Arg2Type1 arg2Name ',' argNType1 argNName ')' 'when' '(' some haskell code ')'
>'{'
>   some haskell code
>'}'

Где "some haskell code" вставляется с обрезанием начальных и хвостовых
пробельных символов в код. Перед кодом тела функции возможно
произвольное число заголовков как с блоком "when", так и без него. Для
всех этих заголовков будет подставлено одинаковое тело. 

В блоке "when ( … )" доступны только динамические переменные. А в
блоках "when { ... }" и "{ body }" только статические

Код в блоке "when" получает аргументы реальных типов и должен
возвращать "Bool". Если вы хотите использовать вместо этого
динамическую функцию, которая возвращает завернутый в "Dynamic" тип
"Bool", то вам необходимо использовать круглые скобки, вместо
фигурных.

При использовании в качестве типа аргумента или типа возвращаемого
значения имени "name", определенного при помощи ключевого слова
"typedef", определение функции будет развернуто в ряд определений, в
каждом из которых вместо "name" будет подставлен один из типов
указаных в списке в правой части "typedef"

Функции с именами symbolTable и readTable игнорируются. 

Функции с именем read (к сожалению без ряда проверок) интерпретируются
как функции
> Type read (String arg) { Body }
То есть блок when игнорируется, все аргументы кроме первого
игнорируются, тип аргумента игнорируется и считается типом String. 

Функции read генерируют структуру readTable вида 
> readTable = [("Type", \arg -> toDyn ((Body) :: Type)), … ]
Которая используется для считывания считывания строки.

Добавлена showTable аналогичная readTalbe. Целесообразность сомнительна, так что возможно стоит её удалить.

-}