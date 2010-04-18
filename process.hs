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

-- Определим прототип функции. Будет использовать для выставления имён переменных
type Prototype = (String, [String]) -- (name, [args])

prototype :: Parser Prototype
prototype = do{ name1 <- try (do{ name <- word
                                ; many sn
                                ; char '('
                                ; return name
                                })
              ; many sn
              ; args <- list
              ; many sn
              ; char ')'
              ; many sn
              ; char ';'
              ; return (name1, args)
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
type Function = (FunDef, When, String)

-- Полностью разбирает функцию: несколько определений с опциональными блоками when и тело функции
function :: Parser [Function]
function = do{ def <- definition
             ; many sn
             ; do{ body <- bodyDefinition
                 ; return [(def, emptyWhen, body)]
                 }
               <|> do{ when <- whenDefinition
                     ; many sn
                     ; do{ body <- bodyDefinition
                         ; return [(def, when, body)]
                         }
                       <|> do{ fun <- function
                             ; let body = (\(_,_,b) -> b) $ head fun
                             ; return ((def, when, body):fun)
                             }
                     }
               <|> do{ fun <- function
                     ; let body = (\(_,_,b) -> b) $ head fun
                     ; return ((def, emptyWhen, body):fun)
                     }
             }
           where
             emptyWhen = Static "True"

type Program = (String {- modules + imports -}, [Typedef], [Prototype], [Function])

-- вспомогательная функция. добавляет токены в программу
merge :: Program -> Program -> Program
merge (s1, t1, p1, f1) (s2, t2, p2, f2) = (sr, tr, pr, fr)
  where
    sr = s1 ++ ('\n':s2)
    tr = t1 ++ t2
    pr = p1 ++ p2
    fr = f1 ++ f2   

-- В этом правиле происходит разбор блоков программы. Модуль, импорт, определение списка типов и определение функции. 
program :: Parser Program
program = do{ t <- ((module1      >>= \x -> return (x,  [],  [], []))
                    <|> (import1  >>= \x -> return (x,  [],  [], []))
                    <|> (typedef  >>= \x -> return ("", [x], [], []))
                    <|> (prototype >>= \x -> return("", [], [x], []))
                    <|> (function >>= \x -> return ("", [],  [], x)))
            ; many sn
            ; do{ ts <- program
                ; return (merge t ts)
                }
              <|> return t
            }
          <|> return ("",  [],  [], [])

-- Базовое правило. Определяет возможность написать опциональный блок полностью на хаскеле в начале кода, отделенный от когда generic программы строкой "\n%%"
code :: Parser Program
code = do{ many sn
         ; do { hc <- try(beforeWord "\n%%")
              ; many sn
              ; (s, t, p, f) <- program
              ; return (hc ++ "\n\n" ++ s, t, p, f)
              }
           <|> do { p <- program
                  ; return p
                  }
         }

--------------------- Грамматика кончилась. Обработка. -------------------------------------

-- Генерация кода
generate :: Program -> String
generate (s, t, p, f) = s ++ "\n" ++ restrictedModules ++ "\n" ++ (unlines $ genFunctions p f')
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
    untypes' func@ (((nt, nn), as), w, b) =
      case lookup nt ts of
        Nothing -> untypesArgs func
        Just types -> concatMap (\t -> untypesArgs (((t, nn), as), w, b)) types

    -- заменяет тип num-того аргумента функции func на newType
    untNArg func@((n,as), w, b) newType num = let an = (snd $ as !! num)
                                              in ((n, changeN as (newType, an) num), w, b)
    
    untypesArgs func@ ((_,as), _, _)   = untypesArgs' func (length as)
      where
        untypesArgs' func 0 = [func]
        untypesArgs' func@ ((n,as), w, b) inum =
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

funDef :: (FunDef, When, String) -> FunDef
funDef  (f, _, _) = f
whenDef :: (FunDef, When, String) -> When
whenDef (_, w, _) = w
bodyDef :: (FunDef, When, String) -> String 
bodyDef (_, _, b) = b

genArgs :: String -> Int -> [String]
genArgs s i = reverse $ genArgs' s i
  where
    genArgs' s 0 = []
    genArgs' s i = (s ++ (show i)) : (genArgs' s (i-1))

genFun :: [Prototype] -> [Function] -> String
genFun p funs@ (f:fs) = let defaultArgName = "someNeverUnuseableName"
                            fname          = funName f
                            nargs          = length $ funTypesArgs f
                            args           = case lookup fname p of
                              Nothing -> genArgs defaultArgName nargs
                              Just al -> al
                            header         = fname ++ " :: [Dynamic] -> Maybe Dynamic\n"
                                             ++ fname ++ " [" ++ (intercalate ", " args) ++ "]"
                            testAndBody    = unlines $ map ((take 8 (repeat ' ')) ++) (genTestAndBody args funs)
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
                                              ++ " -> return (\"((\" ++ (" ++ b ++ ") ++ \") : " ++ t ++ ")\"))))") tas bs
             in "showTable :: [(TypeRep, (Dynamic -> Maybe String))]\n" ++ "showTable = [" ++ (intercalate ", " defs) ++ "]"

genTestAndBody :: [String] -> [Function] -> [String]
genTestAndBody _ []        = [] -- Ветка создается на случай если обработка не реализована
genTestAndBody args (f:fs) = let s = "| " ++ (intercalate " && " (genTests args f))
                                     ++ " && " ++ (genWhen args f) ++ " = " ++ (genBody args f)
                             in s : (genTestAndBody args fs)

genTests :: [String] -> Function -> [String]
genTests args f = genTests' $ zip args (funTypesArgs f)
  where
    genTests' :: [(String, (String, String))] {- [(Name, (Type, InnerName))] -} -> [String]
    genTests' [] = []
    genTests' (d:ds) = let test = "((dynTypeRep " ++ (fst d) ++ ") == (typeOf (undefined :: " ++ (fst $ snd d) ++ ")))"
                       in test : (genTests' ds)

-- Извлекает аргументы из динамиков. Получает список [(Name, (Type, InnerName))]
genExtactArgs ::  [(String, (String, String))] {- [(Name, (Type, InnerName))] -} -> String
genExtactArgs [] = []
genExtactArgs (d:ds) = "((fromDynamic " ++ (fst d) ++ ") :: Maybe " ++ (fst $ snd d) ++ ")"
                       ++ " >>= \\" ++ (snd $ snd d) ++ " -> " ++ (genExtactArgs ds)


genWhen :: [String] -> Function -> String
genWhen args f = "((" ++ (genExtactArgs $ extractingArgs when)
                 ++ "return (" ++ (genWhen' when) ++")) == Just True)"
  where
    when = whenDef f
    fargs = funTypesArgs f
    genWhen' (Static when)  = when
    genWhen' (Dynamic when) = "(fromJust . (fromDynamic :: Dynamic -> Maybe Bool)) (" ++ when ++ ")"
    extractingArgs (Static _) = zip args fargs
    extractingArgs (Dynamic _) = filter shadowArgs (zip args fargs)
      where
        shadowArgs (ext, (_, int)) = ext /= int

genBody :: [String] -> Function -> String
genBody args f = "(" ++ (genExtactArgs $ zip args (funTypesArgs f)) ++ "return $ toDyn "
                 ++ "((" ++ (bodyDef f) ++ ") :: " ++ (funType f) ++ "))"

-- Генерирует набор функций по прототипам и структурам описывающим их. Код не красив. Стоит переписать.
genFunctions :: [Prototype] -> [Function] -> [String]
genFunctions _ [] = []
genFunctions ps funs@ (f:fs) =
  let name = funName f
      (fdefs, others) = partition (((==) name) . funName) funs
  in if name `notElem` registredNames then 
       (genFun ps fdefs) : (genFunctions ps others)
     else
       genFunctions ps others
   
   
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

При создании прототипа функции
>name '(' arg1name ',' arg2Name ',' argNName ')' ';'
Определяются имена динамических переменных. Их можно использовать в
коде блока when или в теле функции.  При использовании имен
динамических переменных в теле функции или в блоке "when { … }" имена
переменных определенные при реализации функции перекрывают имена из
прототипа. В блоке "when ( … )" имена динамических переменных не
скрываются. При использовании прототипов следует быть осторожным и
избегать случаев перестановки имен в прототипе и п реализации. Так в
примере
>name(a,b);
>Type name (Type1 b, Type2 a) { … }
При извлечении переменных мы получим следующий код
>fromDynamic a >>= \b -> fromDynamic b -> \a -> …
Что, очевидно, не будет работать, поскольку "fromDynamic b" пытается
извлечь что-то из переменной имеющей тип отличный от Dynamic'а.

Код в блоке "when" получает аргументы реальных типов и должен
возвращать "Bool". Если вы хотите использовать вместо этого
динамическую функцию, которая возвращает завернутый в "Dynamic" тип
"Bool", то вам необходимо использовать круглые скобки, вместо
фигурных, и использовать аргументы из прототипа функции.

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