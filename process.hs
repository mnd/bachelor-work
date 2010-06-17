import Data.Char
import Data.List
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Token

-----Helping Functions-------------------------
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

----------Grammar--------------------------------
word :: Parser String
word = many1 (alphaNum <|> oneOf "_'." <?> "") <?> "word"

-- space symbols
sn = (space <|> newline <|> tab)

-- world list with sn as delimeter
wordList :: Parser [String]
wordList = do{ s1 <- word
          ; many sn
          ; do { s2 <- wordList
               ; return (s1:s2)
               }
            <|> return [s1]
          }

-- read string before closing ". And we think that openinig " was early.
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

-- Read all text before string "s". Inside ( … ), [ … ], { … } and " … " blocks text does not seek
beforeWord :: String -> Parser String
beforeWord s = do { try (string s)
                  ; return ""
                  }
               <|> do { c <- anyChar
                      ; (case c of
                            -- '\'' -> do{ s0 <- beforeWord "'"
                            --          ; s0' <- beforeWord s
                            --          ; return ((c:s0) ++ ('\'':s0'))
                            --          }
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
          
-- "type haskell code ;" -> type haskell code
type1 :: Parser String
type1 = do{ try (string "type")
            ; s <- fmap trimTail (beforeWord ";")
            ; return ("type" ++ s)
            }

-- Read and return "import haskell code when"
module1 :: Parser String
module1 = do{ try (string "module")
            ; s <- beforeWord "where"
            ; return ("module" ++ s ++ "where")
            }

-- list of words separated by commas.
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
-- Parse "typedef name = [ types ]" block and return pair (name, [types])
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
-- Transform ["Maybe", "Bool", "name"] list in pair ("Maybe Bool", "name") 
typeNameParse :: [String] -> TypeName
typeNameParse x = (unwords (init x), last x)

-- Parse function arguments list. 
funArgs :: Parser [TypeName]
funArgs = do{ a <- fmap typeNameParse wordList
            ; do{ char ','
                ; many sn
                ; as <- funArgs
                ; return (a:as)
                }
              <|> return [a]
            }

-- parse function definition "Type name (types with args)"
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
-- Parse 'when' block
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

-- parse function body
bodyDefinition :: Parser String
bodyDefinition = do { char '{'
                    ; body <- fmap trim (beforeWord "}")
                    ; return body
                    }

type FunDef = (TypeName,[TypeName])
data Function = Function {
  funDef :: FunDef,
  whenDef :: When,
  bodyDef :: String
  }

  
-- Parse whole function: Several definitions with optional blocks 'when' and body of the function
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

data Program = Program String {- modules + imports -} String {- types -} [Typedef] [Function]

-- merge two tokens
merge :: Program -> Program -> Program
merge (Program s1 st1 t1 f1) (Program s2 st2 t2 f2) = (Program sr str tr fr)
  where
    sr  = s1 ++ ('\n':s2)
    str = st1 ++ ('\n':st2)
    tr  = t1 ++ t2
    fr  = f1 ++ f2   

-- Parse programm blocks. module, import, typedefs and functions
program :: Parser Program
program = do{ t <- ((module1      >>= \x -> return $ Program x  "" []  [])
                    <|> (import1  >>= \x -> return $ Program x  "" []  [])
                    <|> (typedef  >>= \x -> return $ Program "" "" [x] [])
                    <|> (type1    >>= \x -> return $ Program "" x  []  [])
                    <|> (function >>= \x -> return $ Program "" "" []  x))
            ; many sn
            ; do{ ts <- program
                ; return (merge t ts)
                }
              <|> return t
            }
          <|> return (Program "" "" [] [])

-- Root rule. Allow to write optional block in pure haskell before "\n%%", and only after this start to parse generic programm
code :: Parser Program
code = do{ many sn
         ; do { hc <- try(beforeWord "\n%%")
              ; many sn
              ; (Program s st t f) <- program
              ; return $ Program (hc ++ "\n\n" ++ s) st t f
              }
           <|> do { p <- program
                  ; return p
                  }
         }

--------------------- And of grammar. Processing -------------------------------------

-- Code generation
generate :: Program -> String
generate (Program s st t f) = s ++ "\n" ++ restrictedModules ++ "\n" ++ st ++ "\n" ++ (unlines $ genFunctions f')
                        ++ "\n" ++ (genSymbolTable f) ++ "\n" ++ (genRead fr) ++ "\n" ++ (genShow fs)
  where
    f1 = untypes t f
    (fr, f2) = partition (((==) "read") . funName) f1
    (fs, f') = partition (((==) "show") . funName) f2
    
-- Restricted modules
restrictedModules = unlines ["import Data.Typeable", "import Data.Dynamic", "import Data.Maybe", "import Monad"]

-- expand types defined by typedef
untypes :: [Typedef] -> [Function] -> [Function]
untypes ts fs = concatMap untypes' fs
  where
    -- (TypeName,[TypeName])
    untypes' func@ (Function ((nt, nn), as) w b) =
      case lookup nt ts of
        Nothing -> untypesArgs func
        Just types -> concatMap (\t -> untypesArgs (Function ((t, nn), as) w b)) types

    -- Change type of num'th argument function func to newType
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
                                                 

-- replace n'th of the list with t
changeN :: [a] -> a -> Int -> [a]
changeN ls t n | 0 <= n && n < length ls = (take n ls) ++ [t] ++ (drop (n+1) ls)
               | otherwise               = ls

-- Extract Data from Function
funTypeName :: Function -> (String, String)
funTypeName = fst . funDef
funName :: Function -> String
funName = snd . funTypeName
funType :: Function -> String
funType = fst . funTypeName

funTypesArgs :: Function -> [TypeName]
funTypesArgs = snd . funDef

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
                                              ++ " -> return (\"(\" ++ (" ++ b ++ ") ++ \" :: " ++ t ++ ")\"))))") tas bs
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

-- Extract args from Dynamic. Get list [(Type, Name)]
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

-- generate function defenitions from Function structs
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

--------------------- End of processing. Using: runFile code "dynamic.gen" >>= return . generate --------------------------------
--------------------------------------------- give to function "generate" output of parser "code" --------------------------------------

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

>'type' haskell type code SPACES ';'

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