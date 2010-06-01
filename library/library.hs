module DynamicFace (
  module Data.Dynamic,
  -- DFunction -> DFunction -> DFunction
  dplus,                        -- Join two dynamic functions

  -- [(String, ([Dynamic] -> Maybe Dynamic))] -> [(String, ([Dynamic] -> Maybe Dynamic))] -> [(String, ([Dynamic] -> Maybe Dynamic))]
  stplus,                       -- Join two symbol table

  -- [(String, (String -> Dynamic))] -> [(String, (String -> Dynamic))] -> [(String, (String -> Dynamic))]
  rtplus,                       -- Join two read table
  
  -- [(TypeRep, (Dynamic -> Maybe String))] -> [(TypeRep, (Dynamic -> Maybe String))] -> [(TypeRep, (Dynamic -> Maybe String))]
  shtplus,                      -- Join two show table
  
  -- [(String, ([Dynamic] -> Maybe Dynamic))] -> String -> Maybe ([Dynamic] -> Maybe Dynamic)
  getFun,                       -- Get function from symbol table by name
  
  -- [(String, (String -> Dynamic))] -> String -> Maybe (String -> Dynamic)
  getRead, -- Get read function from read table by type name

  -- [(TypeRep, (Dynamic -> Maybe String))] -> Dynamic -> Maybe String
  getShow,                      -- Get String representation of variable from show table

  -- String -> Maybe (String, [(String, String)])
  parseString,                  -- Get (functionName, [(ArgRepr, ArgType)]) representation from string
  
  -- [(String, (String -> Dynamic))] -> (String, String) -> Maybe Dynamic
  argToDyn,                     -- Get dynamic argument from read table by (ArgRepr, ArgType) tuple
  
  -- [(String, ([Dynamic] -> Maybe Dynamic))] -- symbolList
  -- -> [(String, (String -> Dynamic))] -- typeList
  -- -> (String, [(String, String)]) -- (fun, argList)
  -- -> Maybe Dynamic
  eval,                          -- Get result of eval of (functionName, [(ArgRepr, ArgType)]) by symbol table and read table
  repl,
  ProgramData (PD, SPD),
  readInput,
  symbolTable,
  readTable,
  showTable,
  importModules
  ) where

import Monad
import Data.List
import Data.Dynamic
import Data.Typeable
import Text.ParserCombinators.Parsec

type DFunction = [Dynamic] -> Maybe Dynamic

-- Собирает две динамические функции в одну
dplus :: DFunction -> DFunction -> DFunction
dplus f1 f2 as = f1 as `mplus` f2 as

type SymbolTable = [(String, DFunction)]

-- Объединяет два списка символов
stplus :: SymbolTable -> SymbolTable -> SymbolTable
stplus as bs = map stplus' ns
  where 
    ns = nub $ map fst (as ++ bs)
    stplus' n = case (lookup n as, lookup n bs) of
      (Just f1, Nothing) -> (n, f1)
      (Nothing, Just f2) -> (n, f2)
      (Just f1, Just f2) -> (n, f1 `dplus` f2)
          
type ReadTable = [(String, (String -> Dynamic))]                            
                            
-- Объединяет два списка типов
rtplus :: ReadTable -> ReadTable -> ReadTable
rtplus = (++)

type ShowTable = [(TypeRep, (Dynamic -> Maybe String))]

-- Объединяет два списка типов выводимых
shtplus :: ShowTable -> ShowTable -> ShowTable
shtplus = (++)

-- По имени функции и таблице символов возвращает функцию
getFun :: SymbolTable -> String -> Maybe ([Dynamic] -> Maybe Dynamic)
getFun = flip lookup

getShow :: ShowTable -> Dynamic -> Maybe String
getShow sl a = case lookup (dynTypeRep a) sl of
  Just f -> f a
  Nothing -> Nothing

-- По имени типа и списку типов возвращает функцию чтения
getRead :: ReadTable -> String -> Maybe (String -> Dynamic)
getRead = flip lookup

---------------------------- Скопировано из ../process.hs явно стоит оформить как-то отдельной библиотекой -----------

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

--
word :: Parser String
word = many1 (alphaNum <|> oneOf "_'." <?> "") <?> "word"

-- пробельные символы, которые как правило будут игнорироваться
sn = (space <|> newline <|> tab)

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

------ Теперь разбор формата команды

number :: Parser Char
number = char '0' <|> char '1' <|> char '2' <|> char '3' <|> char '4'
         <|> char '5' <|> char '6' <|> char '7' <|> char '8' <|> char '9'
         <|> char '.'
         
-- Разбирает аргумент в виде (arg : Type)
argument :: Parser (String, String) -- (Representation, Type)
argument = do{ s <- try $ many1 number
             ; if isInt s then
                 return (s, "Integer")
               else
                 return (s, "Double")
             }
           -- <|> [ array of number ] <|> [[ matrix of number ]] <|> (re, im) complex
           <|> do{ char '('
                 ; r <- fmap trim (beforeWord "::")
                 ; t <- fmap trim (beforeWord ")")
                 ; return (r, t)
                 }

isInt :: String -> Bool
isInt = notElem '.'
           
-- Парсит список аргументов
argList :: Parser [(String, String)]
argList = do{ a <- argument
            ; many sn
            ; do{ as <- argList
                ; return (a:as)
                }
              <|> return [a]
            }
          
function :: Parser (String, [(String, String)]) -- (funname, arglist)
function = do{ many sn
             ; n <- word
             ; many sn
             ; as <- argList
             ; return (n, as)
             }

-- Минимальное определение языка закончено

run :: Parser a -> String -> Maybe a
run p input = 
  case (parse p "" input) of
    Left err -> Nothing
    Right x  -> Just x


parseString :: String -> Maybe (String, [(String, String)])
parseString = run function

argToDyn :: ReadTable -> (String, String) -> Maybe Dynamic
argToDyn tl (r, t) = ap (getRead tl t) (Just r)

-- По списку разобранной функции, списку символов
-- и списку типов вычисляет результат
eval :: SymbolTable -> ReadTable
        -> (String, [(String, String)]) -- (fun, argList)
        -> Maybe Dynamic
eval sl tl (f, al) = do
  fun  <- getFun sl f
  args <- sequence $ map (argToDyn tl) al
  fun args

-- Надо написать функцию реализующую цикл чтения, выполнения, вывода.

-- replace old substring with new string in str
replaceStr :: String -> String -> String -> String
replaceStr "" old new  = ""
replaceStr str "" _    = str
replaceStr str old new = loop str
  where
    n = length old
    loop "" = ""
    loop str =
      let (prefix, rest) = splitAt n str
      in
        if old == prefix                -- found an occurrence?
        then new ++ loop rest           -- yes: replace it

        else head str : loop (tail str) -- no: keep looking


data ProgramData =
  PD { readInput :: IO String,
       symbolTable :: SymbolTable,
       readTable :: ReadTable,
       showTable :: ShowTable,
       importModules :: [(String, (SymbolTable, ReadTable, ShowTable))]
     }
  |
  SPD { symbolTable :: SymbolTable,
        readTable :: ReadTable,
        showTable :: ShowTable,
        importModules :: [(String, (SymbolTable, ReadTable, ShowTable))]
      }
                   

repl :: ProgramData -> IO ()
repl (SPD st rt sht is) = repl (PD getLine st rt sht is)
repl pd = readInput pd >>= \s1 ->
  case words s1 of
    ("quit"):_ -> return ()
    ("import"):name:_ -> case lookup name (importModules pd) of
      Just (st, rt, sht) -> do
        putStrLn "imported"
        repl pd {
          symbolTable = symbolTable pd `stplus` st,
          readTable   = readTable pd `rtplus` rt,
          showTable   = showTable pd `shtplus` sht
          }
      Nothing -> do
        putStrLn "module not found"
        repl pd
    l -> let s' = do
               cmd <- parseString (unwords l)
               res <- eval (symbolTable pd)
                           (readTable pd) cmd
               getShow (showTable pd) res
         in do
          case s' of
            Just s -> putStrLn s
            Nothing -> putStrLn "can't parse input"
          repl pd
