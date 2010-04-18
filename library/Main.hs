import DynamicFace
import qualified Simple
import Monad

main = do
  l <- getLine
  if l == "quit" then putStrLn "quiting" else do
    cmd <- return $ parseString l
    res <- return $ cmd >>= \c -> eval c Simple.symbolTable Simple.readTable
    s <- return $ res >>= getShow Simple.showTable
    case s of
      Just s -> putStrLn s
      Nothing -> putStrLn "error"
    main