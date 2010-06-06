import DynamicFace
import qualified Simple as S
import qualified SimpleTemplate as ST
import Monad

main = repl $ SPD [] [] []
                  [("first", (S.symbolTable, S.readTable, S.showTable)),
                   ("second", (ST.symbolTable, ST.readTable, ST.showTable))]
                  
