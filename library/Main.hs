import DynamicFace
import qualified Simple as S
import qualified SimpleTemplate as ST

main = repl $ defaultPD {
  importModules = [("first", (S.symbolTable, S.readTable, S.showTable)),
                   ("second", (ST.symbolTable, ST.readTable, ST.showTable))]
  }

