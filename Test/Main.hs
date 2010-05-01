import Data.Dynamic
import Dynamic
import qualified Integer
import qualified Complex

main = do
  let printRes = putStrLn . show . myShow
  printRes $ plus [(toDyn (Integer.MyInt 2)), (toDyn (Integer.MyInt 4))]
  printRes $ sub [(toDyn (Integer.MyInt 2)), (toDyn (Integer.MyInt 4))]
  printRes $ plus [(toDyn (Complex.Complex (2, 5))), (toDyn (Complex.Complex (1, 4)))]
  printRes $ sub [(toDyn (Complex.Complex (2, 1))), (toDyn (Complex.Complex (0, 4)))]
  printRes $ mul [(toDyn (Integer.MyInt 2)), (toDyn (Integer.MyInt 4))]
  printRes $ mul [(toDyn (Integer.MyInt 9)), (toDyn (Integer.MyInt 4))]
  
  
myShow :: Maybe Dynamic -> Maybe String
myShow Nothing = Nothing
myShow (Just s) = myShow' s
    where myShow' s
              | dynTypeRep s == typeOf (undefined::Integer.MyInt) =
                  ((fromDynamic s)::(Maybe Integer.MyInt)) >>= return . show
              | dynTypeRep s == typeOf (undefined::Complex.Complex) =
                  ((fromDynamic s)::(Maybe Complex.Complex)) >>= return . show
              | otherwise = Nothing
