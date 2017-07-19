import Text.ParserCombinators.Parsec
import Data.CSV
main :: IO ()
main =
  do result <- parseFromFile csvFile "spam_binarized.csv"
     case result of
       Left err -> print err
       Right xs -> do
         let cleanRandomColumn = map tail xs
         let featuresNames = tail (head cleanRandomColumn)
         let datagrid = tail cleanRandomColumn
         let labels =  map head datagrid
         let features = map tail datagrid
         print (length featuresNames)
         print (length labels)
         print (length (head features))
