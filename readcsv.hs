import Text.ParserCombinators.Parsec
import Data.CSV
import Data.Char
import Math.LinearAlgebra.Sparse.Matrix
import Math.LinearAlgebra.Sparse.Vector

parseFloats :: [String] -> [Double]
parseFloats = map (fromIntegral . read)

proba :: [Double] -> Double
proba column = sum column / fromIntegral (length column)

main :: IO ()
main =
  do result <- parseFromFile csvFile "spam_binarized.csv"
     case result of
       Left err -> print err
       Right xs -> do
         let cleanRandomColumn = map tail xs
         let featuresNames = tail (head cleanRandomColumn)
         let datagrid = map parseFloats (tail cleanRandomColumn)
         let labels =  map head datagrid
         let features = map tail datagrid
         let probY = proba labels
         let trueRows = map tail (filter (\x -> head x == 1.0) datagrid)
         let falseRows = map tail (filter (\x -> head x == 0.0) datagrid)
         let probsTrue = map proba (fillMx (trans (sparseMx trueRows)))
         let probsFalse = map proba (fillMx (trans (sparseMx falseRows)))
         let logtrueF = log probY : (map log probsTrue)
         let logfalseF = log (1.0 - probY) : (map log probsFalse)
         writeFile "logprobstrue.txt" (show logtrueF)
         writeFile "logprobsfalse.txt" (show logfalseF)
         --let logtrue = sparseList logtrueF
         --let logfalse = sparseList logfalseF
         --let multiplier = sparseMx (map (\x -> 1.0:x) features)
         --let predictionsTrue = fillVec (mulMV multiplier logtrue)
         --let predictionsFalse = fillVec (mulMV multiplier logfalse)

