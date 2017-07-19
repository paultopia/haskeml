import Text.ParserCombinators.Parsec
import Data.CSV
import Data.Char
import Math.LinearAlgebra.Sparse.Matrix
import Math.LinearAlgebra.Sparse.Vector

parseFloats :: [String] -> [Double]
parseFloats = map (fromIntegral . read)

proba :: [Double] -> Double
proba column = sum column / fromIntegral (length column)

smoothedProba :: [Double] -> Double
smoothedProba column = ((sum column) + 1.0) / fromIntegral ((length column) + 1)

comparator :: Double -> Double -> Int
comparator falseprob trueprob
  | falseprob >= trueprob = 0
  | trueprob > falseprob = 1

matcher :: Int -> Int -> Int
matcher label prediction
  | label == prediction = 1
  | label /= prediction = 0

-- look at all the imperative code in haskell!  that's what you get for making me put io in a monad.

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
         let probsTrue = map smoothedProba (fillMx (trans (sparseMx trueRows)))
         let probsFalse = map smoothedProba (fillMx (trans (sparseMx falseRows)))
         let logtrueF = log probY : (map log probsTrue)
         let logfalseF = log (1.0 - probY) : (map log probsFalse)
         let logtrue = sparseList logtrueF
         let logfalse = sparseList logfalseF
         -- at this point the "model" is "trained" and applying to new data is just a dot product
         -- since we don't have any new data, and I couldn't be bothered to mess around with a
         -- train/test split, let's just predict our training data like a lazy person.
         let multiplier = sparseMx (map (\x -> 1.0:x) features)
         let predictionsTrue = fillVec (mulMV multiplier logtrue)
         let predictionsFalse = fillVec (mulMV multiplier logfalse)
         let result = zipWith comparator predictionsFalse predictionsTrue
         let correctPredictions = sum (zipWith matcher (map truncate labels) result)
         print correctPredictions

