module Olsgradient where

import Data.List

default (Double)

addIntercept :: [[Double]] -> [[Double]]
addIntercept = map (\x -> 1.0:x)

predict :: [[Double]] -> [Double] -> [Double]
predict observations weights =
  let mult = map (\x -> zipWith (*) x weights) observations
  in map sum mult

subtractMaker :: Double ->  [Double] -> [Double] -> Double
subtractMaker learnRate costs featureList =
  let costFeatureMult = zipWith (*) costs featureList
  in learnRate * sum costFeatureMult

gradientStep :: Double -> [Double] -> [Double] -> [[Double]] -> [Double]
gradientStep learnRate labels weights observations =
  let preds = predict observations weights
      costs = zipWith (-) preds labels
      featureMatrix = transpose observations
      subtractors = map (subtractMaker learnRate costs) featureMatrix
  in zipWith (-) weights subtractors

innerTrainOLS :: [[Double]] -> [Double] -> [Double] -> Double -> Double -> Double -> Double -> [Double]
innerTrainOLS observations labels weights learnRate threshold maxIter numIter 
  | numIter > maxIter = weights
  | sse < threshold = weights
  | otherwise = innerTrainOLS observations labels newWeights learnRate threshold maxIter (numIter + 1)
  where
    preds = predict observations weights
    sse = sum $ map (**2.0) (zipWith (-) labels preds)
    newWeights = gradientStep learnRate labels weights observations

trainOLS :: [[Double]] -> [Double] -> Double -> Double -> Double -> [Double]
trainOLS observations labels learnRate threshold maxIter =
  let obvs = addIntercept observations
      numFeats = length $ head obvs
      initweights = replicate numFeats 1
  in innerTrainOLS obvs labels initweights learnRate threshold maxIter 0

mean :: [Double] -> Double
mean lst = sum lst / fromIntegral (length lst)

standardDeviation :: [Double] -> Double
standardDeviation lst =
  let m = mean lst
      n = length lst
      squaredErrors = map (\x -> (x - m) ** 2.0) lst
  in sqrt (sum squaredErrors / fromIntegral n)

scale :: [Double] -> [Double]
scale lst =
  let m = mean lst
      stdev = standardDeviation lst
  in map (\x -> (x - m) / stdev) lst


testFeatures = transpose (map scale [[2, 8, 11, 10, 8, 4, 2, 2, 9, 8, 4, 11, 12, 2, 4, 4, 20, 1, 10, 15, 15, 16, 17, 6, 5]
                         , [50, 110, 120, 550, 295, 200, 375, 52, 100, 300, 412, 400, 500, 360, 205, 400, 600, 585, 540, 250, 290, 510, 590, 100, 400]])

testLabels = [0.95, 24.45, 31.75, 35.00, 25.02, 16.86, 14.38, 9.60, 24.35, 27.50, 17.08, 37.00, 41.95, 11.66
             , 21.65, 17.89, 69.00, 10.30, 34.93, 46.59, 44.88, 54.12, 56.63, 22.13, 21.15]
 
