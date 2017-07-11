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
