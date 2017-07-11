module OLSGradientSpec where
import Test.Hspec
import Olsgradient

main :: IO ()
main = hspec $ do
  describe "Batch Gradient Descent" $ do
    it "Correctly computes weights for batch gradient descent OLS" $ do
      (trainOLS testFeatures testLabels 0.001 0.01 10000) `shouldBe` [28.672799999999935,14.748771525823136,2.6956202549433357]
