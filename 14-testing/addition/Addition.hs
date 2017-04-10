module Addition where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $
  describe "Addition" $ do
    it "1 + 1 is greater than 1"
      (1 + 1 > (1::Int) `shouldBe` True)

    it "works for negatives" $
      ((-3) + (8::Int)) `shouldBe` 5

    it "x + 1 is always greater than x" $
      property $ \x -> x + 1 > (x::Int)