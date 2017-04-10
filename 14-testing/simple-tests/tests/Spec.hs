module Main where

import Data.List (sort)
import Test.Hspec
import Test.QuickCheck

import Arithmetic

prop_halfDouble :: Property
prop_halfDouble =
  forAll (arbitrary :: Gen Double)
  (\x -> halfIdentity x == x)

prop_listOrdered :: Property
prop_listOrdered =
  forAll (arbitrary :: Gen [String])
  (listOrdered . sort)

prop_associativeAddition :: Property
prop_associativeAddition =
  forAll (arbitrary :: Gen Int)
  plusAssociative

prop_commutativeAddition :: Property
prop_commutativeAddition =
  forAll (arbitrary :: Gen Int)
  plusCommutative

prop_multiplicationAssociative :: Property
prop_multiplicationAssociative =
  forAll (arbitrary :: Gen Int)
  multiplicationAssociative

prop_multiplicationCommutative =
  forAll (arbitrary :: Gen Int)
  multiplicationCommutative

quotRemRelation x y =
  (quot x y) * y + (rem x y) == x

prop_quotRemRelation =
  forAll (arbitrary :: Gen Int)
  quotRemRelation

divModRelation x y =
  (div x y) * y + (mod x y) == x

prop_divModRelation =
  forAll (arbitrary :: Gen Int)
  divModRelation

main :: IO ()
main = hspec $
  describe "Using QuickCheck" $ do
    it "half" $
      property prop_halfDouble

    it "listOrdered" $
      property prop_listOrdered

    it "addition is associative" $
      property prop_associativeAddition

    it "addition is commutative" $
      property prop_commutativeAddition

    it "multiplcation is associative" $
      property prop_multiplicationAssociative

    it "multiplcation is commutative" $
      property prop_commutativeAddition

    it "quot rem laws" $
      property prop_quotRemRelation

    it "div mod laws" $
      property prop_divModRelation
