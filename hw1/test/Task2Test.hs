module Task2Test where

import Control.Exception (evaluate)
import Hedgehog
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (Spec, anyErrorCall, describe, it, shouldBe, shouldThrow, testSpec)

import Block1.Task2
import qualified Data.List as List
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genInteger :: Gen Integer
genInteger = Gen.integral(Range.linear 1 1000)

twoRandomIntMul :: Property
twoRandomIntMul = property $ do
  fi <- forAll genInteger
  se <- forAll genInteger
  (fi * se) === (toIntegerNat (fromInteger fi) * (fromInteger se))

twoRandomIntAdd :: Property
twoRandomIntAdd = property $ do
  fi <- forAll genInteger
  se <- forAll genInteger
  (fi + se) === (toIntegerNat (fromInteger fi) + (fromInteger se))

twoRandomIntLess :: Property
twoRandomIntLess = property $ do
  fi <- forAll genInteger
  se <- forAll genInteger
  (fi < se) === (toIntegerNat (fromInteger fi) < (fromInteger se))

twoRandomIntDiv = property $ do
  fi <- forAll genInteger
  se <- forAll genInteger
  (div fi se) === (toIntegerNat $ divNat (fromInteger fi) (fromInteger se))

twoRandomIntMod = property $ do
  fi <- forAll genInteger
  se <- forAll genInteger
  (mod fi se) === (toIntegerNat $ modNat (fromInteger fi) (fromInteger se))


zero = Z
one = S zero
two = S one
three = S two
four = S three


specTests :: Spec
specTests = do
  describe "Add" $ do
    it "1 + 2" $
      (three)  `shouldBe` (one + two)
    it "0 + 0" $
      (zero)  `shouldBe` (zero + zero)
    it "2 + 2" $
      (four)  `shouldBe` (two + two)
    it "3 + 1" $
      (four)  `shouldBe` (three + one)
  describe "Mul" $ do
    it "1 * 2" $
      (two)  `shouldBe` (one * two)
    it "0 * 0" $
      (zero)  `shouldBe` (zero * zero)
    it "2 * 2" $
      (four)  `shouldBe` (two * two)
    it "3 * 1" $
      (three)  `shouldBe` (three * one)
  describe "Sub" $ do
    it "1 - 2" $
      (zero)  `shouldBe` (one - two)
    it "0 - 0" $
      (zero)  `shouldBe` (zero - zero)
    it "2 - 1" $
      (one)  `shouldBe` (two - one)
    it "3 - 1" $
      (two)  `shouldBe` (three - one)
  describe "fromInteger" $ do
    it "1" $
      (one)  `shouldBe` (fromInteger 1)
    it "0" $
      (zero)  `shouldBe` (fromInteger 0)
    it "2" $
      (four)  `shouldBe` (fromInteger 4)
    it "-1" $
      (one)  `shouldBe` (fromInteger (-1))
  describe "toInteger" $ do
    it "3" $
      (3)  `shouldBe` (toIntegerNat three)
    it "0" $
      (0)  `shouldBe` (toIntegerNat zero)
    it "4" $
      (4)  `shouldBe` (toIntegerNat four)
    it "1" $
      (1)  `shouldBe` (toIntegerNat one)
  describe "Eq" $ do
    it "0 == 0" $
      (True)  `shouldBe` (zero == zero)
    it "1 == 1" $
      (True)  `shouldBe` (one == one)
    it "4 == 2" $
      (False)  `shouldBe` (four == two)
    it "1 == 3" $
      (False)  `shouldBe` (one == three)
  describe "Compare" $ do
    it "0 < 0" $
      (False)  `shouldBe` (zero < zero)
    it "1 >= 1" $
      (True)  `shouldBe` (one >= one)
    it "4 > 2" $
      (True)  `shouldBe` (four > two)
    it "1 >= 3" $
      (False)  `shouldBe` (one >= three)
  describe "Even" $ do
    it "0" $
      (True)  `shouldBe` (isEven zero)
    it "1" $
      (False)  `shouldBe` (isEven one)
    it "3" $
      (False)  `shouldBe` (isEven three)
    it "4" $
      (True)  `shouldBe` (isEven four)
  describe "Div" $ do
    it "2 divNat 0" $
      evaluate (divNat two zero) `shouldThrow` anyErrorCall
    it "1 divNat 2" $
      (zero)  `shouldBe` (divNat one two)
    it "4 divNat 1" $
      (four)  `shouldBe` (divNat four one)
    it "3 divNat 2" $
      (one)  `shouldBe` (divNat three two)
  describe "Mod" $ do
    it "2 modNat 0" $
      evaluate (modNat two zero) `shouldThrow` anyErrorCall
    it "3 modNat 4" $
      (three)  `shouldBe` (modNat three four)
    it "4 modNat 3" $
      (one)  `shouldBe` (modNat four three)
    it "3 modNat 2" $
      (one)  `shouldBe` (modNat three two)

testsNat :: IO TestTree
testsNat = testSpec "All Tests" specTests

propNat :: IO TestTree
propNat = return $
  testGroup "Nat Op"
  [ testProperty "Mul" twoRandomIntMul,
    testProperty "Less" twoRandomIntLess,
    testProperty "Div" twoRandomIntDiv,
    testProperty "Mod" twoRandomIntMod
  ]