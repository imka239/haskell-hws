module Task1Test where

import Block1.Task1

import Hedgehog
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import qualified Data.List as List
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genInteger :: Gen Int
genInteger = Gen.integral(Range.linear 1 1000)

genList :: Gen [Int]
genList = Gen.list(Range.linear 1 10000) genInteger

addWord :: Char -> Int -> String -> String
addWord joiner str xs = (show str) ++ [joiner] ++ xs

joinWith :: Char -> [Int] -> String
joinWith joiner masStr = init $ foldr (addWord joiner) "" masStr

randomEquation :: Property
randomEquation = property $ do
  li <- forAll genList
  (Just (sum li)) === (stringSum (joinWith ' ' li))

specTests :: Spec
specTests = do
  describe "stringSum" $ do
    it "" $
      (Just 0)  `shouldBe` (stringSum "")
    it "error" $
      (Nothing)  `shouldBe` (stringSum "ababva")
    it "1 3" $
      (Just 4)  `shouldBe` (stringSum "1 3")
    it "1 3 afasf 4 5" $
      (Nothing)  `shouldBe` (stringSum "1 3 adsf 4 5")
    it "1 323           4 5" $
      (Just 333)  `shouldBe` (stringSum "1 323           4 5")
    it "1 -323           4 5" $
      (Just (-313))  `shouldBe` (stringSum "1 -323           4 5")
    it "1 -323           4+5" $
      (Nothing)  `shouldBe` (stringSum "1 -323           4+5")


testsStringSum :: IO TestTree
testsStringSum = testSpec "All Tests" specTests

testsStringSumProp :: IO TestTree
testsStringSumProp = return $
  testGroup "Prop Op"
  [ testProperty "rand" randomEquation
  ]