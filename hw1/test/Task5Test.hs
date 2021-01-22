module Task5Test where

import Test.Tasty.Hedgehog (testProperty)
import Hedgehog
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Block2.Task2


import qualified Data.List as List
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.List.NonEmpty (NonEmpty ((:|)))

genString :: Gen [Char]
genString = Gen.list(Range.linear 1 10000) Gen.ascii

genChar :: Gen Char
genChar = Gen.ascii

composeProp :: Property
composeProp = property $ do
  str <- forAll genString
  splitter <- forAll genChar
  (id str) === (joinWith splitter (splitOn splitter str))



specTests :: Spec
specTests = do
  describe "Split" $ do
    it "/path/to/file" $
      ("path" :| ["to", "file"])  `shouldBe` (splitOn '/' "path/to/file")
    it "; x;;y;z" $
      ("asd" :| ["asd"])  `shouldBe` (splitOn 'x' "asdxasd")
    it "x Xxx" $
      ("X" :| ["", ""])  `shouldBe` (splitOn 'x' "Xxx")
    it "y yyy" $
      ("" :| ["", "", ""])  `shouldBe` (splitOn 'y' "yyy")
    it "1 [1, 2, 1, 3, 4]" $
      ([] :| [[2], [3, 4]])  `shouldBe` (splitOn 1 [1, 2, 1, 3, 4])
    it "2 [1, 2, 2, 1]" $
      ([1] :| [[], [1]])  `shouldBe` (splitOn 2 [1, 2, 2, 1])
  describe "joinWith" $ do
    it "/path/to/file" $
      ("path/to/file")  `shouldBe` (joinWith '/' ("path" :| ["to", "file"]))
    it "x asdxasd" $
      ("asdxasd")  `shouldBe` (joinWith 'x' ("asd" :| ["asd"]))
    it "x Xxx" $
      ("Xxx")  `shouldBe` (joinWith 'x' ("X" :| ["", ""]))
    it "y yyy" $
      ("yyy")  `shouldBe` (joinWith 'y'  ("" :| ["", "", ""]))
    it "1 [1, 2, 1, 3, 4]" $
      ([1, 2, 1, 3, 4])  `shouldBe` (joinWith 1 ([] :| [[2], [3, 4]]))
    it "2 [1, 2, 2, 1]" $
      ([1, 2, 2, 1])  `shouldBe` (joinWith 2 ([1] :| [[], [1]]))


splitJoinTests :: IO TestTree
splitJoinTests = testSpec "All Tests" specTests

propSplitJoin :: IO TestTree
propSplitJoin = return $
  testGroup "All tests"
  [ testProperty "splitJoin" composeProp]