module Task5Test where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Block2.Task2

specTests :: Spec
specTests = do
  describe "moving" $ do
    it "moving 4 [1, 5, 3, 8, 7, 9, 6]" $
      ([1.0, 3.0, 3.0, 4.25, 5.75, 6.75, 7.5])  `shouldBe` (moving 4 [1, 5, 3, 8, 7, 9, 6])
    it "[1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5]" $
      ([1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5])  `shouldBe` (moving 2 [1, 5, 3, 8, 7, 9, 6])



testsMoving :: IO TestTree
testsMoving = testSpec "All Tests" specTests

