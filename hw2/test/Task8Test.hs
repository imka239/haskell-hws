module Task8Test where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Block3.Task4
import Block3.Task1Task2

specTests :: Spec
specTests = do
  describe "listlistParser" $ do
     it "0" $
       runParser listlistParser " 0 "  `shouldBe` (Just ([[]], ""))
     it "1, 1" $
      runParser listlistParser "1, 1"  `shouldBe` (Just ([[1]], ""))
     it "3, 1, 5, 9" $
       runParser listlistParser "3,1, 5     , 9        "  `shouldBe` (Just ([ [1, 5, 9] ], ""))
     it "1, 2, 4, 2, 3, 5, 9, 0" $
       runParser listlistParser "1, 2, 4, 2, 3, 5, 9, 0"  `shouldBe` (Just ([ [2], [2, 3, 5, 9], []], ""))
     it "3, -6, +3, 1" $
       runParser listlistParser "3, -6,  +3,     1    "  `shouldBe` (Just ([ [-6, 3, 1] ], ""))
     it "1, 3,-5,-7, 2 " $
       runParser listlistParser "-1, 3,5,-7, 2"  `shouldBe` Nothing
     it "empty" $
       runParser listlistParser ""  `shouldBe` Nothing
     it " , , " $
       runParser listlistParser "  , , "  `shouldBe` Nothing
testsListList :: IO TestTree
testsListList = testSpec "All Tests" specTests

