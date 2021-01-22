module Task7Test where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Block3.Task3
import Block3.Task1Task2

specTests1 :: Spec
specTests1 = do
  describe "brackets" $ do
    it "empty" $
      runParser bracketParser ""  `shouldBe` Just(Empty, "")
    it "()" $
      runParser bracketParser "()"  `shouldBe` Just(Construct (Empty) (Empty), "")
    it "())" $
      runParser bracketParser "())"  `shouldBe` Nothing
    it "(())()" $
        runParser bracketParser "(())()"  `shouldBe` Just (Construct (Construct Empty Empty) (Construct Empty Empty),"")




specTests2 :: Spec
specTests2 = do
  describe "ints" $ do
    it "empty" $
      (runParser intParser "")  `shouldBe` Nothing
    it "12" $
      (runParser intParser "12")  `shouldBe` Just (12, "")
    it "+123" $
      (runParser intParser "+123")  `shouldBe` Just (123, "")
    it "-12df12" $
      (runParser intParser "-12df12")  `shouldBe` Just (-12, "df12")

testsBrackets :: IO TestTree
testsBrackets = testSpec "All Tests" specTests1

testsInts :: IO TestTree
testsInts = testSpec "All Tests" specTests2

