module Task7Test where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Block3.Task2


specTests :: Spec
specTests = do
  describe "ThisOrThat" $ do
    it "(This 1) (That 3) (That 2)" $
      (True)  `shouldBe` ((((This 1) <> (That 3)) <> (That 2)) == ((This 1) <> ((That 3) <> (That 2))))
    it "(This 10) (This 3) (That 2)" $
      (True)  `shouldBe` ((((This 10) <> (This 3)) <> (That 2)) == ((This 10) <> ((This 3) <> (That 2))))
  describe "NonEmpty" $ do
    it "[2:|[3, 4]], [1:|[2, 4]], [10:|[]]" $
      (True)  `shouldBe` (([2:|[3, 4]] <> [1:|[2, 4]] <> [10:|[]]) == ([2:|[3, 4]] <> ([1:|[2, 4]] <> [10:|[]])))
    it "[2:|[]], [1:|[]], [10:|[]]" $
      (True)  `shouldBe` (([2:|[]] <> [1:|[]] <> [10:|[]]) == ([2:|[]] <> ([1:|[]] <> [10:|[]])))
  describe "Name" $ do
    it "file to path" $
      (True)  `shouldBe` ((Name "file" <> Name "to" <> Name "path") == (Name "file" <> (Name "to" <> Name "path")))
    it "some kind" $
      (True)  `shouldBe` ((Name "some" <> Name "kind" <> mempty) == (Name "some" <> (Name "kind" <> mempty)))
  describe "Endo" $ do
    it "(Endo (/ 2) (* 2) id 2)" $
      (True)  `shouldBe` ((getEndo ((Endo (/ 2)) <> (Endo (* 2)) <> (Endo id)) $ 2) == (getEndo((Endo (/ 2)) <> ((Endo (* 2)) <> (Endo id))) $ 2))
    it "(Endo (++ [1]) (++ [3]) (++ [4]) [5]" $
      (True)  `shouldBe` ((getEndo((Endo (++ [1])) <> (Endo (++ [3])) <> (Endo (++ [4]))) $ [5]) == (getEndo((Endo (++ [1])) <> ((Endo (++ [3])) <> (Endo (++ [4])))) $ [5]))


semiGroupTests :: IO TestTree
semiGroupTests = testSpec "All Tests" specTests
