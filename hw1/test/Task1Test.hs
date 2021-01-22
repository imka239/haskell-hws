module Task1Test where

import Block1.Task1

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

specTests :: Spec
specTests = do
  describe "nextDay" $ do
    it "Monday" $
      (Tuesday)  `shouldBe` (nextDay Monday)
    it "Tuesday" $
      (Wednesday)  `shouldBe` (nextDay Tuesday)
    it "Wednesday" $
      (Thursday)  `shouldBe` (nextDay Wednesday)
    it "Thursday" $
      (Friday)  `shouldBe` (nextDay Thursday)
    it "Friday" $
      (Saturday)  `shouldBe` (nextDay Friday)
    it "Saturday" $
      (Sunday)  `shouldBe` (nextDay Saturday)
    it "Sunday" $
      (Monday)  `shouldBe` (nextDay Sunday)
  describe "afterDays" $ do
    it "Monday 1" $
      (Tuesday)  `shouldBe` (afterDays Monday 1)
    it "Tuesday 1" $
      (Wednesday)  `shouldBe` (afterDays Tuesday 1)
    it "Wednesday 1" $
      (Thursday)  `shouldBe` (afterDays Wednesday 1)
    it "Thursday 1" $
      (Friday)  `shouldBe` (afterDays Thursday 1)
    it "Friday 1" $
      (Saturday)  `shouldBe` (afterDays Friday 1)
    it "Saturday 1" $
      (Sunday)  `shouldBe` (afterDays Saturday 1)
    it "Sunday 1" $
      (Monday)  `shouldBe` (afterDays Sunday 1)
    it "Monday 10" $
      (Thursday)  `shouldBe` (afterDays Monday 10)
    it "Tuesday 100" $
      (Thursday)  `shouldBe` (afterDays Tuesday 100)
    it "Wednesday 1000" $
      (Tuesday)  `shouldBe` (afterDays Wednesday 1000)
    it "Thursday 3" $
      (Sunday)  `shouldBe` (afterDays Thursday 3)
    it "Friday 7" $
      (Friday)  `shouldBe` (afterDays Friday 7)
    it "Saturday 11" $
      (Wednesday)  `shouldBe` (afterDays Saturday 11)
    it "Sunday 0" $
      (Sunday)  `shouldBe` (afterDays Sunday 0)
  describe "isWeekend" $ do
    it "Monday" $
      (False)  `shouldBe` (isWeekend Monday)
    it "Tuesday" $
      (False)  `shouldBe` (isWeekend Tuesday)
    it "Wednesday" $
      (False)  `shouldBe` (isWeekend Wednesday)
    it "Thursday" $
      (False)  `shouldBe` (isWeekend Thursday)
    it "Friday" $
      (False)  `shouldBe` (isWeekend Friday)
    it "Saturday" $
      (True)  `shouldBe` (isWeekend Saturday)
    it "Sunday" $
      (True)  `shouldBe` (isWeekend Sunday)
  describe "daysToParty" $ do
    it "Monday" $
      (4)  `shouldBe` (daysToParty Monday)
    it "Tuesday" $
      (3)  `shouldBe` (daysToParty Tuesday)
    it "Wednesday" $
      (2)  `shouldBe` (daysToParty Wednesday)
    it "Thursday" $
      (1)  `shouldBe` (daysToParty Thursday)
    it "Friday" $
      (0)  `shouldBe` (daysToParty Friday)
    it "Saturday" $
      (6)  `shouldBe` (daysToParty Saturday)
    it "Sunday" $
      (5)  `shouldBe` (daysToParty Sunday)


testsDayOfWeek :: IO TestTree
testsDayOfWeek = testSpec "All Tests" specTests
