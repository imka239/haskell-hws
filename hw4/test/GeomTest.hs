module GeomTest where

import GeomTaskFast
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

p1 = Point 0 0

p2 = Point 1 0

p3 = Point 1 1

p4 = Point 0 1

specTests :: Spec
specTests = do
  describe "plus" $ do
    it "1 0 + 0 1" $
      (plus p2 p4) `shouldBe` (p3)
    it "1 1 + 2 2" $
      (plus p3 (Point 2 2)) `shouldBe` (Point 3 3)
    it "1 1 + 1 1" $
      (plus p3 p3) `shouldBe` (Point 2 2)
    it "1 1 + -1 -1" $
      (plus p3 (Point (-1) (-1))) `shouldBe` p1
  describe "minus" $ do
    it "1 0 - 0 1" $
      (minus p2 p4) `shouldBe` (Point 1 (-1))
    it "1 1 - 2 2" $
      (minus p3 (Point 2 2)) `shouldBe` (Point (-1) (-1))
    it "1 1 - 1 1" $
      (minus p3 p3) `shouldBe` (p1)
    it "1 1 - -1 -1" $
      (minus p3 (Point (-1) (-1))) `shouldBe` (Point 2 2)
  describe "perimeter" $ do
    it "square" $
      perimeter [p1, p2, p3, p4] `shouldBe` 4
    it "triangle" $
      perimeter [p1, p2, p3] `shouldBe` (2 + (sqrt 2))
  describe "double area" $ do
    it "square" $
      doubleArea [p1, p2, p3, p4] `shouldBe` 2
    it "triangle" $
      doubleArea [p1, p2, p3] `shouldBe` 1

geomTests :: IO TestTree
geomTests = testSpec "All Tests" specTests
