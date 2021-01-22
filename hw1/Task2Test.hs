module Task2Test (testsTree) where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Data.Functor.Identity (Identity (..))

import Block1.Task2 (Tree (..))
import Data.List(sort)

fromListBalanced :: forall a. Ord a => [a] -> Tree a
fromListBalanced vs =
  let
    buildTree :: [a] -> Int -> Tree a
    buildTree [v] 1 = Leaf v
    buildTree v n =
      let
        nl :: Int
        nl = n `div` 2

        vSplit :: ([a], [a])
        vSplit = splitAt nl v
      in
        Branch (buildTree (fst vSplit) nl) (buildTree (snd vSplit) (n - nl))
  in
    buildTree (sort vs) (length vs)

-- | Returns test tree for current test group
getTestTree :: IO TestTree
getTestTree = testSpec "Tree" spec

smallList :: [Int]
smallList = [1, 2.. 8]

balancedTreeSmall :: Tree Int
balancedTreeSmall = fromListBalanced smallList

bigList :: [Int]
bigList = [1, 2.. 128]

balancedTreeBig :: Tree Int
balancedTreeBig = fromListBalanced bigList

mulMorphism :: Int -> Int
mulMorphism = (* 10)

sumMorphism :: Int -> Int
sumMorphism = (+ 5)

spec :: Spec
spec = do
  describe "Test Functor instance" $ do
    it "Identity law (small)" $
      fmap id balancedTreeSmall `shouldBe` balancedTreeSmall
    it "Identity law (big)" $
      fmap id balancedTreeBig `shouldBe` balancedTreeBig
    it "Composition law (small)" $
      fmap (mulMorphism . sumMorphism) balancedTreeSmall
        `shouldBe` (fmap mulMorphism . fmap sumMorphism) balancedTreeSmall
    it "Composition law (big)" $
      fmap (mulMorphism . sumMorphism) balancedTreeBig
        `shouldBe` (fmap mulMorphism . fmap sumMorphism) balancedTreeBig

  describe "Test Applicative instance" $ do
    it "Identity law (small)" $
      pure id <*> balancedTreeSmall `shouldBe` balancedTreeSmall
    it "Identity law (big)" $
      pure id <*> balancedTreeBig `shouldBe` balancedTreeBig
    it "Homomorphism law" $ do
      let f = mulMorphism :: Int -> Int
      let x = 10 :: Int
      (pure f <*> pure x :: Tree Int) `shouldBe` pure (f x)
    it "Interchange law" $ do
      let u = pure sumMorphism :: Tree (Int -> Int)
      let y = 10 :: Int
      u <*> pure y `shouldBe` pure (\f -> f y) <*> u
    it "Composition law" $ do
      let u = pure sumMorphism :: Tree (Int -> Int)
      let v = pure mulMorphism :: Tree (Int -> Int)
      let w = pure 10 :: Tree Int
      pure (.) <*> u <*> v <*> w `shouldBe` u <*> (v <*> w)

  describe "Test Foldable instance" $ do
    it "Tree sum (small)" $
      foldr (+) 0 balancedTreeSmall `shouldBe` sum smallList
    it "Tree sum (big)" $
      foldr (+) 0 balancedTreeBig `shouldBe` sum bigList
    it "Tree fold monoid" $
      foldMap show balancedTreeBig `shouldBe` foldMap show bigList

  describe "Test Traversable instance" $ do
    it "Identity law" $
      traverse Identity balancedTreeBig `shouldBe` Identity balancedTreeBig

  describe "Test fromListBalanced" $ do
    it "Small" $
      fromListBalanced smallList `shouldBe`
        Branch
          (Branch (Branch (Leaf 1) (Leaf 2)) (Branch (Leaf 3) (Leaf 4)))
          (Branch (Branch (Leaf 5) (Leaf 6)) (Branch (Leaf 7) (Leaf 8)))

testsTree :: IO TestTree
testsTree = testSpec "All Tests" spec
