module Task4Test where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Block1.Task2
import Block1.Task3
import Data.List.NonEmpty (NonEmpty ((:|)), map)

data Mul = Mul (Int) deriving (Eq, Show)



instance Semigroup Mul where
  (<>) (Mul 0) (Mul a) = Mul 0
  (<>) (Mul a) (Mul 0) = Mul 0
  (<>) (Mul a) (Mul b) = Mul (a * b)

instance Monoid (Mul) where
  mempty :: Mul
  mempty = (Mul 1)

fiveNodes = (Node (3 :| []) (Node (2 :| []) (Node (1 :| []) Leaf Leaf) Leaf) (Node (4 :| []) Leaf (Node (5 :| [])
             Leaf Leaf)))

specTests :: Spec
specTests = do
  describe "FoldrFoldMap" $ do
    it "[2, 5, 10, 3, 6, 1]" $
      ([1, 2, 3, 5, 6, 10])  `shouldBe` ((toList . fromList) [2, 5, 10, 3, 6, 1])
    it "[1, 1, 4, 4, 2, 2]" $
      ([1, 1, 2, 2, 4, 4])  `shouldBe` ((toList . fromList) [1, 1, 4, 4, 2, 2])
    it "toList from fiveNodes" $
      ([1, 2, 3, 4, 5])  `shouldBe` (toList fiveNodes)
    it "foldMap for function" $
      (Mul(120))  `shouldBe` (foldMap Mul(fiveNodes))




testsTreeFoldable :: IO TestTree
testsTreeFoldable = testSpec "All Tests" specTests

