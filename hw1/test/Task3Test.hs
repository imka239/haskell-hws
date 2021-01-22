module Task3Test where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Block1.Task3
import Data.List.NonEmpty (NonEmpty ((:|)), map, toList)

empty = (Leaf)
oneNode = (Node (3 :| []) Leaf Leaf)
twoNodes = (Node (3 :| []) (Node (2 :| []) Leaf Leaf) Leaf)
twoNodesV2 =  (Node (2 :| []) (Node (1 :| []) Leaf Leaf) Leaf)
threeNodes = (Node (3 :| []) (Node (2 :| []) Leaf Leaf) (Node (4 :| []) Leaf Leaf))
threeNodesV2 =  (Node (4 :| []) (Node (2 :| []) (Node (1 :| []) Leaf Leaf) Leaf) Leaf)
fourNodes =  (Node (3 :| []) (Node (2 :| []) (Node (1 :| []) Leaf Leaf) Leaf) (Node (4 :| []) Leaf Leaf))
fourNodesV2 =  (Node (3 :| [3]) (Node (2 :| []) (Node (1 :| []) Leaf Leaf) Leaf) (Node (4 :| []) Leaf Leaf))
fiveNodes = (Node (3 :| []) (Node (2 :| []) (Node (1 :| []) Leaf Leaf) Leaf) (Node (4 :| []) Leaf (Node (5 :| [])
             Leaf Leaf)))

specTests :: Spec
specTests = do
  describe "isEmpty" $ do
    it "empty" $
      (True)  `shouldBe` (isEmpty empty)
    it "oneNode" $
      (False)  `shouldBe` (isEmpty oneNode)
    it "fourNodes" $
      (False)  `shouldBe` (isEmpty fourNodes)
    it "fiveNodes" $
      (False)  `shouldBe` (isEmpty fiveNodes)
  describe "Eq" $ do
    it "empty == oneNode" $
      (False)  `shouldBe` (empty == oneNode)
    it "fiveNodes == fiveNodes" $
      (True)  `shouldBe` (fiveNodes == fiveNodes)
    it "fourNodes == twoNodes" $
      (False)  `shouldBe` (fourNodes == twoNodes)
    it "threeNodes == oneNode" $
      (False)  `shouldBe` (threeNodes == oneNode)
  describe "Size" $ do
    it "empty" $
      (0)  `shouldBe` (size empty)
    it "fiveNodes" $
      (5)  `shouldBe` (size fiveNodes)
    it "fourNodes" $
      (4)  `shouldBe` (size fourNodes)
    it "oneNode" $
      (1)  `shouldBe` (size oneNode)
  describe "Find" $ do
    it "empty 1" $
      (False)  `shouldBe` (find empty 1)
    it "fiveNodes 2" $
      (True)  `shouldBe` (find fiveNodes 2)
    it "fourNodes 5" $
      (False)  `shouldBe` (find fourNodes 5)
    it "oneNode 3" $
      (True)  `shouldBe` (find oneNode 3)
  describe "Insert" $ do
    it "empty 3" $
      (oneNode)  `shouldBe` (insert empty 3)
    it "fourNodes 5" $
      (fiveNodes)  `shouldBe` (insert fourNodes 5)
    it "fourNodes 3" $
      (fourNodesV2)  `shouldBe` (insert fourNodes 3)
    it "oneNode 2" $
      (twoNodes)  `shouldBe` (insert oneNode 2)
  describe "FromList" $ do
    it "3" $
      (oneNode)  `shouldBe` (fromList [3])
    it "3 2" $
      (twoNodes)  `shouldBe` (fromList [3, 2])
    it "3 2 1 4" $
      (fourNodes)  `shouldBe` (fromList [3, 2, 1, 4])
    it "3 2 4 1 3" $
      (fourNodesV2)  `shouldBe` (fromList [3, 2, 4, 1, 3])
  describe "Remove" $ do
    it "empty 3" $
      (empty)  `shouldBe` (remove empty 3)
    it "fiveNodes 5" $
      (fourNodes)  `shouldBe` (remove fiveNodes 5)
    it "fourNodesV2 3" $
      (fourNodes)  `shouldBe` (remove fourNodesV2 3)
    it "fourNodes 3" $
      (threeNodesV2)  `shouldBe` (remove fourNodes 3)
    it "threeNodesV2 4" $
      (twoNodesV2)  `shouldBe` (remove threeNodesV2 4)


testsTree :: IO TestTree
testsTree = testSpec "All Tests" specTests

