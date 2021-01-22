module Task6Test where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Block3.Task1

specTests :: Spec
specTests = do
  describe "Maybe" $ do
    it "[Just [1,2,3], Nothing, Just [4,5]]" $
      ([1, 2, 3, 4, 5])  `shouldBe` (maybeConcat [Just [1,2,3], Nothing, Just [4,5]])
    it "[Nothing, Just x, Nothing]" $
      ("x")  `shouldBe` (maybeConcat [Nothing, Just "x", Nothing])
    it "[Nothing, Just [1], Nothing, Just [1]]" $
      ([1, 1])  `shouldBe` (maybeConcat [Nothing, Just [1], Nothing, Just [1]])
    it "[Just x Nothing y]" $
      ("xy")  `shouldBe` (maybeConcat [Just "x", Nothing, Just "y"])
  describe "Either" $ do
    it "[Left [1,2,3], Right xy, Left [4,5]]" $
      ([1, 2, 3, 4, 5], "xy")  `shouldBe` (eitherConcat [Left [1,2,3], Right "xy", Left [4,5]])
    it "[Left #!/bin/bash, Right [1, 2], Right [2, 1]]" $
      ("#!/bin/bash", [1, 2, 2, 1])  `shouldBe` (eitherConcat [Left "#!/bin/bash", Right [1, 2], Right [2, 1]])
    it "[Nothing, Just [1], Nothing, Just [1]]" $
      ([1, 1])  `shouldBe` (maybeConcat [Nothing, Just [1], Nothing, Just [1]])
    it "[Just x Nothing y]" $
      ("xy")  `shouldBe` (maybeConcat [Just "x", Nothing, Just "y"])

concatTests :: IO TestTree
concatTests = testSpec "All Tests" specTests

