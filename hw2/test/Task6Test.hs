module Task6Test where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Block3.Task1Task2

import Control.Applicative (Alternative, (<|>), empty)
import Data.Char (isDigit, digitToInt)

specTests :: Spec
specTests = do
  describe "Applicative" $ do
    it "pure" $
      (runParser (pure 1) "") `shouldBe` Just(1, "")
    it "<*>1" $
      runParser ((++) <$> stream "Has" <*> stream "kell") "Haskell" `shouldBe` Just ("Haskell", "")
    it "<*>2" $
      runParser ((++) <$> stream "Has" <*> stream "kall") "Haskell" `shouldBe` Nothing
  describe "Monad" $ do
    it ">>=" $
      (runParser ((pure 4) >>= (\x -> return (x / 2))) "") `shouldBe` Just(2, "")
  describe "Alternative" $ do
    it "<|>" $
      (runParser (empty <|> pure 239) "") `shouldBe` Just(239, "")
  describe "eof" $ do
    it "eof" $
      (runParser eof "") `shouldBe` Just((), "")
  describe "element" $ do
    it "el1" $
      runParser (element 'c') "caba" `shouldBe` Just ('c', "aba")
    it "el2" $
      runParser (element 'c') "aba" `shouldBe` Nothing
  it "should satisfy" $
      runParser (satisfy isDigit) "1" `shouldBe` Just ('1', "")
  it "should streamed" $
      runParser (stream "1, 2, 3") "1, 2, 3, 4, 5" `shouldBe` Just ("1, 2, 3", ", 4, 5")


testsParser :: IO TestTree
testsParser = testSpec "All Tests" specTests

