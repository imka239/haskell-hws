module Task4Test where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (Spec, describe, it, shouldBe, testSpec)

import Block2.Task1

specTests :: Spec
specTests = do
  describe "eval" $ do
    it "eval (Add (Const 10) (Const 20))" $
      (Right 30)  `shouldBe` (eval (Add (Const 10) (Const 20)))
    it "eval (Mul (Const 10) (Const 20))" $
      (Right 200)  `shouldBe` (eval (Mul (Const 10) (Const 20)))
    it "eval (Sub (Const 10) (Const 20))" $
      (Right (-10))  `shouldBe` (eval (Sub (Const 10) (Const 20)))
    it "eval (Div (Const 20) (Const 10))" $
      (Right 2)  `shouldBe` (eval (Div (Const 20) (Const 10)))
    it "eval (Pow (Const 20) (Const 3))" $
      (Right 8000)  `shouldBe` (eval (Pow (Const 20) (Const 3)))
    it "eval (Div (Const 20) (Const 0))" $
      (Left DivByZero)  `shouldBe` (eval (Div (Const 20) (Const 0)))
    it "eval (Pow (Const 20) (Const (-10)))" $
      (Left NegPow)  `shouldBe` (eval (Pow (Const 20) (Const (-10))))
    it "bigeval" $
      (Right (4))  `shouldBe` (eval (Sub (Mul (Const 10) (Pow (Div (Const 0) (Const 4)) (Const 0))) (Const 6)))





testsExpr :: IO TestTree
testsExpr = testSpec "All Tests" specTests

