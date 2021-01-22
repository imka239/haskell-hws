module Main where

import Task1Test (testsStringSum, testsStringSumProp)
import Task4Test (testsExpr)
import Task5Test (testsMoving)
import Task6Test (testsParser)
import Task7Test (testsBrackets, testsInts)
import Task8Test (testsListList)
import Test.Tasty (defaultMain, testGroup)


main :: IO ()
main = do
 firstTask   <- testsStringSum
 firstTaskProp <- testsStringSumProp
 fourthTask  <- testsExpr
 fifthTask   <- testsMoving

 sixthTask <- testsParser
 seventhTask <- testsBrackets
 seventhTaskInt <- testsInts
 eighthTask <- testsListList
 defaultMain $ testGroup "All" [testGroup "First Block" [firstTask, firstTaskProp],
                                testGroup "Second Block" [fourthTask, fifthTask],
                                testGroup "Third Block" [sixthTask, seventhTask, seventhTaskInt, eighthTask]
                               ]
