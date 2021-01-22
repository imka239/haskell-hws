module Main where

import Task1Test (testsDayOfWeek)
import Task2Test (testsNat, propNat)
import Task3Test (testsTree)
import Task4Test (testsTreeFoldable)
import Task5Test (splitJoinTests, propSplitJoin)
import Task6Test (concatTests)
import Task7Test (semiGroupTests)
import Test.Tasty (defaultMain, testGroup)


main :: IO ()
main = do
 firstTask   <- testsDayOfWeek
 firstTaskProp <- propNat
 secondTask  <- testsNat
 thirdTask   <- testsTree

 fourthTask  <- testsTreeFoldable
 fifthTask   <- splitJoinTests
 fifthTaskProp   <- propSplitJoin

 sixthTask   <- concatTests
 seventhTask <- semiGroupTests
 defaultMain $ testGroup "All" [testGroup "First Task" [firstTask, firstTaskProp],
                                testGroup "Second Task" [secondTask],
                                testGroup "Third Task" [thirdTask],
                                testGroup "Fourth Task" [fourthTask],
                                testGroup "Fifth Task" [fifthTask, fifthTaskProp],
                                testGroup "Sixth Task" [sixthTask],
                                testGroup "Seventh Task" [seventhTask]
                               ]
