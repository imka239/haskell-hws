module Main where

import TaskTest (testsFileSystem)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  task <- testsFileSystem
  defaultMain $ testGroup "All" [testGroup "FS" [task]]
