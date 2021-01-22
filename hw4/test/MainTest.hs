module Main where

import GeomTest (geomTests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  task <- geomTests
  defaultMain $ testGroup "All" [testGroup "FS" [task]]
