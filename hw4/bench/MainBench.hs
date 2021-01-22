module Main
  ( main,
  )
where

import Criterion.Main (defaultMain)
import GeomBench
import MonteKarloBench

main :: IO ()
main =
  defaultMain
    [geometryCompareBench, monteKarloCompareBench]
