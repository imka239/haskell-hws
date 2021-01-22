module GeomBench (geometryCompareBench) where

import Control.DeepSeq (deepseq)
import Criterion.Main (Benchmark, bench, bgroup, nf)
import qualified GeomTaskFast as Fast
import qualified GeomTaskNaive as Naive

geometryCompareBench :: Benchmark
geometryCompareBench =
  bgroup
    "geometry compare"
    [ bgroup
        "perimeter 10000000"
        [ npx `deepseq` bench "naive" $ nf Naive.perimeter npx,
          px `deepseq` bench "fast" $ nf Fast.perimeter px
        ],
      bgroup
        "double area 10000000"
        [ npx `deepseq` bench "naive" $ nf Naive.doubleArea npx,
          px `deepseq` bench "fast" $ nf Fast.doubleArea px
        ]
    ]
  where
    npx = replicate 10000000 $ Naive.Point 4 4
    px = replicate 10000000 $ Fast.Point 4 4
