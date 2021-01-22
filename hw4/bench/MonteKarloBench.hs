module MonteKarloBench (monteKarloCompareBench) where

import Criterion.Main (Benchmark, bench, bgroup, nf)
import MonteKarloTask

monteKarloCompareBench :: Benchmark
monteKarloCompareBench =
  bgroup
    "monteKarlo compare"
    [ bgroup
        "monteKarlo 100000"
        [ bench "par" $ nf (getIntegralPar 100000 1) 2,
          bench "seq" $ nf (getIntegralSeq 100000 1) 2
        ],
      bgroup
        "monteKarlo 1000000"
        [ bench "par" $ nf (getIntegralPar 1000000 1) 2,
          bench "seq" $ nf (getIntegralSeq 1000000 1) 2
        ]
    ]
