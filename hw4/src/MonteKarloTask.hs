{-# LANGUAGE BlockArguments #-}

-- | This is Module MonteKarloTask
-- The aim of this program is to count integral given in the task (1 / tan((x ^ 2)) - cos(x)) with many threads
module MonteKarloTask
  ( -- | Functions
    getIntegralPar,
    getIntegralSeq,
  )
where

import Control.Monad.Par (InclusiveRange (..), parMapReduceRange, runPar)
import Data.List
import System.Random (mkStdGen, randomR, randomRs)

-- | Function to count from Readme
f :: Double -> Double
f x = (1 / tan ((x ^ 2)) - cos (x))

-- | Parallel function to do this work parallely, using parMapReduceRange
getIntegralPar :: Int -> Double -> Double -> Double
getIntegralPar toSplit a b = do
  let ans = parMapReduceRange (InclusiveRange 0 (toSplit - 1)) (\x -> return (f (fst (randomR (a, b) (mkStdGen x))))) (\x y -> return (x + y)) 0.0
  (runPar ans) * (b - a) / (fromIntegral toSplit)

-- | Naive function to do this work in one thread, using just randomRs and mkStdGen
getIntegralSeq :: Int -> Double -> Double -> Double
getIntegralSeq toSplit a b = do
  let seq = take toSplit (randomRs (a, b) (mkStdGen 4))
  let ans = foldl' (+) 0.0 (map (\x -> f x) seq)
  (ans * (b - a) / (fromIntegral toSplit))
