-- | This is Module Block3.Task1.
-- The aim of this program is to make functions 'maybeConcat' and 'eitherConcat' - some kind of concatenation
module Block3.Task1 where

import Data.Either (partitionEithers)
import Data.Maybe (fromJust, isJust)

-- | Function 'maybeConcat' takes List of Maybe Lists and concat them
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = foldr helper []

helper :: Maybe [a] -> [a] -> [a]
helper x y
  | isJust x = ((fromJust x) ++ y)
  | otherwise = y

doWork (x, y) = (mconcat x, mconcat y)

-- | Function 'eitherConcat' takes List of Left ot Right Lists and concat them in two Lists
eitherConcat :: (Monoid m, Monoid n) => [Either m n] -> (m, n)
eitherConcat x = doWork $ partitionEithers x
