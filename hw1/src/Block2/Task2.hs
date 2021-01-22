-- | This is Module Block2.Task2.
-- The aim of this program is to make functions 'splitOn' and 'joinWith' to merge and separate strings
module Block2.Task2 where

import Data.List.NonEmpty (NonEmpty ((:|)))
-- | Function 'splitOn' takes separator and 'List' of elems and returns 'NonEmpty' of 'Lists' after separating
splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn splitter str = foldr (countWord splitter) ([]:|[]) str

-- | Function  'countWord' is a special function to make foldr
countWord :: Eq a => a -> a -> NonEmpty [a] -> NonEmpty [a]
countWord splitter ch (x:|xs)
  | splitter == ch = ([]:|(x:xs))
  | otherwise      = ((ch:x):|xs)

-- | Function 'joinWith' takes joiner and 'NonEmpty' of 'Lists' and returns 'List' after merging
joinWith :: Eq a => a -> NonEmpty [a] -> [a]
joinWith joiner masStr = init $ foldr (\str xs -> str ++ [joiner] ++ xs) [] masStr
