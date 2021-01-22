{-# LANGUAGE InstanceSigs #-}
-- | This is Module Block1.Task3.
-- The aim of this program is to make Type 'Tree' - some kind of search tree
module Block1.Task3
  ( -- | Types
    Tree(..)

    -- | Functions
  , isEmpty
  , size
  , find
  , fromList
  , insert
  , remove
  , toList
  ) where

import Data.List.NonEmpty (NonEmpty ((:|)))

-- | Tree represents a binary tree Node
data Tree node
  = Leaf -- | empty node
  | Node -- | real node
     -- | Field storing node's data.
    (NonEmpty node)
    -- | Field storing left child.
    (Tree node)
    -- | Field storing right child.
    (Tree node)

  deriving Show

-- | 'Tree' is an instance of 'Eq'.
-- We have to make instance 'Eq' to check tests
instance Eq a => Eq (Tree a) where
  (==) :: Eq any => Tree any -> Tree any -> Bool
  (==) Leaf Leaf                           = True
  (==) Leaf (Node _ _ _)                   = False
  (==) (Node _ _ _) Leaf                   = False
  (==) (Node val1 l1 r1) (Node val2 l2 r2) = (and [(l1 == l2), (r1 == r2), (val1 == val2)])

-- | This function will return if our 'Tree' is Empty (for any value in 'Tree')
isEmpty :: Tree any -> Bool
isEmpty Leaf = True
isEmpty _    = False

-- | This function will return our 'Tree' size (for any value in 'Tree')
size :: Tree any -> Int
size Leaf                     = 0
size (Node values left right) = (size left) + (size right) + (length values)

-- | This function will return if our binary 'Tree' got a comparable value (for 'Ord' values in 'Tree')
find :: (Ord t) => Tree t -> t -> Bool
find Leaf _ = False
find (Node (y:|_) left right) x
  | (y == x)  = True
  | (y < x)   = find right x
  | otherwise = find left x

-- | This function will return new 'Tree' after adding new value in binary 'Tree' (for 'Ord' values in 'Tree')
insert :: (Ord t) => Tree t -> t -> Tree t
insert Leaf x = (Node (x:|[]) Leaf Leaf)
insert (Node (y:|ys) left right) x
  | (y == x)  = (Node (x:|(y:ys)) left right)
  | (y < x)   = (Node (y:|ys) left (insert right x))
  | otherwise = (Node (y:|ys) (insert left x) right)

-- | This function will return a 'Tree' that were made from 'List' (for 'Ord' values in 'Tree')
fromList :: (Ord t) => [t] -> Tree t
fromList = foldl insert Leaf

-- | This function will return a ('NonEmpty', 'Tree') (for 'Ord' values in 'Tree')
-- first parameter is a value of the leftest son in 'Tree'
-- second parameter is a new value of son
-- the aim for this function is to make correct remove
findNext :: Tree any -> (NonEmpty any, Tree any)
findNext (Node val Leaf right) = (val, right)
findNext (Node val left right) = (v, (Node val newLeft right))
  where (v, newLeft) = findNext left

-- | This function will return new 'Tree' after removing value from binary 'Tree' (for 'Ord' values in 'Tree')
remove :: (Ord t) => Tree t -> t -> Tree t
remove Leaf x = Leaf
remove (Node val@(y:|_) left right) x
  | (y < x) = (Node val left (remove right x))
  | (y > x) = (Node val (remove left x) right)
remove (Node (_:|(y:ys)) left right) x
  = (Node (y:|ys) left right)
remove (Node _ left Leaf) x  = left
remove (Node _ left right) x = (Node val left newR)
  where (val, newR) = findNext right

-- | 'Tree' is an instance of 'Foldable'.
-- Values of this type can be folded with 'foldMap' or 'foldr'
instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ z Leaf           = z
  foldr f z (Node val l r) = foldr f (foldr f (foldr f z r) val) l

  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Leaf           = mempty
  foldMap f (Node val l r) = (foldMap f l) `mappend` (foldMap f val) `mappend` (foldMap f r)

toList :: (Tree a) -> [a]
toList = foldr (:) []
