{-# LANGUAGE InstanceSigs #-}
module Block3.Task2 where

data NonEmpty a
  = a :| [a]

instance Semigroup(NonEmpty a) where
  (<>) :: (NonEmpty a) -> (NonEmpty a) -> (NonEmpty a)
  (<>) (a:|ax) (b:|bx) = (a:|(ax ++ [b] ++ bx))

instance (Eq a) => Eq (NonEmpty a) where
  (==) :: (NonEmpty a) -> (NonEmpty a) -> (Bool)
  (==) (x1:|xs1) (x2:|xs2) = (x1 == x2) && (xs1 == xs2)

data ThisOrThat a b
  = This a
  | That b
  | Both a b
  deriving (Show)


instance Semigroup(ThisOrThat a b) where
  (<>) :: (ThisOrThat a b) -> (ThisOrThat a b) -> (ThisOrThat a b)
  (<>) (This a) (That b)   = Both a b
  (<>) (This a) (Both _ b) = Both a b
  (<>) (This a) (This _)   = This a
  (<>) (That b) (That _)   = That b
  (<>) (That b) (Both a _) = Both a b
  (<>) (That b) (This a)   = Both a b
  (<>) (Both a b) _        = Both a b

instance (Eq a, Eq b) => Eq (ThisOrThat a b) where
  (==) :: (ThisOrThat a b) -> (ThisOrThat a b) -> (Bool)
  (==) (This x1) (This x2)       = (x1 == x2)
  (==) (That y1) (That y2)       = (y1 == y2)
  (==) (Both x1 y1) (Both x2 y2) = (x1 == x2) && (y1 == y2)
  (==) _ _                       = False

data Name
  = Empty
  | Name (String)
  deriving (Show)

instance Semigroup(Name) where
  (<>) :: Name -> Name -> Name
  (<>) a Empty           = a
  (<>) Empty a           = a
  (<>) (Name x) (Name y) = (Name (x ++ "." ++ y))

instance Eq Name where
  (==) :: Name -> Name -> Bool
  (==) (Name _) Empty    = False
  (==) Empty (Name _)    = False
  (==) (Name x) (Name y) = (x == y)
  (==) _ _               = True

instance Monoid Name where
  mempty :: Name
  mempty = Empty

newtype Endo a = Endo { getEndo :: a -> a }

instance Semigroup(Endo a) where
  (<>) :: Endo a -> Endo a -> Endo a
  (<>) a b = Endo $ (getEndo a . getEndo b)

instance Monoid(Endo a) where
  mempty :: Endo a
  mempty = Endo id
