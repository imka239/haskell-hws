-- | This is Module Block1.Task2. The aim of this program is to make Type 'Nat' - Matlogic kind of Numeric.Natural
module Block1.Task2
  ( -- | Types
    Nat(..),
    -- | Functions
    toIntegerNat,
    isEven,
    divNat,
    modNat
  ) where

-- | in this Type Num is calling Nat if it's 'Zero' or it's next number for another 'Nat"
data Nat
  = Z
  | S Nat
  deriving Show

-- | 'Nat' is an instance of 'Eq'.
-- We have to make instance 'Eq' to check tests
instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  (==) Z Z         = True
  (==) (S n) (S m) = n == m
  (==) _ _         = False


-- | 'Nat' is an instance of 'Ord'.
-- 'Nat' objects can be compared with each other
instance Ord Nat where
  (<=) :: Nat -> Nat -> Bool
  (<=) Z _         = True
  (<=) _ Z         = False
  (<=) (S n) (S m) = n <= m

-- | This function returns (Maybe Nat, Maybe Nat) that represents 'div' and 'mod'
helpDiv :: Nat -> Nat -> Nat -> (Nat, Nat)
helpDiv x y n
  | x < y     = (n, x)
  | otherwise = helpDiv (x - y) y (S n)

-- | 'Nat' is an instance of Num.
-- (+) (*) (-) (signum) (abs) (fromInteger) are correct operations just as Nums.
instance Num Nat where
  (+) :: Nat -> Nat -> Nat
  (+) x Z     = x
  (+) x (S n) = (S x) + n

  (*) :: Nat -> Nat -> Nat
  (*) x Z     = Z
  (*) x (S n) = x * n + x

  (-) :: Nat -> Nat -> Nat
  (-) Z _         = Z
  (-) x Z         = x
  (-) (S x) (S n) = x - n

  fromInteger :: Integer -> Nat
  fromInteger n
    | (n == 0)   = Z
    | (n > 0)    = S (fromInteger (n - 1))
    | otherwise  = S (fromInteger (-n - 1))

  abs :: Nat -> Nat
  abs n = n

  signum :: Nat -> Nat
  signum Z = Z
  signum _ = (S Z)


-- | 'Nat' objects can be checked by the division on 2
isEven :: Nat -> Bool
isEven Z         = True
isEven (S Z)     = False
isEven (S (S n)) = isEven n

-- | 'Nat' objects can be divided on another 'Nat'
-- If 'Nat' object divides on Zero function will return 'Nothing' value otherwise 'Just Nat' value
divNat :: Nat -> Nat -> Nat
divNat x Z = error "Division by zero"
divNat x y = fst $ helpDiv x y Z

-- | 'Nat' objects can be interpretated as Integer
toIntegerNat :: Nat -> Integer
toIntegerNat Z     = 0
toIntegerNat (S n) = (+ 1) $ toIntegerNat n

-- | 'Nat' objects can be divided on another 'Nat' (remain is 'Nat' too)
-- If 'Nat' object divides on Zero function will return 'Nothing' value otherwise 'Just Nat' value
modNat :: Nat -> Nat -> Nat
modNat x Z = error "Division by zero"
modNat x y = snd $ helpDiv x y Z

