-- | This is Module Block1.Task1.
-- The aim of this program is to make Type 'Days', with some functions
module Block1.Task1
  ( -- | Type
    Days(..)

    -- | Functions
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

import Numeric.Natural (Natural)

-- | This is sequence of tokens, that represents all days of the week.
data Days
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving Show

-- | The best way to make functions is to translate all days of the week in Int using 'Enum'
-- We have to show make bijection between days and nums.
instance Enum Days where
  toEnum :: Int -> Days
  toEnum 0 = Monday
  toEnum 1 = Tuesday
  toEnum 2 = Wednesday
  toEnum 3 = Thursday
  toEnum 4 = Friday
  toEnum 5 = Saturday
  toEnum 6 = Sunday

  fromEnum :: Days -> Int
  fromEnum Monday    = 0
  fromEnum Tuesday   = 1
  fromEnum Wednesday = 2
  fromEnum Thursday  = 3
  fromEnum Friday    = 4
  fromEnum Saturday  = 5
  fromEnum Sunday    = 6

-- | For tests we have to check 'Days' for equality
instance Eq Days where
  (==) :: Days -> Days -> Bool
  (==) x y = fromEnum x == fromEnum y

-- | Takes a 'Days' object and returns the next day.
nextDay :: Days -> Days
nextDay x = toEnum $ mod ((fromEnum x) + 1) 7

-- | Takes a 'Days' object and returns the day after a given number.
afterDays :: Days -> Natural -> Days
afterDays day 0 = day
afterDays day n = nextDay $ afterDays day (n - 1)

-- | Takes a 'Days' object and returns distance to Friday.
daysToParty :: Days -> Natural
daysToParty Friday = 0
daysToParty x      = daysToParty(nextDay x) + 1

-- | Takes a 'Days' object and returns if this day is a weekend.
isWeekend :: Days -> Bool
isWeekend x = (fromEnum x) > 4
