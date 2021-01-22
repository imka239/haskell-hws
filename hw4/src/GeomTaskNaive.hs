{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- | This is Module GeomTaskNaive
-- The aim of this program is to make Naive realization of Geometry Points without seq
module GeomTaskNaive
  ( -- | Type
    Point (..),
    -- | Functions
    plus,
    minus,
    scalarProduct,
    crossProduct,
    perimeter,
    doubleArea,
  )
where

import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

-- | Classic realization: two `Ints` representing x and y coordinates
data Point = Point {x :: Int, y :: Int}
  deriving (Eq, Show, Generic, NFData)

-- | Function to summarize perimter and doubleArea using crossPoint and dist
megaF :: (Num t) => (Point -> Point -> t) -> [Point] -> Point -> t
megaF f [v3] v1 = (f v3 v1)
megaF f (v2 : v3 : xs) v1 = (f v2 v3) + (megaF f (v3 : xs) v1)

-- | help function to get distance in Double from to coordinates in Ints
dist :: Point -> Point -> Double
dist (Point x1 y1) (Point x2 y2) = sqrt . fromIntegral $ ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1))

-- | classic operation (plus) for two points as normal vectors
plus :: Point -> Point -> Point
plus (Point x1 y1) (Point x2 y2) = (Point (x1 + x2) (y1 + y2))

-- | classic operation (minus) for two points as normal vectors
minus :: Point -> Point -> Point
minus (Point x1 y1) (Point x2 y2) = (Point (x1 - x2) (y1 - y2))

-- | classic operation (scalarPoints) for two points as normal vectors
scalarProduct :: Point -> Point -> Int
scalarProduct (Point x1 y1) (Point x2 y2) = (x1 * x2 + y2 * y1)

-- | classic operation (crossProduct) for two points as normal vectors
crossProduct :: Point -> Point -> Int
crossProduct (Point x1 y1) (Point x2 y2) = (x1 * y2 - x2 * y1)

-- | classic operation (perimeter) for [Point] to get perimeter (in Double)
perimeter :: [Point] -> Double
perimeter [] = 0
perimeter [x] = 0
perimeter arr@(x1 : x2 : xs) = megaF dist (arr) x1

-- | classic operation (perimeter) for [Point] to get area (using cross product in geometry)
doubleArea :: [Point] -> Int
doubleArea [] = 0
doubleArea [x] = 0
doubleArea arr@(x1 : x2 : xs) = megaF crossProduct (arr) x1
