{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoFieldSelectors #-}

module Math (module Math) where

import Prelude hiding (length)

data Ray = Ray {origin :: Point3, direction :: Vector}

data Vector = Vector Double Double Double deriving (Show, Eq)

type Point3 = Vector

pattern Point3 :: Double -> Double -> Double -> Vector
pattern Point3 x y z = Vector x y z

data Interval = Interval {min :: Double, max :: Double}

normalize :: Vector -> Vector
normalize v = let l = length v in v / Vector l l l

splat :: Double -> Vector
splat d = Vector d d d

length :: Vector -> Double
length (Vector x y z) = sqrt (x * x + y * y + z * z)

dot :: Vector -> Vector -> Double
dot (Vector x1 y1 z1) (Vector x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

at :: Ray -> Double -> Point3
at (Ray {origin, direction}) t = origin + Vector t t t * direction

inside :: Double -> Interval -> Bool
inside x t = t.min < x && x < t.max

instance Num Vector where
  Vector x1 y1 z1 + Vector x2 y2 z2 = Vector (x1 + x2) (y1 + y2) (z1 + z2)
  Vector x1 y1 z1 * Vector x2 y2 z2 = Vector (x1 * x2) (y1 * y2) (z1 * z2)
  Vector x1 y1 z1 - Vector x2 y2 z2 = Vector (x1 - x2) (y1 - y2) (z1 - z2)
  abs (Vector x y z) = Vector (abs x) (abs y) (abs z)
  signum (Vector x y z) = Vector (signum x) (signum y) (signum z)
  fromInteger a = Vector (fromInteger a) (fromInteger a) (fromInteger a)
  negate (Vector x y z) = Vector (-x) (-y) (-z)

instance Fractional Vector where
  Vector x1 y1 z1 / Vector x2 y2 z2 = Vector (x1 / x2) (y1 / y2) (z1 / z2)
  fromRational a = Vector (fromRational a) (fromRational a) (fromRational a)
