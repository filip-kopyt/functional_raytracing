{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE NoFieldSelectors #-}

module Math (module Math) where

import Data.Hashable (Hashable (hash))
import GHC.Float (powerDouble)
import System.Random (Random (randomR), mkStdGen)
import Prelude hiding (length)

data Ray = Ray {origin :: Point3, direction :: Vector}

data Vector = Vector Double Double Double deriving (Show, Eq)

type Point3 = Vector

pattern Point3 :: Double -> Double -> Double -> Vector
pattern Point3 x y z = Vector x y z

data Interval = Interval {min :: Double, max :: Double}

normalize :: Vector -> Vector
normalize v = let l = length v in v / splat l

splat :: Double -> Vector
splat d = Vector d d d

length :: Vector -> Double
length (Vector x y z) = sqrt (x * x + y * y + z * z)

dot :: Vector -> Vector -> Double
dot (Vector x1 y1 z1) (Vector x2 y2 z2) = sum [x1 * x2, y1 * y2, z1 * z2]

at :: Ray -> Double -> Point3
at (Ray{origin, direction}) t = origin + splat t * direction

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

randomNormalizedVector :: Point3 -> Vector
randomNormalizedVector (Vector x y z) =
  let seed = hash [x, y, z]
      gen = mkStdGen seed
      (u, gen') = randomR (0, 1.0) gen
      (v, _) = randomR (0, 1.0) gen'
      theta = 2 * pi * u
      phi = acos (2 * v - 1)
      x' = cos theta * sin phi
      y' = sin theta * sin phi
      z' = cos phi
   in normalize (Vector x' y' z')

onHemisphere :: Vector -> Vector -> Vector
onHemisphere v normal
  | v `dot` normal > 0.0 = v
  | otherwise = -v

sampleOffset :: Int -> Int -> Int -> Vector
sampleOffset x y sample =
  let seed = hash [x, y, sample]
      gen = mkStdGen seed
      (x', gen') = randomR (-0.5, 0.5) gen
      (y', _) = randomR (-0.5, 0.5) gen'
   in Vector x' y' 0

reflect :: Vector -> Vector -> Vector
reflect v normal = v - splat (2 * v `dot` normal) * normal

refract :: Vector -> Vector -> Double -> Vector
refract uv n eta'OverEta =
  let cosTheta = min ((-uv) `dot` n) 1.0
      rOutPerp = splat eta'OverEta * (uv + splat cosTheta * n)
      rOutParallel = splat (-sqrt (abs (1.0 - powerDouble (length rOutPerp) 2.0))) * n
   in rOutPerp + rOutParallel

reflactance :: Double -> Double -> Double
reflactance cosine refractionIndex =
  let r0' = (1 - refractionIndex) / (1 + refractionIndex)
      r0 = r0' * r0'
   in r0 + (1 - r0) * powerDouble (1 - cosine) 5
