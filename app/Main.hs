{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Codec.Picture
import Config
import Data.Foldable (minimumBy)
import Data.Function ((&))
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Ord
import GHC.Float (double2Float, powerDouble, sqrtDouble)
import Math
import Prelude hiding (length)

image :: Image PixelRGBF
image = generateImage pixelRenderer width height

pixelRenderer :: Int -> Int -> PixelRGBF
pixelRenderer x y = map (pixelRenderer' x y) [0 .. samplesPerPixel] & sum & (/ fromIntegral samplesPerPixel) & asPixel

pixelRenderer' :: Int -> Int -> Int -> Vector
pixelRenderer' x y sample = case checkHits x y sample objectList of
  Just hitRecord -> 0.5 * (hitRecord.normal + 1)
  Nothing -> background
  where
    background = blend white blue
    Vector _ deltaV _ = pixelDeltaV
    t = 0.5 * (fromIntegral y * deltaV + 1.0)
    white = 1
    blue = Vector 0.5 0.7 1.0
    blend a b = (1 - splat t) * a + splat t * b

asPixel :: Vector -> PixelRGBF
asPixel (Vector x y z) = PixelRGBF (double2Float x) (double2Float y) (double2Float z)

checkHits :: Int -> Int -> Int -> [Hittable] -> Maybe HitRecord
checkHits x y sample objs = case hits of
  [] -> Nothing
  _ -> Just (minimumBy (comparing rayLength) hits)
  where
    hits = mapMaybe (\obj -> hit obj ray interval) objs
    ray = rayFromPixelXY x y sample
    interval = Interval {Math.min = 0, Math.max = posInf}
    posInf = 1 / 0

rayFromPixelXY :: Int -> Int -> Int -> Ray
rayFromPixelXY x y sample = ray
  where
    Vector x' y' _ = sampleOffset x y sample
    pixelCenter = pixel00Loc + ((fromIntegral x + splat x') * pixelDeltaU) + ((fromIntegral y + splat y') * pixelDeltaV)
    rayDirection = pixelCenter - cameraCenter
    ray = Ray {origin = cameraCenter, direction = rayDirection}

objectList :: [Hittable]
objectList = [Hittable Sphere {radius = 100, center = Point3 0 (-100.5) (-1)}, Hittable Sphere {radius = 0.5, center = Point3 0 0 (-1)}]

data HitRecord = HitRecord {hitPoint :: Point3, normal :: Vector, rayLength :: Double}

class HittableImpl a where
  hit :: a -> Ray -> Interval -> Maybe HitRecord

data Hittable = forall a. (HittableImpl a) => Hittable a

instance HittableImpl Hittable where
  hit (Hittable obj) = hit obj

data Sphere = Sphere {radius :: Double, center :: Point3}

instance HittableImpl Sphere where
  hit sphere ray interval =
    let oc = sphere.center - ray.origin
        a = powerDouble (length ray.direction) 2
        h = ray.direction `dot` oc
        c = powerDouble (length oc) 2 - sphere.radius * sphere.radius
        discriminant = h * h - a * c
        sqrtD = sqrtDouble discriminant
        root1 = (h - sqrtD) / a
        root2 = (h + sqrtD) / a
        root = case (root1 `inside` interval, root2 `inside` interval) of
          (True, _) -> Just root1
          (False, True) -> Just root2
          (False, False) -> Nothing
        rayLength = fromJust root
        hitPoint = ray `at` rayLength
        normal = (hitPoint - sphere.center) / splat sphere.radius
     in if discriminant >= 0 && isJust root
          then
            Just
              HitRecord
                { rayLength,
                  hitPoint,
                  normal
                }
          else Nothing

main :: IO ()
main = writePng "output.png" $ pixelMap toRGB image
  where
    toRGB (PixelRGBF r g b) = PixelRGB8 (f r) (f g) (f b)
      where
        f x = round (clamp (0, 1) x * 255)
