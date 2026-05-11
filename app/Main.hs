{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Codec.Picture
import Config
import Data.Array.Repa (Array, D, DIM2, U, Z (Z), (!), (:.) ((:.)))
import Data.Array.Repa qualified as R
import Data.Foldable (minimumBy)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Ord
import GHC.Float (double2Float, powerDouble, sqrtDouble)
import Math
import Prelude hiding (length)

type RGBF = (Double, Double, Double)

pixelRenderer :: (Z :. Int :. Int) -> RGBF
pixelRenderer (Z :. x :. y) = map (rayColor . rayFromPixelXY x y) [0 .. samplesPerPixel] & sum & (/ fromIntegral samplesPerPixel) & gammaCorrection
 where
  gammaCorrection (Vector !r !g !b) = (gamma r, gamma g, gamma b)
  gamma x
    | x > 0 = sqrt x
    | otherwise = 0

generateImg :: Array D DIM2 RGBF
generateImg = R.fromFunction (Z :. width :. height) pixelRenderer

rayColor :: Ray -> Vector
rayColor = rayColor' maxDepth

rayColor' :: Int -> Ray -> Vector
rayColor' !depth !ray
  | depth > 0 = case checkHits ray objectList of
      Just hitRecord -> color
       where
        material = scatter hitRecord.material ray hitRecord
        (attenuation, childRay) = fromJust material
        color
          | isJust material = attenuation * rayColor' (depth - 1) childRay
          | otherwise = splat 0
      Nothing -> background
       where
        background = blend white blue
        Vector _ !deltaV _ = pixelDeltaV
        Vector _ !y _ = ray.direction
        t = 0.5 * (y * deltaV + 1.0)
        white = 1
        blue = Vector 0.5 0.7 1.0
        blend !a !b = (1 - splat t) * a + splat t * b
  | otherwise = 0

asPixel :: Vector -> PixelRGBF
asPixel (Vector !x !y !z) = PixelRGBF (double2Float x) (double2Float y) (double2Float z)

checkHits :: Ray -> [Hittable] -> Maybe HitRecord
checkHits !ray !objs = case hits of
  [] -> Nothing
  _ -> Just (minimumBy (comparing rayLength) hits)
 where
  !hits = mapMaybe (\obj -> hit obj ray interval) objs
  interval = Interval{Math.min = 1e-8, Math.max = posInf}
  posInf = 1 / 0

rayFromPixelXY :: Int -> Int -> Int -> Ray
rayFromPixelXY !x !y !sample = ray
 where
  Vector !x' !y' _ = sampleOffset x y sample
  pixelCenter = pixel00Loc + ((fromIntegral x + splat x') * pixelDeltaU) + ((fromIntegral y + splat y') * pixelDeltaV)
  rayDirection = pixelCenter - cameraCenter
  ray = Ray{origin = cameraCenter, direction = rayDirection}

objectList :: [Hittable]
objectList =
  [ Hittable Lambertian{l_albedo = Vector 0.8 0.8 0.0} Sphere{radius = 100, center = Point3 0 (-100.5) (-1)}
  , Hittable Lambertian{l_albedo = Vector 0.1 0.2 0.5} Sphere{radius = 0.5, center = Point3 0 0 (-1)}
  , Hittable Metal{m_albedo = Vector 0.8 0.8 0.8} Sphere{radius = 0.5, center = Point3 (-1) 0 (-1)}
  , Hittable Metal{m_albedo = Vector 0.8 0.6 0.2} Sphere{radius = 0.5, center = Point3 1 0 (-1)}
  ]

data HitRecord = HitRecord {point :: Point3, normal :: Vector, rayLength :: Double, material :: Material}

class HittableImpl a where
  hit :: a -> Ray -> Interval -> Maybe HitRecord

data Material = forall a. (MaterialImpl a) => Material a

data Hittable = forall a b. (HittableImpl a, MaterialImpl b) => Hittable b a

instance HittableImpl Hittable where
  hit (Hittable mat obj) ray interval = hit obj ray interval <&> (\r -> r{material = Material mat})

instance MaterialImpl Material where
  scatter (Material mat) = scatter mat

data Sphere = Sphere {radius :: Double, center :: Point3}

instance HittableImpl Sphere where
  hit !sphere !ray !interval =
    let oc = sphere.center - ray.origin
        a = powerDouble (length ray.direction) 2
        h = ray.direction `dot` oc
        c = powerDouble (length oc) 2 - sphere.radius * sphere.radius
        !discriminant = h * h - a * c
        sqrtD = sqrtDouble discriminant
        root1 = (h - sqrtD) / a
        root2 = (h + sqrtD) / a
        !root = case (root1 `inside` interval, root2 `inside` interval) of
          (True, _) -> Just root1
          (False, True) -> Just root2
          (False, False) -> Nothing
        rayLength = fromJust root
        point = ray `at` rayLength
        normal = (point - sphere.center) / splat sphere.radius
     in if discriminant >= 0 && isJust root
          then
            Just
              HitRecord
                { rayLength
                , point
                , normal
                }
          else Nothing

class MaterialImpl a where
  scatter :: a -> Ray -> HitRecord -> Maybe (Vector, Ray)

data Lambertian = Lambertian {l_albedo :: Vector}

instance MaterialImpl Lambertian where
  scatter Lambertian{l_albedo} _ hitRecord =
    let scatter_direction' = hitRecord.normal + (randomNormalizedVector hitRecord.point `onHemisphere` hitRecord.normal)
        scatter_direction
          | length scatter_direction' < 1e-8 = hitRecord.normal
          | otherwise = scatter_direction'
        scattered = Ray{origin = hitRecord.point, direction = scatter_direction}
     in Just (l_albedo, scattered)

data Metal = Metal {m_albedo :: Vector}

instance MaterialImpl Metal where
  scatter Metal{m_albedo} ray hitRecord =
    let reflected = reflect ray.direction hitRecord.normal
        scattered = Ray{origin = hitRecord.point, direction = reflected}
     in Just (m_albedo, scattered)

toImage :: Array U DIM2 RGBF -> Image PixelRGBF
toImage !a = generateImage gen w h
 where
  Z :. w :. h = R.extent a
  convert (r, g, b) = asPixel (Vector r g b)
  gen !x !y = convert (a ! (Z :. x :. y))

main :: IO ()
main = do
  !img <- R.computeP generateImg
  (savePngImage "output.png" . ImageRGBF . toImage) img

{-# INLINE pixelRenderer #-}
{-# INLINE toImage #-}
{-# INLINE generateImg #-}
{-# INLINE rayColor #-}
{-# INLINE checkHits #-}
