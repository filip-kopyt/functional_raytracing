{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.HUnit
import Math
import Data.Functor
import System.Exit
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (eitherDecode)
import System.IO.Temp (withSystemTempFile)
import System.IO (hClose)
import Input
import Config

test_normalize :: Test
test_normalize =
  "normalize" ~: do
    normalize (Vector 3 0 0) @?= Vector 1 0 0
    normalize (Vector 0 4 0) @?= Vector 0 1 0
    normalize (Vector 0 0 5) @?= Vector 0 0 1
    normalize (Vector 1 1 1) @?= Vector (1 / sqrt 3) (1 / sqrt 3) (1 / sqrt 3)

test_dot :: Test
test_dot =
  "dot product" ~: do
    dot (Vector 1 2 3) (Vector 4 5 6) @?= 32 -- 1*4 + 2*5 + 3*6 = 4 + 10 + 18 = 32
    dot (Vector 0 0 0) (Vector 1 2 3) @?= 0
    dot (Vector 1 0 0) (Vector 0 1 0) @?= 0
    dot (Vector 1 1 1) (Vector 1 1 1) @?= 3

test_length :: Test
test_length =
  "vector length" ~: do
    Math.length (Vector 3 4 0) @?= 5
    Math.length (Vector 0 0 0) @?= 0
    Math.length (Vector 1 1 1) @?= sqrt 3

test_splat :: Test
test_splat =
  "splat" ~: do
    splat 7 @?= Vector 7 7 7
    splat 0 @?= Vector 0 0 0
    splat (-3) @?= Vector (-3) (-3) (-3)

test_at :: Test
test_at = "test_at" ~: condition
  where 
    ray = Ray { origin = Point3 (1) (2) (3), direction = Vector (0.3) (-0.1) (0.5)}
    distance = 10
    expected = Vector 4 1 8
    condition = (ray `at` distance) ~?= expected 

test_inside :: Test
test_inside = "test_inside" ~: do
  let interval = Interval { Math.min = 0, Math.max = 10}
  assertBool "5 in interval" (5 `inside` interval)
  assertBool "-5 not in interval" (not ((-5) `inside` interval))
  assertBool "15 not in interval" (not (15 `inside` interval))

test_random_vector :: Test
test_random_vector = "test_random_vector" ~: do
  let lengths = [1..16] <&> (\x -> Vector x x x) <&> randomNormalizedVector <&> Math.length <&> (==1)
  assertBool "All strings are normalized" (all id lengths)

test_onHemisphere :: Test
test_onHemisphere =
  "test_onHemisphere" ~:
    let n1 = Vector 0 1 0
        v1 = Vector 1 1 0
        v2 = Vector 1 (-1) 0
        v3 = Vector 0 1 (-1)
    in do
      onHemisphere v1 n1 @?= v1
      onHemisphere v2 n1 @?= -v2
      onHemisphere v3 n1 @?= v3

test_sampleOffset_bounds :: Test
test_sampleOffset_bounds =
  "sampleOffset bounds" ~:
    let Vector x y z = sampleOffset 4 5 6
    in do
      assertBool "x in range" (-0.5 <= x && x <= 0.5)
      assertBool "y in range" (-0.5 <= y && y <= 0.5)
      z @?= 0
      sampleOffset 4 5 6 @?= sampleOffset 4 5 6 -- should be "deterministic"

test_reflect :: Test
test_reflect = "test_reflect" ~: condition where
  inVector = Vector (-1) (-1) (-1)
  normal = Vector 1 0 0
  outVector = Vector 1 (-1) (-1)
  condition = reflect inVector normal ~?= outVector

test_refract_perpendicular :: Test
test_refract_perpendicular =
  "refract perpendicular" ~:
    refract uv n eta ~?= uv
  where
    uv = Vector 0 (-1) 0
    n = Vector 0 1 0
    eta = 1 / 1.5

test_reflactance_normal :: Test
test_reflactance_normal =
  "reflactance normal incidence" ~:
    assertBool "Reflactance is 0.04" (abs (reflactance 1 1.5 - 0.04) < 1e-6) -- ((1-1.5)/(1+1.5))^2 = 0.04

test_toVector_ok :: Test
test_toVector_ok =
  "toVector ok" ~:
    toVector [1,2,3] ~?= Vector 1 2 3

test_vector_add :: Test
test_vector_add =
  "vector addition" ~:
    Vector 1 2 3 + Vector 4 5 6
      ~?= Vector 5 7 9

test_vector_mul :: Test
test_vector_mul =
  "vector multiplication" ~:
    Vector 1 2 3 * Vector 4 5 6
      ~?= Vector 4 10 18

test_vector_div :: Test
test_vector_div =
  "vector division" ~:
    Vector 8 9 10 / Vector 2 3 5
      ~?= Vector 4 3 2

test_vector_negate :: Test
test_vector_negate =
  "vector negate" ~:
    negate (Vector 1 (-2) 3)
      ~?= Vector (-1) 2 (-3)

test_decode_material_lambertian :: Test
test_decode_material_lambertian =
  "decode lambertian" ~:
    case eitherDecode json of
      Left err ->
        assertFailure err
      Right (LambertianConfig albedo) ->
        albedo @?= [0.2, 0.3, 0.4]
      _ ->
        assertFailure "Expected LambertianConfig"
  where
    json = "{\"type\":\"lambertian\",\"albedo\":[0.2,0.3,0.4]}"

test_decode_material_metal :: Test
test_decode_material_metal =
  "decode metal" ~:
    case eitherDecode json of
      Left err ->
        assertFailure err
      Right (MetalConfig albedo fuzz) -> do
        albedo @?= [0.8, 0.7, 0.6]
        fuzz @?= 0.25
      _ ->
        assertFailure "Expected MetalConfig"
  where
    json =
      "{\"type\":\"metal\",\"albedo\":[0.8,0.7,0.6],\"fuzz\":0.25}"

test_decode_material_dielectric :: Test
test_decode_material_dielectric =
  "decode dielectric" ~:
    case eitherDecode json of
      Left err ->
        assertFailure err
      Right (DielectricConfig ri) ->
        ri @?= 1.5
      _ ->
        assertFailure "Expected DielectricConfig"
  where
    json =
      "{\"type\":\"dielectric\",\"refractionIndex\":1.5}"

test_decode_object_sphere :: Test
test_decode_object_sphere =
  "decode sphere object" ~:
    case eitherDecode json of
      Left err -> assertFailure err
      Right (SphereConfig r c _) -> do
        r @?= 1.5
        c @?= [0,1,2]
  where
    json =
      "{\"type\":\"sphere\",\"radius\":1.5,\"center\":[0,1,2],\"material\":{\"type\":\"lambertian\",\"albedo\":[1,1,1]}}"

test_load_scene_length :: Test
test_load_scene_length =
  "loadScene returns correct number of objects" ~:
    withSystemTempFile "scene.json" $ \path handle -> do
      BL.hPutStr handle sceneJson
      hClose handle
      objs <- loadScene path
      Prelude.length objs @?= 2
  where
    sceneJson =
      "{\"objects\": ["
      <> "{\"type\":\"sphere\",\"radius\":1,\"center\":[0,0,0],\"material\":{\"type\":\"lambertian\",\"albedo\":[1,1,1]}},"
      <> "{\"type\":\"sphere\",\"radius\":2,\"center\":[1,1,1],\"material\":{\"type\":\"metal\",\"albedo\":[0.5,0.5,0.5],\"fuzz\":0.1}}"
      <> "]}"

test_aspect_ratio :: Test
test_aspect_ratio =
  "aspect ratio" ~:
    assertBool "aspect ratio is width/height" (abs (aspectRatio - (fromIntegral width / fromIntegral height)) < 1e-2) -- low precision necessary here due to rounding

suite :: Test
suite =
  TestList
    [ test_normalize
    , test_dot
    , test_length
    , test_splat
    , test_at
    , test_inside
    , test_random_vector
    , test_onHemisphere
    , test_sampleOffset_bounds
    , test_reflect
    , test_refract_perpendicular
    , test_reflactance_normal
    , test_toVector_ok
    , test_vector_add
    , test_vector_mul
    , test_vector_div
    , test_vector_negate
    , test_decode_material_lambertian
    , test_decode_material_metal
    , test_decode_material_dielectric
    , test_decode_object_sphere
    , test_load_scene_length
    , test_aspect_ratio
    ]

main :: IO ()
main = do 
  counts <- runTestTT suite
  if errors counts + failures counts == 0 then exitSuccess else exitFailure
