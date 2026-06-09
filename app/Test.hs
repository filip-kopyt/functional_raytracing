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

test_at :: Test
test_at = "test_at" ~: condition
  where 
    ray = Ray { origin = Point3 (1) (2) (3), direction = Vector (0.3) (-0.1) (0.5)}
    distance = 10
    expected = Vector 4 1 8
    condition = (ray `at` distance) ~=? expected 

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

test_reflect :: Test
test_reflect = "test_reflect" ~: condition where
  inVector = Vector (-1) (-1) (-1)
  normal = Vector 1 0 0
  outVector = Vector 1 (-1) (-1)
  condition = reflect inVector normal ~=? outVector

test_toVector_ok :: Test
test_toVector_ok =
  "toVector ok" ~:
    toVector [1,2,3] ~=? Vector 1 2 3

test_decode_material_lambertian :: Test
test_decode_material_lambertian =
  "decode lambertian" ~:
    case eitherDecode json of
      Left err ->
        assertFailure err
      Right (LambertianConfig albedo) ->
        albedo @=? [0.2, 0.3, 0.4]
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
        albedo @=? [0.8, 0.7, 0.6]
        fuzz @=? 0.25
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
        ri @=? 1.5
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
        r @=? 1.5
        c @=? [0,1,2]
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
      Prelude.length objs @=? 2
  where
    sceneJson =
      "{\"objects\": ["
      <> "{\"type\":\"sphere\",\"radius\":1,\"center\":[0,0,0],\"material\":{\"type\":\"lambertian\",\"albedo\":[1,1,1]}},"
      <> "{\"type\":\"sphere\",\"radius\":2,\"center\":[1,1,1],\"material\":{\"type\":\"metal\",\"albedo\":[0.5,0.5,0.5],\"fuzz\":0.1}}"
      <> "]}"

suite :: Test
suite =
  TestList
    [ test_at
    , test_inside
    , test_random_vector
    , test_reflect
    , test_toVector_ok
    , test_decode_material_lambertian
    , test_decode_material_metal
    , test_decode_material_dielectric
    , test_decode_object_sphere
    , test_load_scene_length
    ]

main :: IO ()
main = do 
  counts <- runTestTT suite
  if errors counts + failures counts == 0 then exitSuccess else exitFailure
