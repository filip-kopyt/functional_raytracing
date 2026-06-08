module Main (main) where

import Test.HUnit
import Math
import Data.Functor
import System.Exit

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

suite :: Test
suite = TestList [test_at, test_inside, test_random_vector, test_reflect]

main :: IO ()
main = do 
  counts <- runTestTT suite
  if errors counts + failures counts == 0 then exitSuccess else exitFailure
