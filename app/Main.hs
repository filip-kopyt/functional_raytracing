{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Input
import Config
import System.FilePath (makeValid)
import Render
import qualified Data.Array.Repa as R
import Codec.Picture
import Data.List (isSuffixOf)

main :: IO ()
main = do
  putStrLn "[Raytracing Image Generation]"
  putStrLn $ "Enter scene filename (default: " ++ defaultScenePath ++ "):"
  input1 <- getLine
  let inFilename = makeValid (if null input1 then defaultScenePath else input1)
  putStrLn $ "Using scene: " ++ inFilename
  objectList <- loadScene inFilename
  putStrLn "Enter output filename:"
  input2 <- getLine
  let baseOut = makeValid (if null input2 then defaultOutput else input2)
  let outFilename = if ".png" `isSuffixOf` baseOut then baseOut else baseOut ++ ".png"
  putStrLn $ "Using output: " ++ outFilename
  putStrLn "Generating image..."
  !img <- R.computeP $ generateImg objectList
  (savePngImage outFilename . ImageRGBF . toImage) img
