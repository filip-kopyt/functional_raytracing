module Config where

import Math

viewpointU, viewpointV, pixelDeltaU, pixelDeltaV :: Vector
viewpointU = Vector viewpointWidth 0 0
viewpointV = Vector 0 (-viewpointHeight) 0
pixelDeltaU = viewpointU / fromIntegral width
pixelDeltaV = viewpointV / fromIntegral height

cameraCenter :: Point3
cameraCenter = 0

viewpointUpperLeft, pixel00Loc :: Point3
viewpointUpperLeft = cameraCenter - Vector 0 0 focalLength - (viewpointU / 2) - (viewpointV / 2)
pixel00Loc = viewpointUpperLeft + 0.5 * (pixelDeltaU + pixelDeltaV)

aspectRatio, focalLength, viewpointWidth, viewpointHeight :: Double
aspectRatio = 16.0 / 9.0
focalLength = 1
viewpointWidth = viewpointHeight * (fromIntegral width / fromIntegral height)
viewpointHeight = 1

width, height :: Int
height = 1080
width = round (fromIntegral height * aspectRatio)

samplesPerPixel :: Int
samplesPerPixel = 1024

maxDepth :: Int
maxDepth = 16

defaultScenePath :: String
defaultScenePath = "scene.json"

defaultOutput :: String
defaultOutput = "output"
