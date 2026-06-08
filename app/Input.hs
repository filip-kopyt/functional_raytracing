module Input (module Input) where

import GHC.Generics (Generic)
import Math
import Render
import Data.Aeson.Types
import Data.Aeson.Key (fromString)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson.Decoding

data SceneConfig = SceneConfig
  { objects :: [ObjectConfig]
  }
  deriving (Show, Generic)

instance FromJSON SceneConfig

data MaterialConfig
  = LambertianConfig
      { materialAlbedo :: [Double]
      }
  | MetalConfig
      { materialAlbedo :: [Double]
      , materialFuzz :: Double
      }
  | DielectricConfig
      { materialRefractionIndex :: Double
      }
  deriving Show

instance FromJSON MaterialConfig where
  parseJSON = withObject "Material" $ \o -> do
    typ <- o .: fromString "type"

    case typ of
      "lambertian" ->
        LambertianConfig
          <$> o .: fromString "albedo"

      "metal" ->
        MetalConfig
          <$> o .: fromString "albedo"
          <*> o .: fromString "fuzz"

      "dielectric" ->
        DielectricConfig
          <$> o .: fromString "refractionIndex"

      _ ->
        fail ("Unknown material type: " ++ typ)

data ObjectConfig
  = SphereConfig
      { sphereRadius :: Double
      , sphereCenter :: [Double]
      , sphereMaterial :: MaterialConfig
      }
  deriving Show

instance FromJSON ObjectConfig where
  parseJSON = withObject "Object" $ \o -> do
    typ <- o .: fromString "type"

    case typ of
      "sphere" ->
        SphereConfig
          <$> o .: fromString "radius"
          <*> o .: fromString "center"
          <*> o .: fromString "material"

      _ ->
        fail ("Unknown object type: " ++ typ)

toVector :: [Double] -> Vector
toVector [x,y,z] = Vector x y z
toVector xs =
  error $
    "Expected exactly 3 coordinates, got "
      ++ show xs

buildObject :: ObjectConfig -> Hittable
buildObject SphereConfig { sphereRadius, sphereCenter, sphereMaterial } =
  let sphere =
        Sphere
          { radius = sphereRadius
          , center = toVector sphereCenter
          }
   in case sphereMaterial of

        LambertianConfig alb ->
          Hittable
            Lambertian
              { l_albedo = toVector alb
              }
            sphere

        MetalConfig alb f ->
          Hittable
            Metal
              { m_albedo = toVector alb
              , fuzz = f
              }
            sphere

        DielectricConfig ri ->
          Hittable
            Dielectric
              { refractionIndex = ri
              }
            sphere

loadScene :: FilePath -> IO [Hittable]
loadScene path = do
  bytes <- BL.readFile path

  case eitherDecode bytes of
    Left err ->
      fail ("Scene parse error:\n" ++ err)

    Right scene ->
      pure $
        map buildObject (objects scene)

