{-# LANGUAGE LambdaCase #-}
module GameEngine.Scene
  ( module GameEngine.Graphics.Frustum
  , module Data.Vect.Float.Util.Quaternion
  , Renderable(..)
  , Picture(..)
  , Camera(..)
  , Scene(..)
  , Resource(..)
  , asResource
  ) where

import Data.Vect
import System.FilePath
import Data.Vect.Float.Util.Quaternion
import GameEngine.Graphics.Frustum

type Scale = Float
type Position = Vec3
type Orientation = UnitQuaternion
type RGBA = Vec4
type SkinName = String
type ShaderName = String

data Renderable
  = MD3             Position Orientation Scale RGBA FilePath
  | MD3Character    Position Orientation Scale RGBA FilePath SkinName
  | BSPInlineModel  Position Orientation Scale RGBA FilePath Int
  | BSPMap          FilePath
  deriving Show

data Camera
  = Camera
  { cameraPosition      :: Position
  , cameraOrientation   :: Orientation
  , cameraProjection    :: Mat4
  , cameraFrustum       :: Frustum
  , cameraViewportSize  :: (Int,Int)
  -- add viewport position
  } deriving Show

data Scene
  = Scene
  { renderables :: [Renderable]
  , pictures    :: [Picture]
  , camera      :: Camera
  -- add shader time
  } deriving Show

data Resource
  = R_MD3             FilePath
  | R_MD3Character    FilePath SkinName
  | R_Shader          ShaderName
  | R_BSPMap          FilePath
  deriving (Eq, Show)

asResource :: Renderable -> Maybe Resource
asResource = \case
  MD3             _ _ _ _ name      -> Just $ R_MD3 name
  MD3Character    _ _ _ _ name skin -> Just $ R_MD3Character name skin
  BSPInlineModel  _ _ _ _ _ _       -> Nothing
  BSPMap          name              -> Just $ R_BSPMap name

data Picture
  = Picture
  { picturePosition :: Vec2
  , pictureSize     :: Vec2
  , pictureUV1      :: Vec2
  , pictureUV2      :: Vec2
  , pictureColor    :: RGBA
  , pictureShader   :: ShaderName
  } deriving Show
