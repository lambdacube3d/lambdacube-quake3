module GameEngine.Scene
  ( module GameEngine.Graphics.Frustum
  , module Data.Vect.Float.Util.Quaternion
  , Renderable(..)
  , Camera(..)
  , Scene(..)
  ) where

import Data.Vect
import System.FilePath
import Data.Vect.Float.Util.Quaternion
import GameEngine.Graphics.Frustum

type Position = Vec3
type Orientation = UnitQuaternion
type RGBA = Vec4
type SkinName = String

data Renderable
  = MD3             Position Orientation RGBA FilePath
  | MD3Character    Position Orientation RGBA FilePath SkinName
  | BSPInlineModel  Position Orientation RGBA FilePath Int
  | BSPMap          FilePath
  deriving Show

data Camera
  = Camera
  { cameraPosition      :: Position
  , cameraOrientation   :: Orientation
  , cameraProjection    :: Mat4
  , cameraFrustum       :: Frustum
  , cameraViewportSize  :: (Int,Int)
  } deriving Show

data Scene
  = Scene
  { renderables :: [Renderable]
  , camera      :: Camera
  } deriving Show
