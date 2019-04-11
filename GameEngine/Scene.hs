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
  , module GameEngine.Scene
  ) where

import Data.Vect
import System.FilePath
import Data.Vect.Float.Util.Quaternion
import Data.Vect.Float.Instances
import GameEngine.Graphics.Frustum
import Data.ByteString (ByteString)

type Scale = Float
type Position = Vec3
type Orientation = UnitQuaternion
type RGBA = Vec4
type SkinName = String
type ShaderName = String

newtype Tag = Tag ByteString
  deriving (Show)

data MD3Data
  = MD3Data
  { md3Position     :: Vec3
  , md3Orientation  :: UnitQuaternion
  , md3Scale        :: Float
  , md3RGBA         :: Vec4
  , md3ModelFile    :: FilePath
  , md3Animation    :: () -- TODO
  , md3Frame        :: Int
  , md3SkinName     :: Maybe String
  , md3Attachments  :: [(Tag, MD3Data)]
  }
  deriving (Show)

defaultMD3Data :: MD3Data
defaultMD3Data = MD3Data
  { md3Position     = Vec3 0 0 0
  , md3Orientation  = unitU
  , md3Scale        = 1
  , md3RGBA         = Vec4 1 1 1 1
  , md3ModelFile    = error "md3ModelFile is mandatory!"
  , md3Animation    = ()
  , md3Frame        = 0
  , md3SkinName     = Nothing
  , md3Attachments  = []
  }

data Renderable
  = MD3             Position Orientation Scale RGBA FilePath
  | MD3Character    Position Orientation Scale RGBA FilePath SkinName
  | MD3New          MD3Data
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

asResource :: Renderable -> [Resource]
asResource = \case
  MD3New d                          -> collectMD3 d
  MD3             _ _ _ _ name      -> [R_MD3 name]
  MD3Character    _ _ _ _ name skin -> [R_MD3Character name skin]
  BSPInlineModel  _ _ _ _ _ _       -> []
  BSPMap          name              -> [R_BSPMap name]
 where
  collectMD3 :: MD3Data -> [Resource]
  collectMD3 d = R_MD3 (md3ModelFile d) : concatMap (collectMD3 . snd) (md3Attachments d)

data Picture
  = Picture
  { picturePosition :: Vec2
  , pictureSize     :: Vec2
  , pictureUV1      :: Vec2
  , pictureUV2      :: Vec2
  , pictureColor    :: RGBA
  , pictureShader   :: ShaderName
  } deriving Show
