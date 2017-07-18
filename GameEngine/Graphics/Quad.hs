{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module GameEngine.Graphics.Quad
  ( addQuad
  , updateQuad
  , QuadInstance(..)
  ) where

import Foreign (withArray, castPtr)
import Data.Vect.Float
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as SB

import LambdaCube.GL
import GameEngine.Scene
import GameEngine.Utils
import GameEngine.Graphics.Storage

data QuadInstance
  = QuadInstance
  { quadBuffer :: Buffer
  , quadObject :: Object
  }

addQuad storage shaderName = do
  let nullSetter _ = pure ()
  buffer <- compileBuffer
    -- vector component count * 3 (triangle vertex count) * triangle count
    [ Array ArrFloat (2 * 3 * 2) nullSetter -- uv
    , Array ArrFloat (3 * 3 * 2) nullSetter -- position
    , Array ArrFloat (4 * 3 * 2) nullSetter -- color
    ]
  let attributes = Map.fromList $
        [ ("diffuseUV",   Stream Attribute_V2F buffer 0 0 6)
        , ("position",    Stream Attribute_V3F buffer 1 0 6)
        , ("color",       Stream Attribute_V4F buffer 2 0 6)
        , ("normal",      ConstV3F (V3 1 1 1))
        , ("lightmapUV",  ConstV2F (V2 0 0))
        ]

  obj <- addObjectWithMaterial storage shaderName TriangleList Nothing attributes ["viewProj", "worldMat","entityRGB","entityAlpha"]
  -- set projection to 2D
  uniformM44F "viewProj" (objectUniformSetter obj) . mat4ToM44F $ one
  uniformM44F "worldMat" (objectUniformSetter obj) . mat4ToM44F $ one
  uniformV3F "entityRGB" (objectUniformSetter obj) (V3 1 1 1)
  uniformFloat "entityAlpha" (objectUniformSetter obj) 1
  pure $ QuadInstance buffer obj

updateQuad QuadInstance{..} Picture{..} = do
  let withV a f = withArray a (\p -> f $ castPtr p)
      Vec2 x y = picturePosition
      Vec2 w h = pictureSize
      Vec2 s1 t1 = pictureUV1
      Vec2 s2 t2 = pictureUV2
      pos = [ x,     y + h, 0
            , x,     y,     0
            , x + w, y + h, 0
            , x + w, y + h, 0
            , x,     y,     0
            , x + w, y,     0
            ]
      uv = [ s1, t2
           , s1, t1
           , s2, t2
           , s2, t2
           , s1, t1
           , s2, t1
           ]
      color = replicate 6 pictureColor
  updateBuffer quadBuffer
    -- vector component count * 3 (triangle vertex count) * triangle count
    [ (0, Array ArrFloat (2 * 3 * 2) (withV uv))
    , (1, Array ArrFloat (3 * 3 * 2) (withV pos))
    , (2, Array ArrFloat (4 * 3 * 2) (withV color))
    ]
