{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns #-}
module GameEngine.Graphics.BSP
  ( addBSP
  , addGPUBSP
  , uploadBSP
  , GPUBSP(..)
  , BSPInstance(..)
  ) where

import Control.Monad
import Data.Maybe
import Data.HashSet (HashSet)
import Data.Map (Map)
import Data.Set (Set)
import Data.Vector (Vector)
import qualified Data.HashSet as HashSet
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import Text.Printf
import Foreign

import Codec.Picture
import LambdaCube.GL

import GameEngine.Data.BSP
import GameEngine.Graphics.Storage
import GameEngine.Graphics.BezierSurface

data GPUBSP
  = GPUBSP
  { gpubspVertexBuffer  :: Buffer
  , gpubspIndexBuffer   :: Buffer
  , gpubspLightmaps     :: Vector TextureData
  , gpubspSurfaces      :: [(String,Primitive,IndexStream Buffer,Map String (Stream Buffer),Maybe TextureData)]
  , gpubspShaders       :: HashSet String
  , gpubspBSPLevel      :: BSPLevel
  }

data BSPInstance
  = BSPInstance
  { bspinstanceBSPLevel :: BSPLevel
  , bspinstanceSurfaces :: Vector [Object]
  }

data BSPSurface
  = BSPTriangleSoup Int Int Int Int -- lightmapIndex, firstVertex, firstIndex, indexCount
  | BSPPatch        Int (Vector DrawVertex) (Vector Int) -- lightmapIndex

{-
  TODO:
    - build index buffer
    - build vertex buffer: bsp vertices + patch verices
    - build lightmap atlas
    - update lightmap uv coordinates
-}
batchBSP shaderMap BSPLevel{..} = (result 0 (Map.toList surfaces) indices,V.concat indices) where
  result _ [] [] = []
  result offset ((name,l):xs) (v:ys) = (lightmapIndex l,0,V.length blDrawVertices,offset,V.length v,TriangleList,name) : result (offset + V.length v) xs ys
  indices = map join $ Map.elems surfaces
  lightmapIndex ((BSPTriangleSoup i _ _ _):_) = i
  lightmapIndex ((BSPPatch i _ _ ):_) = i
  lightmapIndex _ = 0 :: Int-- TODO
  join l = V.concat [(firstVertex +) <$> V.slice firstIndex indexCount blDrawIndices | BSPTriangleSoup lightmap firstVertex firstIndex indexCount <- l]
  surfaces = foldl (\m s -> Map.insertWith (++) (let name = shName $ blShaders V.! srShaderNum s in if Set.member (SB8.unpack name) shaderMap then name else "missing shader") (bspSurface s) m) mempty blSurfaces
  bspSurface s@Surface{..} = if noDraw then [] else case srSurfaceType of
          Patch -> [BSPPatch srLightmapNum v i] where (v,i) = tessellatePatch blDrawVertices s 5
          Flare -> []
          _    -> [BSPTriangleSoup srLightmapNum srFirstVertex srFirstIndex srNumIndices]
        where
          surfaceFlags = shSurfaceFlags $ blShaders V.! srShaderNum
          noDraw = surfaceFlags .&. 0x80 /= 0

uploadBSP :: Set String -> BSPLevel -> IO GPUBSP
uploadBSP shaderMap bsp@BSPLevel{..} = do
  -- construct vertex and index buffer
  let convertSurface (objs,lenV,arrV,lenI,arrI) s@Surface{..} = if noDraw then skip else case srSurfaceType of
          Planar          -> objs'
          TriangleSoup    -> objs'
          -- tessellate, concatenate vertex and index data to fixed vertex and index buffer
          Patch           -> ((srLightmapNum, lenV, lenV', lenI, lenI', TriangleStrip, name):objs, lenV+lenV', v:arrV, lenI+lenI', i:arrI)
            where
              (v,i) = tessellatePatch blDrawVertices s 5
              lenV' = V.length v
              lenI' = V.length i
          Flare           -> skip
        where
          skip  = ((srLightmapNum,srFirstVertex, srNumVertices, srFirstIndex, 0, TriangleList, name):objs, lenV, arrV, lenI, arrI)
          objs' = ((srLightmapNum,srFirstVertex, srNumVertices, srFirstIndex, srNumIndices, TriangleList, name):objs, lenV, arrV, lenI, arrI)
          Shader name sfFlags _ = blShaders V.! srShaderNum
          noDraw = sfFlags .&. 0x80 /= 0
      
      (objs,_,drawVl,_,drawIl) = V.foldl' convertSurface ([],V.length blDrawVertices,[blDrawVertices],V.length blDrawIndices,[blDrawIndices]) blSurfaces
      drawV' = V.concat $ reverse drawVl
      drawI' = V.concat $ reverse drawIl
      
      {-
      (objs,drawI') = batchBSP shaderMap bsp
      drawV' = blDrawVertices
      -}
      withV w a f = w a (\p -> f $ castPtr p)
      attribute f = withV SV.unsafeWith $ SV.convert $ V.map f drawV'
      indices     = SV.convert $ V.map fromIntegral drawI' :: SV.Vector Word32
      vertexCount = V.length drawV'

  vertexBuffer <- compileBuffer $
      [ Array ArrFloat (3 * vertexCount) $ attribute dvPosition
      , Array ArrFloat (2 * vertexCount) $ attribute dvDiffuseUV
      , Array ArrFloat (2 * vertexCount) $ attribute dvLightmaptUV
      , Array ArrFloat (3 * vertexCount) $ attribute dvNormal
      , Array ArrFloat (4 * vertexCount) $ attribute dvColor
      ]
  indexBuffer <- compileBuffer [Array ArrWord32 (SV.length indices) $ withV SV.unsafeWith indices]

  -- upload light maps
  let byteStringToVector = SV.fromList . SB.unpack :: SB.ByteString -> SV.Vector Word8

  lightMapTextures <- fmap V.fromList $ forM (V.toList blLightmaps) $ \(Lightmap d) ->
    uploadTexture2DToGPU' True False True True $ ImageRGB8 $ Image 128 128 $ byteStringToVector d

  let gpuSurface (lmIdx,startV,countV,startI,countI,prim,SB8.unpack -> name) = (name,prim,index,attrs,lightmap) where
        attrs = Map.fromList
            [ ("position",      Stream Attribute_V3F vertexBuffer 0 startV countV)
            , ("diffuseUV",     Stream Attribute_V2F vertexBuffer 1 startV countV)
            , ("lightmapUV",    Stream Attribute_V2F vertexBuffer 2 startV countV)
            , ("normal",        Stream Attribute_V3F vertexBuffer 3 startV countV)
            , ("color",         Stream Attribute_V4F vertexBuffer 4 startV countV)
            ]
        index = IndexStream indexBuffer 0 startI countI
        lightmap = lightMapTextures V.!? lmIdx

      surfaces = map gpuSurface $ reverse objs

  return $ GPUBSP
    { gpubspVertexBuffer  = vertexBuffer
    , gpubspIndexBuffer   = indexBuffer
    , gpubspLightmaps     = lightMapTextures
    , gpubspSurfaces      = surfaces
    , gpubspShaders       = HashSet.fromList [name | (name,_,_,_,_) <- surfaces]
    , gpubspBSPLevel      = bsp
    }

addGPUBSP :: TextureData -> GLStorage -> GPUBSP -> IO BSPInstance
addGPUBSP whiteTexture storage GPUBSP{..} = do
  -- add to storage
  let obj surfaceIdx (name,prim,index,attrs,lightmap) = do
          let objUnis = ["LightMap","worldMat"]
          o <- addObjectWithMaterial storage name prim (Just index) attrs objUnis
          o1 <- addObject storage "LightMapOnly" prim (Just index) attrs objUnis
          {-
              #define LIGHTMAP_2D			-4		// shader is for 2D rendering
              #define LIGHTMAP_BY_VERTEX	-3		// pre-lit triangle models
              #define LIGHTMAP_WHITEIMAGE	-2
              #define	LIGHTMAP_NONE		-1
          -}
          forM_ [o,o1] $ \b -> uniformFTexture2D "LightMap" (objectUniformSetter b) $ fromMaybe whiteTexture lightmap
          return [o,o1]
      surfaceVector = V.fromList gpubspSurfaces
  putStrLn $ printf "add %d bsp surfaces to storage" $ V.length surfaceVector
  BSPInstance gpubspBSPLevel <$> V.imapM obj surfaceVector

addBSP :: Set String -> GLStorage -> BSPLevel -> IO BSPInstance
addBSP shaderMap storage bsp = do
  whiteTexture <- uploadTexture2DToGPU' False False False False $ ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 255 255 255) 1 1
  addGPUBSP whiteTexture storage =<< uploadBSP shaderMap bsp
