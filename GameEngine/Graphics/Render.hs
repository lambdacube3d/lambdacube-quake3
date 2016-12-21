{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns #-}
module GameEngine.Graphics.Render
  ( addBSP
  , addMD3
  , addGPUBSP
  , addGPUMD3
  , setMD3Frame
  , MD3Instance(..)
  , uploadBSP
  , uploadMD3
  , GPUMD3(..)
  , GPUBSP(..)
  , BSPInstance(..)
  ) where

import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Data.Vect.Float hiding (Vector)
import Data.List
import Data.Maybe
import Foreign
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Storable.Mutable as SMV
import qualified Data.Vector.Storable as SV
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import Codec.Picture
import Debug.Trace
import System.FilePath

import LambdaCube.GL
import LambdaCube.GL.Mesh
import GameEngine.Data.BSP
import GameEngine.Data.MD3 (MD3Model(..))
import qualified GameEngine.Data.MD3 as MD3
import GameEngine.Graphics.BezierSurface
import GameEngine.Graphics.Frustum
import GameEngine.Utils

{-
    plans:
        - proper render of q3 objects
        - shadow mapping
        - bloom
        - ssao
-}

addObject' :: GLStorage -> String -> Primitive -> Maybe (IndexStream Buffer) -> Map String (Stream Buffer) -> [String] -> IO Object
addObject' rndr name prim idx attrs unis = addObject rndr name' prim idx attrs' unis
  where
    attrs'  = Map.filterWithKey (\n _ -> elem n renderAttrs) attrs
    setters = objectArrays . schema $ rndr
    alias   = dropExtension name
    name'
      | Map.member name setters = name
      | Map.member alias setters = alias
      | otherwise = "missing shader"
    renderAttrs = Map.keys $ case Map.lookup name' setters of
        Just (ObjectArraySchema _ x)  -> x
        _           -> error $ "material not found: " ++ show name'

data GPUBSP
  = GPUBSP
  { gpubspVertexBuffer  :: Buffer
  , gpubspIndexBuffer   :: Buffer
  , gpubspLightmaps     :: Vector TextureData
  , gpubspSurfaces      :: [(String,Primitive,IndexStream Buffer,Map String (Stream Buffer),Maybe TextureData)]
  , gpubspShaders       :: Set String
  , gpubspBSPLevel      :: BSPLevel
  }

uploadBSP :: BSPLevel -> IO GPUBSP
uploadBSP bsp@BSPLevel{..} = do
  -- construct vertex and index buffer
  let convertSurface (objs,lenV,arrV,lenI,arrI) sf = if noDraw then skip else case srSurfaceType sf of
          Planar          -> objs'
          TriangleSoup    -> objs'
          -- tessellate, concatenate vertex and index data to fixed vertex and index buffer
          Patch           -> ((lmIdx, lenV, lenV', lenI, lenI', TriangleStrip, name):objs, lenV+lenV', v:arrV, lenI+lenI', i:arrI)
            where
              (v,i) = tessellatePatch blDrawVertices sf 5
              lenV' = V.length v
              lenI' = V.length i
          Flare           -> skip
        where
          lmIdx = srLightmapNum sf
          skip  = ((lmIdx,srFirstVertex sf, srNumVertices sf, srFirstIndex sf, 0, TriangleList, name):objs, lenV, arrV, lenI, arrI)
          objs' = ((lmIdx,srFirstVertex sf, srNumVertices sf, srFirstIndex sf, srNumIndices sf, TriangleList, name):objs, lenV, arrV, lenI, arrI)
          Shader name sfFlags _ = blShaders V.! (srShaderNum sf)
          noDraw = sfFlags .&. 0x80 /= 0
      (objs,_,drawVl,_,drawIl) = V.foldl' convertSurface ([],V.length blDrawVertices,[blDrawVertices],V.length blDrawIndices,[blDrawIndices]) blSurfaces
      drawV' = V.concat $ reverse drawVl
      drawI' = V.concat $ reverse drawIl

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
    , gpubspShaders       = Set.fromList [name | (name,_,_,_,_) <- surfaces]
    , gpubspBSPLevel      = bsp
    }

data BSPInstance
  = BSPInstance
  { bspinstanceBSPLevel :: BSPLevel
  , bspinstanceSurfaces :: Vector [Object]
  }

addBSP :: GLStorage -> BSPLevel -> IO BSPInstance
addBSP storage bsp = do
  whiteTexture <- uploadTexture2DToGPU' False False False False $ ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 255 255 255) 1 1
  addGPUBSP whiteTexture storage =<< uploadBSP bsp

addGPUBSP :: TextureData -> GLStorage -> GPUBSP -> IO BSPInstance
addGPUBSP whiteTexture storage GPUBSP{..} = do
  -- add to storage
  let obj surfaceIdx (name,prim,index,attrs,lightmap) = do
          let objUnis = ["LightMap","worldMat"]
          --putStrLn $ "add surface " ++ show surfaceIdx
          o <- addObject' storage name prim (Just index) attrs objUnis
          o1 <- addObject storage "LightMapOnly" prim (Just index) attrs objUnis
          {-
              #define LIGHTMAP_2D			-4		// shader is for 2D rendering
              #define LIGHTMAP_BY_VERTEX	-3		// pre-lit triangle models
              #define LIGHTMAP_WHITEIMAGE	-2
              #define	LIGHTMAP_NONE		-1
          -}
          forM_ [o,o1] $ \b -> uniformFTexture2D "LightMap" (objectUniformSetter b) $ fromMaybe whiteTexture lightmap
          return [o,o1]
  BSPInstance gpubspBSPLevel <$> V.imapM obj (V.fromList gpubspSurfaces)

data MD3Instance
  = MD3Instance
  { md3instanceObject :: [Object]
  , md3instanceBuffer :: Buffer
  , md3instanceFrames :: V.Vector [(Int,Array)]
  }

setMD3Frame :: MD3Instance -> Int -> IO ()
setMD3Frame (MD3Instance{..}) idx = updateBuffer md3instanceBuffer $ md3instanceFrames V.! idx

type MD3Skin = Map String String

data GPUMD3
  = GPUMD3
  { gpumd3Buffer    :: Buffer
  , gpumd3Surfaces  :: [(IndexStream Buffer,Map String (Stream Buffer))] -- index stream, attribute streams
  , gpumd3Frames    :: V.Vector [(Int,Array)]
  , gpumd3Model     :: MD3Model
  , gpumd3Shaders   :: Set String
  }

{-
    buffer layout
      index arrays for surfaces         [index array of surface 0,          index array of surface 1,         ...]
      texture coord arrays for surfaces [texture coord array of surface 0,  texture coord array of surface 1, ...]
      position arrays for surfaces      [position array of surface 0,       position array of surface 1,      ...]
      normal arrays for surfaces        [normal array of surface 0,         normal array of surface 1,        ...]
    in short: [ surf1_idx..surfN_idx
              , surf1_tex..surfN_tex
              , surf1_pos..surfN_pos
              , surf1_norm..surfN_norm
              ]
-}
uploadMD3 :: MD3Model -> IO GPUMD3
uploadMD3 model@MD3Model{..} = do
  let cvtSurface :: MD3.Surface -> (Array,Array,V.Vector (Array,Array))
      cvtSurface MD3.Surface{..} =
        ( Array ArrWord32 (SV.length srTriangles) (withV srTriangles)
        , Array ArrFloat (2 * SV.length srTexCoords) (withV srTexCoords)
        , V.map cvtPosNorm srXyzNormal
        )
        where
          withV a f = SV.unsafeWith a (\p -> f $ castPtr p)
          cvtPosNorm (p,n) = (f p, f n) where f sv = Array ArrFloat (3 * SV.length sv) $ withV sv

      addSurface sf (il,tl,pl,nl,pnl) = (i:il,t:tl,p:pl,n:nl,pn:pnl) where
        (i,t,pn) = cvtSurface sf
        (p,n)    = V.head pn

      (il,tl,pl,nl,pnl) = V.foldr addSurface ([],[],[],[],[]) mdSurfaces

  buffer <- compileBuffer (concat [il,tl,pl,nl])

  let numSurfaces = V.length mdSurfaces
      surfaceData idx MD3.Surface{..} = (index,attributes) where
        index = IndexStream buffer idx 0 (SV.length srTriangles)
        countV = SV.length srTexCoords
        attributes = Map.fromList $
          [ ("diffuseUV",   Stream Attribute_V2F buffer (1 * numSurfaces + idx) 0 countV)
          , ("position",    Stream Attribute_V3F buffer (2 * numSurfaces + idx) 0 countV)
          , ("normal",      Stream Attribute_V3F buffer (3 * numSurfaces + idx) 0 countV)
          , ("color",       ConstV4F (V4 1 1 1 1))
          , ("lightmapUV",  ConstV2F (V2 0 0))
          ]

      frames = foldr addSurfaceFrames emptyFrame $ zip [0..] pnl where
        emptyFrame = V.replicate (V.length mdFrames) []
        addSurfaceFrames (idx,pn) f = V.zipWith (\l (p,n) -> (2 * numSurfaces + idx,p):(3 * numSurfaces + idx,n):l) f pn

  return $ GPUMD3
    { gpumd3Buffer    = buffer
    , gpumd3Surfaces  = zipWith surfaceData [0..] (V.toList mdSurfaces)
    , gpumd3Frames    = frames
    , gpumd3Model     = model
    , gpumd3Shaders   = Set.fromList $ concat [map (SB8.unpack . MD3.shName) $ V.toList srShaders | MD3.Surface{..} <- V.toList mdSurfaces]
    }

addMD3 :: GLStorage -> MD3Model -> MD3Skin -> [String] -> IO MD3Instance
addMD3 r model skin unis = do
    gpuMD3 <- uploadMD3 model
    addGPUMD3 r gpuMD3 skin unis

addGPUMD3 :: GLStorage -> GPUMD3 -> MD3Skin -> [String] -> IO MD3Instance
addGPUMD3 r GPUMD3{..} skin unis = do
    let MD3Model{..} = gpumd3Model
    objs <- forM (zip gpumd3Surfaces $ V.toList mdSurfaces) $ \((index,attrs),sf) -> do
        let materialName s = case Map.lookup (SB8.unpack $ MD3.srName sf) skin of
              Nothing -> SB8.unpack $ MD3.shName s
              Just a  -> a
        objList <- concat <$> forM (V.toList $ MD3.srShaders sf) (\s -> do
          a <- addObject' r (materialName s) TriangleList (Just index) attrs $ nub $"worldMat":unis
          b <- addObject r "LightMapOnly" TriangleList (Just index) attrs $ nub $ "worldMat":unis
          return [a,b])

        -- add collision geometry
        collisionObjs <- case V.toList mdFrames of
          (MD3.Frame{..}:_) -> do
            sphereObj <- uploadMeshToGPU (sphere (V4 1 0 0 1) 4 frRadius) >>= addMeshToObjectArray r "CollisionShape" (nub $ ["worldMat","origin"] ++ unis)
            boxObj <- uploadMeshToGPU (bbox (V4 0 0 1 1) frMins frMaxs) >>= addMeshToObjectArray r "CollisionShape" (nub $ ["worldMat","origin"] ++ unis)
            --when (frOrigin /= zero) $ putStrLn $ "frOrigin: " ++ show frOrigin
            return [sphereObj,boxObj]
          _ -> return []
        {-
          uploadMeshToGPU
          addMeshToObjectArray
          updateMesh :: GPUMesh -> [(String,MeshAttribute)] -> Maybe MeshPrimitive -> IO ()
        -}
        
        return $ objList ++ collisionObjs
    -- question: how will be the referred shaders loaded?
    --           general problem: should the gfx network contain all passes (every possible materials)?
    return $ MD3Instance
        { md3instanceObject = concat objs
        , md3instanceBuffer = gpumd3Buffer
        , md3instanceFrames = gpumd3Frames
        }
