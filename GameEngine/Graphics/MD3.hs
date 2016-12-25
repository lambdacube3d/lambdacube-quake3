{-# LANGUAGE RecordWildCards #-}
module GameEngine.Graphics.MD3
  ( addMD3
  , addGPUMD3
  , setMD3Frame
  , uploadMD3
  , GPUMD3(..)
  , MD3Instance(..)
  ) where

import Control.Monad
import Data.HashSet (HashSet)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.HashSet as HashSet
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.ByteString.Char8 as SB8
import Foreign

import LambdaCube.GL
import LambdaCube.GL.Mesh

import GameEngine.Data.MD3 (MD3Model(..))
import qualified GameEngine.Data.MD3 as MD3
import GameEngine.Graphics.Storage
import GameEngine.Utils

data MD3Instance
  = MD3Instance
  { md3instanceObject :: [Object]
  , md3instanceBuffer :: Buffer
  , md3instanceFrames :: V.Vector [(Int,Array)]
  }

type MD3Skin = Map String String

data GPUMD3
  = GPUMD3
  { gpumd3Buffer    :: Buffer
  , gpumd3Surfaces  :: [(IndexStream Buffer,Map String (Stream Buffer))] -- index stream, attribute streams
  , gpumd3Frames    :: V.Vector [(Int,Array)]
  , gpumd3Model     :: MD3Model
  , gpumd3Shaders   :: HashSet String
  }

setMD3Frame :: MD3Instance -> Int -> IO ()
setMD3Frame (MD3Instance{..}) idx = updateBuffer md3instanceBuffer $ md3instanceFrames V.! idx

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
  let cvtSurface :: MD3.Surface -> (Array,Array,Vector (Array,Array))
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
    , gpumd3Shaders   = HashSet.fromList $ concat [map (SB8.unpack . MD3.shName) $ V.toList srShaders | MD3.Surface{..} <- V.toList mdSurfaces]
    }

addGPUMD3 :: GLStorage -> GPUMD3 -> MD3Skin -> [String] -> IO MD3Instance
addGPUMD3 r GPUMD3{..} skin unis = do
    let MD3Model{..} = gpumd3Model
    objs <- forM (zip gpumd3Surfaces $ V.toList mdSurfaces) $ \((index,attrs),sf) -> do
        let materialName s = case Map.lookup (SB8.unpack $ MD3.srName sf) skin of
              Nothing -> SB8.unpack $ MD3.shName s
              Just a  -> a
        objList <- concat <$> forM (V.toList $ MD3.srShaders sf) (\s -> do
          a <- addObjectWithMaterial r (materialName s) TriangleList (Just index) attrs $ setNub $ "worldMat":unis
          b <- addObject r "LightMapOnly" TriangleList (Just index) attrs $ setNub $ "worldMat":unis
          return [a,b])

        -- add collision geometry
        collisionObjs <- case V.toList mdFrames of
          (MD3.Frame{..}:_) -> do
            sphereObj <- uploadMeshToGPU (sphere (V4 1 0 0 1) 4 frRadius) >>= addMeshToObjectArray r "CollisionShape" (setNub $ ["worldMat","origin"] ++ unis)
            boxObj <- uploadMeshToGPU (bbox (V4 0 0 1 1) frMins frMaxs) >>= addMeshToObjectArray r "CollisionShape" (setNub $ ["worldMat","origin"] ++ unis)
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

addMD3 :: GLStorage -> MD3Model -> MD3Skin -> [String] -> IO MD3Instance
addMD3 r model skin unis = do
    gpuMD3 <- uploadMD3 model
    addGPUMD3 r gpuMD3 skin unis
