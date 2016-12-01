{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns #-}
module GameEngine.Graphics.Render
  ( addBSP
  , addMD3
  , setMD3Frame
  , LCMD3(..)
  ) where

import Control.Applicative
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Data.Vect.Float
import Data.List
import Foreign
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
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
import GameEngine.Data.MD3 (MD3Model)
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

addBSP :: GLStorage -> BSPLevel -> IO (V.Vector [Object])
addBSP renderer BSPLevel{..} = do
    let byteStringToVector :: SB.ByteString -> SV.Vector Word8
        byteStringToVector = SV.fromList . SB.unpack
    lightMapTextures <- fmap V.fromList $ forM (V.toList blLightmaps) $ \(Lightmap d) -> do
        uploadTexture2DToGPU' True False True True $ ImageRGB8 $ Image 128 128 $ byteStringToVector d
    whiteTex <- uploadTexture2DToGPU' False False False False $ ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 255 255 255) 1 1

    -- construct vertex and index buffer
    let lightMapTexturesSize = V.length lightMapTextures
        convertSurface (objs,lenV,arrV,lenI,arrI) sf = if noDraw then skip else case srSurfaceType sf of
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
    -- add to storage
    let obj surfaceIdx (lmIdx,startV,countV,startI,countI,prim,SB8.unpack -> name) = do
            let attrs = Map.fromList $
                    [ ("position",      Stream Attribute_V3F vertexBuffer 0 startV countV)
                    , ("diffuseUV",     Stream Attribute_V2F vertexBuffer 1 startV countV)
                    , ("lightmapUV",    Stream Attribute_V2F vertexBuffer 2 startV countV)
                    , ("normal",        Stream Attribute_V3F vertexBuffer 3 startV countV)
                    , ("color",         Stream Attribute_V4F vertexBuffer 4 startV countV)
                    ]
                index = IndexStream indexBuffer 0 startI countI
                isValidIdx i = i >= 0 && i < lightMapTexturesSize
                objUnis = ["LightMap","worldMat"]
            o <- addObject' renderer name prim (Just index) attrs objUnis
            o1 <- addObject renderer "LightMapOnly" prim (Just index) attrs objUnis
            let lightMap a = forM_ [o,o1] $ \b -> uniformFTexture2D "LightMap" (objectUniformSetter b) a
            {-
                #define LIGHTMAP_2D			-4		// shader is for 2D rendering
                #define LIGHTMAP_BY_VERTEX	-3		// pre-lit triangle models
                #define LIGHTMAP_WHITEIMAGE	-2
                #define	LIGHTMAP_NONE		-1
            -}
            case isValidIdx lmIdx of
                False   -> lightMap whiteTex
                True    -> lightMap $ lightMapTextures V.! lmIdx
            return [o,o1]
    V.imapM obj $ V.fromList $ reverse objs

data LCMD3
    = LCMD3
    { lcmd3Object   :: [Object]
    , lcmd3Buffer   :: Buffer
    , lcmd3Frames   :: V.Vector [(Int,Array)]
    }

setMD3Frame :: LCMD3 -> Int -> IO ()
setMD3Frame (LCMD3{..}) idx = updateBuffer lcmd3Buffer $ lcmd3Frames V.! idx

type MD3Skin = Map String String

addMD3 :: GLStorage -> MD3Model -> MD3Skin -> [String] -> IO LCMD3
addMD3 r model skin unis = do
    let cvtSurface :: MD3.Surface -> (Array,Array,V.Vector (Array,Array))
        cvtSurface sf = ( Array ArrWord32 (SV.length indices) (withV indices)
                        , Array ArrFloat (2 * SV.length texcoords) (withV texcoords)
                        , posNorms
                        )
          where
            withV a f = SV.unsafeWith a (\p -> f $ castPtr p)
            tris = MD3.srTriangles sf
            indices = tris
            {-
            intToWord16 :: Int -> Word16
            intToWord16 = fromIntegral
            addIndex v i (a,b,c) = do
                SMV.write v i $ intToWord16 a
                SMV.write v (i+1) $ intToWord16 b
                SMV.write v (i+2) $ intToWord16 c
                return (i+3)
            indices = SV.create $ do
                v <- SMV.new $ 3 * V.length tris
                V.foldM_ (addIndex v) 0 tris
                return v
            -}
            texcoords = MD3.srTexCoords sf
            cvtPosNorm (p,n) = (f p, f n)
              where
                --f :: V.Vector Vec3 -> Array
                f sv = Array ArrFloat (3 * SV.length sv) $ withV sv
                --(p,n) = V.unzip pn
            posNorms = V.map cvtPosNorm $ MD3.srXyzNormal sf

        addSurface sf (il,tl,pl,nl,pnl) = (i:il,t:tl,p:pl,n:nl,pn:pnl)
          where
            (i,t,pn) = cvtSurface sf
            (p,n)    = V.head pn
        (il,tl,pl,nl,pnl)   = V.foldr addSurface ([],[],[],[],[]) surfaces
        surfaces            = MD3.mdSurfaces model
        numSurfaces         = V.length surfaces
        frames              = foldr addSurfaceFrames emptyFrame $ zip [0..] pnl
          where
            emptyFrame = V.replicate (V.length $ MD3.mdFrames model) []
            -- TODO: ????
            addSurfaceFrames (idx,pn) f = V.zipWith (\l (p,n) -> (2 * numSurfaces + idx,p):(3 * numSurfaces + idx,n):l) f pn

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
    buffer <- compileBuffer $ concat [il,tl,pl,nl]

    objs <- forM (zip [0..] $ V.toList surfaces) $ \(idx,sf) -> do
        let countV = SV.length $ MD3.srTexCoords sf
            countI = SV.length (MD3.srTriangles sf)
            attrs = Map.fromList $
                [ ("diffuseUV",     Stream Attribute_V2F buffer (1 * numSurfaces + idx) 0 countV)
                , ("position",      Stream Attribute_V3F buffer (2 * numSurfaces + idx) 0 countV)
                , ("normal",        Stream Attribute_V3F buffer (3 * numSurfaces + idx) 0 countV)
                , ("color",         ConstV4F (V4 1 1 1 1))
                , ("lightmapUV",    ConstV2F (V2 0 0))
                ]
            index = IndexStream buffer idx 0 countI
            materialName s = case Map.lookup (SB8.unpack $ MD3.srName sf) skin of
              Nothing -> SB8.unpack $ MD3.shName s
              Just a  -> a
        objList <- concat <$> forM (V.toList $ MD3.srShaders sf) (\s -> do
          a <- addObject' r (materialName s) TriangleList (Just index) attrs $ nub $"worldMat":unis
          b <- addObject r "LightMapOnly" TriangleList (Just index) attrs $ nub $ "worldMat":unis
          return [a,b])

        -- add collision geometry
        collisionObjs <- case V.toList $ MD3.mdFrames model of
          (MD3.Frame{..}:_) -> do
            sphereObj <- uploadMeshToGPU (sphere (V4 1 0 0 1) 4 frRadius) >>= addMeshToObjectArray r "CollisionShape" (nub $ ["worldMat","origin"] ++ unis)
            boxObj <- uploadMeshToGPU (bbox (V4 0 0 1 1) frMins frMaxs) >>= addMeshToObjectArray r "CollisionShape" (nub $ ["worldMat","origin"] ++ unis)
            when (frOrigin /= zero) $ putStrLn $ "frOrigin: " ++ show frOrigin
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
    return $ LCMD3
        { lcmd3Object   = concat objs
        , lcmd3Buffer   = buffer
        , lcmd3Frames   = frames
        }
