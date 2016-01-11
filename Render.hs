{-# LANGUAGE ViewPatterns, OverloadedStrings #-}
module Render where

import Control.Applicative
import Control.Monad
import Data.Vect.Float
import Data.List
import Foreign
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.Vector.Storable.Mutable as SMV
import qualified Data.Vector.Storable as SV
import qualified Data.ByteString as SB
import qualified Data.ByteString.Char8 as SB8
import Debug.Trace
import Codec.Picture

import BSP
import LambdaCube.GL
import MD3 (MD3Model)
import qualified MD3 as MD3
import Q3Patch

{-
    plans:
        - proper render of q3 objects
        - shadow mapping
        - bloom
        - ssao
-}

tessellatePatch :: V.Vector DrawVertex -> Surface -> Int -> (V.Vector DrawVertex,V.Vector Int)
tessellatePatch drawV sf level = (V.concat vl,V.concat il)
  where
    (w,h)   = srPatchSize sf
    gridF :: [DrawVertex] -> [[DrawVertex]]
    gridF l = case splitAt w l of
        (x,[])  -> [x]
        (x,xs)  -> x:gridF xs
    grid        = gridF $ V.toList $ V.take (srNumVertices sf) $ V.drop (srFirstVertex sf) drawV
    controls    = [V.fromList $ concat [take 3 $ drop x l | l <- lines] | x <- [0,2..w-3], y <- [0,2..h-3], let lines = take 3 $ drop y grid]
    patches     = [tessellate c level | c <- controls]
    (vl,il)     = unzip $ reverse $ snd $ foldl' (\(o,l) (v,i) -> (o+V.length v, (v,V.map (+o) i):l)) (0,[]) patches

addObject' :: GLStorage -> String -> Primitive -> Maybe (IndexStream Buffer) -> Map String (Stream Buffer) -> [String] -> IO Object
addObject' rndr name prim idx attrs unis = addObject rndr name' prim idx attrs' unis
  where
    attrs'  = Map.filterWithKey (\n _ -> elem n renderAttrs) attrs
    setters = objectArrays . schema $ rndr
    name'  = if Map.member name setters then name else "missing shader"
    renderAttrs = Map.keys $ case Map.lookup name' setters of
        Just (ObjectArraySchema _ x)  -> x
        _           -> error $ "material not found: " ++ show name'

addBSP :: GLStorage -> BSPLevel -> IO (V.Vector Object)
addBSP renderer bsp = do
    let byteStringToVector :: SB.ByteString -> SV.Vector Word8
        byteStringToVector = SV.fromList . SB.unpack
    lightMapTextures <- fmap V.fromList $ forM (V.toList $ blLightmaps bsp) $ \(Lightmap d) -> do
        uploadTexture2DToGPU' False True True $ ImageRGB8 $ Image 128 128 $ byteStringToVector d
    whiteTex <- uploadTexture2DToGPU' False False False $ ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 255 255 255) 128 128

    let lightMapTexturesSize = V.length lightMapTextures
        shaders = blShaders bsp
        convertSurface (objs,lenV,arrV,lenI,arrI) sf = if noDraw then skip else case srSurfaceType sf of
            Planar          -> objs'
            TriangleSoup    -> objs'
            -- tessellate, concatenate vertex and index data to fixed vertex and index buffer
            Patch           -> ((lmIdx, lenV, lenV', lenI, lenI', TriangleStrip, name):objs, lenV+lenV', v:arrV, lenI+lenI', i:arrI)
              where
                (v,i) = tessellatePatch drawV sf 5
                lenV' = V.length v
                lenI' = V.length i
            Flare           -> skip
          where
            lmIdx = srLightmapNum sf
            skip  = ((lmIdx,srFirstVertex sf, srNumVertices sf, srFirstIndex sf, 0, TriangleList, name):objs, lenV, arrV, lenI, arrI)
            objs' = ((lmIdx,srFirstVertex sf, srNumVertices sf, srFirstIndex sf, srNumIndices sf, TriangleList, name):objs, lenV, arrV, lenI, arrI)
            Shader name sfFlags _ = shaders V.! (srShaderNum sf)
            noDraw = sfFlags .&. 0x80 /= 0
        drawV = blDrawVertices bsp
        drawI = blDrawIndices bsp
        (objs,_,drawVl,_,drawIl) = V.foldl' convertSurface ([],V.length drawV,[drawV],V.length drawI,[drawI]) $! blSurfaces bsp
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
    let obj (lmIdx,startV,countV,startI,countI,prim,SB8.unpack -> name) = do
            let attrs = Map.fromList $
                    [ ("position",      Stream Attribute_V3F vertexBuffer 0 startV countV)
                    , ("diffuseUV",     Stream Attribute_V2F vertexBuffer 1 startV countV)
                    , ("lightmapUV",    Stream Attribute_V2F vertexBuffer 2 startV countV)
                    , ("normal",        Stream Attribute_V3F vertexBuffer 3 startV countV)
                    , ("color",         Stream Attribute_V4F vertexBuffer 4 startV countV)
                    ]
                index = IndexStream indexBuffer 0 startI countI
                isValidIdx i = i >= 0 && i < lightMapTexturesSize
            o <- addObject' renderer name prim (Just index) attrs ["LightMap"]
            let lightMap = uniformFTexture2D "LightMap" $ objectUniformSetter o
            {-
                #define LIGHTMAP_2D			-4		// shader is for 2D rendering
                #define LIGHTMAP_BY_VERTEX	-3		// pre-lit triangle models
                #define LIGHTMAP_WHITEIMAGE	-2
                #define	LIGHTMAP_NONE		-1
            -}
            case isValidIdx lmIdx of
                False   -> lightMap whiteTex
                True    -> lightMap $ lightMapTextures V.! lmIdx
            return o
    V.mapM obj $ V.fromList $ reverse objs

data LCMD3
    = LCMD3
    { lcmd3Object   :: [Object]
    , lcmd3Buffer   :: Buffer
    , lcmd3Frames   :: V.Vector [(Int,Array)]
    }

setMD3Frame :: LCMD3 -> Int -> IO ()
setMD3Frame (LCMD3 _ buf frames) idx = updateBuffer buf $ frames V.! idx

addMD3 :: GLStorage -> MD3Model -> [String] -> IO LCMD3
addMD3 r model unis = do
    let cvtSurface :: MD3.Surface -> (Array,Array,V.Vector (Array,Array))
        cvtSurface sf = ( Array ArrWord16 (SV.length indices) (withV indices)
                        , Array ArrFloat (2 * SV.length texcoords) (withV texcoords)
                        , posNorms
                        )
          where
            withV a f = SV.unsafeWith a (\p -> f $ castPtr p)
            tris = MD3.srTriangles sf
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
            texcoords = SV.convert $ MD3.srTexCoords sf :: SV.Vector Vec2
            cvtPosNorm pn = (f p, f n)
              where
                f :: V.Vector Vec3 -> Array
                f v = Array ArrFloat (3 * V.length v) $ withV $ SV.convert v
                (p,n) = V.unzip pn
            posNorms = V.map cvtPosNorm $ MD3.srXyzNormal sf

        addSurface (il,tl,pl,nl,pnl) sf = (i:il,t:tl,p:pl,n:nl,pn:pnl)
          where
            (i,t,pn) = cvtSurface sf
            (p,n)    = V.head pn
        addFrame f (idx,pn) = V.zipWith (\l (p,n) -> (2 * numSurfaces + idx,p):(3 * numSurfaces + idx,n):l) f pn
        (il,tl,pl,nl,pnl)   = V.foldl' addSurface ([],[],[],[],[]) surfaces
        surfaces            = MD3.mdSurfaces model
        numSurfaces         = V.length surfaces
        frames              = foldl' addFrame (V.replicate (V.length $ MD3.mdFrames model) []) $ zip [0..] pnl

    buffer <- compileBuffer $ concat [reverse il,reverse tl,reverse pl,reverse nl]

    objs <- forM (zip [0..] $ V.toList surfaces) $ \(idx,sf) -> do
        let countV = V.length $ MD3.srTexCoords sf
            countI = 3 * V.length (MD3.srTriangles sf)
            attrs = Map.fromList $
                [ ("diffuseUV",     Stream Attribute_V2F buffer (1 * numSurfaces + idx) 0 countV)
                , ("position",      Stream Attribute_V3F buffer (2 * numSurfaces + idx) 0 countV)
                , ("normal",        Stream Attribute_V3F buffer (3 * numSurfaces + idx) 0 countV)
                , ("color",         ConstV4F (V4 0.5 0 0 1))
                , ("lightmapUV",    ConstV2F (V2 0 0))
                ]
            index = IndexStream buffer idx 0 countI
        addObject' r "missing shader" TriangleList (Just index) attrs ["worldMat"]
    -- question: how will be the referred shaders loaded?
    --           general problem: should the gfx network contain all passes (every possible materials)?
    return $ LCMD3
        { lcmd3Object   = objs
        , lcmd3Buffer   = buffer
        , lcmd3Frames   = frames
        }

isClusterVisible :: BSPLevel -> Int -> Int -> Bool
isClusterVisible bl a b
    | a >= 0 = 0 /= (visSet .&. (shiftL 1 (b .&. 7)))
    | otherwise = True
  where
    Visibility nvecs szvecs vecs = blVisibility bl
    i = a * szvecs + (shiftR b 3)
    visSet = vecs V.! i

findLeafIdx bl camPos i
    | i >= 0 = if dist >= 0 then findLeafIdx bl camPos f else findLeafIdx bl camPos b
    | otherwise = (-i) - 1
  where 
    node    = blNodes bl V.! i
    (f,b)   = ndChildren node 
    plane   = blPlanes bl V.! ndPlaneNum node
    dist    = plNormal plane `dotprod` camPos - plDist plane

cullSurfaces :: BSPLevel -> Vec3 -> Frustum -> V.Vector Object -> IO ()
cullSurfaces bsp cam frust objs = case leafIdx < 0 || leafIdx >= V.length leaves of
    True    -> {-trace "findLeafIdx error" $ -}V.forM_ objs $ \obj -> enableObject obj True
    False   -> {-trace ("findLeafIdx ok " ++ show leafIdx ++ " " ++ show camCluster) -}surfaceMask
  where
    leafIdx = findLeafIdx bsp cam 0
    leaves = blLeaves bsp
    camCluster = lfCluster $ leaves V.! leafIdx
    visibleLeafs = V.filter (\a -> (isClusterVisible bsp camCluster $ lfCluster a) && inFrustum a) leaves
    surfaceMask = do
        let leafSurfaces = blLeafSurfaces bsp
        V.forM_ objs $ \obj -> enableObject obj False
        V.forM_ visibleLeafs $ \l ->
            V.forM_ (V.slice (lfFirstLeafSurface l) (lfNumLeafSurfaces l) leafSurfaces) $ \i ->
                enableObject (objs V.! i) True
    inFrustum a = boxInFrustum (lfMaxs a) (lfMins a) frust

data Frustum
    = Frustum
    { frPlanes  :: [(Vec3, Float)]
    , ntl       :: Vec3
    , ntr       :: Vec3
    , nbl       :: Vec3
    , nbr       :: Vec3
    , ftl       :: Vec3
    , ftr       :: Vec3
    , fbl       :: Vec3
    , fbr       :: Vec3
    }

pointInFrustum p fr = foldl' (\b (n,d) -> b && d + n `dotprod` p >= 0) True $ frPlanes fr

sphereInFrustum p r fr = foldl' (\b (n,d) -> b && d + n `dotprod` p >= (-r)) True $ frPlanes fr

boxInFrustum pp pn fr = foldl' (\b (n,d) -> b && d + n `dotprod` (g pp pn n) >= 0) True $ frPlanes fr
  where
    g (Vec3 px py pz) (Vec3 nx ny nz) n = Vec3 (fx px nx) (fy py ny) (fz pz nz)
      where
        Vec3 x y z = n
        [fx,fy,fz] = map (\a -> if a > 0 then max else min) [x,y,z]

frustum :: Float -> Float -> Float -> Float -> Vec3 -> Vec3 -> Vec3 -> Frustum
frustum angle ratio nearD farD p l u = Frustum [ (pl ntr ntl ftl)
                                               , (pl nbl nbr fbr)
                                               , (pl ntl nbl fbl)
                                               , (pl nbr ntr fbr)
                                               , (pl ntl ntr nbr)
                                               , (pl ftr ftl fbl)
                                               ] ntl ntr nbl nbr ftl ftr fbl fbr
  where
    pl a b c = (n,d)
      where
        n = normalize $ (c - b) `crossprod` (a - b)
        d = -(n `dotprod` b)
    m a v = scalarMul a v
    ang2rad = pi / 180
    tang    = tan $ angle * ang2rad * 0.5
    nh  = nearD * tang
    nw  = nh * ratio
    fh  = farD * tang
    fw  = fh * ratio
    z   = normalize $ p - l
    x   = normalize $ u `crossprod` z
    y   = z `crossprod` x

    nc  = p - m nearD z
    fc  = p - m farD z

    ntl = nc + m nh y - m nw x
    ntr = nc + m nh y + m nw x
    nbl = nc - m nh y - m nw x
    nbr = nc - m nh y + m nw x

    ftl = fc + m fh y - m fw x
    ftr = fc + m fh y + m fw x
    fbl = fc - m fh y - m fw x
    fbr = fc - m fh y + m fw x
