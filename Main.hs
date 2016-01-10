{-# LANGUAGE ViewPatterns, OverloadedStrings, PackageImports, CPP #-}

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Control.Applicative hiding (Const)
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (ByteString)
import Data.Char
import Data.IORef
import Data.List (isPrefixOf,partition)
import Data.Maybe
import Data.Trie (Trie)
import Data.Vect
import Data.Vect.Float.Instances ()
import Data.Word
import Data.Map (Map)
import qualified Data.Map as Map
import FRP.Elerea.Param
import System.Directory
import System.Environment
import System.FilePath
import Data.Binary (encodeFile,decodeFile)
import qualified Data.ByteString.Char8 as SB
import qualified Data.Set as Set
import qualified Data.Trie as T
import qualified Data.Trie.Internal as T
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV

import Debug.Trace
import Text.Show.Pretty

import Graphics.GL.Core32

import Data.Digest.CRC32
import Codec.Picture

import LambdaCube.GL as GL
import LambdaCube.GL.Mesh
--import IR as IR
import LambdaCube.Compiler.Driver

--import Effect

import BSP
import Camera
--import Graphics
import Material
import Render
import ShaderParser
import Zip

import Items
import qualified MD3 as MD3

#ifdef CAPTURE
import Codec.Image.DevIL
import Text.Printf
import Foreign
#endif

type Sink a = a -> IO ()

uniformFTexture2D' n s v = do
  putStrLn $ "uniformFTexture2D: " ++ show n
  uniformFTexture2D n s v

-- Utility code
tableTexture :: [Float] -> String -> Map String InputSetter -> IO ()
tableTexture t n s = do
    let width       = length t
        v           = V.fromList t
        bitmap x y  = let a = floor $ min 255 $ max 0 $ 128 + 128 * v V.! x in PixelRGB8 a a a
        texture     = uniformFTexture2D' n s

    tex <- uploadTexture2DToGPU' False False False $ ImageRGB8 $ generateImage bitmap width 1
    texture tex

setupTables :: Map String InputSetter -> IO ()
setupTables s = do
    let funcTableSize = 1024 :: Float
        sinTexture              = [sin (i*2*pi/(funcTableSize-1)) | i <- [0..funcTableSize-1]]
        squareTexture           = [if i < funcTableSize / 2 then 1 else -1 | i <- [0..funcTableSize-1]]
        sawToothTexture         = [i / funcTableSize | i <- [0..funcTableSize-1]]
        inverseSawToothTexture  = reverse [i / funcTableSize | i <- [0..funcTableSize-1]]
        triangleTexture         = l1 ++ map ((-1)*) l1
          where
            n = funcTableSize / 4
            l0 = [i / n | i <- [0..n-1]]
            l1 = l0 ++ reverse l0
    
    tableTexture sinTexture "SinTable" s
    tableTexture squareTexture "SquareTable" s
    tableTexture sawToothTexture "SawToothTable" s
    tableTexture inverseSawToothTexture "InverseSawToothTable" s
    tableTexture triangleTexture "TriangleTable" s

#ifdef CAPTURE
-- framebuffer capture function
withFrameBuffer :: Int -> Int -> Int -> Int -> (Ptr Word8 -> IO ()) -> IO ()
withFrameBuffer x y w h fn = allocaBytes (w*h*4) $ \p -> do
    glReadPixels (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) GL_RGBA GL_UNSIGNED_BYTE $ castPtr p
    fn p
#endif

captureRate :: Double
captureRate = 30

main :: IO ()
main = do
#ifdef CAPTURE
    ilInit
#endif
    ar <- loadArchive

    let imageShader txName = defaultCommonAttrs {caStages = sa:saLM:[]}
          where
            sa = defaultStageAttrs
                { saTexture     = ST_Map txName
                , saBlend       = Nothing
                , saTCGen       = TG_Base
                , saDepthWrite  = True
                , saRGBGen      = RGB_IdentityLighting
                }
            saLM = defaultStageAttrs
                { saTexture = ST_Lightmap
                , saBlend   = Just (B_DstColor,B_Zero)
                , saTCGen   = TG_Lightmap
                , saRGBGen  = RGB_IdentityLighting
                }

    args <- getArgs
    let bspMap = T.fromList [(SB.pack $ takeBaseName n, decompress' e) | e <- ar, let n = eFilePath e, ".bsp" == takeExtensionCI n, isPrefixOfCI "maps" n]
        bspName = case args of
            []     -> head $ T.keys bspMap
            (n:xs) -> SB.pack n
        Just bspData = T.lookup bspName bspMap
        bsp = readBSP bspData
        maxMaterial = 20 -- TODO: remove if we will have fast reducer
        shNames = Set.fromList $ Prelude.take maxMaterial [n | n <- map shName $ V.toList $ blShaders bsp, SB.isInfixOf "floor" n || SB.isInfixOf "wall" n || SB.isInfixOf "door" n]

    mapM_ SB.putStrLn $ map shName $ V.toList $ blShaders bsp
    shMap' <- do
      hasShaderCache <- doesFileExist "q3shader.cache"
      case hasShaderCache of
        True -> putStrLn "load shader cache" >> decodeFile "q3shader.cache"
        False -> let sm = shaderMap ar in putStrLn "create shader cache" >> encodeFile "q3shader.cache" sm >> return sm
    let (normalShNames,textureShNames) = partition (\n -> T.member n shMap') $ Set.toList shNames
        normalShNameSet     = Set.fromList normalShNames
        textureShNameSet    = Set.fromList textureShNames
        normalShMap     = T.mapBy (\n sh -> if Set.member n normalShNameSet then Just sh else Nothing) shMap'
        --textureShMap    = T.fromList [(n,defaultCommonAttrs {caStages = [defaultStageAttrs {saTexture = ST_Map n, saDepthWrite = True}]}) | n <- Set.toList textureShNameSet]
        textureShMap    = T.fromList [(n,imageShader n) | n <- Set.toList textureShNameSet]
        shMap = T.unionL normalShMap textureShMap
        shMapTexSlot = mangleCA <$> shMap
          where
            mangleStageTex stageTex = SB.pack $ "Tex_" ++ show (crc32 $ SB.pack $ show stageTex)
            mangleCA ca = ca {caStages = mangleSA <$> caStages ca}
            mangleSA sa = sa {saTextureUniform = mangleStageTex sa}
        textureUniforms = map SB.unpack . Set.toList . Set.fromList . concat . map name . concat . map caStages $ T.elems shMapTexSlot
          where
            name s = [saTextureUniform s]
            {-
            name s = case saTexture s of
              ST_Map n        -> [n]
              ST_ClampMap n   -> [n]
              ST_AnimMap _ n  -> [head n]
              ST_Lightmap     -> ["LightMap"]
              ST_WhiteImage   -> []
            -}
        -- create gfx network to render active materials
        {-
        TODO: gfx network should be created from shaderMap and bsp
          shader data source
            - shader descriptor
            - image file: tga or jpg
        -}
        {-
        lcnet :: Exp Obj (Image 1 V4F)
        lcnet = PrjFrameBuffer "outFB" tix0 $ q3GFX $ T.toList shMap
        -}
        -- extract spawn points
        ents = parseEntities (SB.unpack bspName) $ blEntities bsp
        spawn e = case T.lookup "classname" e of
            Just "info_player_deathmatch"   -> True
            Just "info_player_start"        -> True
            Just "info_player_intermission" -> True
            _                               -> False
        Just sp0 = T.lookup "origin" $ head $ filter spawn ents
        [x0,y0,z0] = map read $ words $ SB.unpack sp0
        p0 = Vec3 x0 y0 z0


    putStrLn $ "all materials:  " ++ show (T.size shMap')
    putStrLn $ "used materials: " ++ show (T.size shMap)
    putStrLn $ "texture uniforms: \n" ++ ppShow textureUniforms
    writeFile "SampleMaterial.lc" $ unlines
      [ "module SampleMaterial where"
      , "import Material"
      , "sampleMaterial ="
      , unlines . map ("  "++) . lines . ppShow . T.toList $ shMapTexSlot
      ]
    win <- initWindow "LC DSL Quake 3 Demo" 800 600
    let keyIsPressed k = fmap (==KeyState'Pressed) $ getKey win k

    -- CommonAttrs
    let quake3SlotSchema =
          ObjectArraySchema Triangles $ Map.fromList
            [ ("color",       Attribute_V4F)
            , ("diffuseUV",   Attribute_V2F)
            , ("normal",      Attribute_V3F)
            , ("position",    Attribute_V3F)
            , ("lightmapUV",  Attribute_V2F)
            ]
        inputSchema = {-TODO-}
          PipelineSchema
          { objectArrays = Map.fromList $ zip ("missing shader" : map SB.unpack (T.keys shMap)) (repeat quake3SlotSchema)
          , uniforms = Map.fromList $ [ ("viewProj",    M44F)
                                    , ("worldMat",      M44F)
                                    , ("viewMat",       M44F)
                                    , ("orientation",   M44F)
                                    , ("viewOrigin",    V3F)
                                    , ("entityRGB",     V3F)
                                    , ("entityAlpha",   GL.Float)
                                    , ("identityLight", GL.Float)
                                    , ("time",          GL.Float)
                                    , ("LightMap",      FTexture2D)
                                    , ("SinTable",             FTexture2D)
                                    , ("SquareTable",          FTexture2D)
                                    , ("SawToothTable",        FTexture2D)
                                    , ("InverseSawToothTable", FTexture2D)
                                    , ("TriangleTable",        FTexture2D)
                                    ] ++ zip textureUniforms (repeat FTexture2D)
          }
    storage <- allocStorage inputSchema
    print "storage created"
    --print $ slotUniform storage
    --print $ slotStream storage
    --initUtility storage

    let slotU           = uniformSetter storage
        entityRGB       = uniformV3F "entityRGB" slotU
        entityAlpha     = uniformFloat "entityAlpha" slotU
        identityLight   = uniformFloat "identityLight" slotU
        worldMat        = uniformM44F "worldMat" slotU
        overbrightBits  = 0
        idmtx = V4 (V4 1 0 0 0) (V4 0 1 0 0) (V4 0 0 1 0) (V4 0 0 0 1)
    worldMat idmtx
    entityRGB $ V3 1 1 1
    entityAlpha 1
    identityLight $ 1 / (2 ^ overbrightBits)
    setupTables slotU

    putStrLn "loading textures:"
    -- load textures
    let archiveTrie     = T.fromList [(SB.pack $ eFilePath a,a) | a <- ar]
        redBitmap x y   = let v = if (x+y) `mod` 2 == 0 then 255 else 0 in PixelRGB8 v v 0

    defaultTexture <- uploadTexture2DToGPU' False True False $ ImageRGB8 $ generateImage redBitmap 32 32
    animTex <- fmap concat $ forM (Set.toList $ Set.fromList $ concatMap (\sh -> [(saTexture sa,saTextureUniform sa,caNoMipMaps sh) | sa <- caStages sh]) $ T.elems shMapTexSlot) $
      \(stageTex,SB.unpack -> texSlotName,noMip) -> do
        let setTex isClamped img  = uniformFTexture2D' texSlotName slotU =<< loadQ3Texture (not noMip) isClamped defaultTexture archiveTrie img
        case stageTex of
            ST_Map img          -> setTex False img >> return []
            ST_ClampMap img     -> setTex True img >> return []
            ST_AnimMap t imgs   -> do
                txList <- mapM (loadQ3Texture (not noMip) False defaultTexture archiveTrie) imgs
                --return [(1 / t / fromIntegral (length imgs),cycle $ zip (repeat (uniformFTexture2D' texSlotName slotU)) txList)]
                return [(1/t,cycle $ zip (repeat (uniformFTexture2D' texSlotName slotU)) txList)]
            _ -> return []

    putStrLn $ "loading: " ++ show bspName
    objs <- addBSP storage bsp
{-
    -- setup menu
    levelShots <- sequence [(n,) <$> loadQ3Texture True True defaultTexture archiveTrie (SB.append "levelshots/" n) | n <- T.keys bspMap]
    menuObj <- addMesh menuRenderer "postSlot" compiledQuad ["ScreenQuad"]
    let menuObjUnis = objectUniformSetter menuObj
    --uniformFTexture2D' "ScreenQuad" menuObjUnis defaultTexture
    uniformFTexture2D' "ScreenQuad" menuObjUnis $ snd $ head levelShots
-}
    -- add entities
{-
data Item
    = Item
    { itClassName   :: String
    , itPickupSound :: String
    , itWorldModel  :: [String]
    , itIcon        :: String
    , itPickupName  :: String
    , itQuantity    :: Int
    , itType        :: ItemType
    , itTag         :: Tag
    , itPreCaches   :: String
    , itSounds      :: String
    } deriving Show

items =
readMD3 :: LB.ByteString -> MD3Model
-}
    -- load items
    let itemModels = T.fromList [(SB.pack $ itClassName it, [ MD3.readMD3 $ decompress' e | n <- itWorldModel it
                                                            , e <- maybeToList $ T.lookup (SB.pack n) archiveTrie
                                                            ]) | it <- items]
    {-
        "origin" "1012 2090 108"
        "angle" "180"
        "model" "models/mapobjects/visor_posed.md3"
        "classname" "misc_model"
    -}
    {-
        {
        "origin" "1120 2128 16"
        "classname" "item_armor_shard"
        }
    -}
{-
    forM_ ents $ \e -> case T.lookup "classname" e of
        Nothing -> return ()
        Just k  -> case T.lookup k itemModels of
            Just ml -> do
                putStrLn $ "add model: " ++ SB.unpack k
                let Just o = T.lookup "origin" e
                    [x,y,z] = map read $ words $ SB.unpack o
                    p = Vec3 x y z
                forM_ ml $ \md3 -> do
                    lcmd3 <- addMD3 storage md3 ["worldMat"]
                    forM_ (lcmd3Object lcmd3) $ \obj -> do
                        let unis    = objectUniformSetter $  obj
                            woldMat = uniformM44F "worldMat" unis
                            sm = fromProjective (scaling $ Vec3 s s s)
                            s  = 0.005 / 64 * 2 -- FIXE: what is the correct value?
                        woldMat $ mat4ToM44F $ sm .*. (fromProjective $ translation p)
            Nothing -> when (k == "misc_model") $ case T.lookup "model" e of
                Nothing -> return ()
                Just m  -> do
                    -- TODO
                    return ()
-}
    (mousePosition,mousePositionSink) <- external (0,0)
    (fblrPress,fblrPressSink) <- external (False,False,False,False,False)
    (capturePress,capturePressSink) <- external False
    (waypointPress,waypointPressSink) <- external []
    rendererRef <- newIORef =<< fromJust <$> loadQuake3Graphics storage

    let draw captureA = readIORef rendererRef >>= renderFrame >> captureA >> swapBuffers win >> pollEvents

    capRef <- newIORef False
    sc <- start $ do
        anim <- animateMaps animTex
        u <- scene win bsp objs (setScreenSize storage) p0 slotU mousePosition fblrPress anim capturePress waypointPress capRef
        return $ (draw <$> u)
    setTime 0
    s <- fpsState
    driveNetwork sc (readInput rendererRef storage win s mousePositionSink fblrPressSink capturePressSink waypointPressSink capRef)

    disposeRenderer =<< readIORef rendererRef
    print "storage destroyed"

    destroyWindow win

animateMaps :: [(Float, [(SetterFun TextureData, TextureData)])] -> SignalGen Float (Signal [(Float, [(SetterFun TextureData, TextureData)])])
animateMaps l0 = stateful l0 $ \dt l -> zipWith (f $ dt * timeScale) l timing
  where
    timeScale = 1
    timing  = map fst l0
    f :: Float -> (Float,[(SetterFun TextureData,TextureData)]) -> Float -> (Float,[(SetterFun TextureData,TextureData)])
    f dt (t,a) t0
        | t - dt <= 0   = (t-dt+t0,tail a)
        | otherwise     = (t-dt,a)

edge :: Signal Bool -> SignalGen p (Signal Bool)
edge s = transfer2 False (\_ cur prev _ -> cur && not prev) s =<< delay False s

scene :: Window
      -> BSPLevel
      -> V.Vector Object
      -> (Word -> Word -> IO ())
      -> Vec3
      -> Map String InputSetter
      -> Signal (Float, Float)
      -> Signal (Bool, Bool, Bool, Bool, Bool)
      -> Signal [(Float, [(SetterFun TextureData, TextureData)])]
      -> Signal Bool
      -> Signal [Bool]
      -> IORef Bool
      -> SignalGen Float (Signal (IO ()))
scene win bsp objs setSize p0 slotU mousePosition fblrPress anim capturePress waypointPress capRef = do
    time <- stateful 0 (+)
    last2 <- transfer ((0,0),(0,0)) (\_ n (_,b) -> (b,n)) mousePosition
    let mouseMove = (\((ox,oy),(nx,ny)) -> (nx-ox,ny-oy)) <$> last2
    controlledCamera <- userCamera p0 mouseMove fblrPress

    frameCount <- stateful (0 :: Int) (\_ c -> c + 1)
    capture <- transfer2 False (\_ cap cap' on -> on /= (cap && not cap')) capturePress =<< delay False capturePress
    
    [clearWaypoints, setWaypoint, stopPlayback, startPlayback, incPlaybackSpeed, decPlaybackSpeed] <-
        forM (zip [edge, edge, edge, edge, return, return] [0..]) $ \(process, i) -> process (fmap (!! i) waypointPress)

    waypoints <- recordSignalSamples setWaypoint clearWaypoints ((\(camPos, targetPos, _) -> (camPos, targetPos)) <$> controlledCamera)
    playbackSpeed <- transfer2 100 (\dt inc dec speed -> speed + 10*dt*(if inc then 1 else if dec then -1 else 0)) incPlaybackSpeed decPlaybackSpeed
    splineCamera <- playbackCamera startPlayback stopPlayback playbackSpeed waypoints
    
    let activeCamera = do
            camData <- splineCamera
            case camData of
                Nothing -> controlledCamera
                Just camData -> return camData

    let matSetter   = uniformM44F "viewProj" slotU
        viewOrigin  = uniformV3F "viewOrigin" slotU
        orientation = uniformM44F "orientation" slotU
        viewMat     = uniformM44F "viewMat" slotU
        timeSetter  = uniformFloat "time" slotU
        setupGFX (camPos,camTarget,camUp) time (anim,capturing,frameCount) = do
            (w,h) <- getWindowSize win
            let cm = fromProjective (lookat camPos camTarget camUp)
                pm = perspective 0.01 150 (pi/3) (fromIntegral w / fromIntegral h)
                sm = fromProjective (scaling $ Vec3 s s s)
                s  = 0.005
                V4 orientA orientB orientC _ = mat4ToM44F $! cm .*. sm
                Vec3 cx cy cz = camPos
                near = 0.01/s
                far  = 150/s
                fovDeg = 90
                frust = frustum fovDeg (fromIntegral w / fromIntegral h) near far camPos camTarget camUp
            timeSetter $ time / 1
            --putStrLn $ "time: " ++ show time ++ " " ++ show capturing
            viewOrigin $ V3 cx cy cz
            viewMat $ mat4ToM44F cm
            --orientation $ V4 orientA orientB orientC $ V4 0 0 0 1
            matSetter $! mat4ToM44F $! cm .*. sm .*. pm
            forM_ anim $ \(_,a) -> let (s,t) = head a in s t
            setSize (fromIntegral w) (fromIntegral h)
            cullSurfaces bsp camPos frust objs
            return $ do
#ifdef CAPTURE
                when capturing $ do
                    glFinish
                    withFrameBuffer 0 0 w h $ \p -> writeImageFromPtr (printf "frame%08d.jpg" frameCount) (h,w) p
                writeIORef capRef capturing
#endif
                return ()
    r <- effectful3 setupGFX activeCamera time ((,,) <$> anim <*> capture <*> frameCount)
    return r

--vec4ToV4F :: Vec4 -> V4F
vec4ToV4F (Vec4 x y z w) = V4 x y z w

--mat4ToM44F :: Mat4 -> M44F
mat4ToM44F (Mat4 a b c d) = V4 (vec4ToV4F a) (vec4ToV4F b) (vec4ToV4F c) (vec4ToV4F d)

readInput :: IORef GLRenderer
          -> GLStorage
          -> Window
          -> State
          -> Sink (Float, Float)
          -> Sink (Bool, Bool, Bool, Bool, Bool)
          -> Sink Bool
          -> Sink [Bool]
          -> IORef Bool
          -> IO (Maybe Float)
readInput rendererRef storage win s mousePos fblrPress capturePress waypointPress capRef = do
    let keyIsPressed k = fmap (==KeyState'Pressed) $ getKey win k
    t <- maybe 0 id <$> getTime
    setTime 0

    (x,y) <- getCursorPos win
    mousePos (realToFrac x,realToFrac y)

    fblrPress =<< ((,,,,) <$> keyIsPressed Key'Left <*> keyIsPressed Key'Up <*> keyIsPressed Key'Down <*> keyIsPressed Key'Right <*> keyIsPressed Key'RightShift)
    capturePress =<< keyIsPressed Key'P
    waypointPress =<< mapM keyIsPressed [Key'R,Key'E,Key'1,Key'2,Key'F,Key'G]

    isCapturing <- readIORef capRef
    let dt = if isCapturing then recip captureRate else realToFrac t

    reload <- keyIsPressed Key'L
    when reload $ do
      r <- loadQuake3Graphics storage
      case r of
        Nothing -> return ()
        Just a  -> do
          readIORef rendererRef >>= disposeRenderer
          writeIORef rendererRef a

    updateFPS s dt
    k <- keyIsPressed Key'Escape
    return $ if k then Nothing else Just (realToFrac dt)

-- FRP boilerplate
driveNetwork :: (p -> IO (IO a)) -> IO (Maybe p) -> IO ()
driveNetwork network driver = do
    dt <- driver
    case dt of
        Just dt -> do
            join $ network dt
            driveNetwork network driver
        Nothing -> return ()

-- OpenGL/GLFW boilerplate

initWindow :: String -> Int -> Int -> IO Window
initWindow title width height = do
    GLFW.init
    defaultWindowHints
    mapM_ windowHint
      [ WindowHint'ContextVersionMajor 3
      , WindowHint'ContextVersionMinor 3
      , WindowHint'OpenGLProfile OpenGLProfile'Core
      , WindowHint'OpenGLForwardCompat True
      ]
    Just win <- createWindow width height title Nothing Nothing
    makeContextCurrent $ Just win
    glEnable GL_FRAMEBUFFER_SRGB

    return win

-- FPS tracking

data State = State { frames :: IORef Int, t0 :: IORef Double }

fpsState :: IO State
fpsState = State <$> newIORef 0 <*> newIORef 0

updateFPS :: State -> Double -> IO ()
updateFPS state t1 = do
    let t = 1000*t1
        fR = frames state
        tR = t0 state
    modifyIORef fR (+1)
    t0' <- readIORef tR
    writeIORef tR $ t0' + t
    when (t + t0' >= 5000) $ do
    f <- readIORef fR
    let seconds = (t + t0') / 1000
        fps = fromIntegral f / seconds
    putStrLn (show (round fps) ++ " FPS - " ++ show f ++ " frames in " ++ show seconds)
    writeIORef tR 0
    writeIORef fR 0

{-
-- Continuous camera state (rotated with mouse, moved with arrows)
userCamera :: Real p => Vec3 -> Signal (Float, Float) -> Signal (Bool, Bool, Bool, Bool, Bool)
           -> SignalGen p (Signal (Vec3, Vec3, Vec3))
userCamera p mposs keyss = fmap (\(pos,target,up,_) -> (pos,target,up)) <$> transfer2 (p,zero,zero,(0,0)) calcCam mposs keyss
  where
    d0 = Vec4 0 (-1) 0 1
    u0 = Vec4 0 0 (-1) 1
    calcCam dt (dmx,dmy) (ka,kw,ks,kd,turbo) (p0,_,_,(mx,my)) = (p',p'+d,u,(mx',my'))
      where
        f0 c n = if c then (&+ n) else id
        p'  = foldr1 (.) [f0 ka (v &* (-t)),f0 kw (d &* t),f0 ks (d &* (-t)),f0 kd (v &* t)] p0
        k   = if turbo then 500 else 100
        t   = k * realToFrac dt
        mx' = dmx + mx
        my' = dmy + my
        rm  = fromProjective $ rotationEuler $ Vec3 (mx' / 100) (my' / 100) 0
        d   = trim $ rm *. d0 :: Vec3
        u   = trim $ rm *. u0 :: Vec3
        v   = normalize $ d &^ u
-}

-- | Perspective transformation matrix in row major order.
perspective :: Float  -- ^ Near plane clipping distance (always positive).
            -> Float  -- ^ Far plane clipping distance (always positive).
            -> Float  -- ^ Field of view of the y axis, in radians.
            -> Float  -- ^ Aspect ratio, i.e. screen's width\/height.
            -> Mat4
perspective n f fovy aspect = transpose $
    Mat4 (Vec4 (2*n/(r-l))       0       (-(r+l)/(r-l))        0)
         (Vec4     0        (2*n/(t-b))  ((t+b)/(t-b))         0)
         (Vec4     0             0       (-(f+n)/(f-n))  (-2*f*n/(f-n)))
         (Vec4     0             0            (-1)             0)
  where
    t = n*tan(fovy/2)
    b = -t
    r = aspect*t
    l = -r

-- | Pure orientation matrix defined by Euler angles.
rotationEuler :: Vec3 -> Proj4
rotationEuler (Vec3 a b c) = orthogonal $ toOrthoUnsafe $ rotMatrixZ a .*. rotMatrixX b .*. rotMatrixY (-c)

-- | Camera transformation matrix.
lookat :: Vec3   -- ^ Camera position.
       -> Vec3   -- ^ Target position.
       -> Vec3   -- ^ Upward direction.
       -> Proj4
lookat pos target up = translateBefore4 (neg pos) (orthogonal $ toOrthoUnsafe r)
  where
    w = normalize $ pos &- target
    u = normalize $ up &^ w
    v = w &^ u
    r = transpose $ Mat3 u v w

-- pk3 handling

takeExtensionCI = map toLower . takeExtension
isPrefixOfCI a b = isPrefixOf a $ map toLower b

loadArchive :: IO Archive
loadArchive = concat <$> (mapM readArchive =<< filter (\n -> ".pk3" == takeExtensionCI n) <$> getDirectoryContents ".")

shaderMap :: Archive -> T.Trie CommonAttrs
shaderMap ar = T.fromList $ concat [eval n $ parse shaders d | (n,d) <- l]
  where
    l = [(n,decompress e) | e <- ar, let n = eFilePath e, ".shader" == takeExtensionCI n, isPrefixOfCI "scripts" n]
    eval n f = case f of
        Done "" r   -> r
        Done rem r  -> error $ show (n,"Input is not consumed", rem, map fst r)
        Fail _ c _  -> error $ show (n,"Fail",c)
        Partial f'  -> eval n (f' "")

parseEntities :: String -> SB.ByteString -> [T.Trie SB.ByteString]
parseEntities n s = eval n $ parse entities s
  where
    eval n f = case f of
        Done "" r   -> r
        Done rem r  -> error $ show (n,"Input is not consumed", rem, r)
        Fail _ c _  -> error $ show (n,"Fail",c)
        Partial f'  -> eval n (f' "")



loadQ3Texture :: Bool -> Bool -> TextureData -> Trie Entry -> ByteString -> IO TextureData
loadQ3Texture isMip isClamped defaultTex ar name = do

    let name' = SB.unpack name
        n1 = SB.pack $ replaceExtension name' "tga"
        n2 = SB.pack $ replaceExtension name' "jpg"
        b0 = T.member name ar
        b1 = T.member n1 ar
        b2 = T.member n2 ar
        fname   = if b0 then name else if b1 then n1 else n2
    case T.lookup fname ar of
        Nothing -> putStrLn ("    unknown: " ++ show fname) >> return defaultTex
        Just d  -> do
            let eimg = decodeImage $ decompress d
            putStrLn $ "  load: " ++ SB.unpack fname
            case eimg of
                Left msg    -> putStrLn ("    error: " ++ msg) >> return defaultTex
                Right img   -> uploadTexture2DToGPU' True isMip isClamped img


loadQuake3Graphics storage = do
  let srcName = "Graphics"
  putStrLn "compile quake3 graphics pipeline"
  pplRes <- compileMain ["."] OpenGL33 srcName
  case pplRes of
    Left err -> putStrLn ("error: " ++ err) >> return Nothing
    Right ppl -> do
      writeFile "quake3.pipeline" $ ppUnlines $ ppShow ppl
      renderer <- allocRenderer ppl
      setStorage renderer storage
      sortSlotObjects storage
      putStrLn "reloaded"
      return $ Just renderer

ppUnlines :: String -> String
ppUnlines [] = []
ppUnlines ('"':xs) | isMultilineString xs = "unlines\n    [ \"" ++ go xs
  where go ('\\':'n':xs) = "\"\n    , \"" ++ go xs
        go ('\\':c:xs) = '\\':c:go xs
        go ('"':xs) = "\"\n    ]" ++ ppUnlines xs
        go (x:xs) = x : go xs

        isMultilineString ('\\':'n':xs) = True
        isMultilineString ('\\':c:xs) = isMultilineString xs
        isMultilineString ('"':xs) = False
        isMultilineString (x:xs) = isMultilineString xs
        isMultilineString [] = False

ppUnlines (x:xs) = x : ppUnlines xs
