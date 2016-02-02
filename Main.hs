{-# LANGUAGE OverloadedStrings, PackageImports, TupleSections, CPP, DataKinds, ViewPatterns, LambdaCase, RecordWildCards, FlexibleContexts #-}

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Control.Applicative hiding (Const)
import Control.Monad
import Data.Attoparsec.Char8
import Data.ByteString.Char8 (ByteString)
import Data.Char
import Data.IORef
import Data.List (isPrefixOf,partition,elemIndex)
import Data.Maybe
import Data.Trie (Trie)
import Data.Vect
import Data.Vect.Float.Instances ()
import Data.Word
import FRP.Elerea.Param
import System.Directory
import System.Environment
import System.FilePath
import qualified Data.ByteString.Char8 as SB
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Trie as T
import qualified Data.Trie.Internal as T
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import Data.Binary (encodeFile,decodeFile)

import Debug.Trace

import Graphics.Rendering.OpenGL.Raw.Core32

import Data.Bitmap
import Data.Digest.CRC32
import Codec.Image.STB hiding (Image)

import LambdaCube.GL
import LambdaCube.GL.Mesh

import Effect

import BSP
import Camera
import Graphics
import Material
import Render
import ShaderParser
import Zip
import Character

import Items
import qualified MD3 as MD3

#ifdef CAPTURE
import Codec.Image.DevIL
import Text.Printf
import Foreign
#endif

type Sink a = a -> IO ()

-- Utility code
tableTexture :: [Float] -> ByteString -> Trie InputSetter -> IO ()
tableTexture t n s = do
    let width       = length t
        v           = V.fromList t
        bitmap      = createSingleChannelBitmap (width,1) $ \x y -> floor $ min 255 $ max 0 $ 128 + 128 * v V.! x
        oneBitmap   = createSingleChannelBitmap (width,1) $ \x y -> 255
        texture     = uniformFTexture2D n s

    tex <- compileTexture2DRGBAF False False $ combineChannels [bitmap,bitmap,bitmap,oneBitmap]
    texture tex

setupTables :: Trie InputSetter -> IO ()
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
    glReadPixels (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h) gl_RGBA gl_UNSIGNED_BYTE $ castPtr p
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

    let imageShader hasLightmap txName = defaultCommonAttrs {caStages = sa:if hasLightmap then saLM:[] else []}
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
    shMap' <- do
      hasShaderCache <- doesFileExist "q3shader.cache"
      case hasShaderCache of
        True -> putStrLn "load shader cache" >> decodeFile "q3shader.cache"
        False -> let sm = shaderMap ar in putStrLn "create shader cache" >> encodeFile "q3shader.cache" sm >> return sm
    let bspMap = T.fromList [(SB.pack $ takeBaseName n, decompress' e) | e <- ar, let n = eFilePath e, ".bsp" == takeExtensionCI n, isPrefixOfCI "maps" n]
    bspName <- case args of
      (n:xs) -> return $ SB.pack n
      _ -> do
            let maps = map SB.unpack $ T.keys bspMap
            putStrLn $ "Available maps:"
            putStrLn $ unwords maps
            putStrLn "Enter map name:"
            name <- SB.getLine
            return $ if T.member name bspMap then name else SB.pack $ head maps
    let bspData = case T.lookup bspName bspMap of
            Nothing -> error "You need to put pk3 file into your current directory"
            Just bspd -> bspd
        bsp = readBSP bspData
        archiveTrie = T.fromList [(SB.pack $ eFilePath a,a) | a <- ar]
        itemMap = T.fromList [(SB.pack $ itClassName it,it) | it <- items]

        characterNames = {-characterNamesDemo-} characterNamesFull
          where
            characterNamesFull = [ "anarki","biker","bitterman","bones","crash","doom","grunt","hunter","keel","klesk","lucy","major","mynx"
                                 , "orbb","ranger","razor","sarge","slash","sorlag","tankjr","uriel","visor","xaero"
                                 ]
            characterNamesDemo = ["major","visor","sarge","grunt"]
        characterModelSkin name part = T.fromList
            [ (SB.pack . head $ k,SB.pack . head $ v)
            | l <- lines $ SB.unpack $ decompress e
            , i <- maybeToList $ elemIndex ',' l
            , let (words . map toLower -> k,words . map toLower . tail -> v) = splitAt i l
            , not . null $ k
            , not . null $ v
            ]
          where
            Just e = trace n $ T.lookup (SB.pack n) archiveTrie
            n = "models/players/" ++ name ++ "/" ++ part ++ "_default.skin"
        characterSkinMaterials = Set.fromList $ concat [T.elems $ characterModelSkin name part | name <- characterNames, part <- ["head","upper","lower"]]
        characterObjs = [[(mkWorldMat x (y + 100 * fromIntegral i) z, m) | m <- ml] | (i,ml) <- zip [0..] characterModels]
          where
            Vec3 x y z = p0
            characterModels = [[(characterModelSkin name part,"models/players/" ++ name ++ "/" ++ part ++ ".md3") | part <- ["head","upper","lower"]] | name <- characterNames]
        characters = [ parseCharacter fname $ decompress e
                     | name <- characterNames
                     , let fname = "models/players/" ++ name ++ "/animation.cfg"
                           Just e = T.lookup (SB.pack fname) archiveTrie
                     ]

        md3Objs = concatMap collectObj ents
        md3Map = T.fromList [(SB.pack n, MD3.readMD3 $ decompress' m) | n <- Set.toList . Set.fromList . map (snd . snd) $ md3Objs ++ concat characterObjs, m <- maybeToList $ T.lookup (SB.pack n) archiveTrie]
        mkWorldMat x y z = translation $ Vec3 x y z
        collectObj e
          | Just classname <- T.lookup "classname" e
          , Just o <- T.lookup "origin" e
          , [x,y,z] <- map (read :: String -> Float) $ words $ SB.unpack o = case T.lookup classname itemMap of
            Just i -> [(mat, (mempty,m)) | m <- Prelude.take cnt $ itWorldModel i, let mat = mkWorldMat x y z]
              where cnt = if itType i `elem` [IT_HEALTH, IT_POWERUP] then 2 else 1
            Nothing -> case T.lookup "model2" e of
              Just m -> [(mkWorldMat x y z, (mempty,SB.unpack m))]
              Nothing -> []
          | otherwise = []
        md3Materials = Set.fromList . concatMap (concatMap (map MD3.shName . V.toList . MD3.srShaders) . V.toList . MD3.mdSurfaces) $ T.elems md3Map
        shNames = Set.fromList $ map shName (V.toList $ blShaders bsp)
        shMap = T.fromList [mkShader True n | n <- Set.toList shNames] `T.unionL`
                T.fromList [mkShader False n | n <- Set.toList md3Materials] `T.unionL`
                T.fromList [mkShader False n | n <- Set.toList characterSkinMaterials]
        mkShader hasLightmap n = case T.lookup n shMap' of
          Just s -> (n,s)
          Nothing -> let alias = SB.pack . dropExtension . SB.unpack $ n in case T.lookup alias shMap' of
            Just s -> (alias,s)
            Nothing -> (n,imageShader hasLightmap n)
        -- create gfx network to render active materials
        {-
        TODO: gfx network should be created from shaderMap and bsp
          shader data source
            - shader descriptor
            - image file: tga or jpg
        -}
        lcnet :: Exp Obj (Image 1 V4F)
        lcnet = PrjFrameBuffer "outFB" tix0 $ q3GFX $ T.toList shMap

        -- extract spawn points
        ents = parseEntities (SB.unpack bspName) $ blEntities bsp

        spawnPoint e
          | Just classname <- T.lookup "classname" e
          , classname `elem` ["info_player_deathmatch", "info_player_start", "info_player_intermission"]
          , Just origin <- T.lookup "origin" e
          , [x,y,z] <- map read $ words $ SB.unpack origin = [Vec3 x y z]
          | otherwise = []
        spawnPoints = concatMap spawnPoint ents
        p0 = head spawnPoints

    windowSize <- initCommon "LC DSL Quake 3 Demo"

    -- CommonAttrs
    renderer <- compileRenderer $ ScreenOut $ {-fxGamma gamma -}lcnet
    print "renderer created"
    --print $ slotUniform renderer
    --print $ slotStream renderer
    initUtility renderer

    let slotU           = uniformSetter renderer
        draw captureA   = render renderer >> captureA >> swapBuffers
        entityRGB       = uniformV3F "entityRGB" slotU
        entityAlpha     = uniformFloat "entityAlpha" slotU
        identityLight   = uniformFloat "identityLight" slotU
        worldMat        = uniformM44F "worldMat" slotU
        overbrightBits  = 0
    worldMat idmtx'
    entityRGB one'
    entityAlpha 1
    identityLight $ 1 / (2 ^ overbrightBits)
    setupTables slotU

    putStrLn "spawn points:"
    forM_ spawnPoints print
    putStrLn "md3 materials:"
    forM_ md3Materials print
    putStrLn "shaders:"
    forM_ (T.keys shMap) print
    putStrLn "loading textures:"
    -- load textures
    let redBitmap       = createSingleChannelBitmap (2,2) $ \x y -> if (x+y) `mod` 2 == 0 then 255 else 0
        zeroBitmap      = emptyBitmap (2,2) 1
        oneBitmap       = createSingleChannelBitmap (2,2) $ \x y -> 255

    defaultTexture <- compileTexture2DRGBAF' False False False False $ combineChannels [redBitmap,redBitmap,zeroBitmap,oneBitmap]
    animTex <- fmap concat $ forM (Set.toList $ Set.fromList $ map (\(s,m) -> (saTexture s,m)) $
               concatMap (\sh -> [(s,caNoMipMaps sh) | s <- caStages sh]) $ T.elems shMap) $ \(stageTex,noMip) -> do
        let texSlotName = SB.pack $ "Tex_" ++ show (crc32 $ SB.pack $ show stageTex)
            setTexUni = uniformFTexture2D texSlotName slotU
            setTex isClamped img  = setTexUni =<< loadQ3Texture (not noMip) isClamped defaultTexture archiveTrie img
        case stageTex of
            ST_Map img          -> setTex False img >> return []
            ST_ClampMap img     -> setTex True img >> return []
            ST_AnimMap t imgs   -> do
                txList <- mapM (loadQ3Texture (not noMip) False defaultTexture archiveTrie) imgs
                --return [(1 / t / fromIntegral (length imgs),cycle $ zip (repeat (uniformFTexture2D texSlotName slotU)) txList)]
                return [(1/t,cycle $ zip (repeat setTexUni) txList)]
            _ -> return []

    putStrLn $ "loading: " ++ show bspName
    objs <- addBSP renderer bsp
{-
    -- setup menu
    levelShots <- sequence [(n,) <$> loadQ3Texture True True defaultTexture archiveTrie (SB.append "levelshots/" n) | n <- T.keys bspMap]
    menuObj <- addMesh menuRenderer "postSlot" compiledQuad ["ScreenQuad"]
    let menuObjUnis = objectUniformSetter menuObj
    --uniformFTexture2D "ScreenQuad" menuObjUnis defaultTexture
    uniformFTexture2D "ScreenQuad" menuObjUnis $ snd $ head levelShots
-}

    -- add entities
    let addMD3Obj (mat,(skin,name)) = case T.lookup (SB.pack name) md3Map of
          Nothing -> return []
          Just md3 -> do
                    putStrLn ("add model: " ++ name)
                    lcmd3 <- addMD3 renderer md3 skin ["worldMat"]
                    return [(mat,lcmd3)]

    lcMD3Objs <- concat <$> forM md3Objs addMD3Obj

    -- add characters
    lcCharacterObjs <- forM characterObjs
      (\[(mat,(hSkin,hName)),(_,(uSkin,uName)),(_,(lSkin,lName))] -> do
        let Just hMD3 = T.lookup (SB.pack hName) md3Map
            Just uMD3 = T.lookup (SB.pack uName) md3Map
            Just lMD3 = T.lookup (SB.pack lName) md3Map
        hLC <- addMD3 renderer hMD3 hSkin ["worldMat"]
        uLC <- addMD3 renderer uMD3 uSkin ["worldMat"]
        lLC <- addMD3 renderer lMD3 lSkin ["worldMat"]
        return (mat,(hMD3,hLC),(uMD3,uLC),(lMD3,lLC))
      )

    (mousePosition,mousePositionSink) <- external (0,0)
    (fblrPress,fblrPressSink) <- external (False,False,False,False,False)
    (capturePress,capturePressSink) <- external False
    (waypointPress,waypointPressSink) <- external []

    capRef <- newIORef False
    s <- fpsState
    sc <- start $ do
        anim <- animateMaps animTex
        u <- scene characters lcCharacterObjs lcMD3Objs bsp objs (setScreenSize renderer) p0 slotU windowSize mousePosition fblrPress anim capturePress waypointPress capRef
        return $ draw <$> u
    resetTime
    driveNetwork sc (readInput s mousePositionSink fblrPressSink capturePressSink waypointPressSink capRef)

    dispose renderer
    print "renderer destroyed"
    closeWindow

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
{-
scene :: BSPLevel
      -> V.Vector Object
      -> (Word -> Word -> IO ())
      -> Vec3
      -> T.Trie InputSetter
      -> Signal (Int, Int)
      -> Signal (Float, Float)
      -> Signal (Bool, Bool, Bool, Bool, Bool)
      -> Signal [(Float, [(SetterFun TextureData, TextureData)])]
      -> Signal Bool
      -> Signal [Bool]
      -> IORef Bool
      -> SignalGen Float (Signal (IO ()))
-}
scene characters lcCharacterObjs lcMD3Objs bsp objs setSize p0 slotU windowSize mousePosition fblrPress anim capturePress waypointPress capRef = do
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
        setupGFX (w,h) (camPos,camTarget,camUp) time (anim,capturing,frameCount) = do
            let cm = fromProjective (lookat camPos camTarget camUp)
                pm = perspective near far (fovDeg / 180 * pi) (fromIntegral w / fromIntegral h)
                sm = fromProjective (scaling $ Vec3 s s s)
                s  = 0.005
                --V4 orientA orientB orientC _ = mat4ToM44F $! cm .*. sm
                Vec3 cx cy cz = camPos
                near = 0.00001/s
                far  = 100/s
                fovDeg = 60
                frust = frustum fovDeg (fromIntegral w / fromIntegral h) near far camPos camTarget camUp

            forM_ lcMD3Objs $ \(mat,lcmd3) -> do
              forM_ (lcmd3Object lcmd3) $ \obj -> uniformM44F "worldMat" (objectUniformSetter obj) $ mat4ToM44F $ fromProjective $ (rotationEuler (Vec3 time 0 0) .*. mat)

            forM_ (zip characters lcCharacterObjs) $ \(Character{..},(mat,(hMD3,hLC),(uMD3,uLC),(lMD3,lLC))) -> do
              {-
typedef struct {
	vec3_t		origin;
	vec3_t		axis[3];
} orientation_t;

void _VectorCopy( const vec3_t in, vec3_t out );
void _VectorMA( const vec3_t veca, float scale, const vec3_t vecb, vec3_t vecc );
  = vecc[i] = veca[i] + scale*vecb[i]; i={0,1,2}
void MatrixMultiply(float in1[3][3], float in2[3][3], float out[3][3]);

                -- entity, parent, parentModel, parent_tag_name
                CG_PositionRotatedEntityOnTag( &torso, &legs, ci->legsModel, "tag_torso");
                CG_PositionRotatedEntityOnTag( &head, &torso, ci->torsoModel, "tag_head");
              -}
              -- torso = upper
              --  transform torso to legs
              --  transform head to torso (and legs)
              let t = floor $ time * 15
                  legAnim = animationMap Map.! LEGS_IDLE
                  legFrame = aFirstFrame legAnim + t `mod` aNumFrames legAnim
                  torsoAnim = animationMap Map.! TORSO_GESTURE
                  torsoFrame = aFirstFrame torsoAnim + t `mod` aNumFrames torsoAnim

                  tagToMat4 MD3.Tag{..} = translateAfter4 tgOrigin (orthogonal . toOrthoUnsafe $ Mat3 tgAxisX tgAxisY tgAxisZ)
                  hMat = (tagToMat4 $ (MD3.mdTags uMD3 V.! torsoFrame) Map.! "tag_head") .*. uMat
                  uMat = (tagToMat4 $ (MD3.mdTags lMD3 V.! legFrame) Map.! "tag_torso")
                  lMat = one :: Proj4
                  lcMat m = mat4ToM44F $ fromProjective $ m .*. rotationEuler (Vec3 time 0 0) .*. mat
              forM_ (lcmd3Object hLC) $ \obj -> uniformM44F "worldMat" (objectUniformSetter obj) $ lcMat hMat
              forM_ (lcmd3Object uLC) $ \obj -> uniformM44F "worldMat" (objectUniformSetter obj) $ lcMat uMat
              forM_ (lcmd3Object lLC) $ \obj -> uniformM44F "worldMat" (objectUniformSetter obj) $ lcMat lMat
              --setMD3Frame hLC frame
              setMD3Frame uLC torsoFrame
              setMD3Frame lLC legFrame

            timeSetter $ time / 1
            --putStrLn $ "time: " ++ show time ++ " " ++ show capturing
            viewOrigin $ V3 cx cy cz
            viewMat $ mat4ToM44F cm
            --orientation $ V4 orientA orientB orientC $ V4 0 0 0 1
            matSetter $! mat4ToM44F $! cm .*. sm .*. pm
            forM_ anim $ \(_,a) -> let (s,t) = head a in s t
            setSize (fromIntegral w) (fromIntegral h)
            -- hack
            keyIsPressed (CharKey 'S') >>= \case
              True  -> V.forM_ objs $ \obj -> enableObject obj True
              False -> cullSurfaces bsp camPos frust objs
            return $ do
#ifdef CAPTURE
                when capturing $ do
                    glFinish
                    withFrameBuffer 0 0 w h $ \p -> writeImageFromPtr (printf "frame%08d.jpg" frameCount) (h,w) p
                writeIORef capRef capturing
#endif
                return ()
    r <- effectful4 setupGFX windowSize activeCamera time ((,,) <$> anim <*> capture <*> frameCount)
    return r

vec4ToV4F :: Vec4 -> V4F
vec4ToV4F (Vec4 x y z w) = V4 x y z w

mat4ToM44F :: Mat4 -> M44F
mat4ToM44F (Mat4 a b c d) = V4 (vec4ToV4F a) (vec4ToV4F b) (vec4ToV4F c) (vec4ToV4F d)

readInput :: State
          -> Sink (Float, Float)
          -> Sink (Bool, Bool, Bool, Bool, Bool)
          -> Sink Bool
          -> Sink [Bool]
          -> IORef Bool
          -> IO (Maybe Float)
readInput s mousePos fblrPress capturePress waypointPress capRef = do
    t <- getTime
    resetTime

    (x,y) <- getMousePosition
    mousePos (fromIntegral x,fromIntegral y)

    fblrPress =<< ((,,,,) <$> keyIsPressed KeyLeft <*> keyIsPressed KeyUp <*> keyIsPressed KeyDown <*> keyIsPressed KeyRight <*> keyIsPressed KeyRightShift)
    capturePress =<< keyIsPressed (CharKey 'P')
    waypointPress =<< mapM (keyIsPressed . CharKey) "RE12FG"

    isCapturing <- readIORef capRef
    let dt = if isCapturing then recip captureRate else realToFrac t

    updateFPS s dt
    k <- keyIsPressed KeyEsc
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

initCommon :: String -> IO (Signal (Int, Int))
initCommon title = do
    initialize
    openWindow defaultDisplayOptions
        { displayOptions_numRedBits         = 8
        , displayOptions_numGreenBits       = 8
        , displayOptions_numBlueBits        = 8
        , displayOptions_numAlphaBits       = 8
        , displayOptions_numDepthBits       = 24
        , displayOptions_windowIsResizable  = True
        , displayOptions_width              = 1280
        , displayOptions_height             = 720
        , displayOptions_openGLVersion      = (3,2)
        , displayOptions_openGLProfile      = CoreProfile
--        , displayOptions_displayMode    = Fullscreen
        }
    setWindowTitle title

    (windowSize,windowSizeSink) <- external (0,0)
    setWindowSizeCallback $ \w h -> do
        glViewport 0 0 (fromIntegral w) (fromIntegral h)
        putStrLn $ "window size changed " ++ show (w,h)
        windowSizeSink (fromIntegral w, fromIntegral h)

    return windowSize

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
        Nothing -> return defaultTex
        Just d  -> do
            eimg <- decodeImage $ decompress d
            putStrLn $ "  load: " ++ SB.unpack fname
            case eimg of
                Left msg    -> putStrLn ("    error: " ++ msg) >> return defaultTex
                Right img   -> compileTexture2DRGBAF' True True isMip isClamped img
