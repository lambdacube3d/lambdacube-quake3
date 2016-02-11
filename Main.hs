{-# LANGUAGE ViewPatterns, OverloadedStrings, PackageImports, CPP, LambdaCase, TupleSections, RecordWildCards #-}

import System.IO
import Data.Time.Clock
import Text.Printf
import Data.Aeson (encode,eitherDecode)

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Control.Applicative hiding (Const)
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (ByteString)
import Data.Char
import Data.IORef
import Data.List (isPrefixOf,partition,isInfixOf,elemIndex)
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
import qualified Data.ByteString.Lazy as LB
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
import LambdaCube.Compiler hiding (ppShow)

--import Effect

import BSP
import Camera
--import Graphics
import Material hiding (Vec3)
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
tableTexture :: [Float] -> GLUniformName -> Map GLUniformName InputSetter -> IO ()
tableTexture t n s = do
    let width       = length t
        v           = V.fromList t
        bitmap x y  = let a = floor $ min 255 $ max 0 $ 128 + 128 * v V.! x in PixelRGB8 a a a
        texture     = uniformFTexture2D n s

    tex <- uploadTexture2DToGPU' False False False $ ImageRGB8 $ generateImage bitmap width 1
    texture tex

setupTables :: Map GLUniformName InputSetter -> IO ()
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

loadPK3 :: IO (Map String Entry)
loadPK3 = do
  let takeExtensionCI = map toLower . takeExtension
      isPrefixOfCI a b = isPrefixOf a $ map toLower b
  Map.unions <$> (mapM readArchive =<< filter (\n -> ".pk3" == takeExtensionCI n) <$> getDirectoryContents ".")

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
#ifdef CAPTURE
    ilInit
#endif

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

    pk3Data <- loadPK3
    args <- getArgs
    let bspNames = [n | n <- Map.keys pk3Data, ".bsp" == takeExtension n]
    fullBSPName <- head <$> case args of
      (n:xs) -> return $ filter (isInfixOf n) bspNames
      _ -> do
            let maps = map takeBaseName bspNames
            putStrLn $ "Available maps:"
            putStrLn $ unwords maps
            putStrLn "Enter map name:"
            name <- getLine
            return $ filter (isInfixOf name) bspNames
    let bspName = takeBaseName fullBSPName
        bspEntry = case Map.lookup fullBSPName pk3Data of
            Nothing -> error "You need to put pk3 file into your current directory"
            Just bspd -> bspd

    bsp <- readBSP . LB.fromStrict <$> readEntry bspEntry

    -- extract spawn points
    let ents = parseEntities bspName $ blEntities bsp
        spawnPoint e
          | Just classname <- T.lookup "classname" e
          , classname `elem` ["info_player_deathmatch", "info_player_start", "info_player_intermission"]
          , Just origin <- T.lookup "origin" e
          , [x,y,z] <- map read $ words $ SB.unpack origin = [Vec3 x y z]
          | otherwise = []
        spawnPoints = concatMap spawnPoint ents
        p0 = head spawnPoints

    -- MD3 related code
    let itemMap = T.fromList [(SB.pack $ itClassName it,it) | it <- items]

        characterNames = characterNamesFull -- characterNamesDemo
          where
            characterNamesFull = [ "anarki","biker","bitterman","bones","crash","doom","grunt","hunter","keel","klesk","lucy","major","mynx"
                                 , "orbb","ranger","razor","sarge","slash","sorlag","tankjr","uriel","visor","xaero"
                                 ]
            characterNamesDemo = ["major","visor","sarge","grunt"]

        readCharacterModelSkin name part = do
          let fname = "models/players/" ++ name ++ "/" ++ part ++ "_default.skin"
          txt <- case Map.lookup fname pk3Data of
            Nothing -> fail $ "missing skin: " ++ fname
            Just e -> readEntry e
          return $ Map.fromList
            [ (head k,head v)
            | l <- lines $ SB.unpack txt
            , i <- maybeToList $ elemIndex ',' l
            , let (words . map toLower -> k,words . map toLower . tail -> v) = splitAt i l
            , not . null $ k
            , not . null $ v
            ]
    characterSkinMap <- Map.fromList <$> sequence
      [ ((name,part),) <$> readCharacterModelSkin name part 
      | name <- characterNames
      , part <- ["head","upper","lower"]
      ]
    let mkWorldMat x y z = translation $ Vec3 x y z
        characterModelSkin name part = case Map.lookup (name,part) characterSkinMap of
          Nothing -> error $ unwords ["missing skin for", name, "body part", part]
          Just skin -> skin

        characterSkinMaterials = Set.fromList $ concat [map SB.pack . Map.elems $ characterModelSkin name part | name <- characterNames, part <- ["head","upper","lower"]]
        characterObjs = [[(mkWorldMat x (y + 100 * fromIntegral i) z, m) | m <- ml] | (i,ml) <- zip [0..] characterModels]
          where
            Vec3 x y z = p0
            characterModels = [[(characterModelSkin name part,"models/players/" ++ name ++ "/" ++ part ++ ".md3") | part <- ["head","upper","lower"]] | name <- characterNames]

    characters <- sequence
      [ parseCharacter fname <$> readEntry e
      | name <- characterNames
      , let fname = "models/players/" ++ name ++ "/animation.cfg"
            e = maybe (error $ "missing " ++ fname) id $ Map.lookup fname pk3Data
      ]

    let collectObj e
          | Just classname <- T.lookup "classname" e
          , Just o <- T.lookup "origin" e
          , [x,y,z] <- map (read :: String -> Float) $ words $ SB.unpack o = case T.lookup classname itemMap of
            Just i -> [(mat, (mempty,m)) | m <- Prelude.take cnt $ itWorldModel i, let mat = mkWorldMat x y z]
              where cnt = if itType i `elem` [IT_HEALTH, IT_POWERUP] then 2 else 1
            Nothing -> case T.lookup "model2" e of
              Just m -> [(mkWorldMat x y z, (mempty,SB.unpack m))]
              Nothing -> []
          | otherwise = []
        md3Objs = concatMap collectObj ents
    md3Map <- T.fromList <$> sequence
      [ (\a -> (SB.pack n,MD3.readMD3 $ LB.fromStrict a)) <$> readEntry m
      | n <- Set.toList . Set.fromList . map (snd . snd) $ md3Objs ++ concat characterObjs
      , m <- maybeToList $ Map.lookup n pk3Data
      ]
    let md3Materials = Set.fromList . concatMap (concatMap (map MD3.shName . V.toList . MD3.srShaders) . V.toList . MD3.mdSurfaces) $ T.elems md3Map

    --putStrLn $ "level materials"
    --mapM_ SB.putStrLn $ map shName $ V.toList $ blShaders bsp
    shMap' <- do
      hasShaderCache <- doesFileExist "q3shader.cache"
      case hasShaderCache of
        True -> putStrLn "load shader cache" >> decodeFile "q3shader.cache"
        False -> do
                  putStrLn "create shader cache"
                  sm <- shaderMap pk3Data
                  encodeFile "q3shader.cache" sm
                  return sm
    let mkShader hasLightmap n = case T.lookup n shMap' of
          Just s -> (n,s)
          Nothing -> let alias = SB.pack . dropExtension . SB.unpack $ n in case T.lookup alias shMap' of
            Just s -> (alias,s)
            Nothing -> (n,imageShader hasLightmap n)

        maxMaterial = 50 -- TODO: remove if we will have fast reducer
        shNames = Set.fromList $ {-Prelude.take maxMaterial-} selectedMaterials
        allShName = map shName $ V.toList $ blShaders bsp
        (selectedMaterials,ignoredMaterials) = partition (\n -> or $ True:[SB.isInfixOf k n | k <- ["floor","wall","door","trim","block"]]) allShName

        shMap = T.fromList [mkShader True n | n <- Set.toList shNames] `T.unionL`
                T.fromList [mkShader False n | n <- Set.toList md3Materials] `T.unionL`
                T.fromList [mkShader False n | n <- Set.toList characterSkinMaterials]

    let shMapTexSlot = mangleCA <$> shMap
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

    putStrLn $ "all materials:  " ++ show (T.size shMap')
    --putStrLn $ "used materials: " ++ show (T.size shMap)
    --putStrLn $ "texture uniforms: \n" ++ ppShow textureUniforms
    putStrLn $ "used materials: " ++ show (T.size shMapTexSlot)
    putStrLn $ "ignored materials: " ++ show (length ignoredMaterials)
    writeFile "SampleMaterial.lc" $ unlines
      [ "module SampleMaterial where"
      , "import Material"
      , "sampleMaterial ="
      , unlines . map ("  "++) . lines . ppShow . T.toList $ shMapTexSlot
      ]
    SB.putStrLn $ SB.unlines ignoredMaterials
    let pplName = bspName ++ "_ppl.json"
    q3ppl <- compileQuake3GraphicsCached pplName

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
    putStrLn "storage created"
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
    let redBitmap x y   = let v = if (x+y) `mod` 2 == 0 then 255 else 0 in PixelRGB8 v v 0

    defaultTexture <- uploadTexture2DToGPU' False True False $ ImageRGB8 $ generateImage redBitmap 32 32
    animTex <- fmap concat $ forM (Set.toList $ Set.fromList $ concatMap (\sh -> [(saTexture sa,saTextureUniform sa,caNoMipMaps sh) | sa <- caStages sh]) $ T.elems shMapTexSlot) $
      \(stageTex,texSlotName,noMip) -> do
        let texSetter = uniformFTexture2D texSlotName  slotU
            setTex isClamped img = texSetter =<< loadQ3Texture (not noMip) isClamped defaultTexture pk3Data img
        case stageTex of
            ST_Map img          -> setTex False img >> return []
            ST_ClampMap img     -> setTex True img >> return []
            ST_AnimMap t imgs   -> do
                txList <- mapM (loadQ3Texture (not noMip) False defaultTexture pk3Data) imgs
                return [(1/t,cycle $ zip (repeat texSetter) txList)]
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
    let addMD3Obj (mat,(skin,name)) = case T.lookup (SB.pack name) md3Map of
          Nothing -> return []
          Just md3 -> do
                    putStrLn ("add model: " ++ name)
                    lcmd3 <- addMD3 storage md3 skin ["worldMat"]
                    return [(mat,lcmd3)]

    lcMD3Objs <- concat <$> forM md3Objs addMD3Obj

    -- add characters
    lcCharacterObjs <- forM characterObjs
      (\[(mat,(hSkin,hName)),(_,(uSkin,uName)),(_,(lSkin,lName))] -> do
        let Just hMD3 = T.lookup (SB.pack hName) md3Map
            Just uMD3 = T.lookup (SB.pack uName) md3Map
            Just lMD3 = T.lookup (SB.pack lName) md3Map
        hLC <- addMD3 storage hMD3 hSkin ["worldMat"]
        uLC <- addMD3 storage uMD3 uSkin ["worldMat"]
        lLC <- addMD3 storage lMD3 lSkin ["worldMat"]
        return (mat,(hMD3,hLC),(uMD3,uLC),(lMD3,lLC))
      )

    (mousePosition,mousePositionSink) <- external (0,0)
    (fblrPress,fblrPressSink) <- external (False,False,False,False,False)
    (capturePress,capturePressSink) <- external False
    (waypointPress,waypointPressSink) <- external []
    rendererRef <- newIORef =<< fromJust <$> loadQuake3Graphics storage q3ppl

    let draw captureA = readIORef rendererRef >>= renderFrame >> captureA >> swapBuffers win >> pollEvents

    capRef <- newIORef False
    sc <- start $ do
        anim <- animateMaps animTex
        u <- scene characters lcCharacterObjs lcMD3Objs win bsp objs (setScreenSize storage) p0 slotU mousePosition fblrPress anim capturePress waypointPress capRef
        return $ (draw <$> u)
    s <- fpsState
    setTime 0
    driveNetwork sc (readInput pplName rendererRef storage win s mousePositionSink fblrPressSink capturePressSink waypointPressSink capRef)

    disposeRenderer =<< readIORef rendererRef
    putStrLn "storage destroyed"

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
{-
scene :: Window
      -> BSPLevel
      -> V.Vector Object
      -> (Word -> Word -> IO ())
      -> Vec3
      -> Map GLUniformName InputSetter
      -> Signal (Float, Float)
      -> Signal (Bool, Bool, Bool, Bool, Bool)
      -> Signal [(Float, [(SetterFun TextureData, TextureData)])]
      -> Signal Bool
      -> Signal [Bool]
      -> IORef Bool
      -> SignalGen Float (Signal (IO ()))
-}
scene characters lcCharacterObjs lcMD3Objs win bsp objs setSize p0 slotU mousePosition fblrPress anim capturePress waypointPress capRef = do
    time <- stateful 0 (+)
    last2 <- transfer ((0,0),(0,0)) (\_ n (_,b) -> (b,n)) mousePosition
    let mouseMove = (\((ox,oy),(nx,ny)) -> (nx-ox,ny-oy)) <$> last2
    controlledCamera <- userCamera bsp p0 mouseMove fblrPress

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
        setupGFX (camPos,camTarget,camUp) time (anim,capturing,frameCount,(legAnimType,torsoAnimType)) = do
            (w,h) <- getWindowSize win
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
                  legAnim = animationMap Map.! legAnimType
                  legFrame = aFirstFrame legAnim + t `mod` aNumFrames legAnim
                  torsoAnim = animationMap Map.! torsoAnimType
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
            let keyIsPressed k = fmap (==KeyState'Pressed) $ getKey win k
            keyIsPressed (Key'S) >>= \case
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
    let characterAnim = return (LEGS_SWIM,TORSO_STAND2)
    r <- effectful3 setupGFX activeCamera time ((,,,) <$> anim <*> capture <*> frameCount <*> characterAnim)
    return r

--vec4ToV4F :: Vec4 -> V4F
vec4ToV4F (Vec4 x y z w) = V4 x y z w

--mat4ToM44F :: Mat4 -> M44F
mat4ToM44F (Mat4 a b c d) = V4 (vec4ToV4F a) (vec4ToV4F b) (vec4ToV4F c) (vec4ToV4F d)

readInput :: String
          -> IORef GLRenderer
          -> GLStorage
          -> Window
          -> State
          -> Sink (Float, Float)
          -> Sink (Bool, Bool, Bool, Bool, Bool)
          -> Sink Bool
          -> Sink [Bool]
          -> IORef Bool
          -> IO (Maybe Float)
readInput pplName rendererRef storage win s mousePos fblrPress capturePress waypointPress capRef = do
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
      -- TODO: multi threaded reloading (compile in a new thread and switch when it is ready)
      r <- loadQuake3Graphics storage =<< compileQuake3Graphics pplName
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

shaderMap :: Map String Entry -> IO (T.Trie CommonAttrs)
shaderMap ar = do
  let eval n f = case f of
        Done "" r   -> r
        Done rem r  -> error $ show (n,"Input is not consumed", rem, map fst r)
        Fail _ c _  -> error $ show (n,"Fail",c)
        Partial f'  -> eval n (f' "")
  T.fromList . concat <$> forM [(n,e) | (n,e) <- Map.toList ar, ".shader" == takeExtension n, isPrefixOf "scripts" n] (\(n,e) -> eval n . parse shaders <$> readEntry e)

parseEntities :: String -> SB.ByteString -> [T.Trie SB.ByteString]
parseEntities n s = eval n $ parse entities s
  where
    eval n f = case f of
        Done "" r   -> r
        Done rem r  -> error $ show (n,"Input is not consumed", rem, r)
        Fail _ c _  -> error $ show (n,"Fail",c)
        Partial f'  -> eval n (f' "")



loadQ3Texture :: Bool -> Bool -> TextureData -> Map String Entry -> ByteString -> IO TextureData
loadQ3Texture isMip isClamped defaultTex ar name' = do
    let name = SB.unpack name'
        n1 = replaceExtension name "tga"
        n2 = replaceExtension name "jpg"
        b0 = Map.member name ar
        b1 = Map.member n1 ar
        b2 = Map.member n2 ar
        fname   = if b0 then name else if b1 then n1 else n2
    case Map.lookup fname ar of
        Nothing -> putStrLn ("    unknown: " ++ fname) >> return defaultTex
        Just entry  -> do
            eimg <- decodeImage <$> readEntry entry
            putStrLn $ "  load: " ++ fname
            case eimg of
                Left msg    -> putStrLn ("    error: " ++ msg) >> return defaultTex
                Right img   -> uploadTexture2DToGPU' True isMip isClamped img

showTime delta
    | t > 1e-1  = printf "%.3fs" t
    | t > 1e-3  = printf "%.1fms" (t/1e-3)
    | otherwise = printf "%.0fus" (t/1e-6)
  where
    t = realToFrac delta :: Double

timeDiff m = (\s x e -> (diffUTCTime e s, x))
  <$> getCurrentTime
  <*> m
  <*> getCurrentTime

printTimeDiff s a = do
  putStr s
  (t,r) <- timeDiff a
  putStrLn $ showTime t
  return r

compileQuake3GraphicsCached name = doesFileExist name >>= \case
  True -> putStrLn "use cached pipeline" >> return (Just name)
  False -> compileQuake3Graphics name

compileQuake3Graphics name = printTimeDiff "compile quake3 graphics pipeline..." $ do
  compileMain ["."] OpenGL33 "Graphics" >>= \case 
    Left err -> putStrLn ("error: " ++ err) >> return Nothing
    Right ppl -> LB.writeFile name (encode ppl) >> return (Just name)

loadQuake3Graphics storage = \case
  Nothing -> return Nothing
  Just name -> do
    putStrLn $ "load " ++ name
    renderer <- printTimeDiff "allocate pipeline..." $ do
      eitherDecode <$> LB.readFile name >>= \case
        Left err -> fail err
        Right ppl -> allocRenderer ppl
    printTimeDiff "setStorage..." $ setStorage renderer storage
    --sortSlotObjects storage
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
