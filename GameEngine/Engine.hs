{-# LANGUAGE LambdaCase, ViewPatterns, RecordWildCards, TupleSections, PackageImports, OverloadedStrings #-}
module GameEngine.Engine
  ( loadPK3
  , createLoadingScreen
  , drawLoadingScreen
  , engineInit
  , setupStorage
  , updateRenderInput
  -- temp
  , loadQuake3Graphics
  , compileQuake3GraphicsCached
  , getSpawnPoints
  , getBSP
  ) where

import Control.Monad
import Data.Time.Clock
import Text.Printf
import Data.Aeson (encode,eitherDecode)

--import Control.Applicative hiding (Const)
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (ByteString)
import Data.Char
import Data.List (isPrefixOf,partition,isInfixOf,elemIndex)
import Data.Maybe
--import Data.Trie (Trie)
import Data.Vect
import Data.Vect.Float.Instances ()
--import Data.Word
import Data.Map (Map)
import System.FilePath
import System.Directory
import System.Process
import System.Exit

import qualified Data.Map as Map
import Data.Binary (encodeFile,decodeFile)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as SB
import qualified Data.Set as Set
import qualified Data.Trie as T
--import qualified Data.Trie.Internal as T
import qualified Data.Vector as V
--import qualified Data.Vector.Storable as SV

--import Debug.Trace
import Text.Show.Pretty

import Data.Digest.CRC32
import Codec.Picture

import LambdaCube.GL as GL
--import LambdaCube.GL.Mesh

import Control.Exception (evaluate)
import Control.DeepSeq
--import GHC.Generics

import GameEngine.BSP
import GameEngine.Material hiding (Vec3)
import GameEngine.Render
import GameEngine.ShaderParser
import GameEngine.Zip
import GameEngine.Character
import GameEngine.Items
import qualified GameEngine.MD3 as MD3

{-
  TODO:
    remove elerea dep from anim map
-}

------------
-- Game data
------------

loadPK3 :: IO (Map String Entry)
loadPK3 = do
  let takeExtensionCI = map toLower . takeExtension
      isPrefixOfCI a b = isPrefixOf a $ map toLower b
  Map.unions <$> (mapM readArchive =<< filter (\n -> ".pk3" == takeExtensionCI n) <$> getDirectoryContents ".")

mkWorldMat x y z = translation $ Vec3 x y z

readCharacters pk3Data p0 = do
  let --characterNames = characterNamesFull
      characterNames = characterNamesDemo

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

  characterSkinMap <- evaluate =<< (force . Map.fromList <$> sequence
    [ ((name,part),) <$> readCharacterModelSkin name part 
    | name <- characterNames
    , part <- ["head","upper","lower"]
    ])

  let characterModelSkin name part = case Map.lookup (name,part) characterSkinMap of
        Nothing -> error $ unwords ["missing skin for", name, "body part", part]
        Just skin -> skin

      characterSkinMaterials = Set.fromList $ concat [map SB.pack . Map.elems $ characterModelSkin name part | name <- characterNames, part <- ["head","upper","lower"]]
      characterObjs = [[(mkWorldMat (x + r * sin (angle i)) (y + r * cos (angle i)) z, m) | m <- ml] | (i,ml) <- zip [0..] characterModels]
        where
          r = (200 / 24) * (fromIntegral $ length characterNames)
          angle i = fromIntegral i / (fromIntegral $ length characterNames) * pi * 2
          Vec3 x y z = p0
          characterModels = [[(characterModelSkin name part,"models/players/" ++ name ++ "/" ++ part ++ ".md3") | part <- ["head","upper","lower"]] | name <- characterNames]

  characters <- sequence
    [ parseCharacter fname <$> readEntry e
    | name <- characterNames
    , let fname = "models/players/" ++ name ++ "/animation.cfg"
          e = maybe (error $ "missing " ++ fname) id $ Map.lookup fname pk3Data
    ]
  return (characterSkinMaterials,characterObjs,characters)

readMD3Objects characterObjs ents pk3Data = do
    let itemMap = T.fromList [(SB.pack $ itClassName it,it) | it <- items]
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
        md3Objs = concatMap collectObj ents
    md3Map <- T.fromList <$> sequence
      [ (\a -> (SB.pack n,MD3.readMD3 $ LB.fromStrict a)) <$> readEntry m
      | n <- Set.toList . Set.fromList . map (snd . snd) $ md3Objs ++ concat characterObjs
      , m <- maybeToList $ Map.lookup n pk3Data
      ]
    let md3Materials = Set.fromList . concatMap (concatMap (map MD3.shName . V.toList . MD3.srShaders) . V.toList . MD3.mdSurfaces) $ T.elems md3Map

    return (md3Materials,md3Map,md3Objs)

takeExtensionCI = map toLower . takeExtension
isPrefixOfCI a b = isPrefixOf a $ map toLower b

parseEntities :: String -> SB.ByteString -> [T.Trie SB.ByteString]
parseEntities n s = eval n $ parse entities s
  where
    eval n f = case f of
        Done "" r   -> r
        Done rem r  -> error $ show (n,"Input is not consumed", rem, r)
        Fail _ c _  -> error $ show (n,"Fail",c)
        Partial f'  -> eval n (f' "")

-----------
-- Graphics
-----------

shaderMap :: Map String Entry -> IO (T.Trie CommonAttrs)
shaderMap ar = do
  let eval n f = case f of
        Done "" r   -> r
        Done rem r  -> error $ show (n,"Input is not consumed", rem, map fst r)
        Fail _ c _  -> error $ show (n,"Fail",c)
        Partial f'  -> eval n (f' "")
  T.fromList . concat <$> forM [(n,e) | (n,e) <- Map.toList ar, ".shader" == takeExtension n, isPrefixOf "scripts" n] (\(n,e) -> eval n . parse shaders <$> readEntry e)

-- Utility code
tableTexture :: [Float] -> GLUniformName -> Map GLUniformName InputSetter -> IO ()
tableTexture t n s = do
    let width       = length t
        v           = V.fromList t
        bitmap x y  = let a = floor $ min 255 $ max 0 $ 128 + 128 * v V.! x in PixelRGB8 a a a
        texture     = uniformFTexture2D n s

    tex <- uploadTexture2DToGPU' True False False False $ ImageRGB8 $ generateImage bitmap width 1
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
                Right img   -> uploadTexture2DToGPU' True True isMip isClamped img

-- TODO
engineInit pk3Data fullBSPName = do
    let bspName = takeBaseName fullBSPName

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
        bspEntry = case Map.lookup fullBSPName pk3Data of
            Nothing -> error "You need to put pk3 file into your current directory"
            Just bspd -> bspd

    putStrLn $ "loading: " ++ show bspName
    -- load bsp data
    bsp <- readBSP . LB.fromStrict <$> readEntry bspEntry

    SB.writeFile (bspName ++ ".entities") $ blEntities bsp

    -- extract spawn points
    let ents = parseEntities bspName $ blEntities bsp
        spawnPoint e
          | Just classname <- T.lookup "classname" e
          , classname `elem` ["info_player_deathmatch"]
          , Just origin <- T.lookup "origin" e
          , [x,y,z] <- map read $ words $ SB.unpack origin = [Vec3 x y z]
          | otherwise = []
        spawnPoints = concatMap spawnPoint ents
        p0 = head spawnPoints

    -- MD3 related code
    (characterSkinMaterials,characterObjs,characters) <- readCharacters pk3Data p0
    (md3Materials,md3Map,md3Objs) <- readMD3Objects characterObjs ents pk3Data
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

        maxMaterial = 20 -- TODO: remove if we will have fast reducer
        shNames = Set.fromList $ Prelude.take maxMaterial selectedMaterials
        allShName = map shName $ V.toList $ blShaders bsp
        (selectedMaterials,ignoredMaterials) = partition (\n -> or $ [SB.isInfixOf k n | k <- ["floor","wall","door","trim","block"]]) allShName

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

    let quake3SlotSchema =
          ObjectArraySchema Triangles $ Map.fromList
            [ ("color",       Attribute_V4F)
            , ("diffuseUV",   Attribute_V2F)
            , ("normal",      Attribute_V3F)
            , ("position",    Attribute_V3F)
            , ("lightmapUV",  Attribute_V2F)
            ]
        debugSlotSchema =
          ObjectArraySchema Triangles $ Map.fromList
            [ ("position",    Attribute_V3F)
            , ("color",       Attribute_V4F)
            ]
        inputSchema = {-TODO-}
          PipelineSchema
          { objectArrays = Map.fromList $ ("CollisionShape",debugSlotSchema) : zip ("LightMapOnly":"missing shader":map SB.unpack (T.keys shMap)) (repeat quake3SlotSchema)
          , uniforms = Map.fromList $ [ ("viewProj",      M44F)
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
                                      , ("origin",    V3F)
                                      ] ++ zip textureUniforms (repeat FTexture2D)
          }
    return (inputSchema,(bsp,md3Map,md3Objs,characterObjs,characters,shMapTexSlot,spawnPoints))

getBSP (bsp,md3Map,md3Objs,characterObjs,characters,shMapTexSlot,spawnPoints) = bsp
getSpawnPoints (bsp,md3Map,md3Objs,characterObjs,characters,shMapTexSlot,spawnPoints) = spawnPoints

setupStorage pk3Data (bsp,md3Map,md3Objs,characterObjs,characters,shMapTexSlot,_) storage = do
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

    -- default texture
    let redBitmap x y = let v = if (x+y) `mod` 2 == 0 then 255 else 0 in PixelRGB8 v v 0
    defaultTexture <- uploadTexture2DToGPU' False False False False $ ImageRGB8 $ generateImage redBitmap 2 2

    putStrLn "loading textures:"
    -- load textures
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

    objs <- addBSP storage bsp

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
    return (storage,lcMD3Objs,characters,lcCharacterObjs,objs,bsp)
{-
animateMaps :: [(Float, [(SetterFun TextureData, TextureData)])] -> SignalGen Float (Signal [(Float, [(SetterFun TextureData, TextureData)])])
animateMaps l0 = stateful l0 $ \dt l -> zipWith (f $ dt * timeScale) l timing
  where
    timeScale = 1
    timing  = map fst l0
    f :: Float -> (Float,[(SetterFun TextureData,TextureData)]) -> Float -> (Float,[(SetterFun TextureData,TextureData)])
    f dt (t,a) t0
        | t - dt <= 0   = (t-dt+t0,tail a)
        | otherwise     = (t-dt,a)
-}
-- TODO
updateRenderInput (storage,lcMD3Objs,characters,lcCharacterObjs,objs,bsp) (camPos,camTarget,camUp) w h time noBSPCull = do
            let slotU = uniformSetter storage

            let legAnimType = LEGS_SWIM
                torsoAnimType = TORSO_STAND2

            let matSetter   = uniformM44F "viewProj" slotU
                viewOrigin  = uniformV3F "viewOrigin" slotU
                orientation = uniformM44F "orientation" slotU
                viewMat     = uniformM44F "viewMat" slotU
                timeSetter  = uniformFloat "time" slotU

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
            --TODO: forM_ anim $ \(_,a) -> let (s,t) = head a in s t
            setScreenSize storage (fromIntegral w) (fromIntegral h)
            case noBSPCull of
              True  -> V.forM_ objs $ \obj -> enableObject obj True
              False -> cullSurfaces bsp camPos frust objs
            return ()

compileQuake3GraphicsCached name = doesFileExist name >>= \case
  True -> putStrLn "use cached pipeline" >> return True
  False -> compileQuake3Graphics name

compileQuake3Graphics name = printTimeDiff "compile quake3 graphics pipeline..." $ do
  {-
  compileMain ["."] OpenGL33 "Graphics" >>= \case 
    Left err -> putStrLn ("error: " ++ err) >> return False
    Right ppl -> LB.writeFile name (encode ppl) >> return True
  -}
  (exitCode,_,_) <- readProcessWithExitCode "lc" ["Graphics.lc","-o" ++ name] ""
  return $ ExitSuccess == exitCode

loadQuake3Graphics storage name = do
    putStrLn $ "load " ++ name
    renderer <- printTimeDiff "allocate pipeline..." $ do
      eitherDecode <$> LB.readFile name >>= \case
        Left err -> fail err
        Right ppl -> allocRenderer ppl
    printTimeDiff "setStorage..." $ setStorage renderer storage
    --sortSlotObjects storage
    return $ Just renderer

createLoadingScreen = do
  -- default texture
  let redBitmap x y = let v = if (x+y) `mod` 2 == 0 then 255 else 0 in PixelRGB8 v v 0
  defaultTexture <- uploadTexture2DToGPU' False False False False $ ImageRGB8 $ generateImage redBitmap 2 2
  -- storage
  storage <- allocStorage $ makeSchema $ do
    defUniforms $ do
      "LoadingImage" @: FTexture2D
  -- pipeline
  renderer <- eitherDecode <$> LB.readFile "Loading.json" >>= \case
        Left err -> fail err
        Right ppl -> allocRenderer ppl
  -- connect them
  setStorage renderer storage >>= \case -- check schema compatibility
    Just err -> fail err
    Nothing  -> return (storage,renderer,defaultTexture)

drawLoadingScreen w h (storage,renderer,defaultTexture) pk3Data bspName = do
    textureData <- loadQ3Texture True True defaultTexture pk3Data (SB.pack $ "levelshots/" ++ bspName)
    setScreenSize storage (fromIntegral w) (fromIntegral h)
    updateUniforms storage $ do
      "LoadingImage" @= return textureData
    renderFrame renderer

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

--vec4ToV4F :: Vec4 -> V4F
vec4ToV4F (Vec4 x y z w) = V4 x y z w

--mat4ToM44F :: Mat4 -> M44F
mat4ToM44F (Mat4 a b c d) = V4 (vec4ToV4F a) (vec4ToV4F b) (vec4ToV4F c) (vec4ToV4F d)

rotationEuler :: Vec3 -> Proj4
rotationEuler (Vec3 a b c) = orthogonal $ toOrthoUnsafe $ rotMatrixZ a .*. rotMatrixX b .*. rotMatrixY (-c)
