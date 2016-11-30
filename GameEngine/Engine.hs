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
  , getModelIndexFromBrushIndex
  , getTeleportFun
  , getMusicFile
  ) where

import Debug.Trace

import Control.Monad
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
import Text.Show.Pretty (ppShow)

import Data.Digest.CRC32
import Codec.Picture

import LambdaCube.Compiler (compileMain, Backend(..))
import LambdaCube.GL as GL
--import LambdaCube.GL.Mesh

import Control.Exception (evaluate)
import Control.DeepSeq
--import GHC.Generics

import GameEngine.Data.BSP
import GameEngine.Loader.BSP
import GameEngine.Data.Material hiding (Vec3)
import GameEngine.Render
import GameEngine.Loader.ShaderParser
import qualified GameEngine.Loader.Entity as E
import GameEngine.Loader.Zip
import GameEngine.Data.GameCharacter
import GameEngine.Loader.GameCharacter
import GameEngine.Data.Items
import GameEngine.Entity
import GameEngine.Frustum
import GameEngine.Utils
import GameEngine.Culling

import qualified GameEngine.Data.MD3 as MD3
import qualified GameEngine.Loader.MD3 as MD3

import Paths_lambdacube_quake3

{-
  TODO:
    remove elerea dep from anim map
-}

lc_q3_cache = ".lc_q3.cache" -- local cache: generated files, compiled pipelines are stored here
q3shader_cache = lc_q3_cache </> "q3shader.cache"
------------
-- Game data
------------

loadPK3 :: IO (Map String Entry)
loadPK3 = do
  let takeExtensionCI = map toLower . takeExtension
      isPrefixOfCI a b = isPrefixOf a $ map toLower b
  Map.unions <$> (mapM readArchive =<< filter (\n -> ".pk3" == takeExtensionCI n) <$> getDirectoryContents ".")

mkWorldMat x y z = translation $ Vec3 x y z
mkWorldMat' = translation

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

  charactersResult <- sequence <$> sequence
    [ parseCharacter fname <$> readEntry e
    | name <- characterNames
    , let fname = "models/players/" ++ name ++ "/animation.cfg"
          e = maybe (error $ "missing " ++ fname) id $ Map.lookup fname pk3Data
    ]
  case charactersResult of
    Right characters  -> return (characterSkinMaterials,characterObjs,characters)
    Left errorMessage -> fail errorMessage

handWeapon = head $ drop 6 ["models/weapons2/" ++ n ++ "/"++ n ++ ".md3" | n <- weapons]
  where
    weapons = ["bfg","gauntlet","grapple","grenadel","lightning","machinegun","plasma","railgun","rocketl","shells","shotgun"]

readMD3Objects characterObjs ents pk3Data = do
    let itemMap = T.fromList [(SB.pack $ itClassName it,it) | it <- items]
        collectObj E.EntityData{..} = case T.lookup (SB.pack classname) itemMap of
            Just i -> [(mat, (mempty,m)) | m <- Prelude.take cnt $ itWorldModel i, let mat = mkWorldMat' origin]
              where cnt = if itType i `elem` [IT_HEALTH, IT_POWERUP] then 2 else 1
            Nothing -> case model2 of
              Just m -> [(mkWorldMat' origin, (mempty,m))]
              Nothing -> []
        md3Objs = concatMap collectObj ents
    md3Map <- T.fromList <$> sequence
      [ (\a -> (SB.pack n,MD3.readMD3 $ LB.fromStrict a)) <$> readEntry m
      | n <- Set.toList . Set.fromList . map (snd . snd) $ md3Objs ++ concat characterObjs ++ [(mkWorldMat 0 0 0,(mempty,handWeapon))]
      , m <- maybeToList $ Map.lookup n pk3Data
      ]
    let md3Materials = Set.fromList . concatMap (concatMap (map MD3.shName . V.toList . MD3.srShaders) . V.toList . MD3.mdSurfaces) $ T.elems md3Map

    return (md3Materials,md3Map,md3Objs)

takeExtensionCI = map toLower . takeExtension
isPrefixOfCI a b = isPrefixOf a $ map toLower b

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

loadQ3Texture :: Bool -> Bool -> TextureData -> Map String Entry -> ByteString -> ByteString -> IO TextureData
loadQ3Texture isMip isClamped defaultTex ar shName name' = do
    let name = SB.unpack name'
        n1 = replaceExtension name "tga"
        n2 = replaceExtension name "jpg"
        b0 = Map.member name ar
        b1 = Map.member n1 ar
        b2 = Map.member n2 ar
        fname   = if b0 then name else if b1 then n1 else n2
    case Map.lookup fname ar of
        Nothing -> putStrLn ("    unknown texure: " ++ fname ++ " in shader: " ++ SB.unpack shName) >> return defaultTex
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

    createDirectoryIfMissing True lc_q3_cache -- create cache

    SB.writeFile (lc_q3_cache </> bspName ++ ".entities") $ blEntities bsp

    -- extract spawn points
    let Right ents = E.parseEntities bspName $ blEntities bsp
        spawnPoint E.EntityData{..}
          | classname `elem` [ "info_player_deathmatch"
                             , "info_player_start"
                             , "team_CTF_bluespawn"
                             , "team_CTF_redspawn"
                             , "team_CTF_blueplayer"
                             , "team_CTF_redplayer"
                             ] = [origin]
          | otherwise = []
        spawnPoints = concatMap spawnPoint ents
        p0 = head spawnPoints
        teleportData = loadTeleports ents
        music = (head . words) <$> (E.music $ head ents)

    -- MD3 related code
    (characterSkinMaterials,characterObjs,characters) <- readCharacters pk3Data p0
    (md3Materials,md3Map,md3Objs) <- readMD3Objects characterObjs ents pk3Data
    --putStrLn $ "level materials"
    --mapM_ SB.putStrLn $ map shName $ V.toList $ blShaders bsp
    shMap' <- do
      hasShaderCache <- doesFileExist q3shader_cache
      case hasShaderCache of
        True -> putStrLn "load shader cache" >> decodeFile q3shader_cache
        False -> do
                  putStrLn "create shader cache"
                  sm <- shaderMap pk3Data
                  encodeFile q3shader_cache sm
                  return sm
    let mkShader hasLightmap n = case T.lookup n shMap' of
          Just s -> (n,s)
          Nothing -> let alias = SB.pack . dropExtension . SB.unpack $ n in case T.lookup alias shMap' of
            Just s -> (alias,s)
            Nothing -> (n,imageShader hasLightmap n)

        maxMaterial = 20 -- TODO: remove if we will have fast reducer
        shNames = Set.fromList $ {-Prelude.take maxMaterial $ -}selectedMaterials ++ ignoredMaterials
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
    writeFile (lc_q3_cache </> "SampleMaterial.lc") $ unlines
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
                                      , ("Noise",                FTexture2D)
                                      , ("SinTable",             FTexture2D)
                                      , ("SquareTable",          FTexture2D)
                                      , ("SawToothTable",        FTexture2D)
                                      , ("InverseSawToothTable", FTexture2D)
                                      , ("TriangleTable",        FTexture2D)
                                      , ("origin",    V3F)
                                      ] ++ zip textureUniforms (repeat FTexture2D)
          }
    let brushModelMapping = V.replicate (V.length $ blBrushes bsp) (-1) V.//
          (concat $ V.toList $ V.imap (\i Model{..} -> [(n,i) | n <- [mdFirstBrush..mdFirstBrush+mdNumBrushes-1]]) (blModels bsp))
    putStrLn $ "bsp model count: " ++ show (V.length $ blModels bsp)
    print brushModelMapping
    print teleportData
    return (inputSchema,(bsp,md3Map,md3Objs,characterObjs,characters,shMapTexSlot,spawnPoints,brushModelMapping,teleportData,music))

getMusicFile (bsp,md3Map,md3Objs,characterObjs,characters,shMapTexSlot,spawnPoints,brushModelMapping,teleportData,music) = music
getModelIndexFromBrushIndex (bsp,md3Map,md3Objs,characterObjs,characters,shMapTexSlot,spawnPoints,brushModelMapping,teleportData,music) brushIndex = brushModelMapping V.! brushIndex
getBSP (bsp,md3Map,md3Objs,characterObjs,characters,shMapTexSlot,spawnPoints,brushModelMapping,teleportData,music) = bsp
getSpawnPoints (bsp,md3Map,md3Objs,characterObjs,characters,shMapTexSlot,spawnPoints,brushModelMapping,teleportData,music) = spawnPoints
getTeleportFun levelData@(bsp,md3Map,md3Objs,characterObjs,characters,shMapTexSlot,spawnPoints,brushModelMapping,(teleport,teleportTarget),music) brushIndex p =
  let models = map (getModelIndexFromBrushIndex levelData) brushIndex
      hitModels = [tp | TriggerTeleport target model <- teleport, model `elem` models, TargetPosition _ tp <- maybeToList $ T.lookup target teleportTarget]
  --in head $ trace (show ("hitModels",hitModels,models)) hitModels ++ [p]
  in head $ hitModels ++ [p]

setupStorage pk3Data (bsp,md3Map,md3Objs,characterObjs,characters,shMapTexSlot,_,_,_,_) storage = do
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
    animTex <- fmap concat $ forM (Set.toList $ Set.fromList $ concatMap (\(shName,sh) -> [(shName,saTexture sa,saTextureUniform sa,caNoMipMaps sh) | sa <- caStages sh]) $ T.toList shMapTexSlot) $
      \(shName,stageTex,texSlotName,noMip) -> do
        let texSetter = uniformFTexture2D texSlotName  slotU
            setTex isClamped img = texSetter =<< loadQ3Texture (not noMip) isClamped defaultTexture pk3Data shName img
        case stageTex of
            ST_Map img          -> setTex False img >> return []
            ST_ClampMap img     -> setTex True img >> return []
            ST_AnimMap freq imgs   -> do
                txList <- mapM (loadQ3Texture (not noMip) False defaultTexture pk3Data shName) imgs
                let txVector = V.fromList txList
                return [(fromIntegral (V.length txVector) / freq,texSetter,txVector)]
            _ -> return []

    surfaceObjs <- addBSP storage bsp

    -- add entities
    let addMD3Obj (mat,(skin,name)) = case T.lookup (SB.pack name) md3Map of
          Nothing -> return []
          Just md3 -> do
                    putStrLn ("add model: " ++ name)
                    lcmd3 <- addMD3 storage md3 skin ["worldMat"]
                    return [(mat,lcmd3)]

    lcMD3Objs <- concat <$> forM md3Objs addMD3Obj

    lcMD3Weapon <- addMD3 storage (fromJust $ T.lookup (SB.pack handWeapon) md3Map) mempty ["worldMat","viewProj"]

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
    return (storage,lcMD3Objs,characters,lcCharacterObjs,surfaceObjs,bsp,lcMD3Weapon,animTex)

-- TODO
updateRenderInput (storage,lcMD3Objs,characters,lcCharacterObjs,surfaceObjs,bsp,lcMD3Weapon,animTex) (camPos,camTarget,camUp) w h time noBSPCull = do
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

            -- set uniforms
            timeSetter $ time / 1
            --putStrLn $ "time: " ++ show time ++ " " ++ show capturing
            viewOrigin $ V3 cx cy cz
            viewMat $ mat4ToM44F cm
            --orientation $ V4 orientA orientB orientC $ V4 0 0 0 1
            matSetter $! mat4ToM44F $! cm .*. sm .*. pm

            let invCM = mat4ToM44F $ idmtx -- inverse cm .*. (fromProjective $ translation (Vec3 0 (0) (-30)))
                --rot = fromProjective $ rotationEuler (Vec3 (-pi/2+30/pi*2) (pi/2) (-pi))
                rot = fromProjective $ orthogonal $ toOrthoUnsafe $ rotMatrixX (-pi/2) .*. rotMatrixY (pi/2) .*. rotMatrixX (10/pi*2)
            forM_ (lcmd3Object lcMD3Weapon) $ \obj -> do
              uniformM44F "viewProj" (objectUniformSetter obj) $ mat4ToM44F $! rot .*. (fromProjective $ translation (Vec3 3 (-10) (-5))) .*. sm .*. pm
              uniformM44F "worldMat" (objectUniformSetter obj) invCM
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

            forM_ animTex $ \(animTime,texSetter,v) -> do
              let (_,i) = properFraction (time / animTime)
                  idx = floor $ i * fromIntegral (V.length v)
              texSetter $ v V.! idx
            setScreenSize storage (fromIntegral w) (fromIntegral h)
            -- TODO
            let idmtx = V4 (V4 1 0 0 0) (V4 0 1 0 0) (V4 0 0 1 0) (V4 0 0 0 1)
            V.forM_ surfaceObjs $ \objs -> forM_ objs $ \obj -> uniformM44F "worldMat" (objectUniformSetter obj) idmtx
            case noBSPCull of
              True  -> V.forM_ surfaceObjs $ \objs -> forM_ objs $ \obj -> enableObject obj True
              False -> cullSurfaces bsp camPos frust surfaceObjs
            return ()

compileQuake3GraphicsCached name = doesFileExist (lc_q3_cache </> name) >>= \case
  True -> putStrLn "use cached pipeline" >> return True
  False -> compileQuake3Graphics name

compileQuake3Graphics name = printTimeDiff "compile quake3 graphics pipeline..." $ do
  dataDir <- getDataDir
  -- check order: local lc folder, global lc folder
  -- local lc_q3.cache stores the generated SampleMaterial.lc with the level specific material description list
  compileMain [lc_q3_cache, "lc", dataDir </> "lc"] OpenGL33 "Graphics.lc" >>= \case 
    Left err -> putStrLn ("error: " ++ ppShow err) >> return False
    Right ppl -> LB.writeFile (lc_q3_cache </> name) (encode ppl) >> return True

loadQuake3Graphics storage name = do
    putStrLn $ "load " ++ name
    dataDir <- getDataDir
    let localName  = "lc" </> name
        globalName = dataDir </> localName
        paths = [lc_q3_cache </> name,localName,globalName]
    validPaths <- filterM doesFileExist paths
    when (null validPaths) $ fail $ name ++ " is not found in " ++ show paths
    renderer <- printTimeDiff "allocate pipeline..." $ do
      eitherDecode <$> LB.readFile (head validPaths) >>= \case
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
  dataDir <- getDataDir
  let localLoadingName  = "lc" </> "Loading.json"
      globalLoadingName = dataDir </> localLoadingName
      paths = [localLoadingName,globalLoadingName]
  validPaths <- filterM doesFileExist paths
  when (null validPaths) $ fail $ "could not find Loading.json in " ++ show paths
  renderer <- eitherDecode <$> LB.readFile (head validPaths) >>= \case
        Left err -> fail err
        Right ppl -> allocRenderer ppl
  -- connect them
  setStorage renderer storage >>= \case -- check schema compatibility
    Just err -> fail err
    Nothing  -> return (storage,renderer,defaultTexture)

drawLoadingScreen w h (storage,renderer,defaultTexture) pk3Data bspName = do
    textureData <- loadQ3Texture True True defaultTexture pk3Data mempty (SB.pack $ "levelshots/" ++ bspName)
    setScreenSize storage (fromIntegral w) (fromIntegral h)
    updateUniforms storage $ do
      "LoadingImage" @= return textureData
    renderFrame renderer
