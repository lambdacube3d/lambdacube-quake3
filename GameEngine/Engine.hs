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

import Control.Monad
import Data.Char
import Data.List (isPrefixOf,partition,isInfixOf,elemIndex)
import Data.Maybe
import Data.Vect
import Data.Set (Set)
import Data.Map (Map)
import System.FilePath
import System.Directory

import qualified Data.Map as Map
import Data.Binary (encodeFile,decodeFile)
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as SB
import qualified Data.Set as Set
import qualified Data.Vector as V

import Text.Show.Pretty (ppShow)

import Data.Digest.CRC32
import Codec.Picture

import LambdaCube.GL as GL

import GameEngine.Data.BSP
import GameEngine.Data.GameCharacter
import GameEngine.Data.Material hiding (Vec3)
import GameEngine.Content
import GameEngine.Entity
import GameEngine.Graphics.Culling
import GameEngine.Graphics.Frustum
import GameEngine.Graphics.Render
import GameEngine.Graphics.Storage
import GameEngine.Loader.BSP
import GameEngine.Loader.Zip
import GameEngine.Utils
import qualified GameEngine.Data.MD3 as MD3
import qualified GameEngine.Loader.Entity as E

import Paths_lambdacube_quake3

type EngineContent =
  ( BSPLevel
  , Map ByteString MD3.MD3Model
  , [(Proj4, (Map String String, String))]
  , [[(Proj4, (Map String String, [Char]))]]
  , [Character]
  , Map ByteString CommonAttrs
  , [Vec3]
  , V.Vector Int
  , ([GameEngine.Entity.Entity], Map ByteString GameEngine.Entity.Entity)
  , Maybe String
  )

type EngineGraphics =
  ( GLStorage
  , [(Proj4, LCMD3)]
  , [Character]
  , [(Proj4, (MD3.MD3Model, LCMD3), (MD3.MD3Model, LCMD3),(MD3.MD3Model, LCMD3))]
  , V.Vector [Object]
  , BSPLevel
  , LCMD3
  , [(Float, SetterFun TextureData, V.Vector TextureData)]
  )

-- TODO
engineInit :: Map String Entry -> FilePath -> IO (PipelineSchema, EngineContent)
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
    let ents = case E.parseEntities bspName $ blEntities bsp of
            Left err -> error err
            Right x -> x
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
    let mkShader hasLightmap n = case Map.lookup n shMap' of
          Just s -> (n,s)
          Nothing -> let alias = SB.pack . dropExtension . SB.unpack $ n in case Map.lookup alias shMap' of
            Just s -> (alias,s)
            Nothing -> (n,imageShader hasLightmap n)

        maxMaterial = 20 -- TODO: remove if we will have fast reducer
        shNames = Set.fromList $ {-Prelude.take maxMaterial $ -}selectedMaterials ++ ignoredMaterials
        allShName = map shName $ V.toList $ blShaders bsp
        (selectedMaterials,ignoredMaterials) = partition (\n -> or $ [SB.isInfixOf k n | k <- ["floor","wall","door","trim","block"]]) allShName

        shMap = Map.fromList [mkShader True n | n <- Set.toList shNames] `Map.union`
                Map.fromList [mkShader False n | n <- Set.toList md3Materials] `Map.union`
                Map.fromList [mkShader False n | n <- Set.toList characterSkinMaterials]

    let shMapTexSlot = mangleCA <$> shMap
          where
            mangleStageTex stageTex = SB.pack $ "Tex_" ++ show (crc32 $ SB.pack $ show stageTex)
            mangleCA ca = ca {caStages = mangleSA <$> caStages ca}
            mangleSA sa = sa {saTextureUniform = mangleStageTex sa}
        textureUniforms = map SB.unpack . Set.toList . Set.fromList . concat . map name . concat . map caStages $ Map.elems shMapTexSlot
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

    putStrLn $ "all materials:  " ++ show (Map.size shMap')
    --putStrLn $ "used materials: " ++ show (Map.size shMap)
    --putStrLn $ "texture uniforms: \n" ++ ppShow textureUniforms
    putStrLn $ "used materials: " ++ show (Map.size shMapTexSlot)
    putStrLn $ "ignored materials: " ++ show (length ignoredMaterials)
    writeFile (lc_q3_cache </> "SampleMaterial.lc") $ unlines
      [ "module SampleMaterial where"
      , "import Material"
      , "sampleMaterial ="
      , unlines . map ("  "++) . lines . ppShow . Map.toList $ shMapTexSlot
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
          { objectArrays = Map.fromList $ ("CollisionShape",debugSlotSchema) : zip ("LightMapOnly":"missing shader":map SB.unpack (Map.keys shMap)) (repeat quake3SlotSchema)
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
      hitModels = [tp | TriggerTeleport target model <- teleport, model `elem` models, TargetPosition _ tp <- maybeToList $ Map.lookup target teleportTarget]
  --in head $ trace (show ("hitModels",hitModels,models)) hitModels ++ [p]
  in head $ hitModels ++ [p]

setupStorage :: Map String Entry -> EngineContent -> GLStorage -> IO EngineGraphics
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
    animTex <- fmap concat $ forM (Set.toList $ Set.fromList $ concatMap (\(shName,sh) -> [(shName,saTexture sa,saTextureUniform sa,caNoMipMaps sh) | sa <- caStages sh]) $ Map.toList shMapTexSlot) $
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

    putStrLn "add bsp to storage"
    surfaceObjs <- addBSP storage bsp

    -- add entities
    let addMD3Obj (mat,(skin,name)) = case Map.lookup (SB.pack name) md3Map of
          Nothing -> return []
          Just md3 -> do
                    putStrLn ("add model: " ++ name)
                    lcmd3 <- addMD3 storage md3 skin ["worldMat"]
                    return [(mat,lcmd3)]

    lcMD3Objs <- concat <$> forM md3Objs addMD3Obj

    lcMD3Weapon <- addMD3 storage (fromJust $ Map.lookup (SB.pack handWeapon) md3Map) mempty ["worldMat","viewProj"]

    -- add characters
    lcCharacterObjs <- forM characterObjs
      (\[(mat,(hSkin,hName)),(_,(uSkin,uName)),(_,(lSkin,lName))] -> do
        let Just hMD3 = Map.lookup (SB.pack hName) md3Map
            Just uMD3 = Map.lookup (SB.pack uName) md3Map
            Just lMD3 = Map.lookup (SB.pack lName) md3Map
        hLC <- addMD3 storage hMD3 hSkin ["worldMat"]
        uLC <- addMD3 storage uMD3 uSkin ["worldMat"]
        lLC <- addMD3 storage lMD3 lSkin ["worldMat"]
        return (mat,(hMD3,hLC),(uMD3,uLC),(lMD3,lLC))
      )
    return (storage,lcMD3Objs,characters,lcCharacterObjs,surfaceObjs,bsp,lcMD3Weapon,animTex)

-- TODO
updateRenderInput :: EngineGraphics -> (Vec3, Vec3, Vec3) -> Int -> Int -> Float -> Bool -> IO ()
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
