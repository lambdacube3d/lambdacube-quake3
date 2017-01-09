{-# LANGUAGE RecordWildCards, OverloadedStrings, LambdaCase, FlexibleContexts #-}
module GameEngine.RenderSystem
  ( RenderSystem
  , initRenderSystem
  , renderScene
  , Renderable(..)
  , Scene(..)
  ) where

import Control.Monad
import Control.Monad.State.Strict
import Data.Vect hiding (Vector)
import Data.Maybe (fromJust)
import Data.IORef
import Data.List (foldl')
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.Set as Set
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as SB
import Codec.Picture
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Digest.CRC32 (crc32)
import Text.Printf

import LambdaCube.GL

import GameEngine.Data.Material hiding (Vec3)
import GameEngine.Graphics.Storage
import GameEngine.Graphics.Frustum
import GameEngine.Graphics.Culling
import GameEngine.Graphics.MD3
import GameEngine.Graphics.BSP
import GameEngine.Graphics.GameCharacter
import GameEngine.Loader.Zip
import GameEngine.Loader.BSP (readBSP)
import GameEngine.Loader.MD3 (readMD3)
import GameEngine.Content
import GameEngine.Scene
import GameEngine.Utils

type BSPCache         = HashMap String GPUBSP
type BSPInstanceCache = HashMap String [BSPInstance]
type MD3Cache         = HashMap String GPUMD3
type MD3InstanceCache = HashMap String [MD3Instance]
type CharacterCache   = HashMap (String,String) [CharacterInstance]
type ShaderCache      = HashSet String
type TextureCache     = HashMap (String,Bool,Bool) TextureData
type AnimatedTexture  = (Float, SetterFun TextureData, Vector TextureData)

{-
  remove IORefs
  traverse renderables
    load resources if necessary
    collect scene materials
  load new textures if necessary
  create new pipeline and storage if necessary
-}
data RenderSystem
  = RenderSystem
  -- static values
  { rsFileSystem        :: Map String Entry
  , rsShaderMap         :: Map String CommonAttrs
  , rsCheckerTexture    :: TextureData
  , rsWhiteTexture      :: TextureData
  , rsTableTextures     :: TableTextures
  -- resource caches
  , rsBSPCache          :: IORef BSPCache
  , rsBSPInstanceCache  :: IORef BSPInstanceCache
  , rsBSPShaderCache    :: IORef ShaderCache
  , rsMD3Cache          :: IORef MD3Cache
  , rsMD3InstanceCache  :: IORef MD3InstanceCache
  , rsMD3ShaderCache    :: IORef ShaderCache
  , rsCharacterCache    :: IORef CharacterCache
  , rsTextureCache      :: IORef TextureCache
  -- renderer pipeline
  , rsRenderer          :: IORef GLRenderer
  , rsStorage           :: IORef GLStorage
  , rsAnimatedTextures  :: IORef [AnimatedTexture]
  }

initRenderSystem :: Map String Entry -> IO RenderSystem
initRenderSystem pk3 = do
  bspCache <- newIORef mempty
  bspInstanceCache <- newIORef mempty
  bspShaderCache <- newIORef mempty
  md3Cache <- newIORef mempty
  md3InstanceCache <- newIORef mempty
  md3ShaderCache <- newIORef mempty
  characterCache <- newIORef mempty
  textureCache <- newIORef mempty
  shMap <- loadShaderMap pk3
  let (inputSchema,_) = createRenderInfo shMap mempty mempty
  storage <- allocStorage inputSchema
  renderer <- fromJust <$> loadQuake3Graphics storage "SimpleGraphics.json"
  rendererRef <- newIORef renderer
  storageRef <- newIORef storage
  animatedTextures <- newIORef []
  -- default textures
  whiteTexture <- uploadTexture2DToGPU' False False False False $ ImageRGB8 $ generateImage (\_ _ -> PixelRGB8 255 255 255) 1 1
  let redBitmap x y = let v = if (x+y) `mod` 2 == 0 then 255 else 0 in PixelRGB8 v v 0
  checkerTexture <- uploadTexture2DToGPU' False False False False $ ImageRGB8 $ generateImage redBitmap 2 2
  tableTextures <- initTableTextures
  initStorageDefaultValues tableTextures storage
  pure $ RenderSystem
    { rsFileSystem        = pk3
    , rsShaderMap         = shMap
    , rsCheckerTexture    = checkerTexture
    , rsWhiteTexture      = whiteTexture
    , rsTableTextures     = tableTextures
    , rsBSPCache          = bspCache
    , rsBSPInstanceCache  = bspInstanceCache
    , rsBSPShaderCache    = bspShaderCache
    , rsMD3Cache          = md3Cache
    , rsMD3InstanceCache  = md3InstanceCache
    , rsMD3ShaderCache    = md3ShaderCache
    , rsCharacterCache    = characterCache
    , rsTextureCache      = textureCache
    , rsRenderer          = rendererRef
    , rsStorage           = storageRef
    , rsAnimatedTextures  = animatedTextures
    }

loadMD3 :: Map String Entry -> String -> IO GPUMD3
loadMD3 pk3 name = case Map.lookup name pk3 of
  Nothing -> fail $ "file not found: " ++ name
  Just a -> readMD3 . LB.fromStrict <$> readEntry a >>= uploadMD3

loadBSP :: Map String a -> Map String Entry -> String -> IO GPUBSP
loadBSP shaderMap pk3 name = case Map.lookup name pk3 of
  Nothing -> fail $ "file not found: " ++ name
  Just a -> readBSP . LB.fromStrict <$> readEntry a >>= uploadBSP (Map.keysSet shaderMap)

initStorageDefaultValues :: TableTextures -> GLStorage -> IO ()
initStorageDefaultValues tableTextures storage = do
  let slotU           = uniformSetter storage
      overbrightBits  = 0
  uniformM44F "worldMat" slotU $ mat4ToM44F idmtx
  uniformV3F "entityRGB" slotU $ V3 1 1 1
  uniformFloat "entityAlpha" slotU 1
  uniformFloat "identityLight" slotU $ 1 / (2 ^ overbrightBits)
  setupTableTextures slotU tableTextures

initStorageTextures :: RenderSystem -> GLStorage -> Map String CommonAttrs -> IO ()
initStorageTextures RenderSystem{..} storage usedMaterials = do
  let usedTextures = setNub [ (name,saTexture,saTextureUniform,caNoMipMaps)
                            | (name,CommonAttrs{..}) <- Map.toList usedMaterials
                            , StageAttrs{..} <- caStages
                            ]
      cachedTexture isMip isClamped shaderName imageName = do
        textureCache <- readIORef rsTextureCache
        let key = (imageName,isMip,isClamped)
        case HashMap.lookup key textureCache of
          Just texture -> return texture
          Nothing -> do
            texture <- loadQ3Texture isMip isClamped rsCheckerTexture rsFileSystem shaderName imageName
            writeIORef rsTextureCache $ HashMap.insert key texture textureCache
            return texture
  putStrLn "loading textures:"
  -- load textures
  animatedTextures <- fmap concat $ forM usedTextures $ \(shName,stageTex,texSlotName,noMip) -> do
      let texSetter = uniformFTexture2D (SB.pack texSlotName) (uniformSetter storage)
          setTex isClamped img = texSetter =<< cachedTexture (not noMip) isClamped shName img
      case stageTex of
          ST_Map img          -> setTex False img >> return []
          ST_ClampMap img     -> setTex True img >> return []
          ST_AnimMap freq imgs   -> do
              txList <- mapM (cachedTexture (not noMip) False shName) imgs
              let txVector = V.fromList txList
              return [(fromIntegral (V.length txVector) / freq,texSetter,txVector)]
          _ -> return []

  writeIORef rsAnimatedTextures animatedTextures

updateModelCache :: RenderSystem -> [Renderable] -> IO (HashSet String,HashSet String,HashMap String GPUMD3,HashMap String GPUBSP)
updateModelCache RenderSystem{..} renderables = do
  -- load new md3 models
  md3Cache <- readIORef rsMD3Cache
  let newModelNames = setNub [name | MD3 _ _ _ name <- renderables, not $ HashMap.member name md3Cache]
  newModels <- forM newModelNames $ loadMD3 rsFileSystem
  let md3Cache' = md3Cache `HashMap.union` HashMap.fromList (zip newModelNames newModels)
  unless (null newModelNames) $ putStrLn $ unlines $ "new models:" : newModelNames
  writeIORef rsMD3Cache md3Cache'

  -- load new bsp maps
  bspCache <- readIORef rsBSPCache
  let newBSPNames = setNub [name | BSPMap name <- renderables, not $ HashMap.member name bspCache]
  newBSPs <- forM newBSPNames $ loadBSP rsShaderMap rsFileSystem
  let bspCache' = bspCache `HashMap.union` HashMap.fromList (zip newBSPNames newBSPs)
  unless (null newBSPNames) $ putStrLn $ unlines $ "new bsp maps:" : newBSPNames
  writeIORef rsBSPCache bspCache'

  -- collect new materials
  md3ShaderCache <- readIORef rsMD3ShaderCache
  bspShaderCache <- readIORef rsBSPShaderCache
  let newMD3Materials = HashSet.unions (map gpumd3Shaders newModels) `HashSet.difference` md3ShaderCache
      newBSPMaterials = HashSet.unions (map gpubspShaders newBSPs) `HashSet.difference` bspShaderCache
  return (newMD3Materials,newBSPMaterials,md3Cache',bspCache')

updateRenderCache :: RenderSystem -> HashSet String -> HashSet String -> IO (GLStorage,GLRenderer,MD3InstanceCache,BSPInstanceCache,CharacterCache)
updateRenderCache renderSystem@RenderSystem{..} newMD3Materials newBSPMaterials
  | HashSet.null newMD3Materials && HashSet.null newBSPMaterials = do
      md3InstanceCache <- readIORef rsMD3InstanceCache
      bspInstanceCache <- readIORef rsBSPInstanceCache
      characterCache <- readIORef rsCharacterCache
      storage <- readIORef rsStorage
      renderer <- readIORef rsRenderer
      return (storage,renderer,md3InstanceCache,bspInstanceCache,characterCache)
  | otherwise = do
      md3ShaderCache <- readIORef rsMD3ShaderCache
      putStrLn $ unlines $ "new md3 materials:" : HashSet.toList newMD3Materials
      let md3ShaderCache' = md3ShaderCache `HashSet.union` newMD3Materials
      writeIORef rsMD3ShaderCache md3ShaderCache'

      bspShaderCache <- readIORef rsBSPShaderCache
      putStrLn $ unlines $ "new bsp materials:" : HashSet.toList newBSPMaterials
      let bspShaderCache' = bspShaderCache `HashSet.union` newBSPMaterials
      writeIORef rsBSPShaderCache bspShaderCache'

      let (inputSchema,usedMaterials) = createRenderInfo rsShaderMap bspShaderCache' md3ShaderCache'
      storage <- allocStorage inputSchema
      -- load new images and set storage texture uniforms
      initStorageTextures renderSystem storage usedMaterials
      initStorageDefaultValues rsTableTextures storage

      writeSampleMaterial usedMaterials
      let filename = show (crc32 . SB.pack $ show usedMaterials) ++ "_ppl.json"
      compileQuake3GraphicsCached filename >>= \ok -> unless ok $ fail "no renderer"
      renderer <- fromJust <$> loadQuake3Graphics storage filename
      disposeRenderer =<< readIORef rsRenderer
      --renderer <- readIORef rsRenderer
      --setStorage renderer storage
      writeIORef rsStorage storage
      writeIORef rsRenderer renderer
      writeIORef rsMD3InstanceCache mempty
      writeIORef rsBSPInstanceCache mempty
      writeIORef rsCharacterCache mempty
      return (storage,renderer,mempty,mempty,mempty)

renderScene :: RenderSystem -> Float -> Scene -> IO ()
renderScene a b c = do
  --printTimeDiff "scene process time: " $ do
    renderScene' a b c
  --printTimeDiff "render time: " $ do
    renderFrame =<< readIORef (rsRenderer a)

data InstanceCache
  = InstanceCache
  { newMD3        :: !MD3InstanceCache
  , newBSP        :: !BSPInstanceCache
  , newCharacter  :: !CharacterCache
  , oldMD3        :: !MD3InstanceCache
  , oldBSP        :: !BSPInstanceCache
  , oldCharacter  :: !CharacterCache
  }

initCache :: MD3InstanceCache -> BSPInstanceCache -> CharacterCache -> InstanceCache
initCache = InstanceCache mempty mempty mempty

type RenderM = StateT InstanceCache IO

renderScene' :: RenderSystem -> Float -> Scene -> IO ()
renderScene' renderSystem@RenderSystem{..} effectTime Scene{..} = do
  -- load new models
  (newMD3Materials,newBSPMaterials,md3Cache,bspCache) <- updateModelCache renderSystem renderables

  -- check new materials
  (storage,renderer,md3InstanceCache,bspInstanceCache,characterCache) <- updateRenderCache renderSystem newMD3Materials newBSPMaterials

  -- create new instances
  let addInstance :: Renderable -> RenderM ()
      addInstance md3@(MD3 _ _ _ name) = gets oldMD3 >>= \old -> case HashMap.lookup name old of
        Just (model:_) -> do
          liftIO $ setupMD3Instance model md3
          modify' $ \s -> s {oldMD3 = HashMap.adjust tail name old}
        _ -> do
          model <- liftIO $ newMD3Instance name
          liftIO $ setupMD3Instance model md3
          modify' $ \s -> s {newMD3 = HashMap.insertWith (++) name [model] $ newMD3 s}

      addInstance (BSPMap name) = gets oldBSP >>= \old -> case HashMap.lookup name old of
        Just (model:_) -> do
          liftIO $ setupBSPInstance model
          modify' $ \s -> s {oldBSP = HashMap.adjust tail name old}
        _ -> do
          model <- liftIO $ newBSPInstance name
          liftIO $ setupBSPInstance model
          modify' $ \s -> s {newBSP = HashMap.insertWith (++) name [model] $ newBSP s}
      addInstance a@(MD3Character _ _ _ name skin) = gets oldCharacter >>= \old -> case HashMap.lookup (name,skin) old of
        Just (model:_) -> do
          liftIO $ setupCharacterInstance model a
          modify' $ \s -> s {oldCharacter = HashMap.adjust tail (name,skin) old}
        _ -> do
          model <- liftIO $ newCharacterInstance name skin
          liftIO $ setupCharacterInstance model a
          modify' $ \s -> s {newCharacter = HashMap.insertWith (++) (name,skin) [model] $ newCharacter s}
      addInstance _ = return () -- TODO

      newMD3Instance :: String -> IO MD3Instance
      newMD3Instance name = do
        -- creates new instance from model cache
        putStrLn $ "new instance: " ++ name
        addGPUMD3 storage (md3Cache HashMap.! name) mempty ["worldMat","entityRGB","entityAlpha"]

      newBSPInstance :: String -> IO BSPInstance
      newBSPInstance name = do
        -- creates new instance from model cache
        putStrLn $ "new instance: " ++ name
        bspInstance@BSPInstance{..} <- addGPUBSP rsWhiteTexture storage (bspCache HashMap.! name)
        -- set bsp map world matrix
        forM_ bspinstanceSurfaces $ mapM_ (\o -> uniformM44F "worldMat" (objectUniformSetter o) $ mat4ToM44F idmtx)
        return bspInstance

      newCharacterInstance :: String -> String -> IO CharacterInstance
      newCharacterInstance name skin = do
        -- creates new instance from model cache
        putStrLn $ printf "new instance: %s %s" name skin
        addCharacterInstance rsFileSystem storage name skin

      Camera{..} = camera

      setupMD3Instance :: MD3Instance -> Renderable -> IO ()
      setupMD3Instance MD3Instance{..} (MD3 position orientation rgba _) = do
        forM_ md3instanceObject $ \obj -> do
          enableObject obj $ pointInFrustum position cameraFrustum
          -- set model matrix
          uniformM44F "worldMat" (objectUniformSetter obj) . mat4ToM44F . fromProjective $ toWorldMatrix position orientation
          uniformV3F "entityRGB" (objectUniformSetter obj) . vec3ToV3F $ trim rgba
          uniformFloat "entityAlpha" (objectUniformSetter obj) $ _4 rgba

      setupBSPInstance :: BSPInstance -> IO ()
      setupBSPInstance BSPInstance{..} = do
        cullSurfaces bspinstanceBSPLevel cameraPosition cameraFrustum bspinstanceSurfaces

      setupCharacterInstance :: CharacterInstance -> Renderable -> IO ()
      setupCharacterInstance character (MD3Character position orientation rgba _ _) = do
        setupGameCharacter character effectTime cameraFrustum position orientation rgba

  InstanceCache{..} <- execStateT (mapM_ addInstance renderables) (initCache md3InstanceCache bspInstanceCache characterCache)
  writeIORef rsMD3InstanceCache $ HashMap.unionWith (++) md3InstanceCache newMD3
  writeIORef rsBSPInstanceCache $ HashMap.unionWith (++) bspInstanceCache newBSP
  writeIORef rsCharacterCache $ HashMap.unionWith (++) characterCache newCharacter

  -- hide unused instances
  let hideMD3 :: MD3Instance -> IO ()
      hideMD3 MD3Instance{..} = forM_ md3instanceObject $ flip enableObject False
  forM_ (concat $ HashMap.elems oldMD3) hideMD3
  forM_ (concat $ HashMap.elems oldBSP) $ \BSPInstance{..} -> forM_ bspinstanceSurfaces $ mapM_ (flip enableObject False)
  forM_ (concat $ HashMap.elems oldCharacter) $ \CharacterInstance{..} -> do
    hideMD3 characterinstanceHeadModel
    hideMD3 characterinstanceUpperModel
    hideMD3 characterinstanceLowerModel

  setFrameUniforms effectTime camera storage =<< readIORef rsAnimatedTextures

  --renderFrame renderer

setFrameUniforms :: Float -> Camera -> GLStorage -> [AnimatedTexture] -> IO ()
setFrameUniforms time Camera{..} storage animatedTextures = do
  -- set uniforms
  let uniMap = uniformSetter storage

      rot   = orthogonal $ leftOrthoU cameraOrientation
      view  = fromProjective $ translateBefore4 (neg cameraPosition) rot
      (w,h) = cameraViewportSize

      viewMat     = mat4ToM44F view
      viewProj    = mat4ToM44F $ view .*. cameraProjection
      orientation = mat4ToM44F . fromProjective $ rot

  uniformFloat "time"        uniMap time
  uniformV3F   "viewOrigin"  uniMap $ vec3ToV3F cameraPosition
  uniformM44F  "viewMat"     uniMap viewMat
  uniformM44F  "viewProj"    uniMap viewProj
  uniformM44F  "orientation" uniMap orientation

  setScreenSize storage (fromIntegral w) (fromIntegral h)

  forM_ animatedTextures $ \(animTime,texSetter,v) -> do
    let (_,i) = properFraction (time / animTime)
        idx = floor $ i * fromIntegral (V.length v)
    texSetter $ v V.! idx
