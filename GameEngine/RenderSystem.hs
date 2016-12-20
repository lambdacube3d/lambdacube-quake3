{-# LANGUAGE RecordWildCards, OverloadedStrings, LambdaCase #-}
module GameEngine.RenderSystem
  ( RenderSystem
  , initRenderSystem
  , render
  , Renderable(..)
  , Scene(..)
  ) where

import Control.Monad
import Data.Vect hiding (Vector)
import Data.Maybe (fromJust)
import Data.IORef
import Data.List (foldl')
import Data.Set (Set)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Char8 (pack,unpack,ByteString)
import Codec.Picture
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Digest.CRC32 (crc32)

import GameEngine.Utils
import GameEngine.Content (shaderMap)
import GameEngine.Data.Material
import GameEngine.Graphics.Storage
import GameEngine.Graphics.Render
import GameEngine.Loader.Zip
import GameEngine.Loader.MD3 (readMD3)
import LambdaCube.GL

data Scene
  = Scene
  { renderables :: [Renderable]
  , camera      :: Mat4
  } deriving Show

{-
  renderable visual parameters:
    entityRGB Vec3
    entityAlpha Float
    worldMat Mat4
-}
data Renderable
  = MD3 Vec2 String -- model
  | BSPMap String
  | BSPModel String Int
  deriving Show

type MD3Cache = Map String GPUMD3
type Model = [Object]
type InstanceCache = Map String [Model]
type ShaderCache = Set String
type TextureCache = Map (String,Bool,Bool) TextureData
type AnimatedTexture = (Float, SetterFun TextureData, Vector TextureData)

data RenderSystem
  = RenderSystem
  { rsFileSystem        :: Map String Entry
  , rsShaderMap         :: Map String CommonAttrs
  , rsCheckerTexture    :: TextureData
  , rsMD3Cache          :: IORef MD3Cache
  , rsInstanceCache     :: IORef InstanceCache
  , rsShaderCache       :: IORef ShaderCache
  , rsTextureCache      :: IORef TextureCache
  , rsRenderer          :: IORef GLRenderer
  , rsStorage           :: IORef GLStorage
  , rsAnimatedTextures  :: IORef [AnimatedTexture]
  }

initRenderSystem :: Map String Entry -> IO RenderSystem
initRenderSystem pk3 = do
  md3Cache <- newIORef Map.empty
  instanceCache <- newIORef Map.empty
  shaderCache <- newIORef Set.empty
  textureCache <- newIORef Map.empty
  shMap <- shaderMap pk3
  let (inputSchema,_) = createRenderInfo shMap mempty mempty
  storage <- allocStorage inputSchema
  initStorageDefaultValues storage
  renderer <- fromJust <$> loadQuake3Graphics storage "SimpleGraphics.json"
  rendererRef <- newIORef renderer
  storageRef <- newIORef storage
  animatedTextures <- newIORef []
  -- default texture
  let redBitmap x y = let v = if (x+y) `mod` 2 == 0 then 255 else 0 in PixelRGB8 v v 0
  checkerTexture <- uploadTexture2DToGPU' False False False False $ ImageRGB8 $ generateImage redBitmap 2 2
  pure $ RenderSystem
    { rsFileSystem        = pk3
    , rsShaderMap         = shMap
    , rsCheckerTexture    = checkerTexture
    , rsMD3Cache          = md3Cache
    , rsInstanceCache     = instanceCache
    , rsShaderCache       = shaderCache
    , rsTextureCache      = textureCache
    , rsRenderer          = rendererRef
    , rsStorage           = storageRef
    , rsAnimatedTextures  = animatedTextures
    }

loadMD3 pk3 name = case Map.lookup name pk3 of
  Nothing -> fail $ "file not found: " ++ name
  Just a -> readMD3 . LB.fromStrict <$> readEntry a >>= uploadMD3

setNub :: Ord a => [a] -> [a]
setNub = Set.toList . Set.fromList

initStorageDefaultValues storage = do
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

updateMD3Cache RenderSystem{..} renderables = do
  md3Cache <- readIORef rsMD3Cache
  -- load new models
  let newModelNames = setNub [name | MD3 _ name <- renderables, Map.notMember name md3Cache]
  newModels <- forM newModelNames $ loadMD3 rsFileSystem
  let md3Cache' = md3Cache `Map.union` Map.fromList (zip newModelNames newModels)
  unless (null newModelNames) $ putStrLn $ unlines $ "new models:" : newModelNames
  writeIORef rsMD3Cache md3Cache'
  return (newModels,md3Cache')

initStorageTextures RenderSystem{..} storage usedMaterials = do
  let usedTextures = setNub [ (name,saTexture,saTextureUniform,caNoMipMaps)
                            | (name,CommonAttrs{..}) <- Map.toList usedMaterials
                            , StageAttrs{..} <- caStages
                            ]
      cachedTexture isMip isClamped shaderName imageName = do
        textureCache <- readIORef rsTextureCache
        let key = (imageName,isMip,isClamped)
        case Map.lookup key textureCache of
          Just texture -> return texture
          Nothing -> do
            texture <- loadQ3Texture isMip isClamped rsCheckerTexture rsFileSystem shaderName imageName
            writeIORef rsTextureCache $ Map.insert key texture textureCache
            return texture
  putStrLn "loading textures:"
  -- load textures
  animatedTextures <- fmap concat $ forM usedTextures $ \(shName,stageTex,texSlotName,noMip) -> do
      let texSetter = uniformFTexture2D (pack texSlotName) (uniformSetter storage)
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

updateRenderCache renderSystem@RenderSystem{..} newModels = do
  shaderCache <- readIORef rsShaderCache
  let newMaterials = Set.unions (map gpumd3Shaders newModels) `Set.difference` shaderCache
  case Set.size newMaterials == 0 of
    True -> do
      instanceCache <- readIORef rsInstanceCache
      storage <- readIORef rsStorage
      renderer <- readIORef rsRenderer
      return (storage,instanceCache,renderer)
    False -> do
      putStrLn $ unlines $ "new materials:" : Set.toList newMaterials
      let shaderCache' = shaderCache `Set.union` newMaterials
      writeIORef rsShaderCache shaderCache'

      let (inputSchema,usedMaterials) = createRenderInfo rsShaderMap Set.empty shaderCache'
      storage <- allocStorage inputSchema
      -- TODO: load new images and set storage texture uniforms
      initStorageTextures renderSystem storage usedMaterials
      initStorageDefaultValues storage
      writeIORef rsStorage storage
      writeIORef rsInstanceCache mempty

      writeSampleMaterial usedMaterials
      let filename = show (crc32 . pack $ show usedMaterials) ++ "_ppl.json"
      compileQuake3GraphicsCached filename >>= \ok -> unless ok $ fail "no renderer"
      renderer <- fromJust <$> loadQuake3Graphics storage filename
      setStorage renderer storage
      writeIORef rsRenderer renderer
      return (storage,mempty,renderer)

render :: RenderSystem -> Float -> Scene -> IO ()
render renderSystem@RenderSystem{..} time Scene{..} = do
  -- load new models
  (newModels,md3Cache) <- updateMD3Cache renderSystem renderables

  -- check new materials
  (storage,instanceCache,renderer) <- updateRenderCache renderSystem newModels

  -- create new instances
  let addInstance (new,old) md3@(MD3 _ name) = case Map.lookup name old of
        Just (model:_) -> do
          setupInstance model md3
          return (new,Map.adjust tail name old)

        _ -> do
          model <- newInstance name
          setupInstance model md3
          return (Map.insertWith (++) name [model] new,old)

      newInstance name = do
        -- creates new instance from model cache
        putStrLn $ "new instance: " ++ name
        LCMD3{..} <- addGPUMD3 storage (md3Cache Map.! name) mempty ["worldMat"]
        return lcmd3Object

      setupInstance model (MD3 (Vec2 x y) _) = do
        forM_ model $ \obj -> do
          enableObject obj True
          -- set model matrix
          uniformM44F "worldMat" (objectUniformSetter obj) $ mat4ToM44F $ fromProjective $ (translation $ Vec3 x y 0)

  (newInstances,unusedInstances) <- foldM addInstance (Map.empty,instanceCache) renderables
  writeIORef rsInstanceCache $ Map.unionWith (++) instanceCache newInstances

  -- hide unused instances
  forM_ (concat . concat $ Map.elems unusedInstances) $ flip enableObject False

  setFrameUniforms time camera storage =<< readIORef rsAnimatedTextures

  renderFrame renderer

setFrameUniforms :: Float -> Mat4 -> GLStorage -> [AnimatedTexture] -> IO ()
setFrameUniforms time camera storage animatedTextures = do
  -- set uniforms
  let slotU = uniformSetter storage
      viewProj    = uniformM44F "viewProj" slotU
      viewOrigin  = uniformV3F "viewOrigin" slotU
      orientation = uniformM44F "orientation" slotU
      viewMat     = uniformM44F "viewMat" slotU
      timeSetter  = uniformFloat "time" slotU

      cm = fromProjective (lookat camPos camTarget camUp)
      pm = perspective near far (fovDeg / 180 * pi) (fromIntegral w / fromIntegral h)
      sm = fromProjective (scaling $ Vec3 s s s)
      s  = 0.005
      Vec3 cx cy cz = camPos
      near = 0.00001/s
      far  = 100/s
      fovDeg = 60
      --frust = frustum fovDeg (fromIntegral w / fromIntegral h) near far camPos camTarget camUp
      --time = 1
      w = 800
      h = 600
      camPos = Vec3 0 0 1000
      camTarget = Vec3 0 0 0
      camUp = Vec3 0 1 0

  timeSetter $ time / 1
  viewOrigin $ V3 cx cy cz
  viewMat $ mat4ToM44F cm

  viewProj $! mat4ToM44F camera -- $! cm .*. sm .*. pm
  setScreenSize storage w h

  forM_ animatedTextures $ \(animTime,texSetter,v) -> do
    let (_,i) = properFraction (time / animTime)
        idx = floor $ i * fromIntegral (V.length v)
    texSetter $ v V.! idx
