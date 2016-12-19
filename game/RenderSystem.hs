{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module RenderSystem where

import Control.Monad
import Data.Vect
import Data.Maybe (fromJust)
import Data.IORef
import Data.List (foldl')
import Data.Set (Set)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Char8 (unpack,ByteString)

import GameEngine.Utils
import GameEngine.Content (shaderMap)
import GameEngine.Data.Material (CommonAttrs)
import GameEngine.Graphics.Storage
import GameEngine.Graphics.Render
import GameEngine.Loader.Zip
import GameEngine.Loader.MD3 (readMD3)
import RenderGame
import LambdaCube.GL

type MD3Cache = Map String GPUMD3
type Model = [Object]
type InstanceCache = Map String [Model]
type ShaderCache = Set String
type ImageCache = Map String TextureData

data RenderSystem
  = RenderSystem
  { rsFileSystem    :: Map String Entry
  , rsShaderMap     :: Map String CommonAttrs
  , rsMD3Cache      :: IORef MD3Cache
  , rsInstanceCache :: IORef InstanceCache
  , rsShaderCache   :: IORef ShaderCache
  , rsImageCache    :: IORef ImageCache
  , rsRenderer      :: IORef GLRenderer
  , rsStorage       :: IORef GLStorage
  }

initRenderSystem :: Map String Entry -> IO RenderSystem
initRenderSystem pk3 = do
  md3Cache <- newIORef Map.empty
  instanceCache <- newIORef Map.empty
  shaderCache <- newIORef Set.empty
  imageCache <- newIORef Map.empty
  shMap <- shaderMap pk3
  let (inputSchema,_) = createRenderInfo shMap mempty mempty
  storage <- allocStorage inputSchema
  initStorageDefaultValues storage
  renderer <- fromJust <$> loadQuake3Graphics storage "SimpleGraphics.json"
  rendererRef <- newIORef renderer
  storageRef <- newIORef storage
  pure $ RenderSystem
    { rsFileSystem    = pk3
    , rsShaderMap     = shMap
    , rsMD3Cache      = md3Cache
    , rsInstanceCache = instanceCache
    , rsShaderCache   = shaderCache
    , rsImageCache    = imageCache
    , rsRenderer      = rendererRef
    , rsStorage       = storageRef
    }

loadMD3 pk3 name = case Map.lookup name pk3 of
  Nothing -> fail $ "file not found: " ++ name
  Just a -> readMD3 . LB.fromStrict <$> readEntry a >>= uploadMD3

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
  {-
    TODO:
      ok - extract new materials from new models
      if there is any new material:
        ok - clear instance cache
        ok - return (storage,instanceCache,renderer)
        ok - generate material list, generate new pipeline schema
        ok - setup instance uniforms
        ok - setup storage default uniforms values: tables, ...
        - load new images from new materials
        - compile new pipeline
        - update animated textures
  -}
      let (inputSchema,usedMaterials) = createRenderInfo rsShaderMap Set.empty shaderCache'
      storage <- allocStorage inputSchema
      initStorageDefaultValues storage
      renderer <- fromJust <$> loadQuake3Graphics storage "SimpleGraphics.json"
      setStorage renderer storage
      writeIORef rsStorage storage
      writeIORef rsRenderer renderer
      writeIORef rsInstanceCache mempty

      writeSampleMaterial usedMaterials
      instanceCache <- readIORef rsInstanceCache
      return (storage,instanceCache,renderer)

render :: RenderSystem -> [Renderable] -> IO ()
render renderSystem@RenderSystem{..} renderables = do
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

  setFrameUniforms storage

  renderFrame renderer

setFrameUniforms storage = do
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
      time = 1
      w = 800
      h = 600
      camPos = Vec3 0 0 3000
      camTarget = Vec3 0 0 0
      camUp = Vec3 0 1 0

  timeSetter $ time / 1
  viewOrigin $ V3 cx cy cz
  viewMat $ mat4ToM44F cm

  viewProj $! mat4ToM44F $! cm .*. sm .*. pm
  setScreenSize storage w h
