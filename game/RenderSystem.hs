{-# LANGUAGE RecordWildCards #-}
module RenderSystem where

import Control.Monad
import Data.IORef
import Data.List (foldl')
import Data.Set (Set)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as LB

import GameEngine.Graphics.Render
import GameEngine.Loader.Zip
import GameEngine.Loader.MD3 (readMD3)
import RenderGame
import LambdaCube.GL

type MD3Cache = Map String GPUMD3
type Model = [Object]
type InstanceCache = Map String [Model]
type ShaderCache = Set String

data RenderSystem
  = RenderSystem
  { rsFileSystem    :: Map String Entry
  , rsMD3Cache      :: IORef MD3Cache
  , rsInstanceCache :: IORef InstanceCache
  , rsShaderCache   :: IORef ShaderCache
  }

initRenderSystem :: Map String Entry -> IO RenderSystem
initRenderSystem pk3 = do
  md3Cache <- newIORef Map.empty
  instanceCache <- newIORef Map.empty
  shaderCache <- newIORef Set.empty
  pure $ RenderSystem
    { rsFileSystem    = pk3
    , rsMD3Cache      = md3Cache
    , rsInstanceCache = instanceCache
    , rsShaderCache   = shaderCache
    }

loadMD3 pk3 name = case Map.lookup name pk3 of
  Nothing -> fail $ "file not found: " ++ name
  Just a -> readMD3 . LB.fromStrict <$> readEntry a >>= uploadMD3

setNub = Set.toList . Set.fromList

{-
  things to cache
    MD3Name -> GPUMD3
    MD3Name -> Model
    Set ShaderName -> Pipeline
    
-}

{-
  in every frame collect info:
    collect
      MD3Names in Map MD3Name Int -- instance count
      Set ShaderName
    current `difference` collected
    newMaterials - when not null => recompile pipeline in new thread; rebuild storage
    newMD3s - add to model cache
    newMD3Instances - create new instances
-}

{-
  ok - collect models to load
  ok - collect models to add storage
  - create image cache
  - compile pipeline
  - create shader cache
  - create storage

to render:
  setup uniforms: viewport size, q3 uniforms(time,...), camera matrix
-}

updateMD3Cache RenderSystem{..} renderables = do
  md3Cache <- readIORef rsMD3Cache
  -- load new models
  let newModelNames = setNub [name | MD3 _ name <- renderables, Map.notMember name md3Cache]
  newModels <- forM newModelNames $ loadMD3 rsFileSystem
  let md3Cache' = md3Cache `Map.union` Map.fromList (zip newModelNames newModels)
  unless (null newModelNames) $ putStrLn $ unlines $ "new models:" : newModelNames
  writeIORef rsMD3Cache md3Cache'
  return (newModels,md3Cache')

updatePipeline = undefined
updateMaterials = undefined

render :: RenderSystem -> [Renderable] -> IO ()
render renderSystem@RenderSystem{..} renderables = do
  -- load new models
  (newModels,md3Cache) <- updateMD3Cache renderSystem renderables

  -- check new materials
  shaderCache <- readIORef rsShaderCache
  let newMaterials = Set.unions (map gpumd3Shaders newModels) `Set.difference` shaderCache
  when (Set.size newMaterials > 0) $ putStrLn $ unlines $ "new materials:" : Set.toList newMaterials
  let shaderCache' = shaderCache `Set.union` newMaterials
  writeIORef rsShaderCache shaderCache'

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
        --TODO: creates new instance from model cache
        putStrLn $ "new instance: " ++ name
        let storage = error "no storage"
        LCMD3{..} <- addMD3' storage (md3Cache Map.! name) mempty mempty
        return lcmd3Object

      setupInstance model (MD3 position _) = do
        forM_ model $ \obj -> enableObject obj True
        return ()

  instanceCache <- readIORef rsInstanceCache
  (newInstances,unusedInstances) <- foldM addInstance (Map.empty,instanceCache) renderables
  let instanceCache' = Map.unionWith (++) instanceCache newInstances
  writeIORef rsInstanceCache instanceCache'

  -- hide unused instances
  forM_ (concat . concat $ Map.elems unusedInstances) $ flip enableObject False

{-
  -- load level graphics data
  storage <- allocStorage inputSchema

  simpleRenderer <- fromJust <$> loadQuake3Graphics storage "SimpleGraphics.json"
  renderFrame simpleRenderer
-}
