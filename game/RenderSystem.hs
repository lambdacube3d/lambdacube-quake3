{-# LANGUAGE RecordWildCards #-}
module RenderSystem where

import Control.Monad
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as LB

import GameEngine.Graphics.Render
import GameEngine.Loader.Zip
import GameEngine.Loader.MD3 (readMD3)
import RenderGame
import LambdaCube.GL

type MD3Cache = Map String GPUMD3
type Model = [Object]
type StorageCache = Map String Model

data RenderSystem
  = RenderSystem
  { rsFileSystem    :: Map String Entry
  , rsMD3Cache      :: IORef MD3Cache
  , rsStorageCache  :: IORef StorageCache
  }

{-
    create image cache; storage compatible
    create md3 cache;   storage compatible
    create debug pipeline
    create storage
    create storage object cache
-}
initRenderSystem :: Map String Entry -> IO RenderSystem
initRenderSystem pk3 = do
  md3Cache <- newIORef Map.empty
  storageCache <- newIORef Map.empty
  pure $ RenderSystem
    { rsFileSystem    = pk3
    , rsMD3Cache      = md3Cache
    , rsStorageCache  = storageCache
    }

loadMD3 pk3 name = case Map.lookup name pk3 of
  Nothing -> fail $ "file not found: " ++ name
  Just a -> readMD3 . LB.fromStrict <$> readEntry a >>= uploadMD3

setNub = Set.toList . Set.fromList

{-
  ok - collect models to load
  collect models to add storage

to render:
  setup uniforms: viewport size, q3 uniforms(time,...), camera matrix
-}
render :: RenderSystem -> [Renderable] -> IO ()
render RenderSystem{..} l = do
  md3Cache <- readIORef rsMD3Cache
  -- TODO:
  --  count models
  --  create new objects if there is not enough
  --    load models if it's not in cache
  let newModelNames = setNub [name | MD3 _ name <- l, Map.notMember name md3Cache]
  newModels <- forM newModelNames $ loadMD3 rsFileSystem
  let md3Cache' = md3Cache `Map.union` Map.fromList (zip newModelNames newModels)
  unless (null newModelNames) $ putStrLn $ unlines newModelNames
  writeIORef rsMD3Cache md3Cache'

{-
  -- init engine
  (inputSchema,levelData) <- engineInit pk3Data fullBSPName

  -- load level graphics data
  storage <- allocStorage inputSchema
  graphicsData <- setupStorage pk3Data levelData storage

  simpleRenderer <- fromJust <$> loadQuake3Graphics storage "SimpleGraphics.json"
  setStorage simpleRenderer storage

  -- main loop
        updateRenderInput graphicsData (camPos,camTarget,camUp) w h time noBSPCull
        renderFrame simpleRenderer
-}
