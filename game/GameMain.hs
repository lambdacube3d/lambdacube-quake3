{-# LANGUAGE RecordWildCards, PackageImports #-}
import Control.Monad
import Control.Arrow
import Control.Monad.State.Strict
import System.Random.Mersenne.Pure64

import Lens.Micro.Platform
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33

import System.FilePath
import System.Directory
import System.Environment

import Data.Char
import Data.Map (Map)
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as Map

import GameEngine.Utils (lc_q3_cache)
import GameEngine.Content (loadPK3)
import GameEngine.Data.BSP
import GameEngine.Loader.BSP
import GameEngine.Loader.Zip
import qualified GameEngine.Loader.Entity as E

import World
import GameLogic
import LoadEntities
import RenderGame
import GameEngine.RenderSystem

data Event
  = Event
  { ksMoveForward   :: Bool
  , ksMoveBackward  :: Bool
  , ksMoveRight     :: Bool
  , ksMoveLeft      :: Bool
  , ksShoot         :: Bool
  , ksMousePosition :: (Float,Float)
  , ksWindowSize    :: (Int,Int)
  }

inputFun :: Event -> World -> World
inputFun Event{..} w = w & wInput .~ i' where
  f True = 300
  f False = 0

  i@Input{..} = w^.wInput
  i' = i
    { forwardmove = f ksMoveForward - f ksMoveBackward
    , sidemove    = f ksMoveRight - f ksMoveLeft
    , shoot       = ksShoot
    , mouseX      = fst ksMousePosition
    , mouseY      = snd ksMousePosition
    , windowWidth   = fst ksWindowSize
    , windowHeight  = snd ksWindowSize
    }

mapTuple :: (a -> b) -> (a,a) -> (b,b)
mapTuple = join (***)

main = do
  (pk3,ents,mapfile) <- loadMap
  putStrLn $ "entity count: " ++ show (length ents)
  play pk3 (initWorld ents mapfile $ pureMT 123456789) renderFun inputFun stepFun

play :: Map String Entry -> World -> (World -> Scene) -> (Event -> World -> World) -> (RenderSystem -> Float -> World -> World) -> IO ()
play pk3 world0 getScene processInput stepWorld = do
  -- init graphics
  win <- initWindow "LambdaCube 3D Shooter" 800 600
  renderSystem <- initRenderSystem pk3

  let keyIsPressed k = fmap (==KeyState'Pressed) $ getKey win k
      deltaTime = 1/60
      loop oldFraction oldTime oldWorld = do
        -- process input
        pollEvents
        newTime <- maybe 0 realToFrac <$> getTime
        ks <- Event <$> keyIsPressed Key'W
                    <*> keyIsPressed Key'S
                    <*> keyIsPressed Key'D
                    <*> keyIsPressed Key'A
                    <*> keyIsPressed Key'Space
                    <*> (mapTuple realToFrac <$> getCursorPos win)
                    <*> getWindowSize win
        quit <- keyIsPressed Key'Escape

        -- step simulation
        let frameTime = newTime - oldTime
            batchedTime = frameTime + oldFraction
        when (batchedTime > 10) $ putStrLn $ "WARNING: slow machine!"
        let stepSimulation t w | t < deltaTime = (t,w)
                               | otherwise = stepSimulation (t - deltaTime) (stepWorld renderSystem deltaTime w)
            (newFractionTime,newWorld) = stepSimulation batchedTime (processInput ks oldWorld)

        -- render current state
        renderScene renderSystem newTime $ getScene newWorld
        swapBuffers win
        unless quit $ loop newFractionTime newTime newWorld
  time0 <- maybe 0 realToFrac <$> getTime
  loop 0 time0 world0

  destroyWindow win

loadMap = do
  -- init PK3 database
  noPak0_pk3 <- null . filter (\n -> "pak0.pk3" == map toLower n) <$> getDirectoryContents "."
  when noPak0_pk3 $ die "Could not find pak0.pk3. See how to run: https://github.com/lambdacube3d/lambdacube-quake3/blob/master/README.md"

  pk3Data <- loadPK3
  args <- getArgs
  let bspNames = [n | n <- Map.keys pk3Data, ".bsp" == takeExtension n]
  fullBSPName <- head <$> case args of
    (n:xs) -> return $ filter ((== n) . takeBaseName) bspNames
    _ -> do
          let maps = map takeBaseName bspNames
          putStrLn $ "Available maps:"
          putStrLn $ unwords maps
          putStrLn "Enter map name:"
          name <- getLine
          return $ filter ((name ==) . takeBaseName) bspNames

  let bspName = takeBaseName fullBSPName
      bspEntry = case Map.lookup fullBSPName pk3Data of
        Nothing -> error $ "file not found: " ++ fullBSPName
        Just bspd -> bspd

  -- load bsp data
  bsp <- readBSP . LB.fromStrict <$> readEntry bspEntry

  createDirectoryIfMissing True lc_q3_cache -- create cache

  SB.writeFile (lc_q3_cache </> bspName ++ ".entities") $ blEntities bsp

  -- extract spawn points
  let ents = case E.parseEntities bspName $ blEntities bsp of
          Left err -> error err
          Right x -> x
  return (pk3Data,loadEntities ents,fullBSPName)

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
    swapInterval 0
    return win

{-
main :: IO ()
main = do

  -- loading screen
  loadingScreen <- createLoadingScreen
  (w,h) <- getWindowSize win
  drawLoadingScreen w h loadingScreen pk3Data bspName
  swapBuffers win
  pollEvents

  -- init audio
  initAudio 64 44100 1024

  -- init engine
  (inputSchema,levelData) <- engineInit pk3Data fullBSPName

  -- load level graphics data
  storage <- allocStorage inputSchema
  graphicsData <- setupStorage pk3Data levelData storage

  simpleRenderer <- fromJust <$> loadQuake3Graphics storage "SimpleGraphics.json"
  setStorage simpleRenderer storage

  -- main loop
  let keyIsPressed k = fmap (==KeyState'Pressed) $ getKey win k
      loop = do
        -- process input
        time <- maybe 0 id <$> getTime
        setTime 0
        (w,h) <- getWindowSize win
        noBSPCull <- keyIsPressed (Key'X)
        updateRenderInput graphicsData (camPos,camTarget,camUp) w h time noBSPCull
        -- render
        renderFrame simpleRenderer
        swapBuffers win
        pollEvents

  setTime 0
  loop

  -- close audio and graphics
  finishAudio
  destroyWindow win
-}
