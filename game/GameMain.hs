{-# LANGUAGE RecordWildCards, PackageImports #-}
module Main where

import GHC.IO (failIO)
import Control.Monad
import Control.Arrow
import Control.Monad.State.Strict
import System.Random.Mersenne.Pure64

import Lens.Micro.Platform
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.GL.Core33
import Data.Binary (encode, decode)

import System.FilePath
import System.Directory
import System.Environment
import qualified System.Mem

import Data.Char
import Data.Map (Map)
import Data.List (find)
import Data.Vect.Float.Base hiding(Vector)
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
import LoadResources
import RenderGame
import GameEngine.RenderSystem


data Event
  = Event
  { ksMoveForward   :: !Bool
  , ksMoveBackward  :: !Bool
  , ksMoveRight     :: !Bool
  , ksMoveLeft      :: !Bool
  , ksShoot         :: !Bool
  , ksNumKey        :: !(Maybe Int)
  , ksHoldableKey   :: !(Maybe Int)
  , ksMousePosition :: !(Float,Float)
  }

inputFun :: Event -> World -> World
inputFun Event{..} w = w & wInput .~ i' where
  f True = 300
  f False = 0

  oldMU = mouseU i
  oldMV = mouseV i
  dx = newMouseX - mouseX i
  dy = newMouseY - mouseY i
  newMouseX = fst ksMousePosition
  newMouseY = snd ksMousePosition
  i = w^.wInput
  i' = i
    { forwardmove = f ksMoveForward - f ksMoveBackward
    , sidemove    = f ksMoveRight - f ksMoveLeft
    , shoot       = ksShoot
    , mouseX      = newMouseX
    , mouseY      = newMouseY
	, mouseU      = oldMU  - dx / 100
	, mouseV      = clamp 0.1 3.1 $ oldMV + dy / 100
    , changeWeapon  = do { key <- ksNumKey; Map.lookup key weaponKeys }
    , toggleHoldable = do { key <- ksHoldableKey; Map.lookup key holdableKeys }
    }

mapTuple :: (a -> b) -> (a,a) -> (b,b)
mapTuple = join (***)

main = do
  (pk3,ents,mapfile) <- loadMap
  putStrLn $ "entity count: " ++ show (length ents)
  play pk3 (initWorld ents mapfile $ pureMT 123456789) renderFun inputFun stepFun logPlayerChange

noLog _ _ = Nothing

play :: Map String Entry
     -> World
     -> (RenderSettings -> WorldSnapshot -> Scene)
     -> (Event -> World -> World)
     -> (RenderSystem -> Float -> World -> World)
     -> (World -> World -> Maybe String)
     -> IO ()
play pk3 world0 getScene processInput stepWorld logWorldChange = do
  -- init graphics
  win <- initWindow "LambdaCube 3D Shooter" 1920 1080
  renderSystem <- initRenderSystem pk3
  loadResources renderSystem (worldResources world0 ++ hudResources) []

  let keyIsPressed k = fmap (==KeyState'Pressed) $ getKey win k

      mapKeyPressed keyAssoc = (fmap fst . find snd) <$> mapM (\(n, k) -> (,) n <$> keyIsPressed k) keyAssoc

      numKeyPressed = mapKeyPressed
        [ (0,Key'0), (1,Key'1), (2,Key'2), (3,Key'3), (4,Key'4), (5,Key'5), (6,Key'6)
        , (7,Key'7), (8,Key'8), (9,Key'9)
        ]

      holdableKeyPressed = mapKeyPressed
        [ (0, Key'Q), (1, Key'W), (2, Key'E), (3, Key'R), (4, Key'T) ]

      log oldWorld newWorld = maybe (pure ()) putStrLn $ logWorldChange oldWorld newWorld
      deltaTime = 1/60
      loop firstFrame oldFraction oldTime oldWorld = do
        -- process input
        pollEvents
        newTime <- maybe 0 realToFrac <$> getTime
        ks <- Event <$> keyIsPressed Key'W
                    <*> keyIsPressed Key'S
                    <*> keyIsPressed Key'D
                    <*> keyIsPressed Key'A
                    <*> keyIsPressed Key'Space
                    <*> numKeyPressed
                    <*> holdableKeyPressed
                    <*> (mapTuple realToFrac <$> getCursorPos win)
        quit <- keyIsPressed Key'Escape

        -- step simulation
        let frameTime = newTime - oldTime
            batchedTime = frameTime + oldFraction
        when (batchedTime > 10) $ putStrLn $ "WARNING: slow machine!"
        let stepSimulation t w | t < deltaTime = (t,w)
                               | otherwise = stepSimulation (t - deltaTime) (stepWorld renderSystem deltaTime w)
            (newFractionTime,newWorld) = stepSimulation batchedTime (processInput ks oldWorld)

        -- render current state
        (windowWidth, windowHeight) <- getFramebufferSize win
        let renderSettings = RenderSettings
              { windowWidth   = windowWidth
              , windowHeight  = windowHeight
              , sceneTime     = (newWorld ^. wInput . to time)
              , mapFile       = (newWorld ^. wMapFile)
              }
            worldSnapshot = WorldSnapshot
              { gameEntities = (newWorld ^. wEntities)
              }
            worldSnapshotData = encode worldSnapshot
            receivedWorldSnapshot = decode worldSnapshotData
        renderScene renderSystem newTime $ getScene renderSettings receivedWorldSnapshot
        swapBuffers win
        log oldWorld newWorld

        if firstFrame -- cleanup dead data
          then System.Mem.performGC
          else pure () -- System.Mem.performMinorGC

        unless quit $ loop False newFractionTime newTime newWorld
  time0 <- maybe 0 realToFrac <$> getTime
  loop True 0 time0 world0

  destroyWindow win

loadMap = do
  -- init PK3 database
  noPak0_pk3 <- null . filter (\n -> "pak0.pk3" == map toLower n) <$> getDirectoryContents "."
  when noPak0_pk3 $ failIO "Could not find pak0.pk3. See how to run: https://github.com/lambdacube3d/lambdacube-quake3/blob/master/README.md"

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
  let ents = case E.parseEntities bspName $ SB.unpack $ blEntities bsp of
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
