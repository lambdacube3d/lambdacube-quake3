{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module GameEngine.Graphics.Storage where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector as V
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy as LB
import Control.Monad
import System.FilePath
import System.Directory
import Text.Show.Pretty (ppShow)
import Data.Aeson (encode,eitherDecode)
import LambdaCube.Compiler (compileMain, Backend(..))

import LambdaCube.GL
import Codec.Picture

import GameEngine.Loader.Zip
import GameEngine.Utils

import Paths_lambdacube_quake3

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

compileQuake3GraphicsCached :: FilePath -> IO Bool
compileQuake3GraphicsCached name = doesFileExist (lc_q3_cache </> name) >>= \case
  True -> putStrLn "use cached pipeline" >> return True
  False -> compileQuake3Graphics name

compileQuake3Graphics :: FilePath -> IO Bool
compileQuake3Graphics name = printTimeDiff "compile quake3 graphics pipeline..." $ do
  dataDir <- getDataDir
  -- check order: local lc folder, global lc folder
  -- local lc_q3.cache stores the generated SampleMaterial.lc with the level specific material description list
  compileMain [lc_q3_cache, "lc", dataDir </> "lc"] OpenGL33 "Graphics.lc" >>= \case 
    Left err -> putStrLn ("error: " ++ ppShow err) >> return False
    Right ppl -> LB.writeFile (lc_q3_cache </> name) (encode ppl) >> return True

loadQuake3Graphics :: GLStorage -> String -> IO (Maybe GLRenderer)
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

createLoadingScreen :: IO (GLStorage, GLRenderer, TextureData)
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

drawLoadingScreen :: Int -> Int -> (GLStorage, GLRenderer, TextureData) -> Map String Entry -> String -> IO ()
drawLoadingScreen w h (storage,renderer,defaultTexture) pk3Data bspName = do
    textureData <- loadQ3Texture True True defaultTexture pk3Data mempty (SB.pack $ "levelshots/" ++ bspName)
    setScreenSize storage (fromIntegral w) (fromIntegral h)
    updateUniforms storage $ do
      "LoadingImage" @= return textureData
    renderFrame renderer
