{-# LANGUAGE ViewPatterns #-}
module GameEngine.Content where

import Control.Monad
import Data.Char
import Data.List (isPrefixOf,elemIndex,stripPrefix)
import System.Directory
import System.FilePath
import Text.Printf
import Data.Map (Map)
import qualified Data.Map as Map

import GameEngine.Data.Material hiding (Vec3)
import GameEngine.Loader.ShaderParser
import GameEngine.Loader.Zip
import GameEngine.Utils

loadPK3 :: IO (Map String Entry)
loadPK3 = do
  let takeExtensionCI = map toLower . takeExtension
      isPrefixOfCI a b = isPrefixOf a $ map toLower b
  Map.unions <$> (mapM readArchive =<< filter (\n -> ".pk3" == takeExtensionCI n) <$> getDirectoryContents ".")

loadShaderMap :: Map String Entry -> IO (Map String CommonAttrs)
loadShaderMap ar = do
  l <- sequence <$> forM [(n,e) | (n,e) <- Map.toList ar, ".shader" == takeExtension n, isPrefixOf "scripts" n] (\(n,e) -> parseShaders (eArchiveName e ++ ":" ++ n) <$> readEntry e)
  case l of
    Left err -> fail err
    Right (unzip -> (x,w)) -> do
      let shaders = Map.fromList . concat $ x
      writeFile (lc_q3_cache </> "shader.log") $ unlines $ printf "%d shaders" (Map.size shaders) : concat w
      return shaders
