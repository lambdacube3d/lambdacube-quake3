{-# LANGUAGE LambdaCase, OverloadedStrings, RecordWildCards #-}
module GameEngine.Graphics.Storage where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.Set as Set
import qualified Data.Vector as V
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy as LB
import Data.Digest.CRC32
import Control.Monad
import System.FilePath
import System.Directory
import Text.Show.Pretty (ppShow)
import Text.Printf
import Data.Aeson (encode,eitherDecode)
import LambdaCube.Compiler (compileMain, Backend(..))

import LambdaCube.GL
import Codec.Picture hiding (decodeImage)

import GameEngine.Data.Material
import GameEngine.Loader.Zip
import GameEngine.Loader.Image
import GameEngine.Utils

import Paths_lambdacube_quake3

data TableTextures
  = TableTextures
  { ttSin             :: TextureData
  , ttSquare          :: TextureData
  , ttSawTooth        :: TextureData
  , ttInverseSawTooth :: TextureData
  , ttTriangle        :: TextureData
  , ttNoise           :: TextureData
  }

-- Utility code
initTableTextures :: IO TableTextures
initTableTextures = do
  let funcTableSize = 1024 :: Float
      sinTexture              = [sin (i*2*pi/(funcTableSize-1)) | i <- [0..funcTableSize-1]]
      squareTexture           = [if i < funcTableSize / 2 then 1 else -1 | i <- [0..funcTableSize-1]]
      sawToothTexture         = [i / funcTableSize | i <- [0..funcTableSize-1]]
      inverseSawToothTexture  = reverse [i / funcTableSize | i <- [0..funcTableSize-1]]
      triangleTexture         = l1 ++ map ((-1)*) l1 where
        n = funcTableSize / 4
        l0 = [i / n | i <- [0..n-1]]
        l1 = l0 ++ reverse l0
      noiseTexture = sinTexture -- TODO

      tableTexture t = uploadTexture2DToGPU' True False False False $ ImageRGB8 $ generateImage bitmap width 1 where
        width       = length t
        v           = V.fromList t
        bitmap x y  = let a = floor $ min 255 $ max 0 $ 128 + 128 * v V.! x in PixelRGB8 a a a

  TableTextures <$> tableTexture sinTexture
                <*> tableTexture squareTexture
                <*> tableTexture sawToothTexture
                <*> tableTexture inverseSawToothTexture
                <*> tableTexture triangleTexture
                <*> tableTexture noiseTexture

setupTableTextures :: Map GLUniformName InputSetter -> TableTextures -> IO ()
setupTableTextures uniformMap TableTextures{..} = do
  uniformFTexture2D "SinTable" uniformMap ttSin
  uniformFTexture2D "SquareTable" uniformMap ttSquare
  uniformFTexture2D "SawToothTable" uniformMap ttSawTooth
  uniformFTexture2D "InverseSawToothTable" uniformMap ttInverseSawTooth
  uniformFTexture2D "TriangleTable" uniformMap ttTriangle
  uniformFTexture2D "Noise" uniformMap ttNoise

loadQ3Texture :: Bool -> Bool -> TextureData -> Map String Entry -> String -> String -> IO TextureData
loadQ3Texture isMip isClamped defaultTex ar shName name = do
    let n1 = replaceExtension name "tga"
        n2 = replaceExtension name "jpg"
        b0 = Map.member name ar
        b1 = Map.member n1 ar
        b2 = Map.member n2 ar
        fname   = if b0 then name else if b1 then n1 else n2
    case Map.lookup fname ar of
        Nothing -> putStrLn (printf "unknown texure: %s in shader: %s" fname shName) >> return defaultTex
        Just entry  -> readEntry entry >>= decodeImage >>= \case
                Left msg -> putStrLn (printf "error: %s - %s" fname msg) >> return defaultTex
                Right img -> do
                  let (w,h) = case img of
                        ImageRGB8 (Image w h _) -> (w,h)
                        ImageRGBA8 (Image w h _) -> (w,h)
                        _ -> (0,0)
                  putStrLn $ printf "load (%u x %u): %s" w h fname
                  uploadTexture2DToGPU' True True isMip isClamped img

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
    textureData <- loadQ3Texture True True defaultTexture pk3Data mempty ("levelshots/" ++ bspName)
    setScreenSize storage (fromIntegral w) (fromIntegral h)
    updateUniforms storage $ do
      "LoadingImage" @= return textureData
    renderFrame renderer

createRenderInfo :: Map FilePath CommonAttrs -> HashSet FilePath -> HashSet FilePath -> (PipelineSchema, Map FilePath CommonAttrs)
createRenderInfo shMap' levelMaterials modelMaterials = (inputSchema,shMapTexSlot) where
  mkShader hasLightmap n = case Map.lookup n shMap' of
    Just s -> (n,s)
    Nothing -> let alias = dropExtension n in case Map.lookup alias shMap' of
      Just s -> (alias,s)
      Nothing -> (n,imageShader hasLightmap n)

  imageShader hasLightmap txName = defaultCommonAttrs {caStages = sa:if hasLightmap then saLM:[] else []} where
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

  shMap = Map.fromList [mkShader True n | n <- HashSet.toList levelMaterials] `Map.union`
          Map.fromList [mkShader False n | n <- HashSet.toList modelMaterials]

  shMapTexSlot = mangleCA <$> shMap
    where
      mangleStageTex stageTex = "Tex_" ++ show (crc32 $ SB.pack $ show stageTex)
      mangleCA ca = ca {caStages = mangleSA <$> caStages ca}
      mangleSA sa = sa {saTextureUniform = mangleStageTex sa}

  textureUniforms = Set.toList . Set.fromList . concat . map name . concat . map caStages $ Map.elems shMapTexSlot
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
  quake3SlotSchema =
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
    { objectArrays = Map.fromList $ ("CollisionShape",debugSlotSchema) : zip ("LightMapOnly":"missing shader": Map.keys shMap) (repeat quake3SlotSchema)
    , uniforms = Map.fromList $ [ ("viewProj",      M44F)
                                , ("worldMat",      M44F)
                                , ("viewMat",       M44F)
                                , ("orientation",   M44F)
                                , ("viewOrigin",    V3F)
                                , ("entityRGB",     V3F)
                                , ("entityAlpha",   Float)
                                , ("identityLight", Float)
                                , ("time",          Float)
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

writeSampleMaterial :: Map FilePath CommonAttrs -> IO ()
writeSampleMaterial shMapTexSlot = writeFile (lc_q3_cache </> "SampleMaterial.lc") $ unlines
  [ "module SampleMaterial where"
  , "import Material"
  , "sampleMaterial ="
  , unlines . map ("  "++) . lines . ppShow . Map.toList $ shMapTexSlot
  ]

addObjectWithMaterial :: GLStorage -> String -> Primitive -> Maybe (IndexStream Buffer) -> Map String (Stream Buffer) -> [String] -> IO Object
addObjectWithMaterial rndr name prim idx attrs unis = addObject rndr name' prim idx attrs' unis
  where
    attrs'  = Map.filterWithKey (\n _ -> elem n renderAttrs) attrs
    setters = objectArrays . schema $ rndr
    alias   = dropExtension name
    name'
      | Map.member name setters = name
      | Map.member alias setters = alias
      | otherwise = "missing shader"
    renderAttrs = Map.keys $ case Map.lookup name' setters of
        Just (ObjectArraySchema _ x)  -> x
        _           -> error $ "material not found: " ++ show name'
