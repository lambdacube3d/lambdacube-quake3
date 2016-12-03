{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards #-}
module GameEngine.Content where

import Control.Monad
import Data.Set (Set)
import Data.Map (Map)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as SB
import qualified Data.Map as Map
import qualified Data.Trie as T
import qualified Data.Set as Set
import qualified Data.Vector as V
import System.FilePath
import System.Directory
import Data.Char
import Data.List (isPrefixOf,elemIndex)
import Data.Vect
import Data.Maybe

import Control.Exception (evaluate)
import Control.DeepSeq

import GameEngine.Data.Items
import GameEngine.Data.GameCharacter
import GameEngine.Loader.GameCharacter
import GameEngine.Data.Material hiding (Vec3)
import GameEngine.Loader.ShaderParser
import qualified GameEngine.Loader.Entity as E
import qualified GameEngine.Data.MD3 as MD3
import qualified GameEngine.Loader.MD3 as MD3
import GameEngine.Loader.Zip
import GameEngine.Utils

mkWorldMat :: Float -> Float -> Float -> Proj4
mkWorldMat x y z = translation $ Vec3 x y z

mkWorldMat' :: Vec3 -> Proj4
mkWorldMat' = translation

loadPK3 :: IO (Map String Entry)
loadPK3 = do
  let takeExtensionCI = map toLower . takeExtension
      isPrefixOfCI a b = isPrefixOf a $ map toLower b
  Map.unions <$> (mapM readArchive =<< filter (\n -> ".pk3" == takeExtensionCI n) <$> getDirectoryContents ".")

shaderMap :: Map String Entry -> IO (T.Trie CommonAttrs)
shaderMap ar = do
  l <- sequence <$> forM [(n,e) | (n,e) <- Map.toList ar, ".shader" == takeExtension n, isPrefixOf "scripts" n] (\(n,e) -> parseShaders (eArchiveName e ++ ":" ++ n) <$> readEntry e)
  case l of
    Left err -> fail err
    Right (unzip -> (x,w)) -> do
      writeFile (lc_q3_cache </> "shader.log") $ unlines $ concat w
      return . T.fromList . concat $ x

readCharacters :: Map String Entry -> Vec3 -> IO (Set ByteString, [[(Proj4, (Map String String, String))]], [Character])
readCharacters pk3Data p0 = do
  let --characterNames = characterNamesFull
      characterNames = characterNamesDemo

      characterNamesFull = [ "anarki","biker","bitterman","bones","crash","doom","grunt","hunter","keel","klesk","lucy","major","mynx"
                           , "orbb","ranger","razor","sarge","slash","sorlag","tankjr","uriel","visor","xaero"
                           ]
      characterNamesDemo = ["major","visor","sarge","grunt"]

      readCharacterModelSkin name part = do
        let fname = "models/players/" ++ name ++ "/" ++ part ++ "_default.skin"
        txt <- case Map.lookup fname pk3Data of
          Nothing -> fail $ "missing skin: " ++ fname
          Just e -> readEntry e
        return $ Map.fromList
          [ (head k,head v)
          | l <- lines $ SB.unpack txt
          , i <- maybeToList $ elemIndex ',' l
          , let (words . map toLower -> k,words . map toLower . tail -> v) = splitAt i l
          , not . null $ k
          , not . null $ v
          ]

  characterSkinMap <- evaluate =<< (force . Map.fromList <$> sequence
    [ ((name,part),) <$> readCharacterModelSkin name part 
    | name <- characterNames
    , part <- ["head","upper","lower"]
    ])

  let characterModelSkin name part = case Map.lookup (name,part) characterSkinMap of
        Nothing -> error $ unwords ["missing skin for", name, "body part", part]
        Just skin -> skin

      characterSkinMaterials = Set.fromList $ concat [map SB.pack . Map.elems $ characterModelSkin name part | name <- characterNames, part <- ["head","upper","lower"]]
      characterObjs = [[(mkWorldMat (x + r * sin (angle i)) (y + r * cos (angle i)) z, m) | m <- ml] | (i,ml) <- zip [0..] characterModels]
        where
          r = (200 / 24) * (fromIntegral $ length characterNames)
          angle i = fromIntegral i / (fromIntegral $ length characterNames) * pi * 2
          Vec3 x y z = p0
          characterModels = [[(characterModelSkin name part,"models/players/" ++ name ++ "/" ++ part ++ ".md3") | part <- ["head","upper","lower"]] | name <- characterNames]

  charactersResult <- sequence <$> sequence
    [ parseCharacter fname <$> readEntry e
    | name <- characterNames
    , let fname = "models/players/" ++ name ++ "/animation.cfg"
          e = maybe (error $ "missing " ++ fname) id $ Map.lookup fname pk3Data
    ]
  case charactersResult of
    Right characters  -> return (characterSkinMaterials,characterObjs,characters)
    Left errorMessage -> fail errorMessage

handWeapon :: String
handWeapon = head $ drop 6 ["models/weapons2/" ++ n ++ "/"++ n ++ ".md3" | n <- weapons]
  where
    weapons = ["bfg","gauntlet","grapple","grenadel","lightning","machinegun","plasma","railgun","rocketl","shells","shotgun"]

readMD3Objects :: [[(Proj4, (Map String String, String))]]
                 -> [E.EntityData]
                 -> Map String Entry
                 -> IO (Set.Set ByteString, T.Trie MD3.MD3Model, [(Proj4, (Map String String, String))])
readMD3Objects characterObjs ents pk3Data = do
    let itemMap = T.fromList [(SB.pack $ itClassName it,it) | it <- items]
        collectObj E.EntityData{..} = case T.lookup (SB.pack classname) itemMap of
            Just i -> [(mat, (mempty,m)) | m <- Prelude.take cnt $ itWorldModel i, let mat = mkWorldMat' origin]
              where cnt = if itType i `elem` [IT_HEALTH, IT_POWERUP] then 2 else 1
            Nothing -> case model2 of
              Just m -> [(mkWorldMat' origin, (mempty,m))]
              Nothing -> []
        md3Objs = concatMap collectObj ents
    md3Map <- T.fromList <$> sequence
      [ (\a -> (SB.pack n,MD3.readMD3 $ LB.fromStrict a)) <$> readEntry m
      | n <- Set.toList . Set.fromList . map (snd . snd) $ md3Objs ++ concat characterObjs ++ [(mkWorldMat 0 0 0,(mempty,handWeapon))]
      , m <- maybeToList $ Map.lookup n pk3Data
      ]
    let md3Materials = Set.fromList . concatMap (concatMap (map MD3.shName . V.toList . MD3.srShaders) . V.toList . MD3.mdSurfaces) $ T.elems md3Map

    return (md3Materials,md3Map,md3Objs)
