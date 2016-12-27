{-# LANGUAGE ViewPatterns, TupleSections, RecordWildCards #-}
module Content where

import Control.Monad
import Text.Printf
import Data.Set (Set)
import Data.Map (Map)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Char8 as SB
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import System.FilePath
import System.Directory
import Data.Char
import Data.List (isPrefixOf,elemIndex,stripPrefix)
import Data.Vect
import Data.Vect.Float.Instances
import Data.Maybe

import Control.Exception (evaluate)
import Control.DeepSeq

import Items
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

readCharacters :: Map String Entry -> Vec3 -> IO (Set ByteString, [[(Proj4, (Map String String, String))]], [Character])
readCharacters pk3Data p0 = do
  let characterNames = Set.toList $ Set.fromList
        [ (name, skin)
        | path <- Map.keys pk3Data
        , entry <- maybeToList $ stripPrefix "models/players/" path
        , let name = takeWhile (/='/') entry
        , not . null $ name
        , skin <- characterSkins name
        , all (flip Map.member pk3Data) $ characterFiles name skin
        ]
      characterSkins name = [tail . dropWhile (/='_') $ takeBaseName n | n <- Map.keys pk3Data, isPrefixOf prefix n, ".skin" == takeExtension n ] where prefix = printf "models/players/%s/" name
      characterFiles name skin = printf "models/players/%s/animation.cfg" name : concat
                            [ [ printf "models/players/%s/%s_%s.skin" name part skin
                              , printf "models/players/%s/%s.md3" name part
                              ]
                            | part <- characterParts
                            ]
      characterParts = ["head","upper","lower"]
{-
      characterNamesFull = [ "anarki","biker","bitterman","bones","crash","doom","grunt","hunter","keel","klesk","lucy","major","mynx"
                           , "orbb","ranger","razor","sarge","slash","sorlag","tankjr","uriel","visor","xaero"
                           ]
      characterNamesDemo = ["major","visor","sarge","grunt"]
-}
      readCharacterModelSkin name skin part = do
        let fname = printf "models/players/%s/%s_%s.skin" name part skin
        txt <- case Map.lookup fname pk3Data of
          Nothing -> fail $ "missing skin: " ++ fname
          Just e -> readEntry e
        return $ Map.fromList
          [ (head k,head v)
          | l <- lines . map toLower $ SB.unpack txt
          , i <- maybeToList $ elemIndex ',' l
          , let (words -> k,words . tail -> v) = splitAt i l
          , not . null $ k
          , not . null $ v
          ]

  characterSkinMap <- evaluate =<< (force . Map.fromList <$> sequence
    [ ((name,skin,part),) <$> readCharacterModelSkin name skin part
    | (name,skin) <- characterNames
    , part <- characterParts
    ])

  let characterModelSkin name skin part = case Map.lookup (name,skin,part) characterSkinMap of
        Nothing -> error $ unwords ["missing skin for", name, "body part", part]
        Just skin -> skin

      characterSkinMaterials = Set.fromList $ concat [map SB.pack . Map.elems $ characterModelSkin name skin part | (name,skin) <- characterNames, part <- ["head","upper","lower"]]
      characterObjs = [[(mkWorldMat' (p0 + pos i), m) | m <- ml] | (i,ml) <- zip [0..] characterModels]
        where
          step = 80
          size = floor . sqrt . fromIntegral $ length characterModels
          half = size `div` 2
          pos i = Vec3 (fromIntegral $ (x - half) * step) (fromIntegral $ (y - half) * step) 0 where (x,y) = i `divMod` size
          --r = (200 / 24) * (fromIntegral $ length characterNames)
          --angle i = fromIntegral i / (fromIntegral $ length characterNames) * pi * 2
          --Vec3 x y z = p0
          characterModels = [[(characterModelSkin name skin part,printf "models/players/%s/%s.md3" name part) | part <- ["head","upper","lower"]] | (name,skin) <- characterNames]

  charactersResult <- sequence <$> sequence
    [ parseCharacter fname <$> readEntry e
    | (name,skin) <- characterNames
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
                 -> IO (Set ByteString, Map ByteString MD3.MD3Model, [(Proj4, (Map String String, String))])
readMD3Objects characterObjs ents pk3Data = do
    let itemMap = Map.fromList [(SB.pack $ itClassName it,it) | it <- items]
        collectObj E.EntityData{..} = case Map.lookup (SB.pack classname) itemMap of
            Just i -> [(mat, (mempty,m)) | m <- Prelude.take cnt $ itWorldModel i, let mat = mkWorldMat' origin]
              where cnt = case itType i of
                      IT_HEALTH     -> 2
                      IT_POWERUP _  -> 2
                      _ -> 1
            Nothing -> case model2 of
              Just m -> [(mkWorldMat' origin, (mempty,m))]
              Nothing -> []
        md3Objs = concatMap collectObj ents
    md3Map <- Map.fromList <$> sequence
      [ (\a -> (SB.pack n,MD3.readMD3 $ LB.fromStrict a)) <$> readEntry m
      | n <- Set.toList . Set.fromList . map (snd . snd) $ md3Objs ++ concat characterObjs ++ [(mkWorldMat 0 0 0,(mempty,handWeapon))]
      , m <- maybeToList $ Map.lookup n pk3Data
      ]
    let md3Materials = Set.fromList . concatMap (concatMap (map MD3.shName . V.toList . MD3.srShaders) . V.toList . MD3.mdSurfaces) $ Map.elems md3Map

    return (md3Materials,md3Map,md3Objs)
